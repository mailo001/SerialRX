LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;
USE ieee.std_logic_misc.ALL;

ENTITY SERIAL_RX IS
  GENERIC (
    F_ZEGARA : NATURAL := 20_000_000; -- czestotliwosc zegata w [Hz]
    L_BODOW : NATURAL := 9600; -- predkosc nadawania w [bodach]
    B_SLOWA : NATURAL := 8; -- liczba bitow slowa danych (5-8)
    B_PARZYSTOSCI : NATURAL := 1; -- liczba bitow parzystosci (0-1)
    B_STOPOW : NATURAL := 2; -- liczba bitow stopu (1-2)
    N_RX : BOOLEAN := FALSE; -- negacja logiczna sygnalu szeregowego
    N_SLOWO : BOOLEAN := FALSE -- negacja logiczna slowa danych
  );
  PORT (
    R : IN STD_LOGIC; -- sygnal resetowania
    C : IN STD_LOGIC; -- zegar taktujacy
    RX : IN STD_LOGIC; -- odebrany sygnal szeregowy
    SLOWO : OUT STD_LOGIC_VECTOR(B_SLOWA - 1 DOWNTO 0); -- odebrane slowo danych
    GOTOWE : OUT STD_LOGIC; -- flaga potwierdzenia odbioru
    BLAD : OUT STD_LOGIC -- flaga wykrycia bledu w odbiorze
  );
END SERIAL_RX;

ARCHITECTURE behavioural OF SERIAL_RX IS

  SIGNAL wejscie : STD_LOGIC_VECTOR(0 TO 1); -- podwojny rejestr sygnalu RX

  TYPE ETAP IS (CZEKANIE, START, DANA, PARZYSTOSC, STOP); -- lista etapow pracy odbiornika
  SIGNAL stan : ETAP; -- rejestr maszyny stanow odbiornika

  CONSTANT T : POSITIVE := F_ZEGARA/L_BODOW - 1; -- czas jednego bodu - liczba taktów zegara
  SIGNAL l_czasu : NATURAL RANGE 0 TO T; -- licznik czasu jednego bodu
  SIGNAL l_bitow : NATURAL RANGE 0 TO B_SLOWA - 1; -- licznik odebranych bitow danych lub stopu

  SIGNAL bufor : STD_LOGIC_VECTOR(SLOWO'RANGE); -- rejestr kolejno odebranych bitow danych
  SIGNAL problem : STD_LOGIC; -- rejestr (flaga) wykrytego bledu odbioru

BEGIN

  PROCESS (R, C) IS -- proces odbiornika
  BEGIN -- cialo procesu odbiornika

    IF (R = '1') THEN -- asynchroniczna inicjalizacja rejestrow
      wejscie <= (OTHERS => '0'); -- wyzerowanie rejestru sygnalu RX
      stan <= CZEKANIE; -- poczatkowy stan pracy odbiornika
      l_czasu <= 0; -- wyzerowanie licznika czasu bodu
      l_bitow <= 0; -- wyzerowanie licznika odebranych bitow
      bufor <= (OTHERS => '0'); -- wyzerowanie bufora bitow danych
      problem <= '0'; -- wyzerowanie rejestru bledu odbioru			
      SLOWO <= (OTHERS => '0'); -- wyzerowanie wyjsciowego slowa danych
      GOTOWE <= '0'; -- wyzerowanie flagi potwierdzenia odbioru
      BLAD <= '0'; -- wyzerowanie flagi wykrycia bledu w odbiorze

    ELSIF (rising_edge(C)) THEN -- synchroniczna praca odbiornika

      GOTOWE <= '0'; -- defaultowe skasowanie flagi potwierdzenia odbioru
      BLAD <= '0'; -- defaultowe skasowanie flagi wykrycia bledu w odbiorze
      wejscie(0) <= RX; -- zarejestrowanie synchroniczne stanu sygnalu RX
      IF (N_RX = TRUE) THEN -- badanie warunku zanegowania sygnalu szeregowego
        wejscie(0) <= NOT(RX); -- zarejestrowanie synchroniczne zanegowanego sygnalu RX
      END IF; -- zakonczenie instukcji warunkowej  
      wejscie(1) <= wejscie(0); -- zarejestrowanie dwoch kolejnych stanow sygnalu RX

      CASE stan IS -- badanie aktualnego stanu maszyny stanow 

        WHEN CZEKANIE => -- obsluga stanu CZEKANIE
          l_czasu <= 0; -- wyzerowanie licznika czasu bodu
          l_bitow <= 0; -- wyzerowanie licznika odebranych bitow
          bufor <= (OTHERS => '0'); -- wyzerowanie bufora bitow danych
          problem <= '0'; -- wyzerowanie rejestru bledu odbioru
          IF (wejscie(1) = '0' AND wejscie(0) = '1') THEN -- wykrycie poczatku bitu START
            stan <= START; -- przejscie do stanu START
          END IF; -- zakonczenie instukcji warunkowej

        WHEN START => -- obsluga stanu START
          IF (l_czasu /= T/2) THEN -- badanie odliczania okresu T/2
            l_czasu <= l_czasu + 1; -- zwiekszenie o 1 stanu licznika czasu
          ELSE -- zakonczenie odliczania czasu T/2
            l_czasu <= 0; -- wyzerowanie licznika czasu bodu
            stan <= DANA; -- przejscie do stanu DANA
            IF (wejscie(1) = '0') THEN -- badanie nieprawidlowego stanu bitu START
              problem <= '1'; -- ustawienie rejestru bledu odbioru
            END IF; -- zakonczenie instukcji warunkowej  
          END IF; -- zakonczenie instukcji warunkowej

        WHEN DANA => -- obsluga stanu DANA
          IF (l_czasu /= T) THEN -- badanie odliczania okresu T
            l_czasu <= l_czasu + 1; -- zwiekszenie o 1 stanu licznika czasu
          ELSE -- zakonczenie odliczania czasu T
            bufor(bufor'left) <= wejscie(1); -- zapamietanie stanu bitu danych
            bufor(bufor'left - 1 DOWNTO 0) <= bufor(bufor'left DOWNTO 1); -- przesuniecie bitow w buforze
            l_czasu <= 0; -- wyzerowanie licznika czasu bodu

            IF (l_bitow /= B_SLOWA - 1) THEN -- badanie odliczania bitow danych
              l_bitow <= l_bitow + 1; -- zwiekszenie o 1 liczby bitow danych
            ELSE -- zakonczenie odliczania bitow danych
              l_bitow <= 0; -- wyzerowanie licznika odebranych bitow
              IF (B_PARZYSTOSCI = 1) THEN -- badanie odbioru bitu parzystosci
                stan <= PARZYSTOSC; -- przejscie do stanu PARZYSTOSC
              ELSE -- brak odbioru bitu parzystosci  
                stan <= STOP; -- przejscie do stanu STOP
              END IF; -- zakonczenie instukcji warunkowej
            END IF; -- zakonczenie instukcji warunkowej 

          END IF; -- zakonczenie instukcji warunkowej

        WHEN PARZYSTOSC => -- obsluga stanu PARZYSTOSC
          IF (l_czasu /= T) THEN -- badanie odliczania okresu T
            l_czasu <= l_czasu + 1; -- zwiekszenie o 1 stanu licznika czasu
          ELSE -- zakonczenie odliczania czasu T
            l_czasu <= 0; -- wyzerowanie licznika czasu bodu
            stan <= STOP; -- przejscie do stanu STOP
            IF ((wejscie(1) = XOR_REDUCE(bufor)) = N_SLOWO) THEN -- badanie nieprawidlowej parzystosci bitow
              problem <= '1'; -- ustawienie rejestru bledu odbioru
            END IF; -- zakonczenie instukcji warunkowej 
          END IF; -- zakonczenie instukcji warunkowej

        WHEN STOP => -- obsluga stanu STOP
          IF (l_czasu /= T) THEN -- badanie odliczania okresu T
            l_czasu <= l_czasu + 1; -- zwiekszenie o 1 stanu licznika czasu
          ELSE -- zakonczenie odliczania czasu T
            l_czasu <= 0; -- wyzerowanie licznika czasu bodu

            IF (l_bitow /= B_STOPOW - 1) THEN -- badanie odliczania bitow stopu
              l_bitow <= l_bitow + 1; -- zwiekszenie o 1 liczby bitow stopu
              IF (wejscie(1) /= '0') THEN -- badanie nieprawidlowego stanu bitu STOP
                problem <= '1'; -- ustawienie rejestru bledu odbioru
              END IF; -- zakonczenie instukcji warunkowej 
            ELSE -- zakonczenie odliczania bitow stopu
              IF (problem = '0' AND wejscie(1) = '0') THEN -- badanie prawidlowego odbioru szeregowego
                SLOWO <= bufor; -- ustawienie na wyjsciu SLOWO odebranego slowa 
                IF (N_SLOWO = TRUE) THEN -- badanie warunku zanegowania odebranego slowa
                  SLOWO <= NOT(bufor); -- ustawienie na wyjsciu SLOWO odebranego slowa 
                END IF; -- zakonczenie instukcji warunkowej  
                GOTOWE <= '1'; -- ustawienie na wyjsciu flagi potwierdzenia
              ELSE -- wykryto nieprawidlowy odbioru szeregowy
                SLOWO <= (OTHERS => '0'); -- wyzerowanie wyjscia danych
                BLAD <= '1'; -- ustawienie na wyjsciu flagi bledu odbioru
              END IF; -- zakonczenie instukcji warunkowej
              stan <= CZEKANIE; -- przejscie do stanu CZEKANIE
            END IF; -- zakonczenie instukcji warunkowej 

          END IF; -- zakonczenie instukcji warunkowej

      END CASE; -- zakonczenie instukcji warunkowego wyboru

    END IF; -- zakonczenie instukcji warunkowej porcesu

  END PROCESS; -- zakonczenie ciala procesu

END behavioural;