LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;
USE ieee.std_logic_misc.ALL;

ENTITY serial_rx IS
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
END serial_rx;

ARCHITECTURE behavioural OF serial_rx IS

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

      -- Odpowiednie przekierowanie do komponentów
      

    END IF; -- zakonczenie instukcji warunkowej porcesu

  END PROCESS; -- zakonczenie ciala procesu

END behavioural;