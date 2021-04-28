LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;
USE ieee.std_logic_misc.ALL;

ENTITY start IS
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
    WORK : IN STD_LOGIC; -- sygnal czy komponent ma działać
    C : IN STD_LOGIC; -- zegar taktujacy
    RX : IN STD_LOGIC; -- odebrany sygnal szeregowy
    SLOWO : OUT STD_LOGIC_VECTOR(B_SLOWA - 1 DOWNTO 0); -- odebrane slowo danych
    GOTOWE : OUT STD_LOGIC; -- flaga potwierdzenia odbioru
    BLAD : OUT STD_LOGIC -- flaga wykrycia bledu w odbiorze
    
    wejscie : INOUT STD_LOGIC_VECTOR(0 TO 1); -- podwojny rejestr sygnalu RX
    l_czasu : INOUT NATURAL RANGE 0 TO T; -- licznik czasu jednego bodu
    problem : INOUT STD_LOGIC; -- rejestr (flaga) wykrytego bledu odbioru

  );
END start;

ARCHITECTURE behavioural OF start IS

  SIGNAL wejscie : STD_LOGIC_VECTOR(0 TO 1); -- podwojny rejestr sygnalu RX

  --type     ETAP		is (CZEKANIE, START, DANA, PARZYSTOSC, STOP); -- lista etapow pracy odbiornika
  --signal   stan		:ETAP;					-- rejestr maszyny stanow odbiornika

  CONSTANT T : POSITIVE := F_ZEGARA/L_BODOW - 1; -- czas jednego bodu - liczba taktów zegara
  --signal   l_bitow  	:natural range 0 to B_SLOWA-1;		-- licznik odebranych bitow danych lub stopu

  --signal   bufor	:std_logic_vector(SLOWO'range);		-- rejestr kolejno odebranych bitow danych
  --signal   problem	:std_logic;				-- rejestr (flaga) wykrytego bledu odbioru

BEGIN

  PROCESS (WORK, C)
  BEGIN
    IF (WORK = '1') THEN
      IF (rising_edge(C)) THEN
        IF (l_czasu /= T/2) THEN -- badanie odliczania okresu T/2
          l_czasu <= l_czasu + 1; -- zwiekszenie o 1 stanu licznika czasu
        ELSE -- zakonczenie odliczania czasu T/2
          l_czasu <= 0; -- wyzerowanie licznika czasu bodu
          stan <= DANA; -- przejscie do stanu DANA
          IF (wejscie(1) = '0') THEN -- badanie nieprawidlowego stanu bitu START
            problem <= '1'; -- ustawienie rejestru bledu odbioru
          END IF; -- zakonczenie instukcji warunkowej  
        END IF; -- zakonczenie instukcji warunkowej

      END IF;
    END IF;
  END PROCESS;

END behavioural;