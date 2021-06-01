LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;
USE ieee.std_logic_misc.ALL;

ENTITY receive_TB IS
  GENERIC (
    CONSTANT F_ZEGARA : NATURAL := 20_000_000; -- czestotliwosc zegara systemowego w [Hz]
    CONSTANT L_BODOW : NATURAL := 5_000_000; -- predkosc nadawania w [bodach]
    CONSTANT B_SLOWA : NATURAL := 8; -- liczba bitow slowa danych (5-8)
    CONSTANT B_PARZYSTOSCI : NATURAL := 1; -- liczba bitow parzystosci (0-1)
    CONSTANT B_STOPOW : NATURAL := 1; -- liczba bitow stopu (1-2)
    CONSTANT N_RX : BOOLEAN := FALSE; -- negacja logiczna sygnalu szeregowego
    CONSTANT N_SLOWO : BOOLEAN := FALSE -- negacja logiczna slowa danych
  );
END receive_TB;

ARCHITECTURE behavioural OF receive_TB IS

  CONSTANT O_ZEGARA : TIME := 1 sec/F_ZEGARA; -- okres zegara systemowego
  CONSTANT O_BITU : TIME := 1 sec/L_BODOW; -- okres czasu trwania jednego bodu

  SIGNAL R : STD_LOGIC := '0'; -- symulowany sygnal resetujacacy
  SIGNAL C : STD_LOGIC := '1'; -- symulowany zegar taktujacy inicjowany na '1'
  SIGNAL RX : STD_LOGIC; -- symulowane wejscie 'RX'
  SIGNAL DATA_IN : STD_LOGIC_VECTOR(B_SLOWA - 1 DOWNTO 0); -- obserwowane wyjscie 'DATA_IN'
  SIGNAL DATA_OUT : STD_LOGIC_VECTOR(DATA_IN'RANGE); -- obserwowane wyjscie 'DATA_OUT'
  SIGNAL GOTOWE : STD_LOGIC; -- obserwowane wyjscie 'GOTOWE'
  SIGNAL BLAD : STD_LOGIC; -- obserwowane wyjscie 'BLAD'
  SIGNAL D : STD_LOGIC_VECTOR(DATA_IN'RANGE); -- symulowana dana transmitowana
  SIGNAL TX : STD_LOGIC; -- symulowane wyjscie 'TX'
  SIGNAL RX_IND : INTEGER RANGE 0 TO 9;
  SIGNAL TX_IND : INTEGER RANGE 0 TO 9;
BEGIN

  PROCESS IS -- proces bezwarunkowy
  BEGIN -- czesc wykonawcza procesu
    R <= '1';
    WAIT FOR 100 ns; -- ustawienie sygnalu 'res' na '1' i odczekanie 100 ns
    R <= '0';
    WAIT; -- ustawienie sygnalu 'res' na '0' i zatrzymanie
  END PROCESS; -- zakonczenie procesu

  PROCESS IS -- proces bezwarunkowy
  BEGIN -- czesc wykonawcza procesu
    C <= NOT(C);
    WAIT FOR O_ZEGARA/2; -- zanegowanie sygnalu 'clk' i odczekanie pol okresu zegara
  END PROCESS; -- zakonczenie procesu

  PROCESS IS -- proces bezwarunkowy
    FUNCTION neg(V : STD_LOGIC; N : BOOLEAN) RETURN STD_LOGIC IS -- deklaracja funkcji wewnetrznej 'neg'
    BEGIN -- czesc wykonawcza funkcji wewnetrznej
      IF (N = FALSE) THEN
        RETURN (V);
      END IF; -- zwrot wartosc 'V' gdy 'N'=FALSE
      RETURN (NOT(V)); -- zwrot zanegowanej wartosci 'V'
    END FUNCTION; -- zakonczenie funkcji wewnetrznej
  BEGIN -- czesc wykonawcza procesu
    RX <= neg('1', N_RX); -- incjalizacja sygnalu 'RX' na wartosci spoczynkowa
    D <= (OTHERS => '0'); -- wyzerowanie sygnalu 'D'
    WAIT FOR 200 ns; -- odczekanie 200 ns
    LOOP -- rozpoczecie petli nieskonczonej
      RX <= neg('0', N_RX); -- ustawienie 'RX' na wartosc bitu START
      WAIT FOR O_BITU; -- odczekanie jednego bodu
      FOR i IN 0 TO B_SLOWA - 1 LOOP -- petla po kolejnych bitach slowa danych 'D'
        --		if(D>0) then
        --		RX<=TX;
        --		else
        RX <= neg(neg(D(i), N_SLOWO), N_RX); -- ustawienie 'RX' na wartosc bitu 'D(i)'
        --		  end if;
        WAIT FOR O_BITU; -- odczekanie jednego bodu
      END LOOP; -- zakonczenie petli
      --      if (B_PARZYSTOSCI = 1) then				-- badanie aktywowania bitu parzystosci
      --        RX <= neg(neg(XOR_REDUCE(D),N_SLOWO),N_RX);		-- ustawienie 'RX' na wartosc bitu parzystosci	
      --        wait for O_BITU;					-- odczekanie jednego bodu
      --      end if;							-- zakonczenie instukcji warunkowej
      FOR i IN 0 TO B_STOPOW - 1 LOOP -- petla po liczbie bitow STOP
        RX <= neg('1', N_RX); -- ustawienie 'RX' na wartosc bitu STOP
        WAIT FOR O_BITU; -- odczekanie jednego bodu
      END LOOP; -- zakonczenie petli
      D <= D + 5; -- zwiekszenia wartosci 'D' o 7
      WAIT FOR 30 * O_ZEGARA; -- odczekanie 10-ciu okresow zegara
    END LOOP; -- zakonczenie petli
  END PROCESS; -- zakonczenie procesu

  receiver : ENTITY work.receiver -- instancja odbiornika szeregowego 'SERIAL_RX'
    PORT MAP(-- mapowanie sygnalow do portow
      clk => C, -- zegar taktujacy
      rst => R,
      rx => RX, -- odebrany sygnal szeregowy
      data_out => DATA_IN, -- odebrane slowo danych
      GOTOWE => GOTOWE, -- flaga potwierdzenia odbioru
      BLAD => BLAD, -- flaga wykrycia bledu w odbiorze
      ind => RX_IND
    );

  modifier : ENTITY work.modifier
    PORT MAP(
      data_in => DATA_IN, -- modyfikowane wejscie
      GOTOWE => GOTOWE,
      data_out => DATA_OUT -- wynik
    );

  transmitter : ENTITY work.transmitter
    PORT MAP(
      r => R, -- sygnal resetujacy
      clk => C, -- zegar taktujacy
      data_in => DATA_OUT, -- wysylane slowo danych
      tx => TX, -- szeregowe wyjscie
      ind => TX_IND
    );

END behavioural;