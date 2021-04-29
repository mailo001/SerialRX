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


  SIGNAL wejscie : STD_LOGIC_VECTOR(0 TO 1); -- podwojny rejestr sygnalu RX

  --TYPE ETAP IS (CZEKANIE, START, DANA, PARZYSTOSC, STOP); -- lista etapow pracy odbiornika
  --SIGNAL stan : ETAP; -- rejestr maszyny stanow odbiornika

  CONSTANT T : POSITIVE := F_ZEGARA/L_BODOW - 1; -- czas jednego bodu - liczba taktów zegara
  SIGNAL l_czasu : NATURAL RANGE 0 TO T; -- licznik czasu jednego bodu
  SIGNAL l_bitow : NATURAL RANGE 0 TO B_SLOWA - 1; -- licznik odebranych bitow danych lub stopu

  SIGNAL bufor : STD_LOGIC_VECTOR(B_SLOWA - 1 DOWNTO 0); -- rejestr kolejno odebranych bitow danych
  SIGNAL problem : STD_LOGIC; -- rejestr (flaga) wykrytego bledu odbioru


  SIGNAL stan_czekanie : STD_LOGIC;
  SIGNAL stan_dana : STD_LOGIC;
  SIGNAL stan_parzystosc : STD_LOGIC;
  SIGNAL stan_start : STD_LOGIC;
  SIGNAL stan_stop : STD_LOGIC;

  SIGNAL component_clk : STD_LOGIC;

  COMPONENT czekanie IS
    PORT (
      WORK : IN STD_LOGIC; -- sygnal czy komponent ma działać
      C : IN STD_LOGIC; -- zegar taktujacy
      

      wejscie : INOUT STD_LOGIC_VECTOR(0 TO 1); -- podwojny rejestr sygnalu RX
      l_czasu : INOUT NATURAL RANGE 0 TO T; -- licznik czasu jednego bodu
      l_bitow : INOUT NATURAL RANGE 0 TO B_SLOWA - 1; -- licznik odebranych bitow danych lub stopu
      bufor : INOUT STD_LOGIC_VECTOR(B_SLOWA - 1 DOWNTO 0); -- rejestr kolejno odebranych bitow danych
      problem : INOUT STD_LOGIC -- rejestr (flaga) wykrytego bledu odbioru

      stan_start : OUT STD_LOGIC;
    );
  END COMPONENT;

  COMPONENT start IS 
    PORT (
      WORK : IN STD_LOGIC; -- sygnal czy komponent ma działać
      C : IN STD_LOGIC; -- zegar taktujacy
    
    
      wejscie : INOUT STD_LOGIC_VECTOR(0 TO 1); -- podwojny rejestr sygnalu RX
      l_czasu : INOUT NATURAL RANGE 0 TO T; -- licznik czasu jednego bodu
      problem : INOUT STD_LOGIC; -- rejestr (flaga) wykrytego bledu odbioru


      stan_dana : OUT STD_LOGIC;
    );
  END COMPONENT;

  COMPONENT stop IS 
    PORT (
      WORK : IN STD_LOGIC; -- sygnal czy dziala
      C : IN STD_LOGIC; -- zegar taktujacy
        --RX : IN STD_LOGIC; -- odebrany sygnal szeregowy
      SLOWO : OUT STD_LOGIC_VECTOR(B_SLOWA - 1 DOWNTO 0); -- odebrane slowo danych
      GOTOWE : OUT STD_LOGIC; -- flaga potwierdzenia odbioru
      BLAD : OUT STD_LOGIC -- flaga wykrycia bledu w odbiorze

      wejscie : INOUT STD_LOGIC_VECTOR(0 TO 1); -- podwojny rejestr sygnalu RX
      l_czasu : INOUT NATURAL RANGE 0 TO T; -- licznik czasu jednego bodu
      l_bitow : INOUT NATURAL RANGE 0 TO B_SLOWA - 1; -- licznik odebranych bitow danych lub stopu
      bufor : INOUT STD_LOGIC_VECTOR(SLOWO'RANGE); -- rejestr kolejno odebranych bitow danych
      problem : INOUT STD_LOGIC; -- rejestr (flaga) wykrytego bledu odbioru

      stan_czekanie : OUT STD_LOGIC;
    );
  END COMPONENT;


BEGIN

  c1: czekanie PORT MAP (
    WORK => stan_czekanie,
    C => component_clk, 

    wejscie => wejscie,
    l_czasu => l_czasu,
    l_bitow => l_bitow,
    bufor => bufor,
    problem => problem,

    stan_start => stan_start,
  );

  start1: start PORT MAP (
    WORK => stan_start,
    C => component_clk,

    wejscie => wejscie,
    l_czasu => l_czasu,
    problem => problem,

    stan_dana => stan_dana
  );

  stop1: stop PORT MAP (
    WORK => stan_stop,
    C => component_clk, 

    SLOWO => SLOWO,
    GOTOWE => GOTOWE,
    BLAD => BLAD,

    wejscie => wejscie,
    l_czasu => l_czasu,
    l_bitow => l_bitow,
    bufor => bufor,
    problem => problem,

    stan_czekanie => stan_czekanie
  )

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

      stan_czekanie <= '1';
      stan_start <= '0';
      stan_stop <= '0';
      stan_dana <= '0';
      stan_parzystosc <= '0';

      component_clk <= '0';

    ELSIF (rising_edge(C)) THEN -- synchroniczna praca odbiornika

      GOTOWE <= '0'; -- defaultowe skasowanie flagi potwierdzenia odbioru
      BLAD <= '0'; -- defaultowe skasowanie flagi wykrycia bledu w odbiorze
      wejscie(0) <= RX; -- zarejestrowanie synchroniczne stanu sygnalu RX
      IF (N_RX = TRUE) THEN -- badanie warunku zanegowania sygnalu szeregowego
        wejscie(0) <= NOT(RX); -- zarejestrowanie synchroniczne zanegowanego sygnalu RX
      END IF; -- zakonczenie instukcji warunkowej  
      wejscie(1) <= wejscie(0); -- zarejestrowanie dwoch kolejnych stanow sygnalu RX

      -- Odpowiednie przekierowanie do komponentów

      component_clk <= '1';
      wait for 10 ns;
      component_clk <= '0';
      

    END IF; -- zakonczenie instukcji warunkowej porcesu

  END PROCESS; -- zakonczenie ciala procesu

END behavioural;