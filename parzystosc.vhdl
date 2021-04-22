library ieee;
use     ieee.std_logic_1164.all;
use     ieee.std_logic_unsigned.all;
use     ieee.std_logic_misc.all;

entity czekanie is
  generic (
    F_ZEGARA		:natural := 20_000_000;			-- czestotliwosc zegata w [Hz]
    L_BODOW		:natural := 9600;			-- predkosc nadawania w [bodach]
    B_SLOWA		:natural := 8;				-- liczba bitow slowa danych (5-8)
    B_PARZYSTOSCI	:natural := 1;				-- liczba bitow parzystosci (0-1)
    B_STOPOW		:natural := 2;				-- liczba bitow stopu (1-2)
    N_RX		:boolean := FALSE;			-- negacja logiczna sygnalu szeregowego
    N_SLOWO		:boolean := FALSE			-- negacja logiczna slowa danych
  );
  port (
    R		:in  std_logic;					-- sygnal resetowania
    C		:in  std_logic;					-- zegar taktujacy
    RX		:in  std_logic;					-- odebrany sygnal szeregowy
    SLOWO	:out std_logic_vector(B_SLOWA-1 downto 0);	-- odebrane slowo danych
    GOTOWE	:out std_logic;					-- flaga potwierdzenia odbioru
    BLAD	:out std_logic					-- flaga wykrycia bledu w odbiorze
  );
end czekanie;

architecture behavioural of czekanie is

    signal   wejscie	:std_logic_vector(0 to 1);		-- podwojny rejestr sygnalu RX

    --type     ETAP		is (CZEKANIE, START, DANA, PARZYSTOSC, STOP); -- lista etapow pracy odbiornika
    --signal   stan		:ETAP;					-- rejestr maszyny stanow odbiornika
  
    constant T		:positive := F_ZEGARA/L_BODOW-1;	-- czas jednego bodu - liczba taktów zegara
    signal   l_czasu  	:natural range 0 to T;			-- licznik czasu jednego bodu
    signal   l_bitow  	:natural range 0 to B_SLOWA-1;		-- licznik odebranych bitow danych lub stopu
    
    signal   bufor	:std_logic_vector(SLOWO'range);		-- rejestr kolejno odebranych bitow danych
    signal   problem	:std_logic;				-- rejestr (flaga) wykrytego bledu odbioru

begin

    process(R, C)
    begin
        if (R='0') then

            wejscie	<= (others => '0');				-- wyzerowanie rejestru sygnalu RX
            --stan	<= CZEKANIE;					-- poczatkowy stan pracy odbiornika
            l_czasu  <= 0;						-- wyzerowanie licznika czasu bodu
            l_bitow  <= 0;						-- wyzerowanie licznika odebranych bitow
            bufor	<= (others => '0');				-- wyzerowanie bufora bitow danych
            problem 	<= '0';						-- wyzerowanie rejestru bledu odbioru			
            SLOWO	<= (others => '0');				-- wyzerowanie wyjsciowego slowa danych
            GOTOWE	<= '0';						-- wyzerowanie flagi potwierdzenia odbioru
            BLAD	<= '0';						-- wyzerowanie flagi wykrycia bledu w odbiorze


        elsif (rising_edge(C)) then

            GOTOWE     <= '0';					-- defaultowe skasowanie flagi potwierdzenia odbioru
            BLAD       <= '0';					-- defaultowe skasowanie flagi wykrycia bledu w odbiorze
            wejscie(0) <= RX;					-- zarejestrowanie synchroniczne stanu sygnalu RX
            if (N_RX = TRUE) then					-- badanie warunku zanegowania sygnalu szeregowego
                wejscie(0) <= not(RX);					-- zarejestrowanie synchroniczne zanegowanego sygnalu RX
            end if;							-- zakonczenie instukcji warunkowej  
            wejscie(1) <= wejscie(0);				-- zarejestrowanie dwoch kolejnych stanow sygnalu RX



            if (l_czasu /= T) then				-- badanie odliczania okresu T
	            l_czasu <= l_czasu + 1;				-- zwiekszenie o 1 stanu licznika czasu
	        else							-- zakonczenie odliczania czasu T
                l_czasu <= 0;					-- wyzerowanie licznika czasu bodu
	            stan    <= STOP;					-- przejscie do stanu STOP
	            if ((wejscie(1) = XOR_REDUCE(bufor)) = N_SLOWO) then -- badanie nieprawidlowej parzystosci bitow
                    problem <= '1';					-- ustawienie rejestru bledu odbioru
	            end if; 						-- zakonczenie instukcji warunkowej 
	        end if;						-- zakonczenie instukcji warunkowej

        end if;
    end process;  
   
end behavioural;