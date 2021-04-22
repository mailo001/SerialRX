library ieee;
use     ieee.std_logic_1164.all;
use     ieee.std_logic_unsigned.all;
use     ieee.std_logic_misc.all;

entity serial_rx is
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
end serial_rx;

architecture behavioural of serial_rx is

  

begin

  process(R, C)
  begin
    
  end process;  
   
end behavioural;