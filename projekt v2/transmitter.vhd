LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE ieee.std_logic_arith.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

ENTITY transmitter IS
	PORT (
		r : IN STD_LOGIC; -- sygnal resetujacy
		clk : IN STD_LOGIC; -- zegar taktujacy
		data_in : IN STD_LOGIC_VECTOR(7 DOWNTO 0); -- wejscie rownolegle
		tx : OUT STD_LOGIC;
		ind : OUT INTEGER RANGE 0 TO 9); -- wyjscie szeregowe
END transmitter;

ARCHITECTURE Behavioral OF transmitter IS

	SIGNAL pos : NATURAL RANGE 0 TO 7; -- pozycja
	SIGNAL buff : STD_LOGIC_VECTOR(7 DOWNTO 0); -- bufor
	SIGNAL count : STD_LOGIC; --flaga 
	SIGNAL counter : INTEGER RANGE 0 TO 3;
	SIGNAL state : INTEGER RANGE 0 TO 10;
BEGIN

	PROCESS (r, clk)
	BEGIN
		IF (r = '1') THEN -- asynchroniczna inicjalizacja rejestrow
			pos <= 0;
			count <= '0';
			tx <= '0';
			ind <= state;
			state <= 0;
		ELSIF (rising_edge(clk)) THEN

			IF (count = '0') THEN
				tx <= '1';
				IF (unsigned(buff) /= unsigned(data_in)) THEN
					buff <= data_in;
					count <= '1';
				END IF;

			ELSIF (counter = 0) THEN
				CASE (state) IS
					WHEN 0 =>
						tx <= '0'; --start bit
						state <= 1;
						ind <= state;
					WHEN 1 => tx <= buff(0); -- zapisujemy bity po kolei
						state <= 2;
						ind <= state;
					WHEN 2 => tx <= buff(1);
						state <= 3;
						ind <= state;
					WHEN 3 => tx <= buff(2);
						state <= 4;
						ind <= state;
					WHEN 4 => tx <= buff(3);
						state <= 5;
						ind <= state;
					WHEN 5 => tx <= buff(4);
						state <= 6;
						ind <= state;
					WHEN 6 => tx <= buff(5);
						state <= 7;
						ind <= state;
					WHEN 7 => tx <= buff(6);
						state <= 8;
						ind <= state;
					WHEN 8 => tx <= buff(7);
						state <= 9;
						ind <= state;
					WHEN 9 =>
						state <= 10;
						ind <= state;
						tx <= '1'; -- stop bit
					WHEN 10 =>
						state <= 0;
						ind <= state;
						count <= '0';
						tx <= '1'; -- czekamy ma kolejne dane
				END CASE;
			END IF;

		END IF;
	END PROCESS;
	PROCESS (clk)
	BEGIN
		IF (clk = '1' AND clk'event) THEN
			IF (count = '1') THEN
				counter <= (counter + 1)MOD 4; -- 
			ELSE
				counter <= 0;
			END IF;
		END IF;
	END PROCESS;

END Behavioral;