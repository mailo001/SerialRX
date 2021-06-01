LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE IEEE.std_logic_unsigned.ALL;

ENTITY receiver IS
        PORT (
                rx : IN STD_LOGIC;
                rst : IN STD_LOGIC;
                clk : IN STD_LOGIC;
                data_out : OUT STD_LOGIC_VECTOR (7 DOWNTO 0);
                GOTOWE : OUT STD_LOGIC;
                BLAD : OUT STD_LOGIC;
                ind : OUT INTEGER RANGE 0 TO 9);

END receiver;

ARCHITECTURE behavioural OF receiver IS
        SIGNAL count : STD_LOGIC; --flaga 
        SIGNAL counter : INTEGER RANGE 0 TO 3;
        SIGNAL state : INTEGER RANGE 0 TO 9 := 0;
        SIGNAL data : STD_LOGIC_VECTOR (7 DOWNTO 0);
        SIGNAL rx_p : STD_LOGIC; --previous rx;
BEGIN

        PROCESS (clk, rst) --bufer
        BEGIN
                IF (rst = '1') THEN
                        data_out <= (OTHERS => '0');
                        GOTOWE <= '0';
                        BLAD <= '0';
                        count <= '0';
                ELSIF (clk = '1' AND clk'event) THEN

                        IF (rx_p = '1' AND rx = '0') THEN -- jesli start        count <= '1';
                                rx_p <= rx;
                                count <= '1';
                                GOTOWE <= '0';
                                BLAD <= '0';
                        ELSE
                                rx_p <= rx;
                        END IF;

                        IF (counter = 2) THEN
                                CASE (state) IS
                                        WHEN 0 =>
                                                IF (rx = '0') THEN -- jeszcze raz sprawdzamy start bit
                                                        state <= 1;
                                                        ind <= state;
                                                ELSE
                                                        BLAD <= '1';
                                                        state <= 0;
                                                        ind <= state;
                                                        count <= '0';
                                                END IF;
                                        WHEN 1 => data(0) <= rx; -- zapisujemy bity po kolei
                                                state <= 2;
                                                ind <= state;
                                        WHEN 2 => data(1) <= rx;
                                                state <= 3;
                                                ind <= state;
                                        WHEN 3 => data(2) <= rx;
                                                state <= 4;
                                                ind <= state;
                                        WHEN 4 => data(3) <= rx;
                                                state <= 5;
                                                ind <= state;
                                        WHEN 5 => data(4) <= rx;
                                                state <= 6;
                                                ind <= state;
                                        WHEN 6 => data(5) <= rx;
                                                state <= 7;
                                                ind <= state;
                                        WHEN 7 => data(6) <= rx;
                                                state <= 8;
                                                ind <= state;
                                        WHEN 8 => data(7) <= rx;
                                                state <= 9;
                                                ind <= state;
                                        WHEN 9 =>
                                                state <= 0;
                                                ind <= state;
                                                count <= '0';
                                                IF (rx = '1') THEN -- sprawdzamy stop bit
                                                        data_out <= data; -- zwracamy caly bufer
                                                        GOTOWE <= '1';
                                                ELSE
                                                        BLAD <= '1';
                                                END IF;
                                END CASE;
                        END IF;

                END IF;
        END PROCESS;

        PROCESS (clk)
        BEGIN
                IF (clk = '1' AND clk'event) THEN
                        IF (count = '1') THEN
                                counter <= (counter + 1)MOD 4; -- ������ ������� ����� ������� ��������� ���
                        ELSE
                                counter <= 0;
                        END IF;
                END IF;
        END PROCESS;

END ARCHITECTURE;