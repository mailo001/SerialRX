----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    09:59:50 05/28/2020 
-- Design Name: 
-- Module Name:    modifier - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

ENTITY modifier IS
        PORT (
                data_in : IN STD_LOGIC_VECTOR (7 DOWNTO 0);
                GOTOWE : IN STD_LOGIC;
                data_out : OUT STD_LOGIC_VECTOR (7 DOWNTO 0));
END modifier;

ARCHITECTURE Behavioral OF modifier IS

        SIGNAL data : signed(7 DOWNTO 0) := "00000000";
BEGIN
        --  process (GOTOWE)							-- proces bezwarunkowy
        --  begin								-- czesc wykonawcza procesu
        --  
        --		if (GOTOWE = '1') then
        data <= signed(data_in);
        data_out <= STD_LOGIC_VECTOR(data + 1);
        --		end if;
        --  end process;	
END Behavioral;