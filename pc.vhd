LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE IEEE.numeric_std.ALL;

ENTITY pc IS
PORT (
clk : IN STD_ULOGIC;
rst_n : IN STD_ULOGIC;
pc_in : IN STD_ULOGIC_VECTOR(32-1 DOWNTO 0);
PC_en : IN STD_ULOGIC;
pc_out : OUT STD_ULOGIC_VECTOR(32-1 DOWNTO 0) );
END pc;

ARCHITECTURE behave OF pc IS
BEGIN
proc_pc : PROCESS(clk, rst_n)
VARIABLE pc_temp : STD_ULOGIC_VECTOR(32-1 DOWNTO 0);
BEGIN
IF rst_n = '0' THEN
pc_temp := (OTHERS => '0');
ELSIF RISING_EDGE(clk) THEN
IF PC_en = '1' THEN
pc_temp := pc_in;
END IF;
END IF;
pc_out <= pc_temp;
END PROCESS;
END behave; 