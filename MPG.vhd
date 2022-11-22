library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;


-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity MPG is
    Port ( clk : in STD_LOGIC;
           step : out STD_LOGIC;
           btn: in std_logic);
end MPG;

architecture Behavioral of MPG is
    signal tmp          :   STD_LOGIC_VECTOR(15 downto 0);
    signal en1, q1, q2  :   STD_LOGIC;
begin
    
    process (clk)
    begin
        if (clk'event and clk='1') then
            tmp <= tmp + 1;
        end if;
    end process;
    
    
    process(clk)
    begin
        if (clk'event and clk='1') then
             if tmp = x"FFFF" then
                q1 <= btn;
            end if;
        end if;
    end process;
    
    process(clk)
    begin
        if (clk'event and clk='1') then
            q2 <= q1;
        end if;
    end process;
    
    step <= q1 and (not q2);

end Behavioral;