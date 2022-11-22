library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use  IEEE.STD_LOGIC_ARITH.ALL; 
use  IEEE.STD_LOGIC_UNSIGNED.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

entity test_env is
    Port ( clk : in STD_LOGIC;
           btn : in STD_LOGIC_VECTOR (4 downto 0);
           sw : in STD_LOGIC_VECTOR (15 downto 0);
           led : out STD_LOGIC_VECTOR (15 downto 0);
           an : out STD_LOGIC_VECTOR (3 downto 0);
           cat : out STD_LOGIC_VECTOR (6 downto 0));
end test_env;

    architecture Behavioral of test_env is

    component mpg is
    Port ( clk : in STD_LOGIC;
           btn : in STD_LOGIC;
           step : out STD_LOGIC);
    end component;

    component pc is
    Port ( clk : in std_logic;
            rst_n : in std_logic;
            pc_in : in std_ulogic_vector(32 - 1 DOWNTO 0);
            PC_en : in std_ulogic;
            pc_out : out std_ulogic_vector(32-1 DOWNTO 0));
    end component; 
    
    component RAM is
    Port(clk : in STD_LOGIC;
         we  : in STD_LOGIC;
         addr: in STD_LOGIC_VECTOR(7 downto 0);
         di  : in STD_LOGIC_VECTOR(15 downto 0);
         do  : out STD_LOGIC_VECTOR(15 downto 0)); 
    end component;
    
    component regfile is
    Port ( clk  : in STD_LOGIC;
            rst_n : in STD_LOGIC;
            adrport0  : in STD_LOGIC_VECTOR (2 downto 0);
            adrport1  : in STD_LOGIC_VECTOR (2 downto 0);
            adrwport   : in STD_LOGIC_VECTOR (2 downto 0);
            writeport   : in STD_LOGIC_VECTOR (15 downto 0);
            wen  : in STD_LOGIC;
            readport0  : out STD_LOGIC_VECTOR (15 downto 0);
            readport1  : out STD_LOGIC_VECTOR (15 downto 0));
    end component; 

    type rom_mem is array (0 to 255) of STD_LOGIC_VECTOR(15 downto 0);
    
    signal rom: rom_mem := ( -- 100, 101 AND 110 are registers; operations are done on their contents
        B"000_000_000_000_0_110", -- XOR 000
        B"001_000_001_0000001", -- ADDI 1 to 000 in 001 => 1
        B"001_000_010_0000001", -- ADDI 1 to 000 in 010 => 1
        B"000_101_101_101_0_110", -- XOR 101 
        B"000_001_010_011_0_000", -- ADD 001 TO 010 IN 011
        B"000_010_000_001_0_000", -- ADD 010 TO 000 IN 001
        B"000_011_000_010_0_000", -- ADD 011 TO 000 IN 010
        B"001_101_101_0000001", -- ADDI 1 to 101 in 101
        B"100_101_111_0000001", -- BEQ 101 = 111; PC <- PC + 1 + 1
        B"111_0000000000100", -- J 100
        
        others => X"0000"
    );
    
    -- SIGNALS --
    signal count        : STD_LOGIC_VECTOR(15 downto 0);
    signal din          : STD_LOGIC_VECTOR(15 downto 0);
    signal step         : STD_LOGIC;
    
    signal rom_data     : STD_LOGIC_VECTOR(15 downto 0);
    
    signal adrport0          : STD_LOGIC_VECTOR(2 downto 0);
    signal SA           : STD_LOGIC;
    signal adrport1     : STD_LOGIC_VECTOR(2 downto 0);
    signal adrwport           : STD_LOGIC_VECTOR(2 downto 0);
    signal writeport           : STD_LOGIC_VECTOR(15 downto 0);
    signal readport0          : STD_LOGIC_VECTOR(15 downto 0);
    signal readport1          : STD_LOGIC_VECTOR(15 downto 0);
    signal ram_data     : STD_LOGIC_VECTOR(15 downto 0);
    signal alu_res      : STD_LOGIC_VECTOR(15 downto 0);
    signal alu_b        : STD_LOGIC_VECTOR(15 downto 0);
    signal ext_imm      : STD_LOGIC_VECTOR(15 downto 0);
    signal ram_data_in  : STD_LOGIC_VECTOR(15 downto 0);
    
    signal regWrite     : STD_LOGIC;
    signal regWritei    : STD_LOGIC;
    signal memWrite     : STD_LOGIC;
    signal regDst       : STD_LOGIC;
    signal extOp        : std_logic;
    signal aluSrc       : STD_LOGIC;
    signal memToReg     : STD_LOGIC;
    signal BranchNotEqual : STD_LOGIC;
    signal BranchGreaterOrEqualToZero: STD_LOGIC;
    signal branch       : STD_LOGIC;
    signal jmp          : STD_LOGIC;
    signal pcSrc        : STD_LOGIC;       
    signal zeroFlag     : STD_LOGIC;
    signal aluCtrl      : STD_LOGIC_VECTOR(2 downto 0);
    signal opCode       : STD_LOGIC_VECTOR(2 downto 0);
    
    signal RD           : STD_LOGIC_VECTOR(2 downto 0);
    signal RT           : STD_LOGIC_VECTOR(2 downto 0);
    signal RS           : STD_LOGIC_VECTOR(2 downto 0);    

begin
    
    MPG_PORT: mpg 
    Port Map( 
        clk     => clk, 
        btn     => btn(0),
        step    => step
    );
    
    -- PC --
    PC_PORT : pc
    Port Map(
        clk => clk,
        rst_n => rst_n,
        pc_in => count,
        pc_en => step,
        pc_out => count
    )

    -- ROM data in transit 
    rom_data <= rom(conv_integer(count(7 downto 0))); 
    
    opcode <= rom_data(15 downto 13); 
    
    adrport0 <= rom_data(12 downto 10); -- RS
    adrport1 <= rom_data(9 downto 7); -- RT
    
    -- MUX --
    adrwport <= rom_data(6 downto 4) when regDst = '1' else rom_data(9 downto 7); -- RT or RD
    
    regWritei <= regWrite and step;
    --register file
    REGFILE_PORT: regfile
    Port map ( 
        clk  => clk, 
        adrport0  => adrport0, 
        adrport1  => adrport1, 
        adrwport   => adrwport,
        writeport   => writeport, 
        wen  => wen, 
        rst_n => rst_n,
        readport0  => readport0, 
        readport1  => readport1 
    );
    
    -- EXT UNIT --
    ext_imm(6 downto 0) <= rom_data(6 downto 0);
    ext_imm(15 downto 7) <= (others => '0') when extOp = '0' else (others => rom_data(6));
        
    -- ALU SRC MUX
    alu_b <= readport1 when aluSrc = '0' else ext_imm; 
    
    -- ALU --
    process(aluCtrl)
    begin
        case aluCtrl is
            when "000"  => alu_res <= readport0 + alu_b;               -- ADD
            when "001"  => alu_res <= readport0 - alu_b;               -- SUB
            when "010"  => 
                if (SA='1') then -- sll
                    alu_res <= readport0(14 downto 0) & '0';
                else
                    alu_res <= readport0(15 downto 0);
                end if; 
            when "011"  => -- srl
                if (SA='1') then
                    alu_res <= '0' & readport0(15 downto 1);   
                else 
                    alu_res <= readport0(15 downto 0);
                end if;               
            when "100"  => alu_res <= readport0 and alu_b;             -- AND
            when "101"  => alu_res <= readport0 or alu_b;              -- OR
            when "110"  => alu_res <= readport0 xor alu_b;             -- XOR
            when others => 
                if readport0 < readport1 then
                    alu_res <= x"0001";
                else
                    alu_res <= x"0000";
                end if;                
        end case;
    end process;           
    
    -- ZERO FLAG and BRANCHES --
    zeroFlag <= '1' when alu_res = 0 else '0';
    pcSrc <= (branch and zeroFlag) or (BranchNotEqual and (not zeroFlag)) or (BranchGreaterOrEqualToZero and (not alu_res(15)));
    
    -- RAM --
    ramu: RAM 
    Port map(
        clk     => clk, 
        we      => memWrite, 
        addr    => alu_res (7 downto 0),   
        di      => readport1, 
        do      => ram_data
    ); 
    
    -- MemToReg MUX --
    writeport <= alu_res when memToReg = '0' else ram_data; 
    
    --control unit
    process(opcode)
    begin
        case opcode is
            when "000"  => regDst <= '1'; regWrite <= '1'; aluSrc <= '0'; branch <= '0'; BranchNotEqual <= '0'; BranchGreaterOrEqualToZero <= '0'; memWrite <= '0'; memToReg <= '0'; extOp <= '0'; jmp <= '0'; aluCtrl <= rom_data(2 downto 0);   -- ADD,SUB 
            when "001"  => regDst <= '0'; regWrite <= '1'; aluSrc <= '1'; branch <= '0'; BranchNotEqual <= '0'; BranchGreaterOrEqualToZero <= '0'; memWrite <= '0'; memToReg <= '0'; extOp <= '1'; jmp <= '0'; aluCtrl <= "000";                  -- ADDI
            when "010"  => regDst <= '0'; regWrite <= '1'; aluSrc <= '1'; branch <= '0'; BranchNotEqual <= '0'; BranchGreaterOrEqualToZero <= '0'; memWrite <= '0'; memToReg <= '1'; extOp <= '1'; jmp <= '0'; aluCtrl <= "000";                  -- LW
            when "011"  => regDst <= '0'; regWrite <= '0'; aluSrc <= '1'; branch <= '0'; BranchNotEqual <= '0'; BranchGreaterOrEqualToZero <= '0'; memWrite <= '1'; memToReg <= '0'; extOp <= '1'; jmp <= '0'; aluCtrl <= "000";                  -- SW
            when "100"  => regDst <= '0'; regWrite <= '0'; aluSrc <= '0'; branch <= '1'; BranchNotEqual <= '0'; BranchGreaterOrEqualToZero <= '0'; memWrite <= '0'; memToReg <= '0'; extOp <= '1'; jmp <= '0'; aluCtrl <= "001";                  -- BEQ
            when "101"  => regDst <= '0'; regWrite <= '0'; aluSrc <= '0'; branch <= '0'; BranchNotEqual <= '1'; BranchGreaterOrEqualToZero <= '0'; memWrite <= '0'; memToReg <= '0'; extOp <= '1'; jmp <= '0'; aluCtrl <= "001";                  -- BNE
            when "110"  => regDst <= '0'; regWrite <= '0'; aluSrc <= '0'; branch <= '0'; BranchNotEqual <= '0'; BranchGreaterOrEqualToZero <= '1'; memWrite <= '0'; memToReg <= '0'; extOp <= '1'; jmp <= '0'; aluCtrl <= "001";                  -- BGEZ 
            when others => regDst <= '0'; regWrite <= '0'; aluSrc <= '0'; branch <= '0'; BranchNotEqual <= '0'; BranchGreaterOrEqualToZero <= '0'; memWrite <= '0'; memToReg <= '0'; extOp <= '0'; jmp <= '1'; aluCtrl <= "000";                  -- JMP
        end case;
    end process;
    
    --display mux
    process(sw)
    begin
        case sw(2 downto 0) is
            when "000"  => din <= count;
            when "001"  => din <= rom_data;
            when "010"  => din <= readport0;
            when "011"  => din <= readport1;
            when "100"  => din <= ext_imm;
            when "101"  => din <= alu_res;
            when "110"  => din <= ram_data;
            when others => din <= writeport;
        end case;
    end process;

    led <= count;
    
end architecture; 