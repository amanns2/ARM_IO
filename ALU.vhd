
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE work.AVR_Records_pkg.ALL;

PACKAGE alu_pkg IS
    COMPONENT alu IS
        PORT (
            ir_alu_control    	: IN  t_alu_control;
            ir_src_and_dest     : IN  t_src_and_dest;
            ir_sreg             : IN  t_sreg_bits;            
            or_data             : OUT t_data_bus;
            or_sreg             : OUT t_sreg_bits
        );
    END COMPONENT alu;
END PACKAGE alu_pkg;

--------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE work.AVR_Records_pkg.ALL;
USE work.ALU_2_Input_pkg.ALL;
USE work.ALU_1_Input_pkg.ALL;
USE work.ALU_Immediate_pkg.ALL;

ENTITY alu IS
        PORT (
            ir_alu_control    	: IN  t_alu_control;
            ir_src_and_dest     : IN  t_src_and_dest;
            ir_sreg             : IN  t_sreg_bits;            
            or_data             : OUT t_data_bus;
            or_sreg             : OUT t_sreg_bits
        );
END ENTITY alu;

--------------------------------------------------------------------------------

ARCHITECTURE struct OF alu IS

    SIGNAL r_alu_1_sreg         : t_sreg_bits                   := (OTHERS => '0');
    SIGNAL slv8_alu_1_data      : STD_LOGIC_VECTOR(7 DOWNTO 0)  := (OTHERS => '0');
    SIGNAL sl_alu_1_data_valid  : STD_LOGIC                     := '0';
    
    SIGNAL r_alu_2_sreg         : t_sreg_bits                   := (OTHERS => '0');
    SIGNAL slv8_alu_2_data      : STD_LOGIC_VECTOR(7 DOWNTO 0)  := (OTHERS => '0');
    SIGNAL sl_alu_2_data_valid  : STD_LOGIC                     := '0';
    
    SIGNAL r_alu_imm_sreg       : t_sreg_bits                   := (OTHERS => '0');
    SIGNAL slv8_alu_imm_data    : STD_LOGIC_VECTOR(7 DOWNTO 0)  := (OTHERS => '0');
    SIGNAL sl_alu_imm_data_valid: STD_LOGIC                     := '0';
    
BEGIN
          
    u_alu_1_input : alu_1_input PORT MAP (
        isl_enable          => ir_alu_control.sl_alu_1_cmd,
        islv4_operation     => ir_alu_control.slv4_alu_1_op_code,
        islv8_data      	=> ir_src_and_dest.slv8_dest,
        ir_sreg           	=> ir_sreg,
        or_sreg_update      => r_alu_1_sreg,
        oslv8_data          => slv8_alu_1_data,
        osl_data_valid      => sl_alu_1_data_valid
    );

    u_alu_2_input : alu_2_input PORT MAP (
        isl_enable          => ir_alu_control.sl_alu_2_cmd,
        islv6_alu_op_code   => ir_alu_control.slv6_alu_2_op_code,
        islv8_src_data      => ir_src_and_dest.slv8_src,
        islv8_dest_data     => ir_src_and_dest.slv8_dest,
        ir_sreg           	=> ir_sreg,
        or_sreg_update      => r_alu_2_sreg,
        oslv8_data          => slv8_alu_2_data,
        osl_data_valid      => sl_alu_2_data_valid
    );

    u_alu_immediate : alu_immediate PORT MAP (
        isl_enable          => ir_alu_control.sl_alu_imm_cmd,
        islv4_alu_op_code   => ir_alu_control.slv4_alu_imm_op_code,
        islv8_imm_data   	=> ir_alu_control.slv8_imm_data,
        islv8_dest_data   	=> ir_src_and_dest.slv8_dest,
        ir_sreg           	=> ir_sreg,
        or_sreg_update      => r_alu_imm_sreg,
        oslv8_data          => slv8_alu_imm_data,
        osl_data_valid      => sl_alu_imm_data_valid
    );

    sreg_selector_proc : PROCESS (r_alu_1_sreg, r_alu_2_sreg, r_alu_imm_sreg)
    BEGIN
        IF    r_alu_1_sreg.sl_enable = '1'   THEN or_sreg <= r_alu_1_sreg;
        ELSIF r_alu_2_sreg.sl_enable = '1'   THEN or_sreg <= r_alu_2_sreg;
        ELSIF r_alu_imm_sreg.sl_enable = '1' THEN or_sreg <= r_alu_imm_sreg;
        ELSE                                      or_sreg <= ir_sreg;
                                                  or_sreg.sl_enable <= '0'; 
        END IF;
    END PROCESS sreg_selector_proc;
        
    --##    Output Assignments
    --##
    --#########################
    or_data.slv8_data       <= slv8_alu_2_data     OR slv8_alu_1_data     OR slv8_alu_imm_data;
    or_data.sl_valid        <= sl_alu_2_data_valid OR sl_alu_1_data_valid OR sl_alu_imm_data_valid;    

END ARCHITECTURE struct;