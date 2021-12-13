LIBRARY IEEE;
USE work.IO_PORT_pkg.ALL;
USE IEEE.STD_LOGIC_1164.ALL;

PACKAGE avr_pkg IS
    COMPONENT avr IS
        PORT (
                isl_clk_125m     : IN STD_LOGIC;
                islv4_switch     : INOUT STD_LOGIC_VECTOR(3 DOWNTO 0);
                islv4_button     : INOUT STD_LOGIC_VECTOR(3 DOWNTO 0);
                islv4_led        : OUT STD_LOGIC_VECTOR(3 DOWNTO 0)       
            );
    END COMPONENT avr;
END PACKAGE avr_pkg;

--------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

USE work.avr_records_pkg.ALL;
USE work.clock_and_reset_pkg.ALL;

USE work.program_counter_pkg.ALL;
USE work.program_memory_pkg.ALL;
USE work.instr_decoder_pkg.ALL;
USE work.register_block_pkg.ALL;
USE work.alu_pkg.ALL;
USE work.data_bus_pkg.ALL;
USE work.sreg_pkg.ALL;
USE work.sram_pkg.ALL;
USE work.IO_PORT_pkg.ALL;

USE work.simulation_check_pkg.ALL;

ENTITY avr IS
        PORT (
            isl_clk_125m     : IN STD_LOGIC;
            islv4_switch     : INOUT STD_LOGIC_VECTOR(3 DOWNTO 0);
            islv4_button     : INOUTs STD_LOGIC_VECTOR(3 DOWNTO 0);
            islv4_led        : OUT STD_LOGIC_VECTOR(3 DOWNTO 0)          
        );
END ENTITY avr;

--------------------------------------------------------------------------------

ARCHITECTURE rtl OF avr IS

    --  Simulation checking Signals
    SIGNAL a_register_block         : ta_register_block;
    
    --  Processor Instruction Path Signals
    SIGNAL sl_reset                 : STD_LOGIC;
    SIGNAL sl_clock                 : STD_LOGIC;
    SIGNAL slv16_program_count      : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL slv16_op_code            : STD_LOGIC_VECTOR(15 DOWNTO 0);
    
    --  Processor Control Signals
    SIGNAL r_pc_control             : t_pc_control;
    SIGNAL r_alu_control            : t_alu_control;
    SIGNAL r_register_control       : t_register_control;
    SIGNAL r_sram_control           : t_sram_control;
    
    --  Processor Data Path Signals
    SIGNAL r_src_and_dest           : t_src_and_dest;
    SIGNAL r_data_from_reg_block    : t_data_bus;
    SIGNAL r_data_from_alu          : t_data_bus;
    SIGNAL r_data_from_sram         : t_data_bus;
    SIGNAL r_data_bus               : t_data_bus;

    --  SREG Status Bit Signals to and from ALU    
    SIGNAL r_sreg                   : t_sreg_bits;
    SIGNAL r_sreg_update            : t_sreg_bits;
    
    -- IO Signal
    SIGNAL r_io_port_b_control      : t_io_port_control;
    SIGNAL r_io_port_d_control      : t_io_port_control;
    SIGNAL r_data_from_port_b       : t_data_bus;
    SIGNAL r_data_from_port_d       : t_data_bus;
        
    --  Simulation check status
    SIGNAL r_sim_status             : t_sim_status;
    
        

BEGIN

    u_clock_and_reset : clock_and_reset PORT MAP (
        osl_reset                   => sl_reset,
        osl_clock                   => sl_clock,
        isl_clk_125m                => isl_clk_125m
    );
    
    u_program_counter : program_counter PORT MAP (
        isl_reset                   => sl_reset,
        isl_clock                   => sl_clock,
        ir_pc_control               => r_pc_control,
        oslv16_program_count        => slv16_program_count
    );

    u_program_memory : program_memory PORT MAP (
        isl_reset                   => sl_reset,
        isl_clock                   => sl_clock,
        islv16_program_counter      => slv16_program_count,
        oslv16_op_code              => slv16_op_code    
    );


    u_instr_decoder : instr_decoder PORT MAP (
        isl_reset                   => sl_reset,
        isl_clock                   => sl_clock,
        islv16_op_code              => slv16_op_code,
        islv16_program_count        => slv16_program_count,
        ir_sreg                     => r_sreg,
        or_register_control         => r_register_control, 
        or_alu_control              => r_alu_control,
        or_sram_control             => r_sram_control,
        or_pc_control             => r_pc_control
    );

    u_register_block : register_block PORT MAP (
        isl_reset                   => sl_reset,
        isl_clock                   => sl_clock,
        ir_register_control         => r_register_control,
        ir_data_bus                 => r_data_bus,
        or_data_bus                 => r_data_from_reg_block,
        or_src_and_dest             => r_src_and_dest,
        oa_reg_block_check          => a_register_block
    );
    
    u_alu : alu PORT MAP (
        ir_alu_control              => r_alu_control,
        ir_src_and_dest             => r_src_and_dest,
        ir_sreg                     => r_sreg,
        or_data                     => r_data_from_alu,
        or_sreg                     => r_sreg_update 
    );
    
    u_sreg : sreg PORT MAP (
        isl_reset                   => sl_reset,
        isl_clock                   => sl_clock,
        ir_sreg_update              => r_sreg_update,
        or_sreg                     => r_sreg
    );
    
    u_data_bus : data_bus PORT MAP (
        ir_data_from_reg_block      => r_data_from_reg_block,
        ir_data_from_alu            => r_data_from_alu,
        ir_data_from_sram           => r_data_from_sram,
        or_data_bus                 => r_data_bus
    );
    
        
    u_sram : sram PORT MAP (
        isl_reset                   => sl_reset,
        isl_clock                   => sl_clock,
        ir_sram_control             => r_sram_control,
        ir_data_bus                 => r_data_bus,
        or_data_bus                 => r_data_from_sram
    );    

    
    u_simulation_check : simulation_check PORT MAP (
        isl_reset                   => sl_reset,
        isl_clock                   => sl_clock,
        islv16_program_count        => slv16_program_count,
        islv16_op_code              => slv16_op_code,
        ir_data_from_alu            => r_data_from_alu,
        ir_sreg                     => r_sreg,
        ir_data_bus                 => r_data_bus,
        ia_register_block           => a_register_block,
        or_sim_status               => r_sim_status
    );    
    
END ARCHITECTURE rtl;