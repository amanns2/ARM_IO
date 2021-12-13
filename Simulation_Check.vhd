--------------------------------------------------------------------------------
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--    
--        http://www.apache.org/licenses/LICENSE-2.0
--    
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
--------------------------------------------------------------------------------



LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

USE work.avr_records_pkg.ALL;
USE work.register_block_pkg.ALL;
USE work.alu_pkg.ALL;
USE work.data_bus_pkg.ALL;
USE work.sreg_pkg.ALL;

PACKAGE simulation_check_pkg IS

    TYPE t_avr_op IS (   -- Some operations are extended with "_op" to avoid collision with VHDL keywords
        AND_op,     --  Logic AND
        ANDI,       --  Logic AND with Immediate
        EOR,        --  Logic Exclusive OR
        OR_op,      --  Logic OR
        ORI,        --  Logic OR with Immediate
        ADD,        --  Add without Carry
        ADC,        --  Add with Carry
        ASR,        --  Arithmetic Shift Right
        COM,        --  One's Complement
        CP,         --  Compare without Carry
        CPI,        --  Compare Immediate without Carry
        CPC,        --  Compare with Carry
        DEC,        --  Decrement
        INC,        --  Increment
        LDI,        --  Load Immediate
        LSR,        --  Logical Shift Right
        NEG,        --  Two's Complement
        ROR_op,     --  Rotate Right through Carry
        SUB,        --  Subtract without Carry
        SUBI,       --  Subtract Immediate without Carry
        SBC,        --  Subtract with Carry
        SBCI,       --  Subtract Immediate with Carry
        SWAP,       --  Swap Nibbles
        MOV,        --  Copy Register
        LDS,        --  Load Direct from Dataspace
        LD,         --  Load Indirect
        LDD,        --  Load Indirect with Displacement
        STS,        --  Store Direct to Dataspace
        ST,         --  Store Indirect
        STD,        --  Store Indirect with Displacement                        
        
        BRBS,       --  Branch if Status (Sign) Flag Set
        BRBC,       --  Branch if Status (Sign) Flag Cleared
        BREQ,       --  Branch if Zero Flag Set
        BRNE,       --  Branch if Zero Flag Cleared
        BRCS,       --  Branch if Carry Flag SET
        BRCC,       --  Branch if Carry Flag Cleared
        BRSH,       --  Branch if Same or Higher
        BRLO,       --  Branch if Lower
        BRMI,       --  Branch if Minus
        BRPL,       --  Branch if Plus
        BRGE,       --  Branch if Greater or Equal
        BRLT,       --  Branch if Less Than, Signed
        BRHS,       --  Branch if Half Carry Flag Set
        BRHC,       --  Branch if Half Carry Flag Cleared
        BRTS,       --  Branch if Half T Flag Set
        BRTC,       --  Branch if Half T Flag Cleared
        BRVS,       --  Branch if Overflow Flag Set
        BRVC,       --  Branch if Overflow Flag Cleared
        BRIE,       --  Branch if Interrupt Enabled
        BRID,       --  Branch if Interrupt Disabled
        
        JMP,        --  Absolute Jump
        RJMP,       --  Relative Jump
        CALL,       --  Absolute Call Subroutine
        RCALL,      --  Relative Call Subroutine
        RET,        --  Return from Subroutine
        
        LPM,        --  Load Programm Memory
        IN_op,      --  In From I/O Register Space
        OUT_op,     --  Out To I/O Register Space
        PUSH,       --  Push Register on Stack
        POP,        --  Pop Register from Stack
        
        SEC,        --  Set Carry Bit     
        CLC,        --  Clear Carry Bit
        SEN,        --  Set Negative Bit           
        CLN,        --  Clear Negative Bit           
        SEZ,        --  Set Zero Bit           
        CLZ,        --  Clear Zero Bit           
        SEI,        --  Set Global Interrupt Enable Bit           
        CLI,        --  Clear Global Interrupt Enable Bit
        SES,        --  Set Sign Bit           
        CLS,        --  Clear Sign Bit
        SEV,        --  Set Overflow Bit       
        CLV,        --  Clear Overflow Bit       
        SET,        --  Set Transfer Bit
        CLT,        --  Clear Transfer Bit
        SEH,        --  Set Half-Carry Bit
        CLH,        --  Clear Half-Carry Bit
        
        NOP         --  No Operation, must be last element of this list!
    );
    

    TYPE t_sreg_status IS RECORD
        sl_SREG_Interrupt           : STD_LOGIC;
        sl_SREG_Transfer_Bit        : STD_LOGIC;
        sl_SREG_Half_Carry          : STD_LOGIC;
        sl_SREG_Sign_Bit            : STD_LOGIC;
        sl_SREG_Overflow            : STD_LOGIC;
        sl_SREG_Negative            : STD_LOGIC;
        sl_SREG_Zero                : STD_LOGIC;
        sl_SREG_Carry               : STD_LOGIC;         
    END RECORD t_sreg_status;
    
    TYPE t_alu_status IS RECORD
        sl_ALU_Result               : STD_LOGIC;
        sl_ALU_Data_Valid           : STD_LOGIC;    
    END RECORD t_alu_status;        
    
    TYPE t_sim_status IS RECORD
        avr_op                      : t_avr_op;
        sl_all_good                 : STD_LOGIC;
        sl_pc_counter_state         : STD_LOGIC;
        r_ALU_status                : t_alu_status;
        r_SREG_status               : t_sreg_status;
        sl_register_block_ok        : STD_LOGIC;
    END RECORD t_sim_status;    

    COMPONENT simulation_check IS
        PORT (
            isl_reset               : IN  STD_LOGIC;
            isl_clock               : IN  STD_LOGIC;
            islv16_op_code          : IN  STD_LOGIC_VECTOR(15 DOWNTO 0);
            islv16_program_count    : IN  STD_LOGIC_VECTOR(15 DOWNTO 0);
            ir_data_from_alu        : IN  t_data_bus;
            ir_sreg                 : IN  t_sreg_bits;
            ir_data_bus             : IN  t_data_bus;
            ia_register_block       : IN  ta_register_block;
            or_sim_status           : OUT t_sim_status
        );
    END COMPONENT simulation_check;
END PACKAGE simulation_check_pkg;

--------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

USE work.AVR_Records_pkg.ALL;
USE work.Simulation_Check_pkg.ALL;
USE work.Register_Block_pkg.ALL;
USE work.ALU_pkg.ALL;
USE work.Data_Bus_pkg.ALL;
USE work.SREG_pkg.ALL;

ENTITY simulation_check IS
        PORT (
            isl_reset               : IN STD_LOGIC;
            isl_clock               : IN  STD_LOGIC;
            islv16_program_count    : IN  STD_LOGIC_VECTOR(15 DOWNTO 0);
            islv16_op_code          : IN  STD_LOGIC_VECTOR(15 DOWNTO 0);
            ir_data_from_alu        : IN  t_data_bus;
            ir_sreg                 : IN  t_sreg_bits;
            ir_data_bus             : IN  t_data_bus;
            ia_register_block       : IN  ta_register_block;
            or_sim_status           : OUT t_sim_status
        );
END ENTITY simulation_check;

--------------------------------------------------------------------------------

ARCHITECTURE sim OF simulation_check IS


--##################################################################################################
--##################################################################################################
--#############                                                                   ##################
--#############         Instruction Op Codes                                      ##################
--#############                                                                   ##################
--##################################################################################################
--##################################################################################################

    CONSTANT cBRANCH_op_ID          : STD_LOGIC_VECTOR(4 DOWNTO 0)  := b"1111_0"; 
    
    
    CONSTANT cBRCS_code             : STD_LOGIC_VECTOR(3 DOWNTO 0)  := b"0_000";
    CONSTANT cBREQ_code             : STD_LOGIC_VECTOR(3 DOWNTO 0)  := b"0_001";
    CONSTANT cBRMI_code             : STD_LOGIC_VECTOR(3 DOWNTO 0)  := b"0_010";
    CONSTANT cBRVS_code             : STD_LOGIC_VECTOR(3 DOWNTO 0)  := b"0_011";
    CONSTANT cBRLT_code             : STD_LOGIC_VECTOR(3 DOWNTO 0)  := b"0_100";
    CONSTANT cBRHS_code             : STD_LOGIC_VECTOR(3 DOWNTO 0)  := b"0_101";
    CONSTANT cBRTS_code             : STD_LOGIC_VECTOR(3 DOWNTO 0)  := b"0_110";
    CONSTANT cBRIE_code             : STD_LOGIC_VECTOR(3 DOWNTO 0)  := b"0_111";
    CONSTANT cBRCC_code             : STD_LOGIC_VECTOR(3 DOWNTO 0)  := b"1_000";
    CONSTANT cBRSH_code             : STD_LOGIC_VECTOR(3 DOWNTO 0)  := b"1_000";
    CONSTANT cBRNE_code             : STD_LOGIC_VECTOR(3 DOWNTO 0)  := b"1_001";
    CONSTANT cBRPL_code             : STD_LOGIC_VECTOR(3 DOWNTO 0)  := b"1_010";
    CONSTANT cBRVC_code             : STD_LOGIC_VECTOR(3 DOWNTO 0)  := b"1_011";
    CONSTANT cBRGE_code             : STD_LOGIC_VECTOR(3 DOWNTO 0)  := b"1_100";
    CONSTANT cBRHC_code             : STD_LOGIC_VECTOR(3 DOWNTO 0)  := b"1_101";
    CONSTANT cBRTC_code             : STD_LOGIC_VECTOR(3 DOWNTO 0)  := b"1_110";
    CONSTANT cBRID_code             : STD_LOGIC_VECTOR(3 DOWNTO 0)  := b"1_111";


--##################################################################################################
--##################################################################################################
--#############                                                                   ##################
--#############         Simulation Check Records                                  ##################
--#############                                                                   ##################
--##################################################################################################
--##################################################################################################

    --  Duplicate definitions to make Simulation Checker independent from implementation stage
    CONSTANT c_RAM_SIZE         : INTEGER       := 2048;
    CONSTANT c_SRAM_START       : INTEGER       := 256;                         --  = 0x0100
    CONSTANT c_SRAM_END         : INTEGER       := c_SRAM_START + c_RAM_SIZE;   --  = 0x0900
    

    TYPE t_avr_op_type IS (NOP, LDD_STD, RCALL_RJMP, ALU_IMM, LDS_STS, IN_OUT, ALU_2IN, BRANCH, ALU_1IN, CALL_JMP, RET_RETI, INTERN_1, INTERN_2); 
    
    CONSTANT cIO_REGISTER_RANGE     : INTEGER   := 10;
    TYPE ta_io_registers IS ARRAY (0 TO cIO_REGISTER_RANGE - 1) OF STD_LOGIC_VECTOR(7 DOWNTO 0);

    TYPE ta_sram IS ARRAY (0 TO 2047) OF STD_LOGIC_VECTOR(7 DOWNTO 0);
    
    TYPE t_check_types IS (NONE, PC_COUNTER);
    
    TYPE t_registers IS RECORD
        b_decode_instruction        : BOOLEAN;
        avr_op_type                 : t_avr_op_type;
        avr_op                      : t_avr_op;
        slv16_op_code               : STD_LOGIC_VECTOR(15 DOWNTO 0);
        slv4_alu_1_op               : STD_LOGIC_VECTOR( 3 DOWNTO 0);
        slv6_alu_2_op               : STD_LOGIC_VECTOR( 5 DOWNTO 0);
        slv4_alu_imm_op             : STD_LOGIC_VECTOR( 3 DOWNTO 0);
            
        i_dest_addr                 : INTEGER;
        i_src_addr                  : INTEGER;
        i_abs_addr                  : INTEGER;
            
        slv8_dest_arg               : STD_LOGIC_VECTOR(7 DOWNTO 0);
        slv8_src_arg                : STD_LOGIC_VECTOR(7 DOWNTO 0);
        slv8_imm_arg                : STD_LOGIC_VECTOR(7 DOWNTO 0);
        
        slv8_sram_read_data         : STD_LOGIC_VECTOR(7 DOWNTO 0);
        b_sram_to_reg_write       : BOOLEAN;
        
        a_register_block            : ta_register_block;
        a_sram                      : ta_sram;
        i_stack_pointer             : INTEGER;
        a_io_registers              : ta_io_registers;
        
        i_check_at_cycle_x          : INTEGER;
        fsm_what_to_check           : t_check_types;
        i_expected_pc_counter       : INTEGER;
        sl_pc_counter_state         : STD_LOGIC;
    END RECORD t_registers;
                                   
    TYPE t_alu_expected IS RECORD        
        avr_op                      : t_avr_op;
        slv8_data_bus               : STD_LOGIC_VECTOR(7 DOWNTO 0);
        r_sreg                      : t_sreg_bits;
            
        b_check_alu_output          : BOOLEAN;
        b_check_sreg_d1       : BOOLEAN;
        b_register_writeback        : BOOLEAN;
        b_limit_to_4bit_dest_reg    : BOOLEAN;
    END RECORD t_alu_expected;


--##################################################################################################
--##################################################################################################
--#############                                                                   ##################
--#############         Zero-Bit Calculation                                      ##################
--#############                                                                   ##################
--##################################################################################################
--##################################################################################################

    --  Function to evaluate zero flag  ----------------------------------------
    FUNCTION f_zero_flag (islv8_data : IN STD_LOGIC_VECTOR(7 DOWNTO 0))     ----
    RETURN STD_LOGIC IS                                                     ----
        VARIABLE vsl_zero_flag : STD_LOGIC;                                 ----
    BEGIN                                                                   ----
        vsl_zero_flag := '1';                                               ----
        FOR i IN 0 TO 7 LOOP                                                ----
            IF islv8_data(i) = '1' THEN vsl_zero_flag := '0'; END IF;       ----
        END LOOP;                                                           ----
        RETURN STD_LOGIC(vsl_zero_flag);                                    ----
    END FUNCTION f_zero_flag;  -------------------------------------------------



--##################################################################################################
--##################################################################################################
--#############                                                                   ##################
--#############         Simulate_ALU_Immediate                                    ##################
--#############                                                                   ##################
--##################################################################################################
--##################################################################################################


    PROCEDURE simulate_alu_immediate (
        curr_cmd                                      : IN  t_registers;
        curr_sreg                                     : IN  t_sreg_bits;
        VARIABLE or_alu_expected                      : OUT t_alu_expected;
        VARIABLE osl_decoder_status                   : OUT STD_LOGIC
    ) IS    
        VARIABLE vslv8_data_out                       : STD_LOGIC_VECTOR(7 DOWNTO 0);
        VARIABLE vr_sreg                              : t_sreg_bits;
    BEGIN
    
        or_alu_expected.b_check_alu_output            := TRUE;
        or_alu_expected.b_check_sreg_d1         := TRUE;
        or_alu_expected.b_register_writeback          := TRUE;
        or_alu_expected.b_limit_to_4bit_dest_reg      := TRUE;       --  All immediate commands support only 4-bit Destination Register Addressing
        vr_sreg                                       := curr_sreg;
        osl_decoder_status                            := '1';    --  1 is good, as it is shown in green during simulation. X is bad and shown red
        
        CASE curr_cmd.slv4_alu_imm_op IS

            --  ANDI - Logical AND with Immediate, 1-input ALU command
            WHEN B"0111"    =>  or_alu_expected.avr_op:= ANDI;
                                vslv8_data_out        := curr_cmd.slv8_imm_arg AND curr_cmd.slv8_dest_arg;
                                vr_sreg.sl_sign       := vslv8_data_out(7);
                                vr_sreg.sl_overflow   := '0';
                                vr_sreg.sl_negative   := vslv8_data_out(7);
                                vr_sreg.sl_zero       := f_zero_flag(vslv8_data_out);
                                

            --  ORI  - Logical OR with Immediate, 1-input ALU command
            WHEN B"0110"    =>  or_alu_expected.avr_op:= ORI;
                                vslv8_data_out        := curr_cmd.slv8_imm_arg OR curr_cmd.slv8_dest_arg;
                                vr_sreg.sl_sign       := vslv8_data_out(7);
                                vr_sreg.sl_overflow   := '0';
                                vr_sreg.sl_negative   := vslv8_data_out(7);
                                vr_sreg.sl_zero       := f_zero_flag(vslv8_data_out);
                                                

           --  CPI  - Compare Immediate without Carry
           WHEN B"0011"     =>  or_alu_expected.avr_op:= CPI;
                                vslv8_data_out        := STD_LOGIC_VECTOR( SIGNED(curr_cmd.slv8_dest_arg) - SIGNED(curr_cmd.slv8_imm_arg) );
                                vr_sreg.sl_negative   := vslv8_data_out(7);
                                vr_sreg.sl_overflow   := (         curr_cmd.slv8_dest_arg(7)  AND (NOT curr_cmd.slv8_imm_arg(7)) AND (NOT vslv8_data_out(7)) )
                                                         OR ( (NOT curr_cmd.slv8_dest_arg(7)) AND      curr_cmd.slv8_imm_arg(7)  AND      vslv8_data_out(7)  );
                                vr_sreg.sl_sign       := vr_sreg.sl_negative XOR vr_sreg.sl_overflow;
                                vr_sreg.sl_zero       := f_zero_flag(vslv8_data_out);
                                vr_sreg.sl_half_carry := ( (NOT    curr_cmd.slv8_dest_arg(3)) AND curr_cmd.slv8_imm_arg(3)  )
                                                         OR (      curr_cmd.slv8_imm_arg(3)   AND vslv8_data_out(3) )
                                                         OR ( (NOT curr_cmd.slv8_dest_arg(3)) AND vslv8_data_out(3) );
                                vr_sreg.sl_carry      := ( (NOT    curr_cmd.slv8_dest_arg(7)) AND curr_cmd.slv8_imm_arg(7)  )
                                                         OR (      curr_cmd.slv8_imm_arg(7)   AND vslv8_data_out(7) )
                                                         OR ( (NOT curr_cmd.slv8_dest_arg(7)) AND vslv8_data_out(7) );
                                or_alu_expected.b_register_writeback        := FALSE;
                                or_alu_expected.b_check_alu_output          := FALSE;
                                

            --  LDI  - Load Immediate
            WHEN B"1110"    =>  or_alu_expected.avr_op:= LDI;
                                vslv8_data_out        := curr_cmd.slv8_imm_arg;
                                

            --  SUBI  - Subtract Immediate without Carry
            WHEN B"0101"    =>  or_alu_expected.avr_op:= SUBI;
                                vslv8_data_out        := STD_LOGIC_VECTOR( SIGNED(curr_cmd.slv8_dest_arg) - SIGNED(curr_cmd.slv8_imm_arg) );
                                vr_sreg.sl_negative   := vslv8_data_out(7);
                                vr_sreg.sl_overflow   := (         curr_cmd.slv8_dest_arg(7)  AND (NOT curr_cmd.slv8_imm_arg(7)) AND (NOT vslv8_data_out(7)) )
                                                         OR ( (NOT curr_cmd.slv8_dest_arg(7)) AND      curr_cmd.slv8_imm_arg(7)  AND      vslv8_data_out(7)  );
                                vr_sreg.sl_sign       := vr_sreg.sl_negative XOR vr_sreg.sl_overflow;
                                vr_sreg.sl_zero       := f_zero_flag(vslv8_data_out);
                                vr_sreg.sl_half_carry := ( (NOT    curr_cmd.slv8_dest_arg(3)) AND curr_cmd.slv8_imm_arg(3)  )
                                                         OR (      curr_cmd.slv8_imm_arg(3)   AND vslv8_data_out(3) )
                                                         OR ( (NOT curr_cmd.slv8_dest_arg(3)) AND vslv8_data_out(3) );
                                vr_sreg.sl_carry      := ( (NOT    curr_cmd.slv8_dest_arg(7)) AND curr_cmd.slv8_imm_arg(7)  )
                                                         OR (      curr_cmd.slv8_imm_arg(7)   AND vslv8_data_out(7) )
                                                         OR ( (NOT curr_cmd.slv8_dest_arg(7)) AND vslv8_data_out(7) );
                                

            --  SBCI  - Subtract Immediate with Carry
            WHEN B"0100"    =>  or_alu_expected.avr_op:= SBCI;
                                IF curr_sreg.sl_carry = '0' THEN  --  Use intentionally internal SREG, as the external will just now get its value
                                    vslv8_data_out    := STD_LOGIC_VECTOR( SIGNED(curr_cmd.slv8_dest_arg) - SIGNED(curr_cmd.slv8_imm_arg) );
                                ELSE
                                    vslv8_data_out    := STD_LOGIC_VECTOR( SIGNED(curr_cmd.slv8_dest_arg) - SIGNED(curr_cmd.slv8_imm_arg) - 1);
                                END IF;
                                vr_sreg.sl_negative   := vslv8_data_out(7);
                                vr_sreg.sl_overflow   := (         curr_cmd.slv8_dest_arg(7)  AND (NOT curr_cmd.slv8_imm_arg(7)) AND (NOT vslv8_data_out(7)) )
                                                         OR ( (NOT curr_cmd.slv8_dest_arg(7)) AND      curr_cmd.slv8_imm_arg(7)  AND      vslv8_data_out(7)  );
                                vr_sreg.sl_sign       := vr_sreg.sl_negative XOR vr_sreg.sl_overflow;
                                vr_sreg.sl_zero       := f_zero_flag(vslv8_data_out) AND curr_sreg.sl_zero;
                                vr_sreg.sl_half_carry := ( (NOT    curr_cmd.slv8_dest_arg(3)) AND curr_cmd.slv8_imm_arg(3)  )
                                                         OR (      curr_cmd.slv8_imm_arg(3)   AND vslv8_data_out(3) )
                                                         OR ( (NOT curr_cmd.slv8_dest_arg(3)) AND vslv8_data_out(3) );
                                vr_sreg.sl_carry      := ( (NOT    curr_cmd.slv8_dest_arg(7)) AND curr_cmd.slv8_imm_arg(7)  )
                                                         OR (      curr_cmd.slv8_imm_arg(7)   AND vslv8_data_out(7) )
                                                         OR ( (NOT curr_cmd.slv8_dest_arg(7)) AND vslv8_data_out(7) );
                               



            WHEN OTHERS     =>  osl_decoder_status    := 'X';    --  1 is good, as it is shown in green during simulation. X is bad and shown red
                                                              
        END CASE;
    
        or_alu_expected.slv8_data_bus   := vslv8_data_out;
        or_alu_expected.r_sreg          := vr_sreg;
        
    END PROCEDURE simulate_alu_immediate;


--##################################################################################################
--##################################################################################################
--#############                                                                   ##################
--#############         Simulate_ALU_1_Input                                      ##################
--#############                                                                   ##################
--##################################################################################################
--##################################################################################################


    PROCEDURE simulate_alu_1 (
        curr_cmd                                      : IN  t_registers;
        curr_sreg                                     : IN  t_sreg_bits;
        VARIABLE or_alu_expected                      : OUT t_alu_expected;
        VARIABLE osl_decoder_status                   : OUT STD_LOGIC
    ) IS    
        VARIABLE vslv8_data_out                       : STD_LOGIC_VECTOR(7 DOWNTO 0);
        VARIABLE vr_sreg                              : t_sreg_bits;
    BEGIN
    
        or_alu_expected.b_check_alu_output            := TRUE;
        or_alu_expected.b_check_sreg_d1         := TRUE;
        or_alu_expected.b_register_writeback          := TRUE;
        vr_sreg                                       := curr_sreg;
        osl_decoder_status                            := '1';    --  1 is good, as it is shown in green during simulation. X is bad and shown red
        
        CASE curr_cmd.slv4_alu_1_op IS

                                                              
            --  ASR  - Arithmetic Shift Right
            WHEN B"0101"    =>  or_alu_expected.avr_op:= ASR;
                                vslv8_data_out        := curr_cmd.slv8_dest_arg(7) & curr_cmd.slv8_dest_arg(7 DOWNTO 1);
                                vslv8_data_out(7)     := curr_cmd.slv8_dest_arg(7);
                                vr_sreg.sl_carry      := curr_cmd.slv8_dest_arg(0);
                                vr_sreg.sl_negative   := vslv8_data_out(7);
                                vr_sreg.sl_overflow   := vr_sreg.sl_negative XOR vr_sreg.sl_carry;
                                vr_sreg.sl_sign       := vr_sreg.sl_negative XOR vr_sreg.sl_overflow;
                                vr_sreg.sl_zero       := f_zero_flag(vslv8_data_out);
                                
            --  COM  - One's Complement
            WHEN B"0000"    =>  or_alu_expected.avr_op:= COM;
                                vslv8_data_out        := NOT curr_cmd.slv8_dest_arg;
                                vr_sreg.sl_carry      := '1';
                                vr_sreg.sl_negative   := vslv8_data_out(7);
                                vr_sreg.sl_overflow   := '0';
                                vr_sreg.sl_sign       := vr_sreg.sl_negative XOR vr_sreg.sl_overflow;
                                vr_sreg.sl_zero       := f_zero_flag(vslv8_data_out);
                                

            --  DEC  - Decrement
            WHEN B"1010"    =>  or_alu_expected.avr_op:= DEC;
                                vslv8_data_out        := STD_LOGIC_VECTOR( SIGNED(curr_cmd.slv8_dest_arg) - 1 );
                                vr_sreg.sl_negative   := vslv8_data_out(7);
                                vr_sreg.sl_overflow   := (NOT vslv8_data_out(7)) AND vslv8_data_out(6) AND vslv8_data_out(5) AND vslv8_data_out(4)
                                                          AND vslv8_data_out(3)  AND vslv8_data_out(2) AND vslv8_data_out(1) AND vslv8_data_out(0);
                                vr_sreg.sl_sign       := vr_sreg.sl_negative XOR vr_sreg.sl_overflow;
                                vr_sreg.sl_zero       := f_zero_flag(vslv8_data_out);
                                                
            --   INC  - Increment
            WHEN B"0011"    =>  or_alu_expected.avr_op:= INC;
                                vslv8_data_out        := STD_LOGIC_VECTOR( SIGNED(curr_cmd.slv8_dest_arg) + 1 );
                                vr_sreg.sl_negative   := vslv8_data_out(7);
                                vr_sreg.sl_overflow   := vslv8_data_out(7) AND (NOT vslv8_data_out(6)) AND (NOT vslv8_data_out(5)) 
                                                                           AND (NOT vslv8_data_out(4)) AND (NOT vslv8_data_out(3)) 
                                                                           AND (NOT vslv8_data_out(2)) AND (NOT vslv8_data_out(1)) 
                                                                           AND (NOT vslv8_data_out(0));
                                vr_sreg.sl_sign       := vr_sreg.sl_negative XOR vr_sreg.sl_overflow;
                                vr_sreg.sl_zero       := f_zero_flag(vslv8_data_out);


            --  LSR  - Logical Shift Right
            WHEN B"0110"    =>  or_alu_expected.avr_op:= LSR;
                                vslv8_data_out        := '0' & curr_cmd.slv8_dest_arg(7 DOWNTO 1);
                                vr_sreg.sl_carry      := curr_cmd.slv8_dest_arg(0);
                                vr_sreg.sl_negative   := vslv8_data_out(7);
                                vr_sreg.sl_overflow   := vr_sreg.sl_negative XOR vr_sreg.sl_carry;
                                vr_sreg.sl_sign       := vr_sreg.sl_negative XOR vr_sreg.sl_overflow;
                                vr_sreg.sl_zero       := f_zero_flag(vslv8_data_out);
                                                

            --  NEG  - Two's Complement
            WHEN B"0001"    =>  or_alu_expected.avr_op:= NEG;
                                vslv8_data_out        := STD_LOGIC_VECTOR(0 - SIGNED(curr_cmd.slv8_dest_arg));
                                vr_sreg.sl_negative   := vslv8_data_out(7);
                                vr_sreg.sl_overflow   := vslv8_data_out(7) AND (NOT vslv8_data_out(6)) AND (NOT vslv8_data_out(5))
                                                                           AND (NOT vslv8_data_out(4)) AND (NOT vslv8_data_out(3)) AND (NOT vslv8_data_out(2))
                                                                           AND (NOT vslv8_data_out(1)) AND (NOT vslv8_data_out(0));
                                vr_sreg.sl_sign       := vr_sreg.sl_negative XOR vr_sreg.sl_overflow;
                                vr_sreg.sl_zero       := f_zero_flag(vslv8_data_out);
                                vr_sreg.sl_half_carry := vslv8_data_out(3) OR (NOT curr_cmd.slv8_dest_arg(3));
                                vr_sreg.sl_carry      :=    vslv8_data_out(7) OR vslv8_data_out(6) OR vslv8_data_out(5) OR vslv8_data_out(4) 
                                                         OR vslv8_data_out(3) OR vslv8_data_out(2) OR vslv8_data_out(1) OR vslv8_data_out(0);
                                                                                                
            --  ROR  - Rotate Right through Carry
            WHEN B"0111"    =>  or_alu_expected.avr_op:= ROR_op;
                                vslv8_data_out        := curr_sreg.sl_carry & curr_cmd.slv8_dest_arg(7 DOWNTO 1);
                                vr_sreg.sl_carry      := curr_cmd.slv8_dest_arg(0);
                                vr_sreg.sl_negative   := vslv8_data_out(7);
                                vr_sreg.sl_overflow   := vr_sreg.sl_negative XOR vr_sreg.sl_carry;
                                vr_sreg.sl_sign       := vr_sreg.sl_negative XOR vr_sreg.sl_overflow;
                                vr_sreg.sl_zero       := f_zero_flag(vslv8_data_out);                    

                                
            --  SWAP - SWAP Nibbles
            WHEN B"0010"    =>  or_alu_expected.avr_op:= SWAP;
                                vslv8_data_out        := curr_cmd.slv8_dest_arg(3 DOWNTO 0) & curr_cmd.slv8_dest_arg(7 DOWNTO 4);


            WHEN OTHERS     => osl_decoder_status     := 'X';    --  1 is good, as it is shown in green during simulation. X is bad and shown red
                                                              
        END CASE;
    
        or_alu_expected.slv8_data_bus   := vslv8_data_out;
        or_alu_expected.r_sreg          := vr_sreg;
    
    END PROCEDURE simulate_alu_1;
    
    

--##################################################################################################
--##################################################################################################
--#############                                                                   ##################
--#############         Simulate_ALU_2_Input                                      ##################
--#############                                                                   ##################
--##################################################################################################
--##################################################################################################    
    
    PROCEDURE simulate_alu_2 (
        curr_cmd                                      : IN  t_registers;
        curr_sreg                                     : IN  t_sreg_bits;
        VARIABLE or_alu_expected                      : OUT t_alu_expected;
        VARIABLE osl_decoder_status                   : OUT STD_LOGIC
    ) IS
        VARIABLE vslv8_data_out                       : STD_LOGIC_VECTOR(7 DOWNTO 0);
        VARIABLE vr_sreg                              : t_sreg_bits;
    BEGIN
    
        or_alu_expected.b_check_alu_output            := TRUE;
        or_alu_expected.b_check_sreg_d1         := TRUE;
        or_alu_expected.b_register_writeback          := TRUE;
        vr_sreg                                       := curr_sreg;
        osl_decoder_status                            := '1';    --  1 is good, as it is shown in green during simulation. X is bad and shown red
        
        CASE curr_cmd.slv6_alu_2_op IS

            --  AND  - Logical AND
            WHEN B"0010_00" =>  or_alu_expected.avr_op:= AND_op;
                                vslv8_data_out        := curr_cmd.slv8_src_arg AND curr_cmd.slv8_dest_arg;
                                vr_sreg.sl_sign       := vslv8_data_out(7);
                                vr_sreg.sl_overflow   := '0';
                                vr_sreg.sl_negative   := vslv8_data_out(7);
                                vr_sreg.sl_zero       := f_zero_flag(vslv8_data_out);
                          
                          
            --  EOR - Logical XOR      
            WHEN B"0010_01" =>  or_alu_expected.avr_op:= EOR;
                                vslv8_data_out        := curr_cmd.slv8_src_arg XOR curr_cmd.slv8_dest_arg;
                                vr_sreg.sl_sign       := vslv8_data_out(7);
                                vr_sreg.sl_overflow   := '0';
                                vr_sreg.sl_negative   := vslv8_data_out(7);
                                vr_sreg.sl_zero       := f_zero_flag(vslv8_data_out);
                             
                             
            --  OR   - Logical OR
            WHEN B"0010_10" =>  or_alu_expected.avr_op:= OR_op;
                                vslv8_data_out        := curr_cmd.slv8_src_arg OR curr_cmd.slv8_dest_arg;
                                vr_sreg.sl_sign       := vslv8_data_out(7);
                                vr_sreg.sl_overflow   := '0';
                                vr_sreg.sl_negative   := vslv8_data_out(7);
                                vr_sreg.sl_zero       := f_zero_flag(vslv8_data_out);   
                                
                                
            --  ADD  - Add without Carry
            WHEN B"0000_11" =>  or_alu_expected.avr_op:= ADD;
                                vslv8_data_out        := STD_LOGIC_VECTOR( UNSIGNED(curr_cmd.slv8_src_arg) + UNSIGNED(curr_cmd.slv8_dest_arg) );
                                vr_sreg.sl_negative   := vslv8_data_out(7);
                                vr_sreg.sl_overflow   := (         curr_cmd.slv8_src_arg(7)  AND      curr_cmd.slv8_dest_arg(7)  AND (NOT vslv8_data_out(7)) )
                                                         OR ( (NOT curr_cmd.slv8_src_arg(7)) AND (NOT curr_cmd.slv8_dest_arg(7)) AND      vslv8_data_out(7)  );
                                vr_sreg.sl_sign       := vr_sreg.sl_negative XOR vr_sreg.sl_overflow;
                                vr_sreg.sl_zero       := f_zero_flag(vslv8_data_out);
                                vr_sreg.sl_half_carry := (     curr_cmd.slv8_dest_arg(3)  AND curr_cmd.slv8_src_arg(3)   )
                                                         OR (  curr_cmd.slv8_src_arg(3)   AND (NOT vslv8_data_out(3)) )
                                                         OR (  curr_cmd.slv8_dest_arg(3)  AND (NOT vslv8_data_out(3)) );
                                vr_sreg.sl_carry      := (     curr_cmd.slv8_dest_arg(7)  AND curr_cmd.slv8_src_arg(7)   )
                                                         OR (  curr_cmd.slv8_src_arg(7)   AND (NOT vslv8_data_out(7)) )
                                                         OR (  curr_cmd.slv8_dest_arg(7)  AND (NOT vslv8_data_out(7)) );
                                                             
                                                              
            --  ADC  - Add with Carry
            WHEN B"0001_11" =>  or_alu_expected.avr_op:= ADC;
                                IF curr_sreg.sl_carry = '0' THEN  --  Use intentionally internal SREG, as the external will just now get its value
                                    vslv8_data_out    := STD_LOGIC_VECTOR( UNSIGNED(curr_cmd.slv8_src_arg) + UNSIGNED(curr_cmd.slv8_dest_arg) );
                                ELSE
                                    vslv8_data_out    := STD_LOGIC_VECTOR( UNSIGNED(curr_cmd.slv8_src_arg) + UNSIGNED(curr_cmd.slv8_dest_arg) + 1 );
                                END IF;
                                vr_sreg.sl_negative   := vslv8_data_out(7);
                                vr_sreg.sl_overflow   := (         curr_cmd.slv8_src_arg(7)  AND      curr_cmd.slv8_dest_arg(7)  AND (NOT vslv8_data_out(7)) )
                                                         OR ( (NOT curr_cmd.slv8_src_arg(7)) AND (NOT curr_cmd.slv8_dest_arg(7)) AND      vslv8_data_out(7)  );
                                vr_sreg.sl_sign       := vr_sreg.sl_negative XOR vr_sreg.sl_overflow;
                                vr_sreg.sl_zero       := f_zero_flag(vslv8_data_out);
                                vr_sreg.sl_half_carry := (     curr_cmd.slv8_dest_arg(3)  AND curr_cmd.slv8_src_arg(3)   )
                                                         OR (  curr_cmd.slv8_src_arg(3)   AND (NOT vslv8_data_out(3)) )
                                                         OR (  curr_cmd.slv8_dest_arg(3)  AND (NOT vslv8_data_out(3)) );
                                vr_sreg.sl_carry      := (     curr_cmd.slv8_dest_arg(7)  AND curr_cmd.slv8_src_arg(7)   )
                                                         OR (  curr_cmd.slv8_src_arg(7)   AND (NOT vslv8_data_out(7)) )
                                                         OR (  curr_cmd.slv8_dest_arg(7)  AND (NOT vslv8_data_out(7)) );


            --  CP   -  Compare without Carry
            WHEN B"0001_01" =>  or_alu_expected.avr_op:= CP;
                                vslv8_data_out        := STD_LOGIC_VECTOR( SIGNED(curr_cmd.slv8_dest_arg) - SIGNED(curr_cmd.slv8_src_arg) );
                                vr_sreg.sl_negative   := vslv8_data_out(7);
                                vr_sreg.sl_overflow   := (         curr_cmd.slv8_dest_arg(7)  AND (NOT curr_cmd.slv8_src_arg(7)) AND (NOT vslv8_data_out(7)) )
                                                         OR ( (NOT curr_cmd.slv8_dest_arg(7)) AND      curr_cmd.slv8_src_arg(7)  AND      vslv8_data_out(7)  );
                                vr_sreg.sl_sign       := vr_sreg.sl_negative XOR vr_sreg.sl_overflow;
                                vr_sreg.sl_zero       := f_zero_flag(vslv8_data_out);
                                vr_sreg.sl_half_carry := ( (NOT curr_cmd.slv8_dest_arg(3)) AND curr_cmd.slv8_src_arg(3)  )
                                                         OR (      curr_cmd.slv8_src_arg(3)   AND vslv8_data_out(3) )
                                                         OR ( (NOT curr_cmd.slv8_dest_arg(3)) AND vslv8_data_out(3) );
                                vr_sreg.sl_carry      := ( (NOT    curr_cmd.slv8_dest_arg(7)) AND curr_cmd.slv8_src_arg(7)  )
                                                         OR (      curr_cmd.slv8_src_arg(7)   AND vslv8_data_out(7) )
                                                         OR ( (NOT curr_cmd.slv8_dest_arg(7)) AND vslv8_data_out(7) );
                                or_alu_expected.b_register_writeback   := FALSE;
                                or_alu_expected.b_check_alu_output     := FALSE;
                                

            --  CPC  - Compare without Carry
            WHEN B"0000_01" =>  or_alu_expected.avr_op:= CPC;
                                IF curr_sreg.sl_carry = '0' THEN  --  Use intentionally internal SREG, as the external will just now get its value
                                    vslv8_data_out    := STD_LOGIC_VECTOR( SIGNED(curr_cmd.slv8_dest_arg) - SIGNED(curr_cmd.slv8_src_arg) );
                                ELSE
                                    vslv8_data_out    := STD_LOGIC_VECTOR( SIGNED(curr_cmd.slv8_dest_arg) - SIGNED(curr_cmd.slv8_src_arg) - 1);
                                END IF;
                                vr_sreg.sl_negative   := vslv8_data_out(7);
                                vr_sreg.sl_overflow   := (         curr_cmd.slv8_dest_arg(7)  AND (NOT curr_cmd.slv8_src_arg(7)) AND (NOT vslv8_data_out(7)) )
                                                         OR ( (NOT curr_cmd.slv8_dest_arg(7)) AND      curr_cmd.slv8_src_arg(7)  AND      vslv8_data_out(7)  );
                                vr_sreg.sl_sign       := vr_sreg.sl_negative XOR vr_sreg.sl_overflow;
                                vr_sreg.sl_zero       := f_zero_flag(vslv8_data_out) AND curr_sreg.sl_zero;
                                vr_sreg.sl_half_carry := ( (NOT    curr_cmd.slv8_dest_arg(3)) AND curr_cmd.slv8_src_arg(3)  )
                                                         OR (      curr_cmd.slv8_src_arg(3)   AND vslv8_data_out(3) )
                                                         OR ( (NOT curr_cmd.slv8_dest_arg(3)) AND vslv8_data_out(3) );
                                vr_sreg.sl_carry      := ( (NOT    curr_cmd.slv8_dest_arg(7)) AND curr_cmd.slv8_src_arg(7)  )
                                                         OR (      curr_cmd.slv8_src_arg(7)   AND vslv8_data_out(7) )
                                                         OR ( (NOT curr_cmd.slv8_dest_arg(7)) AND vslv8_data_out(7) );
                                or_alu_expected.b_register_writeback   := FALSE;
                                or_alu_expected.b_check_alu_output     := FALSE;
                                

            --  MOV  - Copy Register
            WHEN B"0010_11" =>  or_alu_expected.avr_op:= MOV;
                                vslv8_data_out        := curr_cmd.slv8_src_arg;


            --  SUB  - Compare without Carry
            WHEN B"0001_10" =>  or_alu_expected.avr_op:= SUB;
                                vslv8_data_out        := STD_LOGIC_VECTOR( SIGNED(curr_cmd.slv8_dest_arg) - SIGNED(curr_cmd.slv8_src_arg) );
                                vr_sreg.sl_negative   := vslv8_data_out(7);
                                vr_sreg.sl_overflow   := (         curr_cmd.slv8_dest_arg(7)  AND (NOT curr_cmd.slv8_src_arg(7)) AND (NOT vslv8_data_out(7)) )
                                                         OR ( (NOT curr_cmd.slv8_dest_arg(7)) AND      curr_cmd.slv8_src_arg(7)  AND      vslv8_data_out(7)  );
                                vr_sreg.sl_sign       := vr_sreg.sl_negative XOR vr_sreg.sl_overflow;
                                vr_sreg.sl_zero       := f_zero_flag(vslv8_data_out);
                                vr_sreg.sl_half_carry := ( (NOT    curr_cmd.slv8_dest_arg(3)) AND curr_cmd.slv8_src_arg(3)  )
                                                         OR (      curr_cmd.slv8_src_arg(3)   AND vslv8_data_out(3) )
                                                         OR ( (NOT curr_cmd.slv8_dest_arg(3)) AND vslv8_data_out(3) );
                                vr_sreg.sl_carry      := ( (NOT    curr_cmd.slv8_dest_arg(7)) AND curr_cmd.slv8_src_arg(7)  )
                                                         OR (      curr_cmd.slv8_src_arg(7)   AND vslv8_data_out(7) )
                                                         OR ( (NOT curr_cmd.slv8_dest_arg(7)) AND vslv8_data_out(7) );


            --  SBC  - Subtract without Carry
            WHEN B"0000_10" =>  or_alu_expected.avr_op:= SBC;
                                IF curr_sreg.sl_carry = '0' THEN  --  Use intentionally internal SREG, as the external will just now get its value
                                    vslv8_data_out    := STD_LOGIC_VECTOR( SIGNED(curr_cmd.slv8_dest_arg) - SIGNED(curr_cmd.slv8_src_arg) );
                                ELSE
                                    vslv8_data_out    := STD_LOGIC_VECTOR( SIGNED(curr_cmd.slv8_dest_arg) - SIGNED(curr_cmd.slv8_src_arg) - 1);
                                END IF;
                                vr_sreg.sl_negative   := vslv8_data_out(7);
                                vr_sreg.sl_overflow   := (         curr_cmd.slv8_dest_arg(7)  AND (NOT curr_cmd.slv8_src_arg(7)) AND (NOT vslv8_data_out(7)) )
                                                         OR ( (NOT curr_cmd.slv8_dest_arg(7)) AND      curr_cmd.slv8_src_arg(7)  AND      vslv8_data_out(7)  );
                                vr_sreg.sl_sign       := vr_sreg.sl_negative XOR vr_sreg.sl_overflow;
                                vr_sreg.sl_zero       := f_zero_flag(vslv8_data_out) AND curr_sreg.sl_zero;
                                vr_sreg.sl_half_carry := ( (NOT    curr_cmd.slv8_dest_arg(3)) AND curr_cmd.slv8_src_arg(3)  )
                                                         OR (      curr_cmd.slv8_src_arg(3)   AND vslv8_data_out(3) )
                                                         OR ( (NOT curr_cmd.slv8_dest_arg(3)) AND vslv8_data_out(3) );
                                vr_sreg.sl_carry      := ( (NOT    curr_cmd.slv8_dest_arg(7)) AND curr_cmd.slv8_src_arg(7)  )
                                                         OR (      curr_cmd.slv8_src_arg(7)   AND vslv8_data_out(7) )
                                                         OR ( (NOT curr_cmd.slv8_dest_arg(7)) AND vslv8_data_out(7) );

                                
            WHEN OTHERS         => osl_decoder_status := 'X';    --  1 is good, as it is shown in green during simulation. X is bad and shown red
                                                              
        END CASE;
    
        or_alu_expected.slv8_data_bus   := vslv8_data_out;
        or_alu_expected.r_sreg          := vr_sreg;
    
    END PROCEDURE simulate_alu_2;    
    
    
    
    
--##################################################################################################
--##################################################################################################
--#############                                                                   ##################
--#############         MAIN  ARCHITECTURE                                        ##################
--#############                                                                   ##################
--##################################################################################################
--##################################################################################################    
    
    SIGNAL sl_delayed_clock                 : STD_LOGIC;
    SIGNAL i_cycle_counter                  : INTEGER       := 0;

    SIGNAL r, r_next                        : t_registers;
    SIGNAL r_alu_expected                   : t_alu_expected;
    SIGNAL r_next_alu_expected              : t_alu_expected;
    SIGNAL r_sim_status                     : t_sim_status;      
    
BEGIN

    --##    Decode and track Op-Codes
    --##
    --######################################################################
    comb_proc : PROCESS (r, r_alu_expected, isl_reset, islv16_program_count, islv16_op_code)

        VARIABLE v                              : t_registers;
        VARIABLE v_alu_expected                 : t_alu_expected;
        VARIABLE vsl_alu_imm_decoder_status     : STD_LOGIC;
        VARIABLE vusig16_temp_number            : UNSIGNED(15 DOWNTO 0);
        VARIABLE vslv6_io_reg_addr              : STD_LOGIC_VECTOR(5 DOWNTO 0);
        VARIABLE vi_io_register_addr            : INTEGER;
        
        VARIABLE vb_execute_branch              : BOOLEAN;
        VARIABLE vslv4_branch_op                : STD_LOGIC_VECTOR(3 DOWNTO 0);
        VARIABLE vi_realative_jump_addr         : INTEGER;

    BEGIN
        v                                       := r;       --  Keep registers stable
        v.b_decode_instruction                  := TRUE;    --  If instruction decoding is disabled, it is done so only for a single cycle


        IF r.b_decode_instruction THEN    --    Do NOT decode the op-code if it is just an address argument of a previous instruction
            --  Decode Op-code
            --  ==============
            IF    islv16_op_code(15 DOWNTO  0) = x"0000"    THEN v.avr_op_type := NOP;
            
            ELSIF islv16_op_code(15 DOWNTO  0) = x"C508"    OR
                  islv16_op_code(15 DOWNTO  0) = x"C518"    THEN v.avr_op_type := RET_RETI;
            
            ELSIF islv16_op_code(15 DOWNTO 12) = B"1000"    OR
                  islv16_op_code(15 DOWNTO 12) = B"1010"    THEN v.avr_op_type := LDD_STD;   --  This is either an LDD or STD command
    
            ELSIF islv16_op_code(15 DOWNTO 13) = B"110"     THEN v.avr_op_type := RCALL_RJMP;   --  This is either an RCALL or RJMP command
    
            ELSIF islv16_op_code(15 DOWNTO 12) = B"0011"    OR
                  islv16_op_code(15 DOWNTO 12) = B"1110"    OR
                  islv16_op_code(15 DOWNTO 12) = B"0111"    OR
                  islv16_op_code(15 DOWNTO 14) = B"01"      THEN v.avr_op_type := ALU_IMM;    --  This is an Immediate-and-Regster ALU command
                  
            ELSIF islv16_op_code(15 DOWNTO  9) = B"1001_000" AND
                  islv16_op_code( 3 DOWNTO  0) = B"0000"    THEN v.avr_op_type := LDS_STS;    --  this is an LDS or STS command
                                                                 v.avr_op                   := LDS; 
                                                                 v.b_decode_instruction     := FALSE;
                                                                 
            ELSIF islv16_op_code(15 DOWNTO  9) = B"1001_001" AND
                  islv16_op_code( 3 DOWNTO  0) = B"0000"    THEN v.avr_op_type := LDS_STS;    --  this is an LDS or STS command
                                                                 v.avr_op                   := STS; 
                                                                 v.b_decode_instruction     := FALSE;
                                                                 
                                 
            ELSIF islv16_op_code(15 DOWNTO 12) = B"1011"    THEN v.avr_op_type := IN_OUT;     --  this is an IN or OUT command 
            
            ELSIF islv16_op_code(15 DOWNTO 12) = B"0010"    OR
                  islv16_op_code(15 DOWNTO 11) = B"0000_1"  OR
                  islv16_op_code(15 DOWNTO 11) = B"0001_1"  OR
                  islv16_op_code(15 DOWNTO 10) = B"0000_01" OR
                  islv16_op_code(15 DOWNTO 10) = B"0001_01" THEN v.avr_op_type := ALU_2IN;    --  this is 2-Register ALU command
                  
            ELSIF islv16_op_code(15 DOWNTO 11) = B"1111_0"  THEN v.avr_op_type := BRANCH;      --  this is a BRxx command
            
            ELSIF islv16_op_code(15 DOWNTO  9) = B"1001_010" AND
                  islv16_op_code( 3 DOWNTO  2) /= "11"      THEN v.avr_op_type := ALU_1IN;     --  this is a Single-Input ALU command
                   
            ELSIF islv16_op_code(15 DOWNTO  9) = B"1001_010" AND
                  islv16_op_code( 3 DOWNTO  2) = "11"       THEN v.avr_op_type := CALL_JMP;    --  this is a 2-Instruction CAll or Jump command
                                                                IF islv16_op_code(1) = '0' THEN v.avr_op := JMP;           -- Mark this is a JUMP Instruction
                                                                ELSE                            v.avr_op := CALL; END IF;  -- Mark this as a CALL Instruction
                                                                 v.b_decode_instruction := FALSE;
                   
            ELSE
                  v.avr_op                          := NOP;
                                 
            END IF;
            
        END IF;     -- End decode instruction ... or not, if the op-code is just an address argument
        
        
        --##    Check command execution
        --##
        --#############################
        
        --  Prepare intermediate results for easier coding:
        v.slv16_op_code                         := islv16_op_code;
        v.slv4_alu_1_op                         := (OTHERS => '0');    --  Clear to avoid confusion
        v.slv6_alu_2_op                         := (OTHERS => '0');    --  Clear to avoid confusion
        v.slv4_alu_imm_op                       := (OTHERS => '0');    --  Clear to avoid confusion
        
        v.i_dest_addr                           := 0;                  --  Clear to avoid confusion
        v.i_src_addr                            := 0;                  --  Clear to avoid confusion
        v.i_abs_addr                            := 0;                  --  Clear to avoid confusion
        
        v.slv8_dest_arg                         := (OTHERS => '0');    --  Clear to avoid confusion
        v.slv8_src_arg                          := (OTHERS => '0');    --  Clear to avoid confusion
        v.slv8_imm_arg                          := (OTHERS => '0');    --  Clear to avoid confusion
        
        vslv6_io_reg_addr                       := (OTHERS => '0');    --  Clear to avoid confusion
        vi_io_register_addr                     := 0;                  --  Clear to avoid confusion

        
        v.b_sram_to_reg_write                   := FALSE;

        v_alu_expected.r_sreg                   := r_alu_expected.r_sreg;
        v_alu_expected.b_check_alu_output       := FALSE;     --  By default, do not check ALU output
        v_alu_expected.b_check_sreg_d1          := FALSE;     --  By default, do not check SREG bits
        v_alu_expected.b_register_writeback     := FALSE;     --  By default, do not check Register Block write-back
        
        CASE v.avr_op_type IS
            WHEN ALU_IMM    =>  --  Immediate-and-Register ALU commands
                                --  ===================================
                                --  Check during execution cycle:       Check one cycle after execution:
                                --      - ALU Output                        - SREG state
                                --                                          - Registerblock Entry
                                --
                                v.slv4_alu_imm_op               := islv16_op_code(15 DOWNTO 12);
                                v.i_dest_addr                   := TO_INTEGER( UNSIGNED(islv16_op_code(7 DOWNTO 4)) ) + 16;
                                v.slv8_dest_arg                 := r.a_register_block(v.i_dest_addr);
                                v.slv8_imm_arg                  := islv16_op_code(11 DOWNTO 8) & islv16_op_code(3 DOWNTO 0);
                                simulate_alu_immediate (
                                    curr_cmd                    => v,
                                    curr_sreg                   => r_alu_expected.r_sreg,
                                    or_alu_expected             => v_alu_expected,
                                    osl_decoder_status          => vsl_alu_imm_decoder_status
                                );
                                v.avr_op                        := v_alu_expected.avr_op;
                                --  Force Dest Addr MSB Bit to be always high ... asl Immediate commands are limited to upper half of register block
                                IF v.i_dest_addr < 16 THEN v.i_dest_addr := v.i_dest_addr + 16; END IF;

            
            WHEN ALU_1IN    =>  --  1-Register ALU commands
                                --  =======================
                                --  Check during execution cycle:       Check one cycle after execution:
                                --      - ALU Output                        - SREG state
                                --                                          - Registerblock Entry
                                --
                                v.slv4_alu_1_op                 := islv16_op_code( 3 DOWNTO  0);
                                v.i_dest_addr                   := TO_INTEGER( UNSIGNED(islv16_op_code(8 DOWNTO 4)) );
                                v.slv8_dest_arg                 := r.a_register_block(v.i_dest_addr);
                                simulate_alu_1 (
                                    curr_cmd                    => v,
                                    curr_sreg                   => r_alu_expected.r_sreg,
                                    or_alu_expected             => v_alu_expected,
                                    osl_decoder_status          => vsl_alu_imm_decoder_status
                                );
                                v.avr_op                        := v_alu_expected.avr_op;

            
            WHEN ALU_2IN    =>  --  2-Register ALU commands
                                --  =======================
                                --  Check during execution cycle:       Check one cycle after execution:
                                --      - ALU Output                        - SREG state
                                --                                          - Registerblock Entry
                                --
                                v.slv6_alu_2_op                 := islv16_op_code(15 DOWNTO 10);
                                v.i_dest_addr                   := TO_INTEGER( UNSIGNED(islv16_op_code(8 DOWNTO 4)) );
                                v.i_src_addr                    := TO_INTEGER( UNSIGNED(islv16_op_code(9) & islv16_op_code(3 DOWNTO 0)) );
                                v.slv8_dest_arg                 := r.a_register_block(v.i_dest_addr);
                                v.slv8_src_arg                  := r.a_register_block(v.i_src_addr);
                                simulate_alu_2 (
                                    curr_cmd                    => v,
                                    curr_sreg                   => r_alu_expected.r_sreg,
                                    or_alu_expected             => v_alu_expected,
                                    osl_decoder_status          => vsl_alu_imm_decoder_status
                                );
                                v.avr_op                        := v_alu_expected.avr_op;
                                
            WHEN IN_OUT     =>  --  I/O Register Operation
                                vslv6_io_reg_addr                       := islv16_op_code(10 DOWNTO 9) & islv16_op_code(3 DOWNTO 0);
                                vi_io_register_addr                     := TO_INTEGER( UNSIGNED(vslv6_io_reg_addr) );

                                IF islv16_op_code(11) = '0' THEN    --  This is an IN instruction
                                    v.avr_op                                   := IN_op; 
                                    --  Handle special registers first
                                    IF vi_io_register_addr = 63 THEN        --  SREG
                                        v.a_register_block(r.i_dest_addr)(7)   := r_alu_expected.r_sreg.sl_interrupt;
                                        v.a_register_block(r.i_dest_addr)(6)   := r_alu_expected.r_sreg.sl_transfer;
                                        v.a_register_block(r.i_dest_addr)(5)   := r_alu_expected.r_sreg.sl_half_carry;
                                        v.a_register_block(r.i_dest_addr)(4)   := r_alu_expected.r_sreg.sl_sign;
                                        v.a_register_block(r.i_dest_addr)(3)   := r_alu_expected.r_sreg.sl_overflow;
                                        v.a_register_block(r.i_dest_addr)(2)   := r_alu_expected.r_sreg.sl_negative;
                                        v.a_register_block(r.i_dest_addr)(1)   := r_alu_expected.r_sreg.sl_zero;
                                        v.a_register_block(r.i_dest_addr)(0)   := r_alu_expected.r_sreg.sl_carry;
                                    ELSE
                                        v.a_register_block(r.i_dest_addr)   := r.a_io_registers(vi_io_register_addr);
                                    END IF;
                                    
                                ELSE    --  This is an OUT Instruction
                                    v.avr_op                                   := OUT_op;
                                    --  Handle special registers first
                                    IF vi_io_register_addr = 63 THEN        --  SREG
                                        v_alu_expected.r_sreg.sl_interrupt     := r.a_register_block(r.i_dest_addr)(7);
                                        v_alu_expected.r_sreg.sl_transfer      := r.a_register_block(r.i_dest_addr)(6);
                                        v_alu_expected.r_sreg.sl_half_carry    := r.a_register_block(r.i_dest_addr)(5);
                                        v_alu_expected.r_sreg.sl_sign          := r.a_register_block(r.i_dest_addr)(4);
                                        v_alu_expected.r_sreg.sl_overflow      := r.a_register_block(r.i_dest_addr)(3);
                                        v_alu_expected.r_sreg.sl_negative      := r.a_register_block(r.i_dest_addr)(2);
                                        v_alu_expected.r_sreg.sl_zero          := r.a_register_block(r.i_dest_addr)(1);
                                        v_alu_expected.r_sreg.sl_carry         := r.a_register_block(r.i_dest_addr)(0);
                                    ELSE
                                        v.a_io_registers(vi_io_register_addr)  :=  r.a_register_block(r.i_dest_addr); 
                                    END IF;
                                
                                END IF;

            WHEN LDS_STS    =>  --  This is a 2-Word instruction - only act on the second cycle
                                v_alu_expected                      := r_alu_expected;  --  Do not change SREG
                                v_alu_expected.b_check_alu_output   := FALSE;
                                v_alu_expected.b_check_sreg_d1      := FALSE;
                                IF NOT r.b_decode_instruction THEN
                                    v.i_dest_addr                   := TO_INTEGER( UNSIGNED(r.slv16_op_code(8 DOWNTO 4)) );
                                    v.i_abs_addr                               := TO_INTEGER( UNSIGNED(islv16_op_code) );
                                    IF r.avr_op = LDS THEN    --  This is an LDS instruction
                                        IF v.i_abs_addr >= c_SRAM_START  AND v.i_abs_addr < c_SRAM_END + c_SRAM_START THEN
                                            v.slv8_sram_read_data   := v.a_sram(v.i_abs_addr - c_SRAM_START);
                                            v.b_sram_to_reg_write   := TRUE;
                                        END IF;
                                        
                                    ELSE                                --  This is an STS instruction
                                        IF v.i_abs_addr >= c_SRAM_START AND v.i_abs_addr < c_SRAM_END + c_SRAM_START THEN
                                            v.i_dest_addr                         := TO_INTEGER( UNSIGNED(r.slv16_op_code(8 DOWNTO 4)) );
                                            v.slv8_dest_arg                       := r.a_register_block(v.i_dest_addr);
                                            v.a_sram(v.i_abs_addr - c_SRAM_START) := v.slv8_dest_arg;   --  Use decoded argument from 1st op-code cycle
                                        END IF;
                                    END IF;
                                END IF;

            WHEN BRANCH     =>  --  This is a 2 cycle operation, if the branch is true and jumping
                                IF r.b_decode_instruction THEN
            
                                    vb_execute_branch := FALSE;     --  Per default, do not execute the branch
                                    vslv4_branch_op     := islv16_op_code(10) & islv16_op_code(2 DOWNTO 0);
                                    CASE vslv4_branch_op IS
                                    
                                        WHEN cBRCS_code  => v.avr_op                                                            := BRCS;
                                                            IF r_alu_expected.r_sreg.sl_carry      = '1' THEN vb_execute_branch := TRUE; END IF;
                                        WHEN cBRCC_code  => v.avr_op                                                            := BRCC;
                                                            IF r_alu_expected.r_sreg.sl_carry      = '0' THEN vb_execute_branch := TRUE; END IF;                                                            
                                                            
                                        WHEN cBREQ_code  => v.avr_op                                                            := BREQ;
                                                            IF r_alu_expected.r_sreg.sl_zero       = '1' THEN vb_execute_branch := TRUE; END IF;
                                        WHEN cBRNE_code  => v.avr_op                                                            := BRNE;
                                                            IF r_alu_expected.r_sreg.sl_zero       = '0' THEN vb_execute_branch := TRUE; END IF;                                                            
                                                            
                                        WHEN cBRMI_code  => v.avr_op                                                            := BRMI;
                                                            IF r_alu_expected.r_sreg.sl_negative   = '1' THEN vb_execute_branch := TRUE; END IF;
                                        WHEN cBRPL_code  => v.avr_op                                                            := BRPL;
                                                            IF r_alu_expected.r_sreg.sl_negative   = '0' THEN vb_execute_branch := TRUE; END IF;                                                            
                                                            
                                        WHEN cBRVS_code  => v.avr_op                                                            := BRVS;
                                                            IF r_alu_expected.r_sreg.sl_overflow   = '1' THEN vb_execute_branch := TRUE; END IF;                                                            
                                        WHEN cBRVC_code  => v.avr_op                                                            := BRVC;
                                                            IF r_alu_expected.r_sreg.sl_overflow   = '0' THEN vb_execute_branch := TRUE; END IF;
                                                            
                                        WHEN cBRLT_code  => v.avr_op                                                            := BRLT;
                                                            IF r_alu_expected.r_sreg.sl_sign       = '1' THEN vb_execute_branch := TRUE; END IF;                                                            
                                        WHEN cBRGE_code  => v.avr_op                                                            := BRGE;
                                                            IF r_alu_expected.r_sreg.sl_sign       = '0' THEN vb_execute_branch := TRUE; END IF;
                                                            
                                        WHEN cBRHS_code  => v.avr_op                                                            := BRHS;
                                                            IF r_alu_expected.r_sreg.sl_half_carry = '1' THEN vb_execute_branch := TRUE; END IF;
                                        WHEN cBRHC_code  => v.avr_op                                                            := BRHC;
                                                            IF r_alu_expected.r_sreg.sl_half_carry = '0' THEN vb_execute_branch := TRUE; END IF;                                                            
                                                            
                                        WHEN cBRTS_code  => v.avr_op                                                            := BRTS;
                                                            IF r_alu_expected.r_sreg.sl_transfer   = '1' THEN vb_execute_branch := TRUE; END IF;
                                        WHEN cBRTC_code  => v.avr_op                                                            := BRTC;
                                                            IF r_alu_expected.r_sreg.sl_transfer   = '0' THEN vb_execute_branch := TRUE; END IF;                                                            
                                                            
                                        WHEN cBRIE_code  => v.avr_op                                                            := BRIE;
                                                            IF r_alu_expected.r_sreg.sl_interrupt  = '1' THEN vb_execute_branch := TRUE; END IF;
                                        WHEN cBRID_code  => v.avr_op                                                            := BRID;
                                                            IF r_alu_expected.r_sreg.sl_interrupt  = '0' THEN vb_execute_branch := TRUE; END IF;                                                            
                                                            
                                        WHEN OTHERS      => NULL;
                                    END CASE;
                                    IF vb_execute_branch THEN
                                        vi_realative_jump_addr              := TO_INTEGER( SIGNED( islv16_op_code(9 DOWNTO 3) ) );
                                        v.i_expected_pc_counter             := r.i_expected_pc_counter + vi_realative_jump_addr + 1;
                                        v.b_decode_instruction              := FALSE;
                                    END IF;
                                    
                                ELSE    --  2nd half of executed branch
                                    v.b_decode_instruction                  := TRUE;
                                END IF;


            WHEN CALL_JMP   =>  --  This is a 2-word instruction, 
                                v.i_abs_addr                    := TO_INTEGER( UNSIGNED(islv16_op_code) );
                                IF NOT r.b_decode_instruction THEN
                                    v.fsm_what_to_check         := PC_COUNTER;
                                    v.i_expected_pc_counter     := to_integer( UNSIGNED(islv16_op_code) );
                                    v.b_decode_instruction      := FALSE;   --  Skip one cycle of instruction decoding
                                    IF r.slv16_op_code(1) = '0' THEN    --  This is a Jump Instruction, skip one more cycle
                                        v.i_check_at_cycle_x    := i_cycle_counter + 1;
                                        v.avr_op_type           := INTERN_1;
                                    ELSE                                --  This is a Call Instruction, skipp two more cycles
                                        v.i_check_at_cycle_x    := i_cycle_counter + 2;
                                        vusig16_temp_number     := UNSIGNED(islv16_program_count);
                                        v.a_sram(r.i_stack_pointer - c_SRAM_START)     := STD_LOGIC_VECTOR( vusig16_temp_number(15 DOWNTO 8) );
                                        v.a_sram(r.i_stack_pointer - c_SRAM_START - 1) := STD_LOGIC_VECTOR( vusig16_temp_number( 7 DOWNTO 0) );
                                        v.i_stack_pointer       := r.i_stack_pointer - 2;
                                        v.avr_op_type           := INTERN_2;
                                    END IF;
                                END IF; 
                                
            WHEN RET_RETI   =>  --  Grab return address from SRAM
                                v.i_check_at_cycle_x    := i_cycle_counter + 2;
                                v.fsm_what_to_check     := PC_COUNTER;
                                vusig16_temp_number( 7 DOWNTO 0) := UNSIGNED( r.a_sram(r.i_stack_pointer - c_SRAM_START + 1) ); 
                                vusig16_temp_number(15 DOWNTO 8) := UNSIGNED( r.a_sram(r.i_stack_pointer - c_SRAM_START + 2) ); 
                                v.i_expected_pc_counter          := to_integer(vusig16_temp_number);
                                v.i_stack_pointer                := r.i_stack_pointer + 2;
                                v.b_decode_instruction           := FALSE;
                                v.avr_op_type                    := INTERN_2;
                                v.avr_op                         := RET;    

            WHEN NOP        =>  v_alu_expected                   := v_alu_expected;
                                v.avr_op                         := NOP;  
            
            WHEN INTERN_1   =>  NULL;   -- Intenionally skip one cycle, like instruction loading during a jump command
            
            WHEN INTERN_2   =>  v.b_decode_instruction           := FALSE;   -- Intenionally skip two cycles, like instruction loading during a call command
                                v.avr_op_type                    := INTERN_1;                    
            
            WHEN OTHERS     =>  NULL;

        END CASE;
        
        --  Update Register Block
        IF (v.avr_op_type = ALU_IMM OR v.avr_op_type = ALU_1IN OR v.avr_op_type = ALU_2IN) AND v_alu_expected.b_register_writeback THEN
            v.a_register_block(v.i_dest_addr)   := v_alu_expected.slv8_data_bus;
        END IF;
        
        --  Write to register block from SRAM data read
        IF v.b_sram_to_reg_write THEN
            v.a_register_block(v.i_dest_addr)   := v.slv8_sram_read_data;
        END IF;
        
        --  Delayed checking control ... of whatever was specified          --  To be verified!
        IF r.i_check_at_cycle_x = i_cycle_counter THEN                      --  To be verified!
            v.i_check_at_cycle_x      := 0;                                 --  To be verified!
            v.fsm_what_to_check                      := NONE;               --  To be verified! 
        END IF;                                                             --  To be verified!
        
        
        --  Reset
        IF isl_reset = '1' THEN
            FOR i IN 0 TO 31                    LOOP v.a_register_block(i) := (OTHERS => '0'); END LOOP;
            FOR i IN 0 TO c_RAM_SIZE - 1        LOOP v.a_sram(i)           := (OTHERS => '0'); END LOOP;
            FOR i IN 0 TO cIO_REGISTER_RANGE -1 LOOP v.a_io_registers(i)   := (OTHERS => '0'); END LOOP;
            v.b_decode_instruction              := TRUE;
            v.i_check_at_cycle_x                := 0;
            v.fsm_what_to_check                 := NONE;
            v.i_expected_pc_counter             := 0;
            v.sl_pc_counter_state               := '1';
            v.i_stack_pointer                   := c_SRAM_END - 1;
            v.slv16_op_code                     := (OTHERS => '0');
            v.slv4_alu_1_op                     := (OTHERS => '0');
            v.slv6_alu_2_op                     := (OTHERS => '0');
            v.slv4_alu_imm_op                   := (OTHERS => '0');
            v.slv8_dest_arg                     := (OTHERS => '0');
            v.slv8_src_arg                      := (OTHERS => '0');
            v.slv8_imm_arg                      := (OTHERS => '0');
            v.slv8_sram_read_data               := (OTHERS => '0');
            
            v_alu_expected.avr_op               := NOP;
            v_alu_expected.slv8_data_bus        := (OTHERS => '0');
            v_alu_expected.r_sreg.sl_enable     := '0';
            v_alu_expected.r_sreg.sl_interrupt  := '0';
            v_alu_expected.r_sreg.sl_transfer   := '0';
            v_alu_expected.r_sreg.sl_half_carry := '0';
            v_alu_expected.r_sreg.sl_sign       := '0';
            v_alu_expected.r_sreg.sl_overflow   := '0';
            v_alu_expected.r_sreg.sl_negative   := '0';
            v_alu_expected.r_sreg.sl_zero       := '0';
            v_alu_expected.r_sreg.sl_carry      := '0';
            
        END IF; --  End Reset        
        

        --  Copy Variables to Signals
        r_next                  <= v;
        r_next_alu_expected     <= v_alu_expected;
        
    
    END PROCESS comb_proc;
    
    
    --##    Synchronous Process
    --##
    --######################################################################
    reg_proc : PROCESS (isl_clock)
    BEGIN
        IF RISING_EDGE(isl_clock) THEN
            r                       <= r_next;
            r_alu_expected          <= r_next_alu_expected;
        END IF;
    END PROCESS reg_proc;
    
    
    --##    Result Checking
    --##
    --######################################################################
    sl_delayed_clock    <= isl_clock AFTER 2ns;
    result_checker : PROCESS (isl_reset, sl_delayed_clock)
        VARIABLE vi_check_delay                                 : INTEGER   := 0;  --  Ignore SREG compare during first 3 cycles
    BEGIN
        --  Clear Error flags upon Reset
        IF isl_reset = '1' THEN
            r_sim_status.avr_op                                 <= NOP;
            r_sim_status.sl_all_good                            <= '1';
            r_sim_status.sl_pc_counter_state                    <= '1'; 
            r_sim_status.r_SREG_status.sl_SREG_Interrupt        <= '1';
            r_sim_status.r_SREG_status.sl_SREG_Transfer_Bit     <= '1';
            r_sim_status.r_SREG_status.sl_SREG_Half_Carry       <= '1';
            r_sim_status.r_SREG_status.sl_SREG_Sign_Bit         <= '1';
            r_sim_status.r_SREG_status.sl_SREG_Overflow         <= '1';
            r_sim_status.r_SREG_status.sl_SREG_Negative         <= '1';
            r_sim_status.r_SREG_status.sl_SREG_Zero             <= '1';
            r_sim_status.r_SREG_status.sl_SREG_Carry            <= '1';
            r_sim_status.sl_register_block_ok                   <= '1';
            r_sim_status.r_ALU_status.sl_ALU_Result             <= '1';
            r_sim_status.r_ALU_status.sl_ALU_Data_Valid         <= '1';
            
        ELSIF rising_edge(sl_delayed_clock) THEN
            r_sim_status.avr_op                                 <= r_next.avr_op;            
            r_sim_status.r_ALU_status.sl_ALU_Result             <= '1';
            r_sim_status.r_ALU_status.sl_ALU_Data_Valid         <= '1';
            
            IF r_next_alu_expected.b_check_alu_output THEN
                IF ir_data_from_alu.slv8_data /= r_next_alu_expected.slv8_data_bus THEN
                    REPORT "Result mismatch" SEVERITY WARNING;
                    r_sim_status.r_ALU_status.sl_ALU_Result     <= 'X';
                    r_sim_status.sl_all_good                    <= 'X';
                END IF;
            END IF;
            
            --  Delayed checking of registered SREG
            r_sim_status.r_SREG_status.sl_SREG_Interrupt        <= '1';
            r_sim_status.r_SREG_status.sl_SREG_Transfer_Bit     <= '1';
            r_sim_status.r_SREG_status.sl_SREG_Half_Carry       <= '1';
            r_sim_status.r_SREG_status.sl_SREG_Sign_Bit         <= '1';
            r_sim_status.r_SREG_status.sl_SREG_Overflow         <= '1';
            r_sim_status.r_SREG_status.sl_SREG_Negative         <= '1';
                r_sim_status.r_SREG_status.sl_SREG_Zero         <= '1';
            r_sim_status.r_SREG_status.sl_SREG_Carry            <= '1';
              
            IF r_alu_expected.b_check_sreg_d1 THEN
                IF ir_sreg.sl_interrupt  /= r_alu_expected.r_sreg.sl_interrupt  THEN 
                    r_sim_status.r_SREG_status.sl_SREG_Interrupt    <= 'X'; 
                    r_sim_status.sl_all_good                        <= 'X';
                END IF;
                IF ir_sreg.sl_transfer   /= r_alu_expected.r_sreg.sl_transfer   THEN 
                    r_sim_status.r_SREG_status.sl_SREG_Transfer_Bit <= 'X'; 
                    r_sim_status.sl_all_good                        <= 'X';
                END IF;
                IF ir_sreg.sl_half_carry /= r_alu_expected.r_sreg.sl_half_carry THEN 
                    r_sim_status.r_SREG_status.sl_SREG_Half_Carry   <= 'X'; 
                    r_sim_status.sl_all_good                        <= 'X';
                END IF;
                IF ir_sreg.sl_sign       /= r_alu_expected.r_sreg.sl_sign       THEN 
                    r_sim_status.r_SREG_status.sl_SREG_Sign_Bit     <= 'X'; 
                    r_sim_status.sl_all_good                        <= 'X';
                END IF;
                IF ir_sreg.sl_overflow   /= r_alu_expected.r_sreg.sl_overflow   THEN 
                    r_sim_status.r_SREG_status.sl_SREG_Overflow     <= 'X'; 
                    r_sim_status.sl_all_good                        <= 'X';
                END IF;
                IF ir_sreg.sl_negative   /= r_alu_expected.r_sreg.sl_negative   THEN 
                    r_sim_status.r_SREG_status.sl_SREG_Negative     <= 'X'; 
                    r_sim_status.sl_all_good                        <= 'X';
                END IF;
                IF ir_sreg.sl_zero       /= r_alu_expected.r_sreg.sl_zero       THEN 
                    r_sim_status.r_SREG_status.sl_SREG_Zero         <= 'X'; 
                    r_sim_status.sl_all_good                        <= 'X';
                END IF;
                IF ir_sreg.sl_carry      /= r_alu_expected.r_sreg.sl_carry      THEN 
                    r_sim_status.r_SREG_status.sl_SREG_Carry        <= 'X'; 
                    r_sim_status.sl_all_good                        <= 'X';
                END IF;
                
            END IF;
            
            --  Checks only to be performed after the first 3 cycles of simulation
            IF vi_check_delay < 10 THEN vi_check_delay := vi_check_delay + 1; END IF;
            
            --  Check Register Block
            IF r.a_register_block /= ia_register_block THEN
                r_sim_status.sl_register_block_ok    <= 'X';
                REPORT "Register Block mismatch detected" SEVERITY WARNING;
            END IF;
            
            --  Delayed checking ... of whatever was specified
            IF r.i_check_at_cycle_x = i_cycle_counter THEN                                                          --  To be verified!
                CASE r.fsm_what_to_check IS                                                                         --  To be verified!
                    WHEN PC_COUNTER =>  IF r.i_expected_pc_counter /= UNSIGNED(islv16_program_count) THEN           --  To be verified!
                                            r_sim_status.sl_pc_counter_state     <= 'X';                            --  To be verified!
                                        END IF;                                                                     --  To be verified!
                    WHEN OTHERS     =>  NULL;                                                                       --  To be verified!
                END CASE;                                                                                           --  To be verified!
            END IF;                                                                                                 --  To be verified!
            
            
        END IF;     --  end falling clock cycle processing
    END PROCESS result_checker;
    
    --  Output Assignments
    or_sim_status       <= r_sim_status;
    
END ARCHITECTURE sim;

