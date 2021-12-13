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
USE work.AVR_Records_pkg.ALL;

PACKAGE Program_Memory_pkg IS

    CONSTANT c_sim_cycles    : INTEGER       := 1000;
    
    CONSTANT c_SRAM_RANGE    : REAL          :=   5.0;  --  Select a small SRAM Range, so each location gets hit more often
    CONSTANT c_IRAM_RANGE    : REAL          := 500.0;  --  Instruction IRAM Space, size does not really matter

    --##    Single-cycle Commands
    --###########################

     CONSTANT cNOP           : INTEGER   := 0;  

     --  2 Input ALU Operations
     CONSTANT cAND           : INTEGER   := 1;
     CONSTANT cEOR           : INTEGER   := 2;
     CONSTANT cOR            : INTEGER   := 3;
     CONSTANT cADD           : INTEGER   := 4;
     CONSTANT cADC           : INTEGER   := 5;
     CONSTANT cCP            : INTEGER   := 6;
     CONSTANT cCPC           : INTEGER   := 7;
     CONSTANT cMOV           : INTEGER   := 8;
     CONSTANT cSUB           : INTEGER   := 9;
     CONSTANT cSBC           : INTEGER   := 10;
     --
     --  1 Input ALU Operations
     CONSTANT cASR           : INTEGER   := 11;
     CONSTANT cCOM           : INTEGER   := 12;
     CONSTANT cDEC           : INTEGER   := 13;
     CONSTANT cINC           : INTEGER   := 14;
     CONSTANT cLSR           : INTEGER   := 15;
     CONSTANT cNEG           : INTEGER   := 16;
     CONSTANT cROR           : INTEGER   := 17;
     CONSTANT cSWAP          : INTEGER   := 18;  
     --
     --  1 Input and Immediate ALU Operation
     CONSTANT cANDI          : INTEGER   := 19;
     CONSTANT cORI           : INTEGER   := 20;
     CONSTANT cCPI           : INTEGER   := 21;
     CONSTANT cLDI           : INTEGER   := 22;
     CONSTANT cSUBI          : INTEGER   := 23;
     CONSTANT cSBCI          : INTEGER   := 24;
     --

    --##    Multi-cycle Commands
    --##########################

     CONSTANT cLDS           : INTEGER   := 25;  
     CONSTANT cSTS           : INTEGER   := 26; 
     -- 
     CONSTANT cJMP           : INTEGER   := 27;
     CONSTANT cRJMP          : INTEGER   := 28;
     CONSTANT cCALL          : INTEGER   := 29;
     CONSTANT cRCALL         : INTEGER   := 30;
     CONSTANT cRET           : INTEGER   := 31;
     CONSTANT cRETI          : INTEGER   := 32;
     --
     CONSTANT cBREQ          : INTEGER   := 33;
     CONSTANT cBRNE          : INTEGER   := 34;
     CONSTANT cBRCS          : INTEGER   := 35;
     CONSTANT cBRCC          : INTEGER   := 36;
     CONSTANT cBRSH          : INTEGER   := 37;
     CONSTANT cBRLO          : INTEGER   := 38;
     CONSTANT cBRMI          : INTEGER   := 39;
     CONSTANT cBRPL          : INTEGER   := 40;
     CONSTANT cBRGE          : INTEGER   := 41;
     CONSTANT cBRLT          : INTEGER   := 42;
     CONSTANT cBRHS          : INTEGER   := 43;
     CONSTANT cBRHC          : INTEGER   := 44;
     CONSTANT cBRTS          : INTEGER   := 45;
     CONSTANT cBRTC          : INTEGER   := 46;
     CONSTANT cBRVS          : INTEGER   := 47;
     CONSTANT cBRVC          : INTEGER   := 48;
     CONSTANT cBRIE          : INTEGER   := 49;
     CONSTANT cBRID          : INTEGER   := 50;
     
     CONSTANT c_num_of_ops   : INTEGER   := 51; 

     CONSTANT supported_ops  : BIT_VECTOR(0 TO c_num_of_ops)  := (

                --##    Single-cycle Commands
                --###########################
                
                cNOP     => '1',    --  No Operation
                --  2 Input ALU Operations
                cAND     => '1',    --  Logical AND
                cEOR     => '1',    --  Logical XOR
                cOR      => '1',    --  Logical OR
                cADD     => '1',    --  Add
                cADC     => '1',    --  Add with Carry
                cCP      => '1',    --  Compare
                cCPC     => '1',    --  Compare with Carry
                cMOV     => '1',    --  Move Register
                cSUB     => '1',    --  Subtract
                cSBC     => '1',    --  Subtract with Carry

                --  1 Input ALU Operations
                cASR     => '1',    --  Arithmetic Shift Right
                cCOM     => '1',    --  One's Complement
                cDEC     => '1',    --  Decrement Register
                cINC     => '1',    --  Increment Register
                cLSR     => '1',    --  Logical Shift Right
                cNEG     => '1',    --  Two's Complement
                cROR     => '1',    --  Rotate Right through Carry
                cSWAP    => '1',    --  Swap 4-Bit nibbles

                --  1 Input and Immediate ALU Operations
                cANDI    => '1',    --  Logical AND with Immedaite
                cORI     => '1',    --  Logical OR with Immediate
                cCPI     => '1',    --  Compare Immediate
                cLDI     => '1',    --  Load Immediate
                cSUBI    => '1',    --  Subtract Immediate
                cSBCI    => '1',    --  Subtract Immediate with Carry
                
                --##    Multi-cycle Commands
                --##########################

                --  Load- / Store Operations
                cLDS     => '1',    --  Load Direct from Data Space
                cSTS     => '1',    --  Store Direct to Data Space
                                
                --  Jump Operations
                cJMP     => '1',    --  Absolute Jump
--                cRJMP    => '1',    --  Relative Jump
--                cCALL    => '1',    --  Absolute call of a subroutine
--                cRCALL   => '1',    --  Relative Call of a subroutine
--                cRET     => '1',    --  Return from subroutine
--                cRETI    => '1',    --  Return from interrupt
                
                --  Branch Operations
                cBREQ    => '1',    --  Branch if Zero Flag Set
                cBRNE    => '1',    --  Branch if Zero Flag Cleared
                cBRCS    => '1',    --  Branch if Carry Flag SET
                cBRCC    => '1',    --  Branch if Carry Flag Cleared
                cBRSH    => '1',    --  Branch if Same or Higher
                cBRLO    => '1',    --  Branch if Lower
                cBRMI    => '1',    --  Branch if Minus
                cBRPL    => '1',    --  Branch if Plus
                cBRGE    => '1',    --  Branch if Greater or Equal
                cBRLT    => '1',    --  Branch if Less Than, Signed
                cBRHS    => '1',    --  Branch if Half Carry Flag Set
                cBRHC    => '1',    --  Branch if Half Carry Flag Cleared
                cBRTS    => '1',    --  Branch if Half T Flag Set
                cBRTC    => '1',    --  Branch if Half T Flag Cleared
                cBRVS    => '1',    --  Branch if Overflow Flag Set
                cBRVC    => '1',    --  Branch if Overflow Flag Cleared
                cBRIE    => '1',    --  Branch if Interrupt Enabled
                cBRID    => '1',    --  Branch if Interrupt Disabled

                OTHERS  => '0'
    );
    

    COMPONENT Program_Memory IS
        PORT (
            isl_reset               : IN  STD_LOGIC;
            isl_clock               : IN  STD_LOGIC;
            islv16_program_counter  : IN  STD_LOGIC_VECTOR(15 DOWNTO 0);
            oslv16_op_code          : OUT STD_LOGIC_VECTOR(15 DOWNTO 0)
        );
    END COMPONENT Program_Memory;
END PACKAGE Program_Memory_pkg;

--------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
--  PRAGMA synthesis_off
USE IEEE.MATH_REAL.ALL;         --  Vivado Error is only for synthesis - can be ignored here
--  PRAGMA synthesis_on

USE work.AVR_Records_pkg.ALL;
USE work.Program_Memory_pkg.ALL;

ENTITY Program_Memory IS
    PORT (
        isl_reset               : IN  STD_LOGIC;
        isl_clock               : IN  STD_LOGIC;
        islv16_program_counter  : IN  STD_LOGIC_VECTOR(15 DOWNTO 0);
        oslv16_op_code          : OUT STD_LOGIC_VECTOR(15 DOWNTO 0)
    );
END ENTITY Program_Memory;


ARCHITECTURE sim OF Program_Memory IS

     --##  List of possible Op-Codes
     --##
     --#############################
     --  This construct is a list of possible ALU operations,
     --  where some of them can be selected for testing within the Testbench
     
     -- Some operations are extended with "_op" to avoid collision with VHDL keywords
     TYPE t_avr_op IS (AND_op,ANDI,EOR,OR_op,ORI,ADD,ADC,ASR,COM,CP,CPI,CPC,DEC,INC,LDI,LSR,NEG,ROR_op,SUB,SUBI,SBC,SBCI,SWAP,MOV,LDS,LD,LDD,
                       STS,ST,STD,BRBS,BRBC,BREQ,BRNE,BRCS,BRCC,BRSH,BRLO,BRMI,BRPL,BRGE,BRLT,BRHS,BRHC,BRTS,BRTC,BRVS,BRVC,BRIE,BRID,
                       JMP,RJMP,CALL,RCALL,RET,LPM,IN_op,OUT_op,PUSH,POP,SEC,CLC,SEN,CLN,SEZ,CLZ,SEI,CLI,SES,CLS,SEV,CLV,SET,CLT,SEH,CLH,NOP);     
 
-----------------------------------------------------------------------------------------------------------------------------
 
    TYPE t_registers IS RECORD
        i_test_cmd              : INTEGER;
        avr_op                  : t_avr_op;
        i_cycle_counter         : INTEGER;
        b_generate_command      : BOOLEAN;
        slv16_op_code           : STD_LOGIC_VECTOR(15 DOWNTO 0);
        slv5_src_reg            : STD_LOGIC_VECTOR(4 DOWNTO 0);
        slv5_dest_reg           : STD_LOGIC_VECTOR(4 DOWNTO 0);
        slv8_src_arg            : STD_LOGIC_VECTOR(7 DOWNTO 0);
        slv8_dest_arg           : STD_LOGIC_VECTOR(7 DOWNTO 0);
        slv8_imm_arg            : STD_LOGIC_VECTOR(7 DOWNTO 0);
        sig7_branch_offset      : SIGNED(6 DOWNTO 0);
                                
    END RECORD t_registers;
    
    SIGNAL r, r_next            : t_registers;
    
                --  Initialize Variables for Random generation
    CONSTANT c_range_of_5bit    : REAL  := 31.9;
    CONSTANT c_range_of_8bit    : REAL  := 255.9;
    CONSTANT c_range_br_offset  : REAL  := 127.9;
    
BEGIN    

    --##    Stimulus Generator for Operations and Arguments
    --##
    --######################################################################
    comb_proc : PROCESS (r, isl_reset)
    
        VARIABLE v                      : t_registers;
        
        VARIABLE v_seed1, v_seed2       : POSITIVE;                 -- seed values for random generator
        VARIABLE v_rand                 : REAL;                     -- random real-number value in range 0 to 1.0  
        VARIABLE v_range_of_ops         : REAL;                     -- the range of random values to create
        VARIABLE vb_valid_cmd           : BOOLEAN;
        
        VARIABLE v_rand_num             : INTEGER;                  -- generated random integer
        
        VARIABLE v_sig17_instr_address  : SIGNED(16 DOWNTO 0);      --  Check relativ jump range
        
    BEGIN
    
        v   := r;   --  Keep variables stable
        
        --##    Instruction Cycle Timer
        --#############################
        v.i_cycle_counter               := r.i_cycle_counter + 1;
        IF r.i_cycle_counter >= c_sim_cycles THEN
            REPORT "End of Simulation" SEVERITY FAILURE;
        END IF; 


        --##    Generate command, when due
        --################################
        IF r.b_generate_command THEN
    
        

            --##    Generate random command out of the list of implemented commands
            --##
            --######################################################################
            vb_valid_cmd    := FALSE;
            WHILE NOT vb_valid_cmd LOOP
                --  PRAGMA synthesis_off
                UNIFORM(v_seed1, v_seed2, v_rand);                -- Generate random number    / Vivado Error is only for synthesis - can be ignored here
                --  PRAGMA synthesis_on
                v_rand_num := integer(v_rand * REAL(c_num_of_ops));   -- Rescale to required range
                v.i_test_cmd     := INTEGER(v_rand_num);
                IF supported_ops(v.i_test_cmd) = '1' THEN
                    IF r.i_test_cmd >= cJMP THEN    --  Additional restriction: After Multi-cycle Instructions like Jump, Return or Branches ...
                        IF v.i_test_cmd <= cSBCI THEN   --  ... only single-cycle instructions are permitted (as they are often skipped)
                            vb_valid_cmd    := TRUE;
                        END IF;
                    ELSE
                        vb_valid_cmd    := TRUE;
                    END IF;
                END IF;
            END LOOP;         
            
            --  Generate random values for source- and destination register address and 8-Bit immediate value
            --  PRAGMA synthesis_off
            UNIFORM(v_seed1, v_seed2, v_rand);                -- Generate random number    / Vivado Error is only for synthesis - can be ignored here
            --  PRAGMA synthesis_on
            v_rand_num := integer(v_rand * c_range_of_5bit);  -- Rescale to required range
            v.slv5_src_reg  := STD_LOGIC_VECTOR( TO_UNSIGNED(v_rand_num, 5) );

            --  PRAGMA synthesis_off
            UNIFORM(v_seed1, v_seed2, v_rand);                -- Generate random number    / Vivado Error is only for synthesis - can be ignored here
            --  PRAGMA synthesis_on
            v_rand_num := integer(v_rand * c_range_of_5bit);  -- Rescale to required range
            v.slv5_dest_reg := STD_LOGIC_VECTOR( TO_UNSIGNED(v_rand_num, 5) );

            --  PRAGMA synthesis_off
            UNIFORM(v_seed1, v_seed2, v_rand);                -- Generate random number    / Vivado Error is only for synthesis - can be ignored here
            --  PRAGMA synthesis_on
            v_rand_num := integer(v_rand * c_range_of_8bit);  -- Rescale to required range
            v.slv8_src_arg := STD_LOGIC_VECTOR( TO_UNSIGNED(v_rand_num, 8) );

            --  PRAGMA synthesis_off
            UNIFORM(v_seed1, v_seed2, v_rand);                -- Generate random number    / Vivado Error is only for synthesis - can be ignored here
            --  PRAGMA synthesis_on
            v_rand_num := integer(v_rand * c_range_of_8bit);  -- Rescale to required range
            v.slv8_dest_arg := STD_LOGIC_VECTOR( TO_UNSIGNED(v_rand_num, 8) );

            --  PRAGMA synthesis_off
            UNIFORM(v_seed1, v_seed2, v_rand);                -- Generate random number    / Vivado Error is only for synthesis - can be ignored here
            --  PRAGMA synthesis_on
            v_rand_num := integer(v_rand * c_range_of_8bit);  -- Rescale to required range
            v.slv8_imm_arg := STD_LOGIC_VECTOR( TO_UNSIGNED(v_rand_num, 8) );

            --  PRAGMA synthesis_off
            UNIFORM(v_seed1, v_seed2, v_rand);                -- Generate random number    / Vivado Error is only for synthesis - can be ignored here
            --  PRAGMA synthesis_on
            v_rand_num := integer(v_rand * c_range_br_offset);  -- Rescale to required range
            v.sig7_branch_offset := TO_SIGNED(v_rand_num, 7);
            --  Check Range due to relative jumps
            v_sig17_instr_address := SIGNED('0' & islv16_program_counter);
            IF (v_sig17_instr_address + v.sig7_branch_offset) < 0 THEN
                v.sig7_branch_offset := NOT( v_sig17_instr_address(6 DOWNTO 0) ) + 1;
            END IF;  
                        
            
            --  Override Random Settings
            -- CASE v.i_cycle_counter IS
            CASE TO_INTEGER(UNSIGNED(islv16_program_counter)) IS
                WHEN 0      =>  --  NOP;
                                v.i_test_cmd := cNOP; 
                
                WHEN 1      =>  --  LDI R18, 0x55;
                                v.i_test_cmd := cLDI; 
                                v.slv5_dest_reg  := "10010";
                                v.slv5_src_reg   := "00000";
                                v.slv8_imm_arg   := x"55";
                                
                WHEN 2      =>  --  LDI R19, 0xA5;
                                v.i_test_cmd := cLDI; 
                                v.slv5_dest_reg  := "10011";
                                v.slv5_src_reg   := "00000";
                                v.slv8_imm_arg   := x"A5";
                                
                WHEN 3      =>  --  AND R18, R19;
                                v.i_test_cmd := cAND; 
                                v.slv5_dest_reg  := "10010";
                                v.slv5_src_reg   := "10011";
                                v.slv8_imm_arg   := x"00";
                                
                WHEN 4      =>  --  NOP;
                                v.i_test_cmd := cNOP; 
                                
                WHEN 5      =>  --  NOP;
                                v.i_test_cmd := cNOP; 
                                                
                WHEN 6      =>  --  NOP;
                                v.i_test_cmd := cNOP; 
                                                
                WHEN 7      =>  --  NOP;
                                v.i_test_cmd := cNOP; 
                                                
                WHEN 8      =>  --  NOP;
                                v.i_test_cmd := cNOP; 
                                                
                WHEN 9      =>  --  NOP;
                                v.i_test_cmd := cNOP; 

                --##  Random Fill Register Block for more meaningfull testing
                --##  (Values are taken from random generator in Line 349)                                                
                --##
                --###########################################################
                --  Random Fill Registers R16 - R31
                WHEN 10 TO 25  =>  --  Random Fill Register Block
                                   v.i_test_cmd     := cLDI;     --  LDI
                                   v.slv5_dest_reg  := STD_LOGIC_VECTOR( TO_UNSIGNED(v.i_cycle_counter + 6, 5) );
                                  
                --  Random Fill Registers R16 - R31
                WHEN 26 TO 41  =>  --  Copy upper half of register block to lower half 
                                   v.i_test_cmd     := cMOV;      --  MOV
                                   v.slv5_src_reg   := STD_LOGIC_VECTOR( TO_UNSIGNED(v.i_cycle_counter - 26, 5) );
                                   v.slv5_dest_reg  := STD_LOGIC_VECTOR( TO_UNSIGNED(v.i_cycle_counter - 10, 5) );
                                                  
                --  Random Fill Registers R16 - R31
                WHEN 42 TO 57  =>  --  Random Fill Register Block
                                   v.i_test_cmd     := cLDI;     --  LDI
                                   v.slv5_dest_reg  := STD_LOGIC_VECTOR( TO_UNSIGNED(v.i_cycle_counter + 26, 5) );
                            
                WHEN OTHERS =>  null;   --  Perform randomized testing
            END CASE;
            
                
                
            --##    Assemble actual test parameters for each command
            --##
            --######################################################

            CASE v.i_test_cmd IS
                
                WHEN cAND    => --  Logical AND, 2-input ALU command
                                v.avr_op                    := AND_op; 
                                v.slv16_op_code             := B"0010_00" &  v.slv5_src_reg(4) & v.slv5_dest_reg & v.slv5_src_reg(3 DOWNTO 0);
                                
                WHEN cANDI   => --  Logical AND with Immediate, 1-input ALU command
                                v.avr_op                    := ANDI;
                                v.slv16_op_code             := B"0111" & v.slv8_imm_arg(7 DOWNTO 4) & v.slv5_dest_reg(3 DOWNTO 0) & v.slv8_imm_arg(3 DOWNTO 0);
                                
                WHEN cEOR    => --  Logical Exclusive OR, 2-input ALU command
                                v.avr_op                    := EOR;
                                v.slv16_op_code             := B"0010_01" &  v.slv5_src_reg(4) & v.slv5_dest_reg & v.slv5_src_reg(3 DOWNTO 0);
                                
                WHEN cOR     => --  Logical OR, 2-input ALU command
                                v.avr_op                    := OR_op;
                                v.slv16_op_code             := B"0010_10" &  v.slv5_src_reg(4) & v.slv5_dest_reg & v.slv5_src_reg(3 DOWNTO 0);
                                
                WHEN cORI    => --  Logical OR with Immediate, 1-input ALU command
                                v.avr_op                    := ORI;
                                v.slv16_op_code             := B"0110" & v.slv8_imm_arg(7 DOWNTO 4) & v.slv5_dest_reg(3 DOWNTO 0) & v.slv8_imm_arg(3 DOWNTO 0);
                                                
                WHEN cADD    => --  Add without Carry
                                v.avr_op                    := ADD;
                                v.slv16_op_code             := B"0000_11" &  v.slv5_src_reg(4) & v.slv5_dest_reg & v.slv5_src_reg(3 DOWNTO 0);
                                
                WHEN cADC    => --  Add with Carry
                                v.avr_op                    := ADC;
                                v.slv16_op_code             := B"0001_11" &  v.slv5_src_reg(4) & v.slv5_dest_reg & v.slv5_src_reg(3 DOWNTO 0);

                WHEN cASR    => --  Arithmetic Shift Right
                                v.avr_op                    := ASR;
                                v.slv16_op_code             := B"1001_010" & v.slv5_dest_reg & B"0101";
                                
                WHEN cCOM    => --  One's Complement
                                v.avr_op                    := COM;
                                v.slv16_op_code             := B"1001_010" & v.slv5_dest_reg & B"0000";
                                
                WHEN cCP     => --  Compare without Carry
                                v.avr_op                    := CP;
                                v.slv16_op_code             := B"0001_01" &  v.slv5_src_reg(4) & v.slv5_dest_reg & v.slv5_src_reg(3 DOWNTO 0);
                                
                WHEN cCPI    => --  Compare Immediate without Carry
                                v.avr_op                    := CPI;
                                v.slv16_op_code             := B"0011" &  v.slv8_imm_arg(7 DOWNTO 4) & v.slv5_dest_reg(3 DOWNTO 0) & v.slv8_imm_arg(3 DOWNTO 0);
                                
                WHEN cCPC    => --  Compare without Carry
                                v.avr_op                    := CPC;
                                v.slv16_op_code             := B"0000_01" &  v.slv5_src_reg(4) & v.slv5_dest_reg & v.slv5_src_reg(3 DOWNTO 0);
                                
                WHEN cDEC    => --  Decrement
                                v.avr_op                    := DEC;
                                v.slv16_op_code             := B"1001_010" & v.slv5_dest_reg & B"1010";
                                                
                WHEN cINC    => --  Increment
                                v.avr_op                    := INC;
                                v.slv16_op_code             := B"1001_010" & v.slv5_dest_reg & B"0011";

                WHEN cLDI    => --  Load Immediate
                                v.avr_op                    := LDI;
                                v.slv16_op_code             := B"1110" &  v.slv8_imm_arg(7 DOWNTO 4) & v.slv5_dest_reg(3 DOWNTO 0) & v.slv8_imm_arg(3 DOWNTO 0);
                                
                WHEN cLSR    => --  Logical Shift Right
                                v.avr_op                    := LSR;
                                v.slv16_op_code             := B"1001_010" & v.slv5_dest_reg & B"0110";
                                                
                WHEN cMOV    => --  Copy Register
                                v.avr_op                    := MOV;
                                v.slv16_op_code             := B"0010_11" &  v.slv5_src_reg(4) & v.slv5_dest_reg & v.slv5_src_reg(3 DOWNTO 0);

                WHEN cNEG    => --  Two's Complement
                                v.avr_op                    := NEG;
                                v.slv16_op_code             := B"1001_010" & v.slv5_dest_reg & B"0001";
                                                                                                
                WHEN cROR    => --  Rotate Right through Carry
                                v.avr_op                    := ROR_op;
                                v.slv16_op_code             := B"1001_010" & v.slv5_dest_reg & B"0111";
                                                                                                
                WHEN cSUB    => --  Compare without Carry
                                v.avr_op                    := SUB;
                                v.slv16_op_code             := B"0001_10" &  v.slv5_src_reg(4) & v.slv5_dest_reg & v.slv5_src_reg(3 DOWNTO 0);

                WHEN cSUBI   => --  Subtract Immediate without Carry
                                v.avr_op                    := SUBI;
                                v.slv16_op_code             := B"0101" &  v.slv8_imm_arg(7 DOWNTO 4) & v.slv5_dest_reg(3 DOWNTO 0) & v.slv8_imm_arg(3 DOWNTO 0);
                                
                WHEN cSBC    => --  Subtract without Carry
                                v.avr_op                    := SBC;
                                v.slv16_op_code             := B"0000_10" &  v.slv5_src_reg(4) & v.slv5_dest_reg & v.slv5_src_reg(3 DOWNTO 0);

                WHEN cSBCI  => --  Subtract Immediate with Carry
                                v.avr_op                    := SBCI;
                                v.slv16_op_code             := B"0100" &  v.slv8_imm_arg(7 DOWNTO 4) & v.slv5_dest_reg(3 DOWNTO 0) & v.slv8_imm_arg(3 DOWNTO 0);
                                
                WHEN cSWAP   => --  SWAP Nibbles
                                v.avr_op                    := SWAP;
                                v.slv16_op_code             := B"1001_010" & v.slv5_dest_reg & B"0010";

                WHEN cLDS    => --  Load Direct from Data Space
                                v.avr_op                    := LDS;
                                v.slv16_op_code             := B"1001_000" & v.slv5_dest_reg & B"0000";
                                v.b_generate_command        := FALSE;
                                                                                            
                WHEN cSTS    => --  Store Direct from Data Space
                                v.avr_op                    := STS;
                                v.slv16_op_code             := B"1001_001" & v.slv5_dest_reg & B"0000";
                                v.b_generate_command        := FALSE;
                                                                                            
                WHEN cJMP    => --  Jump to absolute Address
                                v.avr_op                    := JMP;
                                v.slv16_op_code             := B"1001_0100_0000_1100";
                                v.b_generate_command        := FALSE;



                WHEN cBREQ   => --  Branch if Zero Flag Set
                                v.avr_op                    := BREQ;
                                v.slv16_op_code             := B"1111_00" & STD_LOGIC_VECTOR(v.sig7_branch_offset) & "001";

                WHEN cBRNE   => --  Branch if Zero Flag Cleared
                                v.avr_op                    := BRNE;
                                v.slv16_op_code             := B"1111_01" & STD_LOGIC_VECTOR(v.sig7_branch_offset) & "001";
                                
                WHEN cBRCS   => --  Branch if Carry Flag SET
                                v.avr_op                    := BRCS;
                                v.slv16_op_code             := B"1111_00" & STD_LOGIC_VECTOR(v.sig7_branch_offset) & "000";

                WHEN cBRCC   => --  Branch if Carry Flag Cleared
                                v.avr_op                    := BRCC;
                                v.slv16_op_code             := B"1111_01" & STD_LOGIC_VECTOR(v.sig7_branch_offset) & "000";

                WHEN cBRSH   => --  Branch if Same or Higher
                                v.avr_op                    := BRSH;
                                v.slv16_op_code             := B"1111_01" & STD_LOGIC_VECTOR(v.sig7_branch_offset) & "000";

                WHEN cBRLO   => --  Branch if Lower
                                v.avr_op                    := BRLO;
                                v.slv16_op_code             := B"1111_00" & STD_LOGIC_VECTOR(v.sig7_branch_offset) & "000";

                WHEN cBRMI   => --  Branch if Minus
                                v.avr_op                    := BRMI;
                                v.slv16_op_code             := B"1111_00" & STD_LOGIC_VECTOR(v.sig7_branch_offset) & "010";
    
                WHEN cBRPL   => --  Branch if Plus
                                v.avr_op                    := BRPL;
                                v.slv16_op_code             := B"1111_01" & STD_LOGIC_VECTOR(v.sig7_branch_offset) & "010";

                WHEN cBRGE   => --  Branch if Greater or Equal
                                v.avr_op                    := BRGE;
                                v.slv16_op_code             := B"1111_01" & STD_LOGIC_VECTOR(v.sig7_branch_offset) & "100";

                WHEN cBRLT   => --  Branch if Less Than, Signed
                                v.avr_op                    := BRLT;
                                v.slv16_op_code             := B"1111_00" & STD_LOGIC_VECTOR(v.sig7_branch_offset) & "100";

                WHEN cBRHS   => --  Branch if Half Carry Flag Set
                                v.avr_op                    := BRHS;
                                v.slv16_op_code             := B"1111_00" & STD_LOGIC_VECTOR(v.sig7_branch_offset) & "101";

                WHEN cBRHC   => --  Branch if Half Carry Flag Cleared
                                v.avr_op                    := BRHC;
                                v.slv16_op_code             := B"1111_01" & STD_LOGIC_VECTOR(v.sig7_branch_offset) & "101";

                WHEN cBRTS   => --  Branch if Half T Flag Set
                                v.avr_op                    := BRTS;
                                v.slv16_op_code             := B"1111_00" & STD_LOGIC_VECTOR(v.sig7_branch_offset) & "110";

                WHEN cBRTC   => --  Branch if Half T Flag Cleared
                                v.avr_op                    := BRTC;
                                v.slv16_op_code             := B"1111_01" & STD_LOGIC_VECTOR(v.sig7_branch_offset) & "110";

                WHEN cBRVS   => --  Branch if Overflow Flag Set
                                v.avr_op                    := BRVS;
                                v.slv16_op_code             := B"1111_00" & STD_LOGIC_VECTOR(v.sig7_branch_offset) & "011";

                WHEN cBRVC   => --  Branch if Overflow Flag Cleared
                                v.avr_op                    := BRVC;
                                v.slv16_op_code             := B"1111_01" & STD_LOGIC_VECTOR(v.sig7_branch_offset) & "011";

                WHEN cBRIE   => --  Branch if Interrupt Enabled
                                v.avr_op                    := BRIE;
                                v.slv16_op_code             := B"1111_00" & STD_LOGIC_VECTOR(v.sig7_branch_offset) & "111";

                WHEN cBRID   => --  Branch if Interrupt Disabled
                                v.avr_op                    := BRID;
                                v.slv16_op_code             := B"1111_01" & STD_LOGIC_VECTOR(v.sig7_branch_offset) & "111";
                                                                
                WHEN OTHERS => --  NOP  No Operation
                                v.avr_op                    := NOP;
                                v.slv16_op_code             := (OTHERS => '0');     --  NOP
            
            END CASE;


        --##    If this is a multi-cycle instruction, generate the 2nd command word
        --#########################################################################
        ELSE
            
            --  PRAGMA synthesis_off
            UNIFORM(v_seed1, v_seed2, v_rand);                -- Generate random number    / Vivado Error is only for synthesis - can be ignored here
            --  PRAGMA synthesis_on
        
            CASE r.i_test_cmd IS
            
                
                --  2nd cycle of Load / Store command
                WHEN cLDS | cSTS  => v_rand_num := integer(v_rand * c_SRAM_RANGE);  -- Rescale to required range
                                     v.slv16_op_code := STD_LOGIC_VECTOR( TO_UNSIGNED(v_rand_num + c_SRAM_START, 16) );
                                     
                WHEN cJMP         => v_rand_num := integer(v_rand * c_IRAM_RANGE);  -- Rescale to required range
                                     v.slv16_op_code := STD_LOGIC_VECTOR( TO_UNSIGNED(v_rand_num, 16) );
                
                WHEN OTHERS       => null;
            END CASE;
            v.b_generate_command    := TRUE;     --  Return to normal command generation
            
        END IF;


        --  Reset
        IF isl_reset = '1' THEN
            v.i_test_cmd            := 0;
            v.b_generate_command    := TRUE;
            v.avr_op                := NOP;
            v.i_cycle_counter       := 0;
            v.slv16_op_code         := (OTHERS => '0');
            
        END IF;
              
        r_next                      <= v;  
    END PROCESS comb_proc;
    
    
    --##    Registered Process
    --########################
    reg_proc : PROCESS (isl_clock)
    BEGIN
        IF rising_edge(isl_clock) THEN r <= r_next; END IF; 
    END PROCESS reg_proc;
    

    --##    Output Assignments
    --########################
    oslv16_op_code                         <= r.slv16_op_code;


END ARCHITECTURE sim;
