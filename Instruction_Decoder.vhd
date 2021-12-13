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

--  Simple Instruction Decoder for ALU Testing
--  ==========================================
--  Based on the 2014 Atmel document Atmel-0856J-AVR-Instruction-Set-Manual_07/2014

--  As a RISC Processor, many commands are immediately executed.
--  Certain commands require an additional 16-bit op-code as argument,
--  while commands like JMP, CALL or RET require multiple cycles to be executed

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE work.avr_records_pkg.ALL;

PACKAGE instr_decoder_pkg IS
    COMPONENT instr_decoder IS
        PORT (
            isl_reset               : IN  STD_LOGIC;
            isl_clock               : IN  STD_LOGIC;    
            islv16_program_count    : IN  STD_LOGIC_VECTOR(15 DOWNTO 0);
            islv16_op_code          : IN  STD_LOGIC_VECTOR(15 DOWNTO 0);
            ir_sreg                 : IN  t_sreg_bits;
            or_register_control     : OUT t_register_control;
            or_alu_control    	    : OUT t_alu_control;
            or_sram_control    	    : OUT t_sram_control;
            or_pc_control    	    : OUT t_pc_control;
            or_io_port_b_control    : OUT t_io_port_control;
            or_io_port_d_control    : OUT t_io_port_control
            
        );
    END COMPONENT instr_decoder;
END PACKAGE instr_decoder_pkg;

--------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

USE work.avr_records_pkg.ALL;

ENTITY instr_decoder IS
        PORT (
            isl_reset               : IN  STD_LOGIC;
            isl_clock               : IN  STD_LOGIC;
            islv16_program_count    : IN  STD_LOGIC_VECTOR(15 DOWNTO 0);
            islv16_op_code          : IN  STD_LOGIC_VECTOR(15 DOWNTO 0);
            ir_sreg                 : IN  t_sreg_bits;
            or_register_control     : OUT t_register_control;
            or_alu_control          : OUT t_alu_control;
            or_sram_control         : OUT t_sram_control;
            or_pc_control    	    : OUT t_pc_control;
            or_io_port_b_control    : OUT t_io_port_control;
            or_io_port_d_control    : OUT t_io_port_control
        );
END ENTITY instr_decoder;

--------------------------------------------------------------------------------

ARCHITECTURE rtl OF instr_decoder IS

    TYPE t_avr_op IS (NOP, ALU_IMM, ALU_2IN, ALU_1IN, LDS, STS, JMP, BRANCH, WAIT_1, WAIT_2, UNKNOWN); 
    
    TYPE t_registers IS RECORD
        avr_op                      : t_avr_op;
        r_alu_control               : t_alu_control;
        r_register_control          : t_register_control;
        r_sram_control              : t_sram_control;
        r_pc_control                : t_pc_control;
        r_io_port_b_control         : t_io_port_control;
        r_io_port_d_control         : t_io_port_control;
        sl_eval_next_cmd            : STD_LOGIC;
        
        sig17_new_jump_addr           : SIGNED(16 DOWNTO 0);
    END RECORD t_registers;
    
    SIGNAL r, r_next                : t_registers;

BEGIN

    --  Select which operation to perform ... and which output to route out
    comb_proc : PROCESS (r, isl_reset, islv16_op_code)
    
        VARIABLE v                              : t_registers;
        VARIABLE vusig4_alu_imm_opcode_bits     : UNSIGNED( 3 DOWNTO 0);
        VARIABLE vusig4_alu_2_opcode_bits       : UNSIGNED( 3 DOWNTO 0);
        VARIABLE vusig4_alu_1_opcode_bits       : UNSIGNED( 3 DOWNTO 0);
        
        VARIABLE vusig16_abs_addr               : UNSIGNED(15 DOWNTO 0);
        VARIABLE vint_abs_addr                  : INTEGER;
        VARIABLE vusig16_sram_addr              : UNSIGNED(15 DOWNTO 0);
        
        VARIABLE vslv4_br_op_code               : STD_LOGIC_VECTOR(3 DOWNTO 0);
        VARIABLE vsl_do_the_branch              : STD_LOGIC;
        VARIABLE vsig7_realative_jump_addr      : SIGNED(6 DOWNTO 0);
        VARIABLE vsig17_new_jump_addr           : SIGNED(16 DOWNTO 0);
        
    BEGIN
    
        v                                       :=  r;  --  Keep everything stable
        
        --  Single-cycle signals
        v.r_register_control.sl_data_bus_write  := '0';
        v.r_register_control.sl_data_bus_read   := '0';
        v.r_sram_control.sl_write_enable        := '0';
        v.r_sram_control.sl_read_enable         := '0';
        v.r_pc_control.sl_set_new_pc            := '0';

        v.r_io_port_b_control.sl_read_ddr       := '0';
        v.r_io_port_b_control.sl_write_ddr      := '0';
        v.r_io_port_b_control.sl_read_port      := '0';
        v.r_io_port_b_control.sl_write_port     := '0';
        v.r_io_port_b_control.sl_read_pin       := '0';

        v.r_io_port_d_control.sl_read_ddr       := '0';
        v.r_io_port_d_control.sl_write_ddr      := '0';
        v.r_io_port_d_control.sl_read_port      := '0';
        v.r_io_port_d_control.sl_write_port     := '0';
        v.r_io_port_d_control.sl_read_pin       := '0';
        
        --  Reset all ALU control signals
        v.r_alu_control.sl_alu_1_cmd            := '0';     --  No ALU operation by default
        v.r_alu_control.sl_alu_2_cmd            := '0';     --  No ALU operation by default
        v.r_alu_control.sl_alu_imm_cmd          := '0';     --  No ALU operation by default
        
        
        --##    Evaluate this OpCode as a command
        --#######################################
        IF r.sl_eval_next_cmd = '1' THEN
        

            --  Transform ALU op-code bits to numeric values
            vusig4_alu_imm_opcode_bits              := UNSIGNED(islv16_op_code(15 DOWNTO 12) );
            vusig4_alu_2_opcode_bits                := UNSIGNED(islv16_op_code(13 DOWNTO 10) );
            vusig4_alu_1_opcode_bits                := UNSIGNED(islv16_op_code( 3 DOWNTO  0) );
            
            --  Generate partial op-codes
            v.r_alu_control.slv4_alu_1_op_code      := islv16_op_code(3 DOWNTO 0);
            v.r_alu_control.slv6_alu_2_op_code      := islv16_op_code(15 DOWNTO 10);
            v.r_alu_control.slv4_alu_imm_op_code    := islv16_op_code(15 DOWNTO 12);
            v.r_alu_control.slv8_imm_data           := islv16_op_code(11 DOWNTO 8) & islv16_op_code(3 DOWNTO 0);
            
            
            --  Assmebled Register Control
            v.r_register_control.slv5_dest_reg      := islv16_op_code(8 DOWNTO 4);
            v.r_register_control.slv5_src_reg       := islv16_op_code(9) & islv16_op_code(3 DOWNTO 0);
    
    
            --##    Decode the Op-code
            --##    ==================
            --########################
            IF    islv16_op_code(15 DOWNTO  0) = x"0000"    THEN 
                        REPORT "NOP Instruction";
                        v.avr_op := NOP;
                    
            --##    ALU Immediate Instruction
            --###############################
            ELSIF (vusig4_alu_imm_opcode_bits >= 3 AND vusig4_alu_imm_opcode_bits <= 7 ) OR
                  islv16_op_code(15 DOWNTO 12) = B"1110"    THEN
                        REPORT "ALU Immediate Instruction";
                        v.avr_op := ALU_IMM;    --  This is an Immediate-and-Regster ALU command
                        v.r_alu_control.sl_alu_imm_cmd                  := '1';
                        v.r_register_control.slv5_dest_reg(4)           := '1'; --  Force 
                        IF islv16_op_code(15 DOWNTO 12) = "0011" THEN    --  This is a compare command
                            v.r_register_control.sl_data_bus_write      := '0';
                        ELSE
                            v.r_register_control.sl_data_bus_write      := '1';
                        END IF;
                                  
            --##    ALU 2 Register Instruction
            --################################
            ELSIF islv16_op_code(15 DOWNTO 14) = B"00"    AND
                  vusig4_alu_2_opcode_bits >= 1 AND vusig4_alu_2_opcode_bits <= 11 THEN 
                            REPORT "ALU 2-Register Instruction";
                            v.avr_op := ALU_2IN;    --  this is 2-Register ALU command
                            v.r_alu_control.sl_alu_2_cmd                := '1';
                            IF islv16_op_code(15 DOWNTO 13) = "000" AND islv16_op_code(11 DOWNTO 10) = "01" THEN    --  This is a compare command
                                v.r_register_control.sl_data_bus_write  := '0';
                            ELSE
                                v.r_register_control.sl_data_bus_write  := '1';
                            END IF;
                                                                                    
            --##    ALU 1 Register Instruction
            --################################
            ELSIF islv16_op_code(15 DOWNTO  9) = B"1001_010" AND
                  islv16_op_code( 3 DOWNTO  2) /= "11"      THEN 
                            v.avr_op                                := ALU_1IN;     --  this is a Single-Input ALU command
                            v.r_alu_control.sl_alu_1_cmd            := '1';
                            v.r_register_control.sl_data_bus_write  := '1';
                            
            --##    LDS   Load Direct from Data Space
            --#######################################
            ELSIF islv16_op_code(15 DOWNTO  9) = B"1001_000" AND
                  islv16_op_code( 3 DOWNTO  0) = B"0000"     THEN 
                            REPORT "LDS Instruction";       --  This is a SRAM Direct Load / Store Operation
                            v.avr_op                                := LDS;
                            v.sl_eval_next_cmd                      := '0';
                                            
            --##    STS   Store Direct from Data Space
            --########################################                                        
            ELSIF islv16_op_code(15 DOWNTO  9) = B"1001_001" AND
                  islv16_op_code( 3 DOWNTO  0) = B"0000"     THEN 
                            REPORT "STS Instruction";       --  This is a SRAM Direct Load / Store Operation
                            v.avr_op                                := STS;
                            v.sl_eval_next_cmd                      := '0';
                            
            --##    JMP   Long Jump to absolute Address
            --########################################                                        
            ELSIF islv16_op_code(15 DOWNTO  9) = B"1001_010" AND
                  islv16_op_code( 3 DOWNTO  1) = B"110"     THEN 
                            REPORT "JMP Instruction";       --  This is a SRAM Direct Load / Store Operation
                            v.avr_op                                := JMP;
                            v.sl_eval_next_cmd                      := '0';
                            
            --##    Branch Commands
            --########################################
            ELSIF islv16_op_code(15 DOWNTO 11) = B"1111_0" THEN
                            REPORT "Branch Instruction";
                            vslv4_br_op_code  := islv16_op_code(10) & islv16_op_code(2 DOWNTO 0);
                            vsl_do_the_branch := '0';
                            CASE vslv4_br_op_code IS
                                WHEN "0000" =>  --  BRCS Command
                                                IF ir_sreg.sl_carry      = '1' THEN 
                                                    vsl_do_the_branch   := '1'; 
                                                END IF;
                                                
                                WHEN "1000" =>  --  BRCC Command
                                                IF ir_sreg.sl_carry      = '0' THEN 
                                                    vsl_do_the_branch   := '1'; 
                                                END IF;
                                                
                                WHEN "0001" =>  --  BREQ Command
                                                IF ir_sreg.sl_zero       = '1' THEN 
                                                    vsl_do_the_branch   := '1'; 
                                                END IF;
                                                
                                WHEN "1001" =>  --  BRNE Command
                                                IF ir_sreg.sl_zero       = '0' THEN 
                                                    vsl_do_the_branch   := '1'; 
                                                END IF;
                                                
                                WHEN "0010" =>  --  BRMI Command
                                                IF ir_sreg.sl_negative   = '1' THEN 
                                                    vsl_do_the_branch   := '1'; 
                                                END IF;
                                                
                                WHEN "1010" =>  --  BRPL Command
                                                IF ir_sreg.sl_negative   = '0' THEN 
                                                    vsl_do_the_branch   := '1'; 
                                                END IF;
                                                
                                WHEN "0011" =>  --  BRVS Command
                                                IF ir_sreg.sl_overflow   = '1' THEN 
                                                    vsl_do_the_branch   := '1'; 
                                                END IF;
                                                
                                WHEN "1011" =>  --  BRVC Command
                                                IF ir_sreg.sl_overflow   = '0' THEN 
                                                    vsl_do_the_branch   := '1'; 
                                                END IF;
                                                
                                WHEN "0100" =>  --  BRLT Command
                                                IF ir_sreg.sl_sign       = '1' THEN 
                                                    vsl_do_the_branch   := '1'; 
                                                END IF;
                                                
                                WHEN "1100" =>  --  BRGE Command
                                                IF ir_sreg.sl_sign       = '0' THEN 
                                                    vsl_do_the_branch   := '1'; 
                                                END IF;
                                                
                                WHEN "0101" =>  --  BRHS Command
                                                IF ir_sreg.sl_half_carry = '1' THEN 
                                                    vsl_do_the_branch   := '1'; 
                                                END IF;
                                                
                                WHEN "1101" =>  --  BRHC Command
                                                IF ir_sreg.sl_half_carry = '0' THEN 
                                                    vsl_do_the_branch   := '1'; 
                                                END IF;
                                                
                                WHEN "0110" =>  --  BRTS Command
                                                IF ir_sreg.sl_transfer   = '1' THEN 
                                                    vsl_do_the_branch   := '1'; 
                                                END IF;
                                                
                                WHEN "1110" =>  --  BRTC Command
                                                IF ir_sreg.sl_transfer   = '0' THEN 
                                                    vsl_do_the_branch   := '1'; 
                                                END IF;
                                                
                                WHEN "0111" =>  --  BRIE Command
                                                IF ir_sreg.sl_interrupt  = '1' THEN 
                                                    vsl_do_the_branch   := '1'; 
                                                END IF;
                                                
                                WHEN "1111" =>  --  BRID Command
                                                IF ir_sreg.sl_interrupt  = '0' THEN 
                                                    vsl_do_the_branch   := '1'; 
                                                END IF;
                                                
                                WHEN OTHERS =>  null;
                            END CASE;
                            IF vsl_do_the_branch = '1' THEN
                                vsig7_realative_jump_addr           := SIGNED( islv16_op_code(9 DOWNTO 3) );
                                IF vsig7_realative_jump_addr > 0 THEN
                                    vsig17_new_jump_addr            := SIGNED('0' & islv16_program_count) + vsig7_realative_jump_addr + 1;
                                ELSE
                                    vsig17_new_jump_addr            := SIGNED('0' & islv16_program_count) + vsig7_realative_jump_addr;
                                END IF;
                                v.sig17_new_jump_addr               := vsig17_new_jump_addr;
                                v.r_pc_control.usig16_new_pc_value  := UNSIGNED( vsig17_new_jump_addr(15 DOWNTO 0) );                            
                                v.r_pc_control.sl_set_new_pc        := '1';
                                v.avr_op                            := BRANCH;
                                v.sl_eval_next_cmd                  := '0';
                            END IF;
                            
                            
            ELSE
                            v.avr_op                                := UNKNOWN; 
                  
            END IF;


        --##    Delayed execution due to multi-cycle command
        --##################################################
        ELSE
        
            vusig16_abs_addr      := UNSIGNED(islv16_op_code);
            vint_abs_addr         := TO_INTEGER(vusig16_abs_addr);
            vusig16_sram_addr     := vusig16_abs_addr - c_SRAM_START;
        
            CASE r.avr_op IS
            
                WHEN LDS        =>  --  This is the 2nd cycle of a Store operation, with the address in the op-code
                                    v.r_register_control.sl_data_bus_write  := '1';
                                    v.r_sram_control.slv11_sram_addr        := STD_LOGIC_VECTOR(vusig16_sram_addr(10 DOWNTO 0));
                                    v.sl_eval_next_cmd                          := '1';
                                    CASE vint_abs_addr IS
                                        WHEN c_PINB_ADDR    => v.r_io_port_b_control.sl_read_pin    := '1';
                                        WHEN c_DDRB_ADDR    => v.r_io_port_b_control.sl_read_ddr    := '1';
                                        WHEN c_PORTB_ADDR   => v.r_io_port_b_control.sl_read_port   := '1';
                                        
                                        WHEN c_PIND_ADDR    => v.r_io_port_d_control.sl_read_pin    := '1';
                                        WHEN c_DDRD_ADDR    => v.r_io_port_d_control.sl_read_ddr    := '1';
                                        WHEN c_PORTD_ADDR   => v.r_io_port_d_control.sl_read_port   := '1';
                                        
                                        WHEN c_SRAM_START TO
                                             c_SRAM_END     => v.r_sram_control.sl_read_enable      := '1';
                                             
                                         WHEN OTHERS        => NULL;

                                    END CASE;                                    
                
                WHEN STS        =>  --  This is the 2nd cycle of a Store operation, with the address in the op-code
                                    v.r_register_control.sl_data_bus_read       := '1';
                                    v.r_sram_control.slv11_sram_addr        := STD_LOGIC_VECTOR(vusig16_sram_addr(10 DOWNTO 0));
                                    v.sl_eval_next_cmd                          := '1';
                                    CASE vint_abs_addr IS
                                        WHEN c_DDRB_ADDR    => v.r_io_port_b_control.sl_write_ddr   := '1';
                                        WHEN c_PORTB_ADDR   => v.r_io_port_b_control.sl_write_port  := '1';
                                        
                                        WHEN c_DDRD_ADDR    => v.r_io_port_d_control.sl_write_ddr   := '1';
                                        WHEN c_PORTD_ADDR   => v.r_io_port_d_control.sl_write_port  := '1';
                                        
                                        WHEN c_SRAM_START TO
                                             c_SRAM_END     => v.r_sram_control.sl_write_enable     := '1';

                                         WHEN OTHERS        => NULL;

                                    END CASE;
                                    
                WHEN JMP        =>  --   This is the 2nd cycle of a Jump-Operation
                                    v.r_pc_control.sl_set_new_pc                := '1';
                                    v.r_pc_control.usig16_new_pc_value          := vusig16_abs_addr;
                                    v.avr_op                                    := WAIT_1;
                                   
                WHEN WAIT_1     =>  v.sl_eval_next_cmd                          := '1';
                
                WHEN BRANCH     =>  v.sl_eval_next_cmd                          := '1';
                                    
                WHEN OTHERS     =>  null;
            END CASE;
        
        END IF; --  End multi-cycle command execution
        
        
        --  Reset
        IF isl_reset = '1' THEN
            v.sl_eval_next_cmd                      := '1';
            v.r_sram_control.slv11_sram_addr        := (OTHERS => '0'); 
        END IF;

                                                
        r_next  <= v;       --  Copy variables to signals        
    END PROCESS comb_proc;
    
    
    --##    Registered Process
    --##
    --#########################
    reg_proc : PROCESS (isl_clock)
    BEGIN
        IF rising_edge(isl_clock) THEN r   <= r_next; END IF;
    END PROCESS;
    
    
    --##    Output Assignments
    --##
    --#########################
    or_alu_control                              <= r_next.r_alu_control; 
    or_register_control                         <= r_next.r_register_control;
    or_sram_control                             <= r_next.r_sram_control;
    or_pc_control                               <= r_next.r_pc_control;
    or_io_port_b_control                        <= r_next.r_io_port_b_control;
    or_io_port_d_control                        <= r_next.r_io_port_d_control;
    
END ARCHITECTURE rtl;
