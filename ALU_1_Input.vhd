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

--  Single-Input AVR ALU
--  ====================
--  Based on the 2014 Atmel document Atmel-0856J-AVR-Instruction-Set-Manual_07/2014

--  Supported commands:
--
--  ASR		Arithmetic Shift Right     Op-Code 1001 010d dddd 0101
--  COM		One's Complement           Op-Code 1001 010d dddd 0000
--  DEC		Decrement                  Op-Code 1001 010d dddd 1010
--  INC		Increment                  Op-Code 1001 010d dddd 0011
--  LSR		Logic Shift Right          Op-Code 1001 010d dddd 0110
--  NEG		Two's Complement           Op-Code 1001 010d dddd 0001
--  ROR		Rotate Right through Carry Op-Code 1001 010d dddd 0111
--  SWAP	Swap Nibbles               Op-Code 1001 010d dddd 0010

--  Input:  Only last 4 bits of the Op-Code, as the rest is handled outside
--          8-Bit data (Destination Register)
--          Enable bit
--          SREG bits
--          
--  Output  8-Bit data (Destination Register)
--          SREG update information
--
--  Special This block is purely combinatorial, all registers are outside

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE work.AVR_Records_pkg.ALL;

PACKAGE ALU_1_Input_pkg IS
    COMPONENT ALU_1_Input IS
        PORT (
            isl_enable          : IN  STD_LOGIC;
            islv4_operation     : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
            islv8_data      	: IN  STD_LOGIC_VECTOR(7 DOWNTO 0);
            ir_sreg           	: IN  t_sreg_bits;    --  Need Carry-Bit for DEC, INC and ROR operation
            or_sreg_update      : OUT t_sreg_bits;
            oslv8_data          : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
            osl_data_valid      : OUT STD_LOGIC
        );
    END COMPONENT ALU_1_Input;
END PACKAGE ALU_1_Input_pkg;

--------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE work.AVR_Records_pkg.ALL;
USE work.ALU_Functions_pkg.ALL;

ENTITY ALU_1_Input IS
    PORT (
        isl_enable          : IN  STD_LOGIC;
        islv4_operation     : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
        islv8_data          : IN  STD_LOGIC_VECTOR(7 DOWNTO 0);
        ir_sreg           	: IN  t_sreg_bits;    --  Need Carry-Bit for DEC, INC and ROR operation
        or_sreg_update      : OUT t_sreg_bits;
        oslv8_data          : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
        osl_data_valid      : OUT STD_LOGIC
    );
END ENTITY ALU_1_Input;

--------------------------------------------------------------------------------

ARCHITECTURE rtl OF ALU_1_Input IS 

    CONSTANT cASR_OP        : STD_LOGIC_VECTOR(3 DOWNTO 0)  := "0101";
    CONSTANT cCOM_OP        : STD_LOGIC_VECTOR(3 DOWNTO 0)  := "0000";
    CONSTANT cDEC_OP        : STD_LOGIC_VECTOR(3 DOWNTO 0)  := "1010";
    CONSTANT cINC_OP        : STD_LOGIC_VECTOR(3 DOWNTO 0)  := "0011";
    CONSTANT cLSR_OP        : STD_LOGIC_VECTOR(3 DOWNTO 0)  := "0110";
    CONSTANT cNEG_OP        : STD_LOGIC_VECTOR(3 DOWNTO 0)  := "0001";
    CONSTANT cROR_OP        : STD_LOGIC_VECTOR(3 DOWNTO 0)  := "0111";
    CONSTANT cSWAP_OP       : STD_LOGIC_VECTOR(3 DOWNTO 0)  := "0010";
    
BEGIN

    comb_proc : PROCESS (isl_enable, islv4_operation, islv8_data, ir_sreg)
        VARIABLE vslv8_data         : STD_LOGIC_VECTOR(7 DOWNTO 0);
        VARIABLE vr_sreg            : t_sreg_bits;
        
    BEGIN
    
        --  Keep SREG bits unchanged, unless intentionally altered
        vr_sreg                     := ir_sreg;
        
        IF isl_enable = '1' THEN

            vr_sreg.sl_enable       := '1';     --  Per default, update SREG
            
            CASE islv4_operation IS
            
                WHEN cASR_OP    =>  --  Arithmetical Shift Right
                                    vslv8_data(6 DOWNTO 0)        := islv8_data(7 DOWNTO 1);
                                    vslv8_data(7)                 := islv8_data(7);
                                    vr_sreg.sl_zero               := f_zero_flag(vslv8_data);
                                    vr_sreg.sl_negative           := vslv8_data(7);
                                    vr_sreg.sl_carry              := islv8_data(0);   --  This is special for ASR
                                    vr_sreg.sl_overflow           := vr_sreg.sl_negative XOR vr_sreg.sl_carry;
                                    vr_sreg.sl_sign               := vr_sreg.sl_negative XOR vr_sreg.sl_overflow;
                                    
                WHEN cCOM_OP    =>  --  One's complement = Invert all the bits
                                    vslv8_data                    := NOT islv8_data;
                                    vr_sreg.sl_zero               := f_zero_flag(vslv8_data);
                                    vr_sreg.sl_negative           := vslv8_data(7);
                                    vr_sreg.sl_carry              := '1';             --  This is special for COM
                                    vr_sreg.sl_overflow           := '0';             --  This is special for COM
                                    vr_sreg.sl_sign               := vslv8_data(7);   --  This is special for COM
                                                                        
                WHEN cDEC_OP    =>  --  Decrement by one
                                    vslv8_data                    := std_logic_vector(signed(islv8_data) - 1);
                                    vr_sreg.sl_zero               := f_zero_flag(vslv8_data);
                                    vr_sreg.sl_negative           := vslv8_data(7);
                                    --  No change to carry and half-carry bits with DEC operation
                                    vr_sreg.sl_negative           := vslv8_data(7);
                                    vr_sreg.sl_overflow           := (NOT vslv8_data(7)) AND vslv8_data(6) AND vslv8_data(5) AND vslv8_data(4) AND vslv8_data(3)
                                                                                          AND vslv8_data(2) AND vslv8_data(1) AND vslv8_data(0);
                                    vr_sreg.sl_sign               := vr_sreg.sl_negative XOR vr_sreg.sl_overflow;
                                     
            
                WHEN cINC_OP    =>  --  Increment by one
                                    vslv8_data                    := std_logic_vector(signed(islv8_data) + 1);
                                    vr_sreg.sl_zero               := f_zero_flag(vslv8_data);
                                    vr_sreg.sl_negative           := vslv8_data(7);
                                    --  No change to carry and half-carry bits with INC operation
                                    vr_sreg.sl_overflow           := vslv8_data(7) AND (NOT vslv8_data(6)) AND (NOT vslv8_data(5)) 
                                                                                   AND (NOT vslv8_data(4)) AND (NOT vslv8_data(3)) 
                                                                                   AND (NOT vslv8_data(2)) AND (NOT vslv8_data(1)) 
                                                                                   AND (NOT vslv8_data(0));
                                    vr_sreg.sl_sign               := vr_sreg.sl_negative XOR vr_sreg.sl_overflow;
                                    
                WHEN cLSR_OP    =>  --  Logical Shift Right
                                    vslv8_data(6 DOWNTO 0)        := islv8_data(7 DOWNTO 1);
                                    vslv8_data(7)                 := '0';
                                    vr_sreg.sl_zero               := f_zero_flag(vslv8_data);
                                    vr_sreg.sl_negative           := vslv8_data(7);
                                    vr_sreg.sl_carry              := islv8_data(0);   --  This is special for LSR
                                    vr_sreg.sl_overflow           := vslv8_data(7) XOR islv8_data(0);
                                    vr_sreg.sl_sign               := vr_sreg.sl_negative XOR vr_sreg.sl_overflow;
                                    
                WHEN cNEG_OP    =>  --  Two's Complement
                                    vslv8_data                    := STD_LOGIC_VECTOR( 0 - SIGNED(islv8_data) );
                                    vr_sreg.sl_zero               := f_zero_flag(vslv8_data);
                                    vr_sreg.sl_negative           := vslv8_data(7);
                                    vr_sreg.sl_half_carry         := vslv8_data(3) OR (NOT islv8_data(3));
                                    vr_sreg.sl_carry              :=    vslv8_data(7) OR vslv8_data(6) OR vslv8_data(5) OR vslv8_data(4) 
                                                                     OR vslv8_data(3) OR vslv8_data(2) OR vslv8_data(1) OR vslv8_data(0);
                                    vr_sreg.sl_overflow           := vslv8_data(7)  AND (NOT vslv8_data(6)) AND (NOT vslv8_data(5)) 
                                                                     AND (NOT vslv8_data(4)) AND (NOT vslv8_data(3)) AND (NOT vslv8_data(2)) 
                                                                     AND (NOT vslv8_data(1)) AND (NOT vslv8_data(0));
                                    vr_sreg.sl_sign               := vr_sreg.sl_negative XOR vr_sreg.sl_overflow;
                                    
                WHEN cROR_OP    =>  --  Rotate Right through Carry
                                    vslv8_data(6 DOWNTO 0)        := islv8_data(7 DOWNTO 1);
                                    vslv8_data(7)                 := ir_sreg.sl_carry;
                                    vr_sreg.sl_zero               := f_zero_flag(vslv8_data);
                                    vr_sreg.sl_negative           := vslv8_data(7);
                                    vr_sreg.sl_carry              := islv8_data(0);   --  This is special for ROR
                                    vr_sreg.sl_overflow           := vslv8_data(7) XOR islv8_data(0);
                                    vr_sreg.sl_sign               := vr_sreg.sl_negative XOR vr_sreg.sl_overflow;
                                    
                WHEN cSWAP_OP   =>  --  Swap Nibbles
                                    vslv8_data                    := islv8_data(3 DOWNTO 0) & islv8_data(7 DOWNTO 4);
                                    vr_sreg.sl_enable             := '0';
                                    
                                    
            
                WHEN OTHERS     =>  REPORT "Unsuported ALU 1 Operation" SEVERITY WARNING;
                                    vr_sreg.sl_enable             := '0';
            END CASE;
            
            oslv8_data                      <= vslv8_data;  
            osl_data_valid                  <= '1';
            or_sreg_update                  <= vr_sreg;
                
        ELSE
            oslv8_data                      <= (OTHERS => '0');
            osl_data_valid                  <= '0';
            or_sreg_update.sl_enable        <= '0';         --  No SREG update by default

        END IF;
    END PROCESS comb_proc;

END ARCHITECTURE rtl;
