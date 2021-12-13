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

--  Two-Input AVR ALU
--  ====================
--  Based on the 2014 Atmel document Atmel-0856J-AVR-Instruction-Set-Manual_07/2014

--  Supported commands:
--
--  AND		Logical AND                Op-Code 0010 00rd dddd rrrr
--  EOR		Logical XOR                Op-Code 0010 01rd dddd rrrr
--  OR		Logical OR                 Op-Code 0010 10rd dddd rrrr
--  ADD		Add without Carry          Op-Code 0000 11rd dddd rrrr
--  ADC		Add with Carry             Op-Code 0001 11rd dddd rrrr
--  CP		Compare                    Op-Code 0001 01rd dddd rrrr
--  CPC		Compare with Carry         Op-Code 0000 01rd dddd rrrr
--  SUB		Subract without Carry      Op-Code 0001 10rd dddd rrrr
--  SBC		Subtract with Carry        Op-Code 0000 10rd dddd rrrr
--  MOV		Move / Copy                Op-Code 0010 11rd dddd rrrr

--  Input:  Only first 6 bits of the Op-Code, as the rest is handled outside
--          2 x 8-Bit data (Source and Destination Register)
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

PACKAGE ALU_2_Input_pkg IS
    COMPONENT ALU_2_Input IS
        PORT (
            isl_enable          : IN  STD_LOGIC;
            islv6_alu_op_code   : IN  STD_LOGIC_VECTOR(5 DOWNTO 0);
            islv8_src_data     	: IN  STD_LOGIC_VECTOR(7 DOWNTO 0);
            islv8_dest_data    	: IN  STD_LOGIC_VECTOR(7 DOWNTO 0);
            ir_sreg           	: IN  t_sreg_bits;    --  Need Carry-Bit for DEC, INC and ROR operation
            or_sreg_update      : OUT t_sreg_bits;
            oslv8_data          : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
            osl_data_valid      : OUT STD_LOGIC
        );
    END COMPONENT ALU_2_Input;
END PACKAGE ALU_2_Input_pkg;

--------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE work.AVR_Records_pkg.ALL;
USE work.ALU_Functions_pkg.ALL;

ENTITY ALU_2_Input IS
        PORT (
            isl_enable          : IN  STD_LOGIC;
            islv6_alu_op_code   : IN  STD_LOGIC_VECTOR(5 DOWNTO 0);
            islv8_src_data      : IN  STD_LOGIC_VECTOR(7 DOWNTO 0);
            islv8_dest_data     : IN  STD_LOGIC_VECTOR(7 DOWNTO 0);
            ir_sreg             : IN  t_sreg_bits;    --  Need Carry-Bit for DEC, INC and ROR operation
            or_sreg_update      : OUT t_sreg_bits;
            oslv8_data          : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
            osl_data_valid      : OUT STD_LOGIC
        );
END ENTITY ALU_2_Input;

--------------------------------------------------------------------------------

ARCHITECTURE rtl OF ALU_2_Input IS

    CONSTANT cAND_OP        : STD_LOGIC_VECTOR(5 DOWNTO 0)  := B"0010_00";
    CONSTANT cEOR_OP        : STD_LOGIC_VECTOR(5 DOWNTO 0)  := B"0010_01";
    CONSTANT cOR_OP         : STD_LOGIC_VECTOR(5 DOWNTO 0)  := B"0010_10";
    CONSTANT cADD_OP        : STD_LOGIC_VECTOR(5 DOWNTO 0)  := B"0000_11";
    CONSTANT cADC_OP        : STD_LOGIC_VECTOR(5 DOWNTO 0)  := B"0001_11";
    CONSTANT cCP_OP         : STD_LOGIC_VECTOR(5 DOWNTO 0)  := B"0001_01";
    CONSTANT cCPC_OP        : STD_LOGIC_VECTOR(5 DOWNTO 0)  := B"0000_01";
    CONSTANT cSUB_OP        : STD_LOGIC_VECTOR(5 DOWNTO 0)  := B"0001_10";
    CONSTANT cSBC_OP        : STD_LOGIC_VECTOR(5 DOWNTO 0)  := B"0000_10";
    CONSTANT cMOV_OP        : STD_LOGIC_VECTOR(5 DOWNTO 0)  := B"0010_11";
    CONSTANT cNOP_OP        : STD_LOGIC_VECTOR(5 DOWNTO 0)  := B"0000_00";
    
    CONSTANT cslv9_zero     : SIGNED(8 DOWNTO 0)            := (OTHERS => '0');
    
BEGIN

    comb_proc : PROCESS (isl_enable, islv6_alu_op_code, islv8_src_data, islv8_dest_data, ir_sreg)
        VARIABLE vslv8_data         : STD_LOGIC_VECTOR(7 DOWNTO 0);
        VARIABLE vr_sreg            : t_sreg_bits;
        
    BEGIN
    
        --  Keep SREG bits unchanged, unless intentionally altered
        vr_sreg                     := ir_sreg;
        
        IF isl_enable = '1' THEN
        
            --  Set data valid at the beginning - just in case it can be cleared during a compare operation!
            osl_data_valid                  <= '1';

            
            CASE islv6_alu_op_code IS
            
                WHEN cAND_OP    =>  --  Logical AND Operation
                                    vslv8_data                  := islv8_src_data AND islv8_dest_data;
                                    vr_sreg.sl_carry            := ir_sreg.sl_carry;   --  This is special for Logic Operations
                                    vr_sreg.sl_negative         := vslv8_data(7);
                                    vr_sreg.sl_zero             := f_zero_flag(vslv8_data);
                                    vr_sreg.sl_overflow         := '0';                --  This is special for Logic Operations
                                    vr_sreg.sl_sign             := vslv8_data(7);      --  This is special for Logic Operations
                                
                WHEN cEOR_OP    =>  --  Logical XOR Operation
                                    vslv8_data                  := islv8_src_data XOR islv8_dest_data;
                                    --  No change to carry and half-carry flag with EOR operation
                                    vr_sreg.sl_negative         := vslv8_data(7);
                                    vr_sreg.sl_zero             := f_zero_flag(vslv8_data);
                                    vr_sreg.sl_overflow         := '0';                --  This is special for Logic Operations
                                    vr_sreg.sl_sign             := vslv8_data(7);      --  This is special for Logic Operations
                                                    
                WHEN cOR_OP     =>  --  Logical OR Operation
                                    vslv8_data                  := islv8_src_data OR islv8_dest_data;
                                    --  No change to carry and half-carry flag with OR operation
                                    vr_sreg.sl_negative         := vslv8_data(7);
                                    vr_sreg.sl_zero             := f_zero_flag(vslv8_data);
                                    vr_sreg.sl_overflow         := '0';                --  This is special for Logic Operations
                                    vr_sreg.sl_sign             := vslv8_data(7);      --  This is special for Logic Operations
                                                                                                                                                
                WHEN cADD_OP    =>  --  Add without Carry
                                    vslv8_data                  := std_logic_vector(signed(islv8_src_data) + signed(islv8_dest_data));
                                    vr_sreg.sl_negative         := vslv8_data(7);
                                    vr_sreg.sl_zero             := f_zero_flag(vslv8_data);
                                    vr_sreg.sl_half_carry       :=     ( islv8_src_data(3)  AND islv8_dest_data(3)  ) 
                                                                    OR ( islv8_src_data(3)  AND (NOT vslv8_data(3)) )
                                                                    OR ( islv8_dest_data(3) AND (NOT vslv8_data(3)) );
                                    vr_sreg.sl_carry            :=     ( islv8_src_data(7)  AND islv8_dest_data(7)  ) 
                                                                    OR ( islv8_src_data(7)  AND (NOT vslv8_data(7)) )
                                                                    OR ( islv8_dest_data(7) AND (NOT vslv8_data(7)) );
                                    vr_sreg.sl_overflow         :=    ( islv8_dest_data(7) AND islv8_src_data(7) AND (NOT vslv8_data(7)) )
                                                                    OR ( (NOT islv8_dest_data(7)) AND (NOT islv8_src_data(7)) AND vslv8_data(7) );
                                    vr_sreg.sl_sign             := vr_sreg.sl_overflow XOR vslv8_data(7);
                                     
                WHEN cADC_OP    =>  --  Add with Carry
                                    IF ir_sreg.sl_carry = '0' THEN
                                        vslv8_data              := STD_LOGIC_VECTOR( SIGNED( islv8_src_data) + SIGNED(islv8_dest_data) ); 
                                    ELSE
                                        vslv8_data              := STD_LOGIC_VECTOR(SIGNED( islv8_src_data) + SIGNED(islv8_dest_data) + 1);
                                    END IF;
                                    vr_sreg.sl_negative         := vslv8_data(7);
                                    vr_sreg.sl_zero             := f_zero_flag(vslv8_data);
                                    vr_sreg.sl_half_carry       :=     ( islv8_src_data(3)  AND islv8_dest_data(3)  ) 
                                                                    OR ( islv8_src_data(3)  AND (NOT vslv8_data(3)) )
                                                                    OR ( islv8_dest_data(3) AND (NOT vslv8_data(3)) );
                                    vr_sreg.sl_carry            :=     ( islv8_src_data(7)  AND islv8_dest_data(7)  ) 
                                                                    OR ( islv8_src_data(7)  AND (NOT vslv8_data(7)) )
                                                                    OR ( islv8_dest_data(7) AND (NOT vslv8_data(7)) );
                                    vr_sreg.sl_overflow         :=     ( islv8_dest_data(7) AND islv8_src_data(7) AND (NOT vslv8_data(7)) )
                                                                    OR ( (NOT islv8_dest_data(7)) AND (NOT islv8_src_data(7)) AND vslv8_data(7) );
                                    vr_sreg.sl_sign             := vr_sreg.sl_overflow XOR vslv8_data(7);
                                     
            
                WHEN cCP_OP     =>  --  Compare
                                    vslv8_data                  := std_logic_vector(signed(islv8_dest_data) - signed(islv8_src_data));
                                    vr_sreg.sl_negative         := vslv8_data(7);
                                    vr_sreg.sl_zero             := f_zero_flag(vslv8_data);
                                    vr_sreg.sl_half_carry       :=     ( (NOT islv8_dest_data(3)) AND islv8_src_data(3) ) 
                                                                    OR (      islv8_src_data(3)   AND vslv8_data(3)     )
                                                                    OR ( (NOT islv8_dest_data(3)) AND vslv8_data(3)     );
                                    vr_sreg.sl_carry            :=     ( (NOT islv8_dest_data(7)) AND islv8_src_data(7) ) 
                                                                    OR ( islv8_src_data(7)        AND vslv8_data(7)     )
                                                                    OR ( (NOT islv8_dest_data(7)) AND vslv8_data(7)     );
                                    vr_sreg.sl_overflow         :=     (      islv8_dest_data(7)  AND (NOT islv8_src_data(7)) AND (NOT vslv8_data(7)) )
                                                                    OR ( (NOT islv8_dest_data(7)) AND      islv8_src_data(7)  AND      vslv8_data(7)  );
                                    vr_sreg.sl_sign             := vr_sreg.sl_overflow XOR vslv8_data(7);
                                    osl_data_valid              <= '0';     --  No change to data with compare operation !
                                     
                WHEN cCPC_OP    =>  --  Compare with Carry
                                    IF ir_sreg.sl_carry = '0' THEN
                                        vslv8_data              := std_logic_vector(signed(islv8_dest_data) - signed(islv8_src_data));
                                    ELSE
                                        vslv8_data              := std_logic_vector(signed(islv8_dest_data) - signed(islv8_src_data) - 1);
                                    END IF;
                                    vr_sreg.sl_negative         := vslv8_data(7);
                                    vr_sreg.sl_zero             := f_zero_flag(vslv8_data) AND ir_sreg.sl_zero;
                                    vr_sreg.sl_half_carry       :=     ( (NOT islv8_dest_data(3)) AND islv8_src_data(3) ) 
                                                                    OR (      islv8_src_data(3)   AND vslv8_data(3)     )
                                                                    OR ( (NOT islv8_dest_data(3)) AND vslv8_data(3)     );
                                    vr_sreg.sl_carry            :=     ( (NOT islv8_dest_data(7)) AND islv8_src_data(7) ) 
                                                                    OR ( islv8_src_data(7)        AND vslv8_data(7)     )
                                                                    OR ( (NOT islv8_dest_data(7)) AND vslv8_data(7)     );
                                    vr_sreg.sl_overflow         :=     (      islv8_dest_data(7)  AND (NOT islv8_src_data(7)) AND (NOT vslv8_data(7)) )
                                                                    OR ( (NOT islv8_dest_data(7)) AND      islv8_src_data(7)  AND      vslv8_data(7)  );
                                    vr_sreg.sl_sign             := vr_sreg.sl_overflow XOR vslv8_data(7);
                                    osl_data_valid              <= '0';     --  No change to data with compare operation !
                                     
                WHEN cSUB_OP     =>  --  Subtract without Carry
                                    vslv8_data                  := std_logic_vector(signed(islv8_dest_data) - signed(islv8_src_data));
                                    vr_sreg.sl_negative         := vslv8_data(7);
                                    vr_sreg.sl_zero             := f_zero_flag(vslv8_data);
                                    vr_sreg.sl_half_carry       :=     ( (NOT islv8_dest_data(3)) AND islv8_src_data(3) ) 
                                                                    OR (      islv8_src_data(3)   AND vslv8_data(3)     )
                                                                    OR ( (NOT islv8_dest_data(3)) AND vslv8_data(3)     );
                                    vr_sreg.sl_carry            :=     ( (NOT islv8_dest_data(7)) AND islv8_src_data(7) ) 
                                                                    OR ( islv8_src_data(7)        AND vslv8_data(7)     )
                                                                    OR ( (NOT islv8_dest_data(7)) AND vslv8_data(7)     );
                                    vr_sreg.sl_overflow         :=     (      islv8_dest_data(7)  AND (NOT islv8_src_data(7)) AND (NOT vslv8_data(7)) )
                                                                    OR ( (NOT islv8_dest_data(7)) AND      islv8_src_data(7)  AND      vslv8_data(7)  );
                                    vr_sreg.sl_sign             := vr_sreg.sl_overflow XOR vslv8_data(7);
                                     
                WHEN cSBC_OP    =>  --  Subtract with Carry
                                    IF ir_sreg.sl_carry = '0' THEN
                                        vslv8_data              := std_logic_vector(signed(islv8_dest_data) - signed(islv8_src_data));
                                    ELSE
                                        vslv8_data              := std_logic_vector(signed(islv8_dest_data) - signed(islv8_src_data) - 1);
                                    END IF;
                                    vr_sreg.sl_negative         := vslv8_data(7);
                                    vr_sreg.sl_zero             := f_zero_flag(vslv8_data) AND ir_sreg.sl_zero;
                                    vr_sreg.sl_half_carry       :=     ( (NOT islv8_dest_data(3)) AND islv8_src_data(3) ) 
                                                                    OR (      islv8_src_data(3)   AND vslv8_data(3)     )
                                                                    OR ( (NOT islv8_dest_data(3)) AND vslv8_data(3)     );
                                    vr_sreg.sl_carry            :=     ( (NOT islv8_dest_data(7)) AND islv8_src_data(7) ) 
                                                                    OR ( islv8_src_data(7)        AND vslv8_data(7)     )
                                                                    OR ( (NOT islv8_dest_data(7)) AND vslv8_data(7)     );
                                    vr_sreg.sl_overflow         :=     (      islv8_dest_data(7)  AND (NOT islv8_src_data(7)) AND (NOT vslv8_data(7)) )
                                                                    OR ( (NOT islv8_dest_data(7)) AND      islv8_src_data(7)  AND      vslv8_data(7)  );
                                    vr_sreg.sl_sign             := vr_sreg.sl_overflow XOR vslv8_data(7);
                                     
                WHEN cMOV_OP    =>  --  Copy Register
                                    vslv8_data                   := islv8_src_data;
                                     
                WHEN cNOP_OP    =>  --  Copy Register
                                    vslv8_data                   := (OTHERS => '0');

            
                WHEN OTHERS     =>   REPORT "Unsuported ALU 2 Operation" SEVERITY WARNING;
            END CASE;
            
            oslv8_data                      <= vslv8_data;  
            or_sreg_update                  <= vr_sreg;
            or_sreg_update.sl_enable        <= '1';
                
        ELSE
            oslv8_data                      <= (OTHERS => '0');
            osl_data_valid                  <= '0';
            or_sreg_update.sl_enable        <= '0';         --  No SREG update by default
        END IF;
        
    END PROCESS comb_proc;

END ARCHITECTURE rtl;
