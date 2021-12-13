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

--  Single-Register and Immediate Value Input AVR ALU
--  =================================================
--  Based on the 2014 Atmel document Atmel-0856J-AVR-Instruction-Set-Manual_07/2014

--  Supported commands:
--
--  LDI		Load Immediate             Op-Code 1110 kkkk dddd kkkk
--  ANDI	Logical AND with Immediate Op-Code 0111 kkkk dddd kkkk
--  ORI		Logical OR with Immediate  Op-Code 0110 kkkk dddd kkkk
--  CPI		Compare with Immediate     Op-Code 0011 kkkk dddd kkkk
--  SUBI	Subtract Immediate         Op-Code 0101 kkkk dddd kkkk
--  SBIC	Subtract Imm. with Carry   Op-Code 0100 kkkk dddd kkkk

--  Input:  All bits of the Op-Code, as the immediate arguments are spread out
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

PACKAGE ALU_Immediate_pkg IS
    COMPONENT ALU_Immediate IS
        PORT (
            isl_enable          : IN  STD_LOGIC;
            islv4_alu_op_code   : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
            islv8_imm_data  	: IN  STD_LOGIC_VECTOR(7 DOWNTO 0);
            islv8_dest_data  	: IN  STD_LOGIC_VECTOR(7 DOWNTO 0);
            ir_sreg           	: IN  t_sreg_bits;    --  Need Carry-Bit for DEC, INC and ROR operation
            or_sreg_update      : OUT t_sreg_bits;
            oslv8_data          : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
            osl_data_valid      : OUT STD_LOGIC
        );
    END COMPONENT ALU_Immediate;
END PACKAGE ALU_Immediate_pkg;

--------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE work.AVR_Records_pkg.ALL;
USE work.ALU_Functions_pkg.ALL;

ENTITY ALU_Immediate IS
    PORT (
            isl_enable          : IN  STD_LOGIC;
            islv4_alu_op_code   : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
            islv8_imm_data      : IN  STD_LOGIC_VECTOR(7 DOWNTO 0);
            islv8_dest_data     : IN  STD_LOGIC_VECTOR(7 DOWNTO 0);
            ir_sreg             : IN  t_sreg_bits;    --  Need Carry-Bit for DEC, INC and ROR operation
            or_sreg_update      : OUT t_sreg_bits;
            oslv8_data          : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
            osl_data_valid      : OUT STD_LOGIC
    );
END ENTITY ALU_Immediate;

--------------------------------------------------------------------------------

ARCHITECTURE rtl OF ALU_Immediate IS

    CONSTANT cLDI_OP        : STD_LOGIC_VECTOR(3 DOWNTO 0)  := "1110";
    CONSTANT cANDI_OP       : STD_LOGIC_VECTOR(3 DOWNTO 0)  := "0111";
    CONSTANT cORI_OP        : STD_LOGIC_VECTOR(3 DOWNTO 0)  := "0110";
    CONSTANT cCPI_OP        : STD_LOGIC_VECTOR(3 DOWNTO 0)  := "0011";
    CONSTANT cSUBI_OP       : STD_LOGIC_VECTOR(3 DOWNTO 0)  := "0101";
    CONSTANT cSBCI_OP       : STD_LOGIC_VECTOR(3 DOWNTO 0)  := "0100";
    
BEGIN

    comb_proc : PROCESS (isl_enable, islv4_alu_op_code, islv8_imm_data, islv8_dest_data, ir_sreg)
        VARIABLE vslv8_data         : STD_LOGIC_VECTOR(7 DOWNTO 0);
        VARIABLE vr_sreg            : t_sreg_bits;
        
    BEGIN
    
        --  Keep SREG bits unchanged, unless intentionally altered
        vr_sreg                     := ir_sreg;
        
        IF isl_enable = '1' THEN
        
            --  Set data valid at the beginning - just in case it can be cleared during a compare operation!
            osl_data_valid                  <= '1';
            vr_sreg.sl_enable               := '1';
            
            CASE islv4_alu_op_code IS
            
                WHEN cLDI_OP    =>  --  Load Immediate value to Register
                                    vslv8_data(7 DOWNTO 0)      := islv8_imm_data;
                                    vr_sreg.sl_enable               := '0';     --  All SREG bits stay the same
                                    
                WHEN cANDI_OP   =>  --  Logical AND Operation with Immediate
                                    vslv8_data                  := islv8_dest_data AND islv8_imm_data;
                                    --  No change to carry and half-carry flags
                                    vr_sreg.sl_negative         := vslv8_data(7);
                                    vr_sreg.sl_zero             := f_zero_flag(vslv8_data);
                                    vr_sreg.sl_overflow         := '0';                --  This is special for Logic Operations
                                    vr_sreg.sl_sign             := vslv8_data(7);      --  This is special for Logic Operations
                                
                WHEN cORI_OP    =>  --  Logical OR Operation with Immediate
                                    vslv8_data                  := islv8_dest_data OR islv8_imm_data;
                                    --  No change to carry and half-carry flags
                                    vr_sreg.sl_negative         := vslv8_data(7);
                                    vr_sreg.sl_zero             := f_zero_flag(vslv8_data);
                                    vr_sreg.sl_overflow         := '0';                --  This is special for Logic Operations
                                    vr_sreg.sl_sign             := vslv8_data(7);      --  This is special for Logic Operations
                                     
                WHEN cCPI_OP    =>  --  Compare with Immediate
                                    vslv8_data                  := STD_LOGIC_VECTOR( SIGNED(islv8_dest_data) - SIGNED(islv8_imm_data));
                                    vr_sreg.sl_negative         := vslv8_data(7);
                                    vr_sreg.sl_zero             := f_zero_flag(vslv8_data);
                                    vr_sreg.sl_half_carry       :=     ( (NOT islv8_dest_data(3)) AND islv8_imm_data(3) ) 
                                                                    OR (      islv8_imm_data(3)  AND vslv8_data(3)      )
                                                                    OR ( (NOT islv8_dest_data(3)) AND vslv8_data(3)      );
                                    vr_sreg.sl_carry            :=     ( (NOT islv8_dest_data(7)) AND islv8_imm_data(7) ) 
                                                                    OR (      islv8_imm_data(7)  AND vslv8_data(7)      )
                                                                    OR ( (NOT islv8_dest_data(7)) AND vslv8_data(7)      );
                                    vr_sreg.sl_overflow         :=     (      islv8_dest_data(7)  AND (NOT islv8_imm_data(7)) AND (NOT vslv8_data(7)) )
                                                                    OR ( (NOT islv8_dest_data(7)) AND      islv8_imm_data(7)  AND      vslv8_data(7)  );
                                    vr_sreg.sl_sign             := vr_sreg.sl_overflow XOR vslv8_data(7);
                                    osl_data_valid              <= '0';
                                                                          
                WHEN cSUBI_OP    =>  --  Subtract Immediate without Carry
                                    vslv8_data                  := STD_LOGIC_VECTOR( SIGNED(islv8_dest_data) - SIGNED(islv8_imm_data));
                                    vr_sreg.sl_negative         := vslv8_data(7);
                                    vr_sreg.sl_zero             := f_zero_flag(vslv8_data);
                                    vr_sreg.sl_half_carry       :=     ( (NOT islv8_dest_data(3)) AND islv8_imm_data(3) ) 
                                                                    OR (      islv8_imm_data(3)  AND vslv8_data(3)      )
                                                                    OR ( (NOT islv8_dest_data(3)) AND vslv8_data(3)      );
                                    vr_sreg.sl_carry            :=     ( (NOT islv8_dest_data(7)) AND islv8_imm_data(7) ) 
                                                                    OR ( islv8_imm_data(7)       AND vslv8_data(7)      )
                                                                    OR ( (NOT islv8_dest_data(7)) AND vslv8_data(7)      );
                                    vr_sreg.sl_overflow         :=     (      islv8_dest_data(7)  AND (NOT islv8_imm_data(7)) AND (NOT vslv8_data(7)) )
                                                                    OR ( (NOT islv8_dest_data(7)) AND      islv8_imm_data(7)  AND      vslv8_data(7)  );
                                    vr_sreg.sl_sign             := vr_sreg.sl_overflow XOR vslv8_data(7);
                                     
                WHEN cSBCI_OP   =>  --  Subtract Immediate with Carry
                                    IF ir_sreg.sl_carry = '0' THEN
                                        vslv8_data              := STD_LOGIC_VECTOR( SIGNED(islv8_dest_data) - SIGNED(islv8_imm_data));
                                    ELSE
                                        vslv8_data              := STD_LOGIC_VECTOR( SIGNED(islv8_dest_data) - SIGNED(islv8_imm_data) - 1);
                                    END IF;
                                    vr_sreg.sl_negative         := vslv8_data(7);
                                    vr_sreg.sl_zero             := f_zero_flag(vslv8_data) AND ir_sreg.sl_zero;
                                    vr_sreg.sl_half_carry       :=     ( (NOT islv8_dest_data(3)) AND islv8_imm_data(3) ) 
                                                                    OR (      islv8_imm_data(3)  AND vslv8_data(3)      )
                                                                    OR ( (NOT islv8_dest_data(3)) AND vslv8_data(3)      );
                                    vr_sreg.sl_carry            :=     ( (NOT islv8_dest_data(7)) AND islv8_imm_data(7) ) 
                                                                    OR ( islv8_imm_data(7)       AND vslv8_data(7)      )
                                                                    OR ( (NOT islv8_dest_data(7)) AND vslv8_data(7)      );
                                    vr_sreg.sl_overflow         :=     (      islv8_dest_data(7)  AND (NOT islv8_imm_data(7)) AND (NOT vslv8_data(7)) )
                                                                    OR ( (NOT islv8_dest_data(7)) AND      islv8_imm_data(7)  AND      vslv8_data(7)  );
                                    vr_sreg.sl_sign             := vr_sreg.sl_overflow XOR vslv8_data(7);
                                                         
                                                
                WHEN OTHERS     =>   REPORT "Unsuported ALU Immediate Operation" SEVERITY WARNING;
            END CASE;
            
            oslv8_data                      <= vslv8_data;  
            or_sreg_update                  <= vr_sreg;
                
        ELSE
            oslv8_data                      <= (OTHERS => '0');
            osl_data_valid                  <= '0';
            or_sreg_update.sl_enable        <= '0';         --  No SREG update by default
        END IF;
        
    END PROCESS comb_proc;

END ARCHITECTURE rtl;
