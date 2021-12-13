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
USE work.alu_pkg.ALL;
USE work.avr_records_pkg.ALL;

PACKAGE register_block_pkg IS
    COMPONENT register_block IS
        PORT (
            isl_reset               : IN  STD_LOGIC;
            isl_clock               : IN  STD_LOGIC;
            ir_register_control     : IN  t_register_control;
            or_src_and_dest         : OUT t_src_and_dest;
            ir_data_bus             : IN  t_data_bus;
            or_data_bus             : OUT t_data_bus;
            -- Backdoor for checking
            oa_reg_block_check      : OUT ta_register_block
        );
    END COMPONENT register_block;
END PACKAGE register_block_pkg;

--------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE work.avr_records_pkg.ALL;

ENTITY register_block IS
        PORT (
            isl_reset               : IN  STD_LOGIC;
            isl_clock               : IN  STD_LOGIC;
            ir_register_control     : IN  t_register_control;
            or_src_and_dest         : OUT t_src_and_dest;
            ir_data_bus             : IN  t_data_bus;
            or_data_bus             : OUT t_data_bus;
            -- Backdoor for checking
            oa_reg_block_check      : OUT ta_register_block
        );
END ENTITY register_block;

--------------------------------------------------------------------------------

ARCHITECTURE rtl OF register_block IS

    TYPE t_registers IS RECORD
        a_register_block              : ta_register_block;
    END RECORD;

    SIGNAL r, r_next                  : t_registers;

BEGIN

    comb_proc : PROCESS (r, isl_reset, ir_register_control, ir_data_bus)
        VARIABLE v                    : t_registers;
        VARIABLE vi_dest_reg_addr     : INTEGER;
        VARIABLE vi_src_reg_addr      : INTEGER;
    BEGIN
        v := r;
        vi_dest_reg_addr              := TO_INTEGER(UNSIGNED(ir_register_control.slv5_dest_reg));
        vi_src_reg_addr               := TO_INTEGER(UNSIGNED(ir_register_control.slv5_src_reg));
                                    
        --  Combinatorial output                                        
        or_data_bus.slv8_data         <= r.a_register_block(vi_dest_reg_addr);
        or_data_bus.sl_valid          <= ir_register_control.sl_data_bus_read;
        or_src_and_dest.slv8_dest     <= r.a_register_block(vi_dest_reg_addr);
        or_src_and_dest.slv8_src      <= r.a_register_block(vi_src_reg_addr);
        
        --  Write to Register
        IF ir_register_control.sl_data_bus_write = '1' THEN
            v.a_register_block(vi_dest_reg_addr)    := ir_data_bus.slv8_data;
        END IF;

        --  Synchronous Reset
        IF isl_reset = '1' THEN
            FOR i IN 0 TO 31 LOOP
              v.a_register_block(i)   := (OTHERS => '0');
            END LOOP;
        END IF; -- End Reset                
        
        r_next  <= v;   --  Copy Variable to Signal
    END PROCESS comb_proc;
    
    
    
    reg_proc : PROCESS (isl_clock)
    BEGIN
        IF rising_edge(isl_clock) THEN
            r <= r_next;
        END IF;
    END PROCESS reg_proc;
    
    --  Output Assignments
    oa_reg_block_check   <= r.a_register_block;

END ARCHITECTURE rtl;
