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

PACKAGE program_counter_pkg IS
    COMPONENT program_counter IS
        PORT (
            isl_reset               : IN  STD_LOGIC;
            isl_clock               : IN  STD_LOGIC;
            ir_pc_control           : IN  t_pc_control;
            oslv16_program_count    : OUT STD_LOGIC_VECTOR(15 DOWNTO 0)
        );
    END COMPONENT program_counter;
END PACKAGE program_counter_pkg;

--------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE work.avr_records_pkg.ALL;

ENTITY program_counter IS
        PORT (
            isl_reset               : IN  STD_LOGIC;
            isl_clock               : IN  STD_LOGIC;
            ir_pc_control           : IN  t_pc_control;
            oslv16_program_count    : OUT STD_LOGIC_VECTOR(15 DOWNTO 0)
        );
END ENTITY program_counter;

--------------------------------------------------------------------------------

ARCHITECTURE sim OF program_counter IS

    TYPE t_registers IS RECORD
        usig16_program_count        : UNSIGNED(15 DOWNTO 0);
    END RECORD t_registers;

    SIGNAL r, r_next                : t_registers;

BEGIN

    --##    Combinatorial Process
    --##
    --###########################
    comb_proc : PROCESS (r, isl_reset, ir_pc_control) 
        VARIABLE v                  : t_registers;    
    BEGIN
        v                           := r;
        
        IF ir_pc_control.sl_set_new_pc = '1' THEN
            v.usig16_program_count  := ir_pc_control.usig16_new_pc_value;
        ELSE
            v.usig16_program_count  := r.usig16_program_count + 1;
        END IF;
        
        
        --  Synchronous Reset
        IF isl_reset = '1' THEN
            v.usig16_program_count  := (OTHERS => '0');
        END IF;
        
        r_next                      <= v;
    END PROCESS comb_proc;
    
    
    --##    Registered Process
    --##
    --###########################
    reg_proc : PROCESS (isl_clock)
    BEGIN
        IF rising_edge(isl_clock) THEN
            r <= r_next;
        END IF;
    END PROCESS reg_proc;
    
 
    --##    Output Assignments
    --##
    --###########################
    oslv16_program_count       <= STD_LOGIC_VECTOR( r.usig16_program_count);
    
END ARCHITECTURE sim;
