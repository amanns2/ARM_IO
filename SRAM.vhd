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

PACKAGE sram_pkg IS
    COMPONENT sram IS
        PORT (
            isl_clock               : IN  STD_LOGIC;
            isl_reset               : IN  STD_LOGIC;
            ir_sram_control         : IN  t_sram_control;
            ir_data_bus             : IN  t_data_bus;
            or_data_bus             : OUT t_data_bus
        );
    END COMPONENT sram;
END PACKAGE sram_pkg;

--------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE work.avr_records_pkg.ALL;
USE work.sram_pkg.ALL;

ENTITY sram IS
        PORT (
            isl_clock               : IN  STD_LOGIC;
            isl_reset               : IN  STD_LOGIC;
            ir_sram_control         : IN  t_sram_control;
            ir_data_bus             : IN  t_data_bus;
            or_data_bus             : OUT t_data_bus
        );
END ENTITY sram;

--------------------------------------------------------------------------------

ARCHITECTURE rtl OF sram IS

    TYPE ta_2k_sram IS ARRAY (0 TO c_RAM_SIZE - 1) OF STD_LOGIC_VECTOR(7 DOWNTO 0);

    TYPE t_registers IS RECORD
        a_sram                      : ta_2k_sram;
        slv8_sram_data              : STD_LOGIC_VECTOR(7 DOWNTO 0);
    END RECORD t_registers;

    SIGNAL r, r_next                : t_registers;
    
BEGIN

    comb_proc : PROCESS (r, isl_reset, ir_sram_control, ir_data_bus)
        VARIABLE v                  : t_registers;
        VARIABLE vi_sram_addr       : INTEGER;
    BEGIN
        v := r;
        vi_sram_addr                := TO_INTEGER( UNSIGNED(ir_sram_control.slv11_sram_addr) );
    
        --  Write Action        
        IF ir_sram_control.sl_write_enable = '1' THEN
            v.a_sram(vi_sram_addr) := ir_data_bus.slv8_data;
        END IF;
        
        --  Read Access
        v.slv8_sram_data            := r.a_sram(vi_sram_addr);
        
        --  Reset
        IF isl_reset = '1' THEN
            FOR i IN 0 TO c_RAM_SIZE - 1 LOOP
                v.a_sram(i) := (OTHERS => '0');
            END LOOP;
        END IF;
    
        r_next <= v;
    END PROCESS comb_proc;
    
    
    
    reg_proc : PROCESS (isl_clock)
    BEGIN
        IF rising_edge(isl_clock) THEN
            r <= r_next;
        END IF;
    END PROCESS reg_proc;
    
    

    --  Output Assignment
    or_data_bus.sl_valid        <= ir_sram_control.sl_read_enable;
    or_data_bus.slv8_data       <= r_next.slv8_sram_data;

END ARCHITECTURE rtl;
