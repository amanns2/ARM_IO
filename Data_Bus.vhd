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

PACKAGE data_bus_pkg IS
    COMPONENT data_bus IS
        PORT (
            ir_data_from_reg_block  : IN  t_data_bus;
            ir_data_from_alu        : IN  t_data_bus;
            ir_data_from_sram       : IN  t_data_bus;
            or_data_bus             : OUT t_data_bus
        );
    END COMPONENT data_bus;
END PACKAGE data_bus_pkg;

--------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE work.avr_records_pkg.ALL;

ENTITY data_bus IS
        PORT (
            ir_data_from_reg_block  : IN  t_data_bus;
            ir_data_from_alu        : IN  t_data_bus;
            ir_data_from_sram       : IN  t_data_bus;
            or_data_bus             : OUT t_data_bus
        );
END ENTITY data_bus;

--------------------------------------------------------------------------------

ARCHITECTURE rtl OF data_bus IS


BEGIN

    comb_proc : PROCESS (ir_data_from_reg_block, ir_data_from_alu, ir_data_from_sram)
    BEGIN
        IF    ir_data_from_reg_block.sl_valid = '1' THEN
                    or_data_bus  <= ir_data_from_reg_block;
        ELSIF ir_data_from_alu.sl_valid = '1' THEN
                    or_data_bus  <= ir_data_from_alu;
        ELSIF ir_data_from_sram.sl_valid = '1' THEN
                    or_data_bus  <= ir_data_from_sram;
        ELSE
                    or_data_bus.sl_valid   <= 'Z';                -- Mark data bus as high-impedance if not driven!
                    or_data_bus.slv8_data  <= (OTHERS => 'Z');    -- Mark data bus as high-impedance if not driven!
        END IF;
    END PROCESS comb_proc;


END ARCHITECTURE rtl;
