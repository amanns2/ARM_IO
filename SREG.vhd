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

--  SREG
--  ====================
--  Based on the 2014 Atmel document Atmel-0856J-AVR-Instruction-Set-Manual_07/2014

--  Supported commands:
--
--  

--  Input:  SREG update information from different sources
--          Asynchronous Reset and Clock
--          
--  Output  SREG bits
--

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE work.avr_records_pkg.ALL;

PACKAGE sreg_pkg IS
    COMPONENT sreg IS
        PORT (
            isl_reset           : IN  STD_LOGIC;
            isl_clock           : IN  STD_LOGIC;
            ir_sreg_update     	: IN  t_sreg_bits;
            or_sreg             : OUT t_sreg_bits
        );
    END COMPONENT sreg;
END PACKAGE sreg_pkg;

--------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE work.avr_records_pkg.ALL;

ENTITY sreg IS
        PORT (
            isl_reset           : IN  STD_LOGIC;
            isl_clock           : IN  STD_LOGIC;
            ir_sreg_update      : IN  t_sreg_bits;
            or_sreg             : OUT t_sreg_bits
        );
END ENTITY sreg;

--------------------------------------------------------------------------------

ARCHITECTURE rtl OF sreg IS

    SIGNAL r, r_next            : t_sreg_bits;
    
BEGIN

    comb_proc : PROCESS (r, isl_reset, ir_sreg_update)
        VARIABLE v              : t_sreg_bits;    
    BEGIN
        v := r;
        IF ir_sreg_update.sl_enable = '1' THEN 
            v := ir_sreg_update;
        END IF;            

        IF isl_reset = '1' THEN
            v   := ('0','0','0','0','0','0','0','0','0');
        END IF; -- End Reset        
        
        r_next <= v;
    END PROCESS comb_proc;

    reg_proc : PROCESS (isl_clock) IS
    BEGIN
        IF rising_edge(isl_clock) THEN r <= r_next; END IF;
    END PROCESS reg_proc;
    
    or_sreg                  <= r;  --  Output assignments

END ARCHITECTURE rtl;