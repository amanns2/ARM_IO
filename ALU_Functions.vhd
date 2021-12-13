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

--  Simple:  Return a 1 if all bits in the input are zero

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

PACKAGE ALU_Functions_pkg IS

    --  Function to evaluate zero flag  ----------------------------------------
    FUNCTION f_zero_flag (
        islv8_data              : IN STD_LOGIC_VECTOR(7 DOWNTO 0)
    ) RETURN STD_LOGIC;
    
END PACKAGE ALU_Functions_pkg;

--------------------------------------------------------------------------------

PACKAGE BODY ALU_Functions_pkg IS

    --  Function to evaluate zero flag  ----------------------------------------
    FUNCTION f_zero_flag (islv8_data : IN STD_LOGIC_VECTOR(7 DOWNTO 0))     ----
    RETURN STD_LOGIC IS                                                     ----
        VARIABLE vsl_zero_flag : STD_LOGIC;                                 ----
    BEGIN                                                                   ----
        vsl_zero_flag := '1';                                               ----
        FOR i IN 0 TO 7 LOOP                                                ----
            IF islv8_data(i) = '1' THEN vsl_zero_flag := '0'; END IF;       ----
        END LOOP;                                                           ----
        RETURN STD_LOGIC(vsl_zero_flag);                                    ----
    END FUNCTION f_zero_flag;  -------------------------------------------------

END PACKAGE BODY ALU_Functions_pkg;

