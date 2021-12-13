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

PACKAGE clock_and_reset_pkg IS
    COMPONENT clock_and_reset IS
        PORT (
            isl_clk_125m            : IN  STD_LOGIC;
            osl_reset               : OUT STD_LOGIC;
            osl_clock               : OUT STD_LOGIC
        );
    END COMPONENT clock_and_reset;
END PACKAGE clock_and_reset_pkg;

--------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY clock_and_reset IS
        PORT (
            isl_clk_125m            : IN  STD_LOGIC;
            osl_reset               : OUT STD_LOGIC;
            osl_clock               : OUT STD_LOGIC
        );
END ENTITY clock_and_reset;

--------------------------------------------------------------------------------

ARCHITECTURE sim OF clock_and_reset IS

    TYPE t_registers IS RECORD
        usig4_clock_counter         : UNSIGNED(3 DOWNTO 0);
        usig4_reset_delay           : UNSIGNED(3 DOWNTO 0);
        sl_clock                    : STD_LOGIC;
        sl_reset                    : STD_LOGIC;
    END RECORD t_registers;
    
    SIGNAL r_next                   : t_registers;
    SIGNAL r                        : t_registers := (
                                        usig4_clock_counter   => (OTHERS => '0'),
                                        usig4_reset_delay     => (OTHERS => '0'),
                                        sl_clock              => '1',
                                        sl_reset              => '1'
                                    );
                                    
    SIGNAL i_half_clock_cycle_count : INTEGER   := 7;
    SIGNAL i_full_clock_cycle_count : INTEGER   := 14;
BEGIN

    --  Override Clock divider settings for faster simulation
    -- pragma synthesis_off
    i_half_clock_cycle_count    <= 2;
    i_full_clock_cycle_count    <= 4;
    -- pragma synthesis_on


    --##    Combinatorial Process
    --##
    --###########################
    comb_proc : PROCESS (r)
    VARIABLE v                      : t_registers;
    BEGIN
        v := r;
        
        v.usig4_clock_counter       := r.usig4_clock_counter + 1;
        IF    r.usig4_clock_counter =  i_half_clock_cycle_count THEN
            v.sl_clock              := '0';
        ELSIF r.usig4_clock_counter = i_full_clock_cycle_count THEN
            v.sl_clock              := '1';
            v.usig4_clock_counter   := (OTHERS => '0');
            v.usig4_reset_delay     := r.usig4_reset_delay + 1;
        END IF;
        
        IF r.usig4_reset_delay = 4 THEN
            v.sl_reset              := '0';
        END IF;
        
        r_next  <= v;
    END PROCESS comb_proc;
    
    
    --##    Registered Process
    --##
    --###########################
    reg_proc: PROCESS (isl_clk_125m)
    BEGIN
        IF rising_edge(isl_clk_125m) THEN r <= r_next; END IF;
    END PROCESS reg_proc;
    
   
     --##    Output Assignments
     --##
     --###########################
     osl_reset           <= r.sl_reset;
     osl_clock           <= r.sl_clock;
    
END ARCHITECTURE sim;
