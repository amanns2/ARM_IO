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
USE IEEE.NUMERIC_STD.ALL;

PACKAGE avr_records_pkg IS

    CONSTANT c_RAM_SIZE         : INTEGER       := 2048;
    CONSTANT c_SRAM_START       : INTEGER       := 256;                         --  = 0x0100
    CONSTANT c_SRAM_END         : INTEGER       := c_SRAM_START + c_RAM_SIZE;   --  = 0x0900
    
    TYPE t_pc_control IS RECORD
        sl_set_new_pc           : STD_LOGIC;
        usig16_new_pc_value     : UNSIGNED(15 DOWNTO 0);
    END RECORD t_pc_control;      
    
    TYPE t_alu_control IS RECORD
        sl_alu_1_cmd            : STD_LOGIC;
        slv4_alu_1_op_code      : STD_LOGIC_VECTOR(3 DOWNTO 0);
        sl_alu_2_cmd            : STD_LOGIC;
        slv6_alu_2_op_code      : STD_LOGIC_VECTOR(5 DOWNTO 0);
        sl_alu_imm_cmd          : STD_LOGIC;
        slv4_alu_imm_op_code    : STD_LOGIC_VECTOR(3 DOWNTO 0);
        slv8_imm_data           : STD_LOGIC_VECTOR(7 DOWNTO 0);
    END RECORD t_alu_control;

    TYPE t_src_and_dest IS RECORD
        slv8_dest               : STD_LOGIC_VECTOR(7 DOWNTO 0);
        slv8_src                : STD_LOGIC_VECTOR(7 DOWNTO 0);
    END RECORD t_src_and_dest;
    
    TYPE t_data_bus IS RECORD
        slv8_data               : STD_LOGIC_VECTOR(7 DOWNTO 0);
        sl_valid                : STD_LOGIC;
    END RECORD t_data_bus;

    TYPE t_sreg_bits IS RECORD
        sl_enable               : STD_LOGIC;
        sl_interrupt            : STD_LOGIC;
        sl_transfer             : STD_LOGIC;
        sl_half_carry           : STD_LOGIC;
        sl_sign                 : STD_LOGIC;
        sl_overflow             : STD_LOGIC;
        sl_negative             : STD_LOGIC;
        sl_zero                 : STD_LOGIC;
        sl_carry                : STD_LOGIC;
    END RECORD t_sreg_bits;
    
    TYPE t_register_control IS RECORD
        slv5_src_reg            : STD_LOGIC_VECTOR(4 DOWNTO 0);
        slv5_dest_reg           : STD_LOGIC_VECTOR(4 DOWNTO 0);
        sl_data_bus_write       : STD_LOGIC;
        sl_data_bus_read        : STD_LOGIC;
    END RECORD t_register_control;

    TYPE ta_register_block IS ARRAY (0 TO 31) OF STD_LOGIC_VECTOR(7 DOWNTO 0);
    
    TYPE t_sram_control IS RECORD
        slv11_sram_addr         : STD_LOGIC_VECTOR(10 DOWNTO 0);
        sl_write_enable         : STD_LOGIC; 
        sl_read_enable          : STD_LOGIC; 
    END RECORD t_sram_control;
    
    TYPE t_io_port_control IS RECORD
        sl_write_ddr             : STD_LOGIC;
        sl_read_ddr              : STD_LOGIC;
        sl_write_port            : STD_LOGIC;
        sl_read_port             : STD_LOGIC;
        sl_read_pin              : STD_LOGIC;
    END RECORD t_io_port_control;

END PACKAGE avr_records_pkg;