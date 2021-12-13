LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE work.AVR_Records_pkg.ALL;

PACKAGE IO_PORT_pkg IS
    COMPONENT IO_PORT IS
        PORT (
            isl_reset               : IN  STD_LOGIC;
            isl_clock               : IN  STD_LOGIC;
            ir_io_port_control      : IN  t_io_port_control;
            ir_data_bus             : IN  t_data_bus;
            or_data_from_io         : OUT t_data_bus;
            io_slv8_port            : INOUT  STD_LOGIC_VECTOR(7 DOWNTO 0)
        );
    END COMPONENT IO_PORT;
END PACKAGE IO_PORT_pkg;

--------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE work.AVR_Records_pkg.ALL;

USE work.IO_PORT_pkg.ALL;

ENTITY IO_PORT IS
    PORT (
        isl_reset               : IN  STD_LOGIC;
        isl_clock               : IN  STD_LOGIC;
        ir_io_port_control      : IN  t_io_port_control;
        ir_data_bus             : IN  t_data_bus;
        or_data_from_io         : OUT t_data_bus;
        io_slv8_port            : INOUT  STD_LOGIC_VECTOR(7 DOWNTO 0)
    );
END ENTITY IO_PORT;


ARCHITECTURE rtl OF IO_PORT IS

    TYPE t_registers IS RECORD
            slv8_ddr       :  STD_LOGIC_VECTOR(7 DOWNTO 0);
            slv8_port      :  STD_LOGIC_VECTOR(7 DOWNTO 0);
            slv8_port_in_1 :  STD_LOGIC_VECTOR(7 DOWNTO 0);
            slv8_port_in_2 :  STD_LOGIC_VECTOR(7 DOWNTO 0);
    END RECORD t_registers;
    
    SIGNAL r, r_next             : t_registers;
    
BEGIN

    --##    Combinatorial Process
    --##
    --############################
    comb_proc : PROCESS (r, io_slv8_port, ir_io_port_control, ir_data_bus, isl_reset)
    VARIABLE v                  : t_registers;
    BEGIN
        v                       := r;
        
        v.slv8_port_in_1    := io_slv8_port;
        v.slv8_port_in_2    := r.slv8_port_in_1;
        
		IF ir_io_port_control.sl_write_ddr = '1' THEN
            v.slv8_ddr      := ir_data_bus.slv8_data;
		END IF;
		
		IF ir_io_port_control.sl_write_port = '1' THEN
            v.slv8_port      := ir_data_bus.slv8_data;        
        END IF;
        
        IF ir_io_port_control.sl_read_ddr = '1' THEN
            or_data_from_io.slv8_data <= v.slv8_ddr;
            or_data_from_io.sl_valid <= '1';
        ELSIF ir_io_port_control.sl_read_port = '1' THEN
            or_data_from_io.slv8_data <= v.slv8_ddr;
            or_data_from_io.sl_valid <= '1';      
        ELSIF ir_io_port_control.sl_read_pin = '1' THEN
            or_data_from_io.slv8_data <= v.slv8_port_in_2;
            or_data_from_io.sl_valid <= '1';     
        ELSE
            or_data_from_io.slv8_data <= (OTHERS => 'Z');
            or_data_from_io.sl_valid <= '0';
        END IF;
        
        FOR i IN 0 TO 7 LOOP
            IF r.slv8_ddr(i) = '1' THEN
                io_slv8_port(i)         <= r.slv8_port(i);
            ELSE
                io_slv8_port(i)         <= 'Z';
            END IF;
        END LOOP;
        
        IF isl_reset = '1' THEN
            v.slv8_ddr := (OTHERS => '0');
            v.slv8_port := (OTHERS => '0');
            v.slv8_port_in_1 := (OTHERS => '0');
            v.slv8_port_in_2 := (OTHERS => '0');
        END IF;
		
        r_next                  <= v;
    END PROCESS comb_proc;
    

    --##    Registered Process
    --##
    --############################
    reg_proc : PROCESS (isl_clock)
    BEGIN
        IF rising_edge(isl_clock) THEN r <= r_next; END IF;
    END PROCESS reg_proc;
    

    --##    Output Assignments
    --##
    --############################

	
	
END ARCHITECTURE rtl;
