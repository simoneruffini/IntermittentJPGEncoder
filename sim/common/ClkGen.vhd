----------------------------------------------------------------------------------
-- Company: 
-- Engineer: Simone Ruffini [simone.ruffini@tutanota.com]
-- 
-- Create Date: 03/16/2021 02:49:33 PM
-- Design Name: 
-- Module Name: buf_fifo_tb - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------

----------------------------- PACKAGES/LIBRARIES ------------------------------
library IEEE;
  use IEEE.STD_LOGIC_1164.ALL;
--  use IEEE.NUMERIC_STD.ALL;   -- arithmetic functions with Signed or Unsigned values


----------------------------- ENTITY ------------------------------------------
entity ClkGen is
  generic (
    CLK_HZ        : integer := 100000000
  );
	port (
	     CLK        : out  std_logic;
	     RST        : out  std_logic
	);
end entity ClkGen;


----------------------------- ARCHITECTURE ------------------------------------
architecture Behavioral of ClkGen is


	constant CLOCK_PERIOD_NS : time := (1e9/CLK_HZ) * 1 ns;

	signal clk_s : std_logic := '0'; 
	signal rst_s : std_logic := '0'; 
	

begin

  --------------------------- PROCESSES --------------------------------------

  -- Clock generator (50% duty cycle)
	CLK_GEN: process
	begin
		clk_s <= '0';
		wait for CLOCK_PERIOD_NS/2;
		clk_s <= '1';
		wait for CLOCK_PERIOD_NS/2;
	end process clk_gen;
  
	CLK <= clk_s;
  

	RESET_GEN: process
	begin
		wait until rising_edge(clk_s);
		rst_s <= '0';
		wait until rising_edge(clk_s);
		rst_s <= '1';
		wait until rising_edge(clk_s);
		rst_s <= '0';
		wait;
	end process reset_gen;

	RST <= rst_s;


end architecture Behavioral;
