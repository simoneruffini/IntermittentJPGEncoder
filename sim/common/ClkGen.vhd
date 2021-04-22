----------------------------------------------------------------------------------
-- Company:
-- Engineer: Simone Ruffini [simone.ruffini@tutanota.com]
--
-- Create Date: 03/16/2021 02:49:33 PM
-- Design Name: ClkGen
-- Module Name: ClkGen.vhd- Behavioral
-- Project Name: Intermittent JPEG Encoder
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

----------------------------- PACKAGES/LIBRARIES -------------------------------

library IEEE;
  use IEEE.std_logic_1164.all;
  --  use IEEE.NUMERIC_STD.ALL;   -- arithmetic functions with Signed or Unsigned values

----------------------------- ENTITY -------------------------------------------

entity ClkGen is
  generic (
    CLK_HZ        : integer := 100000000
  );
  port (
    CLK        : out   std_logic;
    RST        : out   std_logic
  );
end entity ClkGen;

----------------------------- ARCHITECTURE -------------------------------------

architecture BEHAVIORAL of ClkGen is

  --########################### CONSTANTS ######################################
  constant C_CLOCK_PERIOD_NS : time := (1e9 / CLK_HZ) * 1 ns;

  --########################### SIGNALS ########################################
  signal clk_s               : std_logic := '0';
  signal rst_s               : std_logic := '0';

  --########################### ARCHITECTURE BEGIN #############################
begin

  --########################## PROCESSES #######################################

  -- Clock generator (50% duty cycle)
  P_CLK_GEN : process is
  begin

    clk_s <= '0';
    wait for C_CLOCK_PERIOD_NS / 2;
    clk_s <= '1';
    wait for C_CLOCK_PERIOD_NS / 2;

  end process P_CLK_GEN;

  CLK <= clk_s;

  P_RESET_GEN : process is
  begin

    wait until rising_edge(clk_s);
    rst_s <= '0';
    wait until rising_edge(clk_s);
    rst_s <= '1';
    wait until rising_edge(clk_s);
    rst_s <= '0';
    wait;

  end process P_RESET_GEN;

  RST <= rst_s;

end architecture BEHAVIORAL;
