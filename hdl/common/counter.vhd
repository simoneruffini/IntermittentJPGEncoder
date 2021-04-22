----------------------------------------------------------------------------------
-- Company:
-- Engineer: Simone Ruffini [simone.ruffini@tutanota.com]
--
-- Create Date: 05/15/2020 09:30:25 PM
-- Design Name: counter
-- Module Name: counter - Behavioral
-- Project Name: Interminttent Jpeg Encoder
-- Target Devices:
-- Tool Versions:
-- Description:
--
-- Dependencies:
--
-- Revision:
-- Revision 0.01 - Simone Ruffini
-- * File Created
-- Revision 0.02 -Simone Ruffini
-- * Refactocring
-- Additional Comments:
--
----------------------------------------------------------------------------------

----------------------------- PACKAGES/LIBRARIES -------------------------------

library IEEE;
  use IEEE.std_logic_1164.all;

----------------------------- ENTITY -------------------------------------------

entity COUNTER is
  generic (
    MAX         : integer;
    INIT_VALUE  : integer;
    INCREASE_BY : integer
  );
  port (
    CLK         : in    std_logic;
    RST         : in    std_logic;
    INIT        : in    std_logic;
    CE          : in    std_logic;
    TC          : out   std_logic;
    VALUE       : out   integer RANGE 0 to MAX
  );
end entity COUNTER;

----------------------------- ARCHITECTURE -------------------------------------

architecture BEHAVIORAL of COUNTER is

  --########################### SIGNALS ########################################
  signal counter : integer RANGE 0 to MAX := INIT_VALUE;

begin

  --########################### COMBINATORIAL LOGIC ############################
  VALUE <= counter;
  TC    <= '1' when counter = MAX else
           '0';

  --########################## PROCESSES #######################################
  COUNT : process (CLK, RST) is
  begin

    if (RST = '1') then
      counter <= 0;
    elsif (CLK'event and CLK = '1') then
      if (INIT = '1') then
        counter <= INIT_VALUE;
      elsif (CE = '1') then
        if (counter = MAX) then
          counter <= 0;
        else
          counter <= counter + INCREASE_BY;
        end if;
      end if;
    end if;

  end process COUNT;

end architecture BEHAVIORAL;
