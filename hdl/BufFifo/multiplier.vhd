----------------------------------------------------------------------------------
-- Company:
-- Engineer:
--
-- Create Date:    12:18:01 03/12/2011
-- Design Name:
-- Module Name:    multiplier - Behavioral
-- Project Name:
-- Target Devices:
-- Tool versions:
-- Description:
--
-- Dependencies:
--
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
--
----------------------------------------------------------------------------------

library IEEE;
  use IEEE.std_logic_1164.ALL;
  use ieee.numeric_std.all;

entity MULTIPLIER is
  port (
    CLK                : in    std_logic;
    RST                : in    std_logic;
    --
    IMG_SIZE_X         : in    std_logic_vector(15 downto 0);
    IMG_SIZE_Y         : in    std_logic_vector(15 downto 0);

    --
    RESULT             : out   std_logic_vector(31 downto 0);
    THRESHOLD          : out   std_logic_vector(31 downto 0)
  );
end entity MULTIPLIER;

architecture BEHAVIORAL of MULTIPLIER is

  signal prev_x : std_logic_vector(15 downto 0);
  signal prev_y : std_logic_vector(15 downto 0);

begin

  MULT : process (CLK, RST) is
  begin

    if (RST = '1') then
      RESULT    <= (others => '0');
      THRESHOLD <= (others => '0');
      prev_x    <= (others => '0');
      prev_y    <= (others => '0');
    elsif (CLK'event and CLK = '1') then
      if (prev_x /= IMG_SIZE_X or prev_y /= IMG_SIZE_Y) then
        RESULT    <= std_logic_vector(unsigned(IMG_SIZE_X) * unsigned(IMG_SIZE_Y), RESULT'length);
        THRESHOLD <= std_logic_vector(unsigned(IMG_SIZE_X) * 7, THRESHOLD'length);
      end if;

      prev_x <= IMG_SIZE_X;
      prev_y <= IMG_SIZE_Y;
    end if;

  end process MULT;

end architecture BEHAVIORAL;

