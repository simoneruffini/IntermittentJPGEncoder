--------------------------------------------------------------------------------
-- Company:
-- Engineer:  Michal Krepa
--            Simone Ruffini [simone.ruffini@tutanota.com]
--
-- Create Date:     01/03/2008
-- Design Name:     SingleSM
-- Module Name:     SingleSM.vhd - RTL
-- Project Name:    JPEG_ENC
-- Target Devices:
-- Tool Versions:
-- Description:
--
-- Dependencies:
--
-- Revision:
-- Revision 00 - Michal Krepa
--  * File Created
-- Revision 01 - Simone Ruffini
--  * Refactoring + comments
-- Additional Comments:
--
--------------------------------------------------------------------------------

----------------------------- PACKAGES/LIBRARIES -------------------------------

library IEEE;
  use IEEE.std_logic_1164.all;

----------------------------- ENTITY -------------------------------------------

entity SINGLESM is
  port (
    CLK                : in    std_logic;
    RST                : in    std_logic;
    -- from/to SM(m-1)
    START_I            : in    std_logic;
    IDLE_O             : out   std_logic;
    -- from/to SM(m+1)
    IDLE_I             : in    std_logic;
    START_O            : out   std_logic;
    -- from/to processing block
    PB_RDY_I           : in    std_logic;
    PB_START_O         : out   std_logic;
    -- state debug
    FSM_O              : out   std_logic_vector(1 downto 0)
  );
end entity SINGLESM;

----------------------------- ARCHITECTURE -------------------------------------

architecture RTL of SINGLESM is

  --########################### SIGNALS ##########################################

  -- types
  type t_state is (IDLE, WAIT_FOR_BLK_RDY, WAIT_FOR_BLK_IDLE);

  -- signals
  signal state : t_state;

  --########################### ARCHITECTURE BEGIN ###############################

begin

  --########################### COMBINATORIAL LOGIC ##############################

  FSM_O <= "00" when state = IDLE else
           "01" when state = WAIT_FOR_BLK_RDY else
           "10" when state = WAIT_FOR_BLK_IDLE else
           "11";

  --########################### PROCESSES ########################################

  P_FSM : process (CLK, RST) is
  begin

    if (RST = '1') then
      IDLE_O     <= '0';
      START_O    <= '0';
      PB_START_O <= '0';
      state      <= IDLE;
    elsif (CLK'event and CLK = '1') then
      IDLE_O     <= '0';
      START_O    <= '0';
      PB_START_O <= '0';

      case state is

        when IDLE =>
          IDLE_O <= '1';
          -- this fsm is started
          if (START_I = '1') then
            state <= WAIT_FOR_BLK_RDY;
            -- start processing block associated with this FSM
            PB_START_O <= '1';
            IDLE_O     <= '0';
          end if;

        when WAIT_FOR_BLK_RDY =>
          -- wait until processing block completes
          if (PB_RDY_I = '1') then
            -- wait until next FSM is idle before starting it
            if (IDLE_I = '1') then
              state   <= IDLE;
              START_O <= '1';
            else
              state <= WAIT_FOR_BLK_IDLE;
            end if;
          end if;

        when WAIT_FOR_BLK_IDLE =>
          if (IDLE_I = '1') then
            state   <= IDLE;
            START_O <= '1';
          end if;

        when others =>
          IDLE_O     <= '0';
          START_O    <= '0';
          PB_START_O <= '0';
          state      <= IDLE;

      end case;

    end if;

  end process P_FSM;

end architecture RTL;
