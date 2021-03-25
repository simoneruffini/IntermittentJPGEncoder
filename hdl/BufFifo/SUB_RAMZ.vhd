--------------------------------------------------------------------------------
--                                                                            --
--                          V H D L    F I L E                                --
--                          COPYRIGHT (C) 2006                                --
--                                                                            --
--------------------------------------------------------------------------------
--                                                                            --
-- Title       : SUB_RAMZ                                                     --
-- Design      : EV_JPEG_ENC                                                  --
-- Author      : Michal Krepa                                                 --
--                                                                            --
--------------------------------------------------------------------------------
--
-- File        : SUB_RAMZ.vhd
-- Created     : 22/03/2009
--
--------------------------------------------------------------------------------
--
--  Description : RAM memory simulation model
--
--------------------------------------------------------------------------------

library IEEE;
  use IEEE.STD_LOGIC_1164.all;
  use IEEE.NUMERIC_STD.all;

entity SUB_RAMZ is
  generic (

    RAMADDR_W     : integer := 6;
    RAMDATA_W     : integer := 12
  );
  port (
    D                 : in    std_logic_vector(RAMDATA_W - 1 downto 0);
    WADDR             : in    std_logic_vector(RAMADDR_W - 1 downto 0);
    RADDR             : in    std_logic_vector(RAMADDR_W - 1 downto 0);
    WE                : in    std_logic;
    CLK               : in    std_logic;

    Q                 : out   std_logic_vector(RAMDATA_W - 1 downto 0)
  );
end entity SUB_RAMZ;

architecture RTL of SUB_RAMZ is

  type mem_type is array ((2 ** RAMADDR_W) - 1 downto 0) of std_logic_vector(RAMDATA_W - 1 downto 0);

  signal mem                    : mem_type;
  signal read_addr              : std_logic_vector(RAMADDR_W - 1 downto 0);

  --attribute ram_style: string;
  --attribute ram_style of mem : signal is "distributed";

begin

  Q <= mem(to_integer(unsigned(read_addr)));

  -------------------------------------------------------------------------------
  -- register read address
  -------------------------------------------------------------------------------
  READ_PROC : process (CLK) is
  begin

    if (CLK'event and CLK = '1') then
      read_addr <= RADDR;
    end if;

  end process READ_PROC;

  -------------------------------------------------------------------------------
  --write access
  -------------------------------------------------------------------------------
  WRITE_PROC : process (CLK) is
  begin

    if (CLK'event and CLK = '1') then
      if (WE = '1') then
        mem(to_integer(unsigned(WADDR))) <= D;
      end if;
    end if;

  end process WRITE_PROC;

end architecture RTL;
