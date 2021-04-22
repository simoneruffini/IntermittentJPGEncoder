--------------------------------------------------------------------------------
--                                                                            --
--                          V H D L    F I L E                                --
--                          COPYRIGHT (C) 2009                                --
--                                                                            --
--------------------------------------------------------------------------------
--
-- Title       : JPEG_PKG
-- Design      : JPEG_ENC
-- Author      : Michal Krepa
--
--------------------------------------------------------------------------------
--
-- File        : JPEG_PKG.VHD
-- Created     : Sat Mar 7 2009
--
--------------------------------------------------------------------------------
--
--  Description : Package for JPEG core
--
--------------------------------------------------------------------------------

library IEEE;
  use IEEE.STD_LOGIC_1164.all;
  use ieee.numeric_std.all;
  
package JPEG_PKG is

  -- do not change, constant
  constant C_HDR_SIZE         : integer := 623;
  
  -- warning! this parameter heavily affects memory size required
  -- if expected image width is known change this parameter to match this
  -- otherwise some onchip RAM will be wasted and never used
  constant C_MAX_LINE_WIDTH   : integer := 640;
  
  -- memory/performance tradeoff
  -- 8 extra lines highest performance
  -- 0 extra lines lowest area
  --constant C_EXTRA_LINES  : integer := 0; -- from 0 to 8

  
  -- 24 bit format RGB/YCbCr 888 bits
  -- 16 bit format RGB/YCbCr 565 bits
  constant C_PIXEL_BITS    : integer := 24;
  
  -- 0 = RGB
  -- 1 = YUV/YCbCr
  constant C_YUV_INPUT  : std_logic := '0';

  -- Witdth of the main bus
  constant C_BUS_BIT_WIDTH  : natural := 32;
  
  type T_SM_SETTINGS is record
    -- Count sample in horizontal direction. Incremented ins steps of 16 samples. (16 bits)
    x_cnt               : unsigned(15 downto 0);
    -- Count sample in vertical direction. Incremented ins steps of 8 samples. (16 bits)
    y_cnt               : unsigned(15 downto 0);
    -- Component index. Indicates currently processed color subsampling component (Y1,Y2, Cb, Cr).
    cmp_idx             : unsigned(2 downto 0); 
  end record;
  
  
  function log2(n : natural) return natural;
  
end package JPEG_PKG;

package body JPEG_PKG is

  -----------------------------------------------------------------------------
  function log2(n : natural) 
  return natural is
  begin
    for i in 0 to 31 loop
      if (2**i) >= n then
        return i;
      end if;
    end loop;
    return 32;
  end log2;
  -----------------------------------------------------------------------------

end package body JPEG_PKG;