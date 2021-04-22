--------------------------------------------------------------------------------
-- Company:
-- Engineer: Simone Ruffini [simone.ruffini@tutanota.com]
--
-- Create Date: 2021-04-07T14:40:06UTC
-- Design Name:
-- Module Name: ctrlsm_tb- Behavioral
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
--------------------------------------------------------------------------------

----------------------------- PACKAGES/LIBRARIES -------------------------------

library IEEE;
  use IEEE.STD_LOGIC_1164.ALL;
  use IEEE.NUMERIC_STD.ALL;   -- arithmetic functions with Signed or Unsigned values
  use IEEE.math_real.all; -- for uniform random distribution

-- User libraries

library work;
  use work.JPEG_PKG.ALL;

entity CTRLSM_TB is
  --  Port ( );
end entity CTRLSM_TB;

----------------------------- ARCHITECTURE -------------------------------------

architecture BEHAVIORAL of CTRLSM_TB is

  --########################### SHARED OBJECTS #################################
  -- this variable must be shared to compute random numbers across multiple calls of rand_int
  shared variable seed1, seed2     : integer := 999;

  --########################### FUNCIONS #######################################

  impure function rand_int (min_val, max_val : integer) return integer is
    variable r : real;
  begin
    uniform(seed1, seed2, r);
    --return integer(round(r * real(max_val - min_val + 1) + real(min_val) - 0.5));
    return integer(round(r * real(max_val - min_val + 1) + real(min_val)));
  end function;

  --########################### CONSTANTS ######################################
  constant C_MASTER_CLK_SPEED_HZ   : natural := 1000000; -- Master clk speed in HZ.
  constant C_MASTER_CLK_PERIOD_NS  : time := (1.0 / real(C_MASTER_CLK_SPEED_HZ)) * 1 sec;
  constant C_NUM_OF_STAGES         : natural := 7;       -- jfif stage too

  --########################### SIGNALS ########################################
  signal master_clk                : std_logic;
  signal master_rst                : std_logic;
  signal bus_msync_s               : std_logic;          --master sync signal
  signal bus_rx_s                  : std_logic_vector(C_BUS_BIT_WIDTH - 1 downto 0);
  signal bus_tx_s                  : std_logic_vector(C_BUS_BIT_WIDTH - 1 downto 0);
  signal bus_ssync_s               : std_logic;          --slave sync signal

  signal outif_almost_full_s       : std_logic;

  signal sof_s                     : std_logic;
  signal img_size_x_s              : std_logic_vector(15 downto 0);
  signal img_size_y_s              : std_logic_vector(15 downto 0);
  signal jpeg_ready_s              : std_logic;
  signal jpeg_busy_s               : std_logic;

  signal fdct_start_s              : std_logic;
  signal fdct_ready_s              : std_logic;
  signal fdct_sm_settings_s        : t_sm_settings;

  signal zig_start_s               : std_logic;
  signal zig_ready_s               : std_logic;
  signal zig_sm_settings_s         : t_sm_settings;

  signal qua_start_s               : std_logic;
  signal qua_ready_s               : std_logic;
  signal qua_sm_settings_s         : t_sm_settings;

  signal rle_start_s               : std_logic;
  signal rle_ready_s               : std_logic;
  signal rle_sm_settings_s         : t_sm_settings;

  signal huf_start_s               : std_logic;
  signal huf_ready_s               : std_logic;
  signal huf_sm_settings_s         : t_sm_settings;

  signal bs_start_s                : std_logic;
  signal bs_ready_s                : std_logic;
  signal bs_sm_settings_s          : t_sm_settings;

  signal jfif_start_s              : std_logic;
  signal jfif_ready_s              : std_logic;
  signal jfif_eoi_s                : std_logic;

  signal out_mux_ctrl_s            : std_logic;

  signal ready_bit_set             : std_logic_vector(C_NUM_OF_STAGES  - 1 downto 0);

begin

  --######################### ENTITY DEFINITION ################################

  CLK_GEN_INSTANCE : entity work.clkgen
    generic map (
      CLK_HZ => C_MASTER_CLK_SPEED_HZ
    )
    port map (
      CLK => master_clk,
      RST => master_rst
    );

  CTRLSM_INSTANCE : entity work.ctrlsm
    port map (
      CLK => master_clk,
      RST => master_rst,
      -- Intermittent Enhancment Ports
      BUS_MSYNC => bus_msync_s,
      BUS_RX    => bus_rx_s,
      BUS_TX    => bus_tx_s,
      BUS_SSYNC => bus_ssync_s,
      -- output IF
      OUTIF_ALMOST_FULL => outif_almost_full_s,
      -- HOST IF
      SOF        => sof_s,
      IMG_SIZE_X => img_size_x_s,
      IMG_SIZE_Y => img_size_y_s,
      JPEG_READY => jpeg_ready_s,
      JPEG_BUSY  => jpeg_busy_s,
      -- JFIF GEN
      JFIF_START => jfif_start_s,
      JFIF_READY => jfif_ready_s,
      JFIF_EOI   => jfif_eoi_s,
      -- FDCT
      FDCT_START       => fdct_start_s,
      FDCT_READY       => fdct_ready_s,
      FDCT_SM_SETTINGS => fdct_sm_settings_s,
      -- ZIGZAG
      ZIG_START       => zig_start_s,
      ZIG_READY       => zig_ready_s,
      ZIG_SM_SETTINGS => zig_sm_settings_s,
      -- QUANTIZER
      QUA_START       => qua_start_s,
      QUA_READY       => qua_ready_s,
      QUA_SM_SETTINGS => qua_sm_settings_s,
      -- RLE
      RLE_START       => rle_start_s,
      RLE_READY       => rle_ready_s,
      RLE_SM_SETTINGS => rle_sm_settings_s,
      -- HUFFMAN
      HUF_START       => huf_start_s,
      HUF_READY       => huf_ready_s,
      HUF_SM_SETTINGS => huf_sm_settings_s,
      -- BYTE STUFFER
      BS_START       => bs_start_s,
      BS_READY       => bs_ready_s,
      BS_SM_SETTINGS => bs_sm_settings_s,
      -- OUT MUX
      OUT_MUX_CTRL => out_mux_ctrl_s

    );

  --######################## PROCESSES #########################################

  jfif_ready_s <= ready_bit_set(0);
  fdct_ready_s <= ready_bit_set(1);
  zig_ready_s  <= ready_bit_set(2);
  qua_ready_s  <= ready_bit_set(3);
  rle_ready_s  <= ready_bit_set(4);
  huf_ready_s  <= ready_bit_set(5);
  bs_ready_s   <= ready_bit_set(6);

  outif_almost_full_s <= '0';

  READY_GENERATOR : for i in 0 to C_NUM_OF_STAGES - 1 generate

    READY_P : process (master_clk, master_rst) is

      variable counter       : integer;
      constant C_COUNTER_END : integer := rand_int(5, 21);

    begin

      if (master_rst= '1') then
        counter := 0;
        ready_bit_set(i) <= '1';
      elsif (master_clk'event and master_clk = '1') then
        if (counter = C_COUNTER_END) then
          counter := 0;
          ready_bit_set(i) <= '1';
        else
          ready_bit_set(i) <= '0';
          counter := counter + 1;
        end if;
      end if;

    end process READY_P;

  end generate READY_GENERATOR;

  TESTBENCH : process is

  begin

    sof_s        <= '0';
    img_size_x_s <= std_logic_vector(to_unsigned(32, img_size_x_s'length));
    img_size_y_s <= std_logic_vector(to_unsigned(32, img_size_y_s'length));
    bus_msync_s  <= '0';
    bus_rx_s     <= (others => '0');
    wait for 10 *  C_MASTER_CLK_PERIOD_NS;
    sof_s        <= '1';
    wait for 50 *  C_MASTER_CLK_PERIOD_NS;
    bus_msync_s  <= '1';                                 -- state restore
    bus_rx_s     <= (others => '0');
    sof_s        <= '0';
    wait for 1 *  C_MASTER_CLK_PERIOD_NS;
    bus_rx_s     <= x"00000004";                         -- main state
    wait for 1 *  C_MASTER_CLK_PERIOD_NS;
    bus_rx_s     <= x"00030005";                         -- rsm_xy_cnt
    wait for 1 *  C_MASTER_CLK_PERIOD_NS;
    bus_rx_s     <= x"100F0002";                         -- reg_xy_cnt 1
    wait for 1 *  C_MASTER_CLK_PERIOD_NS;
    bus_rx_s     <= x"00070104";                         -- reg_xy_cnt 2
    wait for 1 *  C_MASTER_CLK_PERIOD_NS;
    bus_rx_s     <= x"001A0004";                         -- reg_xy_cnt 3
    wait for 1 *  C_MASTER_CLK_PERIOD_NS;
    bus_rx_s     <= x"00000001";                         -- reg_xy_cnt 4
    wait for 1 *  C_MASTER_CLK_PERIOD_NS;
    bus_rx_s     <= x"00030013";                         -- reg_xy_cnt 5
    wait for 1 *  C_MASTER_CLK_PERIOD_NS;
    bus_rx_s     <= x"00030006";                         -- reg_xy_cnt 6
    wait for 1 *  C_MASTER_CLK_PERIOD_NS;
    bus_rx_s     <= x"00000006";                         -- rsm_cmp_indx
    wait for 1 *  C_MASTER_CLK_PERIOD_NS;
    bus_rx_s     <= b"00000000000000100111001010000101"; -- reg_cmp_indx
    sof_s        <= '1';
    wait for 1 *  C_MASTER_CLK_PERIOD_NS;
    bus_rx_s     <= (others => '0');                     -- reg_cmp_indx
    bus_msync_s  <= '0';
    wait for 100 *  C_MASTER_CLK_PERIOD_NS;
    bus_msync_s  <= '1';
    wait for 5 *  C_MASTER_CLK_PERIOD_NS;
    bus_msync_s  <= '0';
    --wait for 100 *  C_MASTER_CLK_PERIOD_NS;
    --sof_s        <= '0';
    wait for 200 *  C_MASTER_CLK_PERIOD_NS;
    bus_msync_s <= '1';
    wait for 2 * C_MASTER_CLK_PERIOD_NS;
    bus_msync_s <= '0';
    wait;

  end process TESTBENCH;

end architecture BEHAVIORAL;
