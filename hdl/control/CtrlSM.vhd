--------------------------------------------------------------------------------
-- Company:
-- Engineer:  Michal Krepa
--            Simone Ruffini [simone.ruffini@tutanota.com]
--
-- Create Date:     01/03/2009
-- Design Name:     CtrlSM
-- Module Name:     CtrlSM.vhd - RTL
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
  use IEEE.numeric_std.all;

-- User libraries

library work;
  use work.JPEG_PKG.all;

----------------------------- ENTITY -------------------------------------------

entity CTRLSM is
  port (
    CLK                : in    std_logic;
    RST                : in    std_logic;

    -- output IF
    OUTIF_ALMOST_FULL  : in    std_logic;

    -- HOST IF
    SOF                : in    std_logic;
    IMG_SIZE_X         : in    std_logic_vector(15 downto 0);
    IMG_SIZE_Y         : in    std_logic_vector(15 downto 0);
    JPEG_READY         : out   std_logic;
    JPEG_BUSY          : out   std_logic;

    -- FDCT
    FDCT_START         : out   std_logic;
    FDCT_READY         : in    std_logic;
    FDCT_SM_SETTINGS   : out   T_SM_SETTINGS;

    -- ZIGZAG
    ZIG_START          : out   std_logic;
    ZIG_READY          : in    std_logic;
    ZIG_SM_SETTINGS    : out   T_SM_SETTINGS;

    -- Quantizer
    QUA_START          : out   std_logic;
    QUA_READY          : in    std_logic;
    QUA_SM_SETTINGS    : out   T_SM_SETTINGS;

    -- RLE
    RLE_START          : out   std_logic;
    RLE_READY          : in    std_logic;
    RLE_SM_SETTINGS    : out   T_SM_SETTINGS;

    -- Huffman
    HUF_START          : out   std_logic;
    HUF_READY          : in    std_logic;
    HUF_SM_SETTINGS    : out   T_SM_SETTINGS;

    -- ByteStuffdr
    BS_START           : out   std_logic;
    BS_READY           : in    std_logic;
    BS_SM_SETTINGS     : out   T_SM_SETTINGS;

    -- JFIF GEN
    JFIF_START         : out   std_logic;
    JFIF_READY         : in    std_logic;
    JFIF_EOI           : out   std_logic;

    -- OUT MUX
    OUT_MUX_CTRL       : out   std_logic
  );
end entity CTRLSM;

----------------------------- ARCHITECTURE -------------------------------------

architecture RTL of CTRLSM is

  --########################### SIGNALS ########################################

  -- constants
  constant C_NUM_STAGES  : integer := 6;
  constant C_CMP_MAX     : std_logic_vector(2 downto 0) := "100";
  constant C_SM_SETTINGS : t_sm_settings :=
  (
    (others => '0'),
    (others => '0'),
    (others => '0')
  );

  -- types

  type t_state is (
    -- JPEG encoder is inactive waiting to be started by Host
    IDLES,
    -- Generation of JFIF header.
    JFIF,
    -- Encoding process blocks of 16x8 samples until horizontal counter reaches width of image.
    HORIZ,
    -- Component processing. Process one 16x8 block from each component in interleaved fashion including subsampling
    COMP,
    -- Reset horizontal counter and advance vertical counter by 8. Check if end of image reached.
    VERT,
    -- Generation of EOI marker (End Of Image)
    EOI
  );

  type fsm_state_arr_t is array(C_NUM_STAGES downto 1) of std_logic_vector(1 downto 0);

  type t_arr_sm_settings is array(C_NUM_STAGES + 1 downto 1) of t_sm_settings;

  -- signals
  signal main_state      : t_state;

  signal idle            : std_logic_vector(C_NUM_STAGES + 1 downto 1);
  signal start           : std_logic_vector(C_NUM_STAGES + 1 downto 1);
  signal start_pb        : std_logic_vector(C_NUM_STAGES downto 1);
  signal ready_pb        : std_logic_vector(C_NUM_STAGES downto 1);

  signal rsm             : t_sm_settings;
  signal reg             : t_arr_sm_settings;

  signal start1_d        : std_logic;
  signal out_mux_ctrl_s  : std_logic;
  signal out_mux_ctrl_s2 : std_logic;

  signal fsm_state       : fsm_state_arr_t;

  --########################### ARCHITECTURE BEGIN #############################

begin

  --########################### ENTITY DEFINITION ##############################

  -- CTRLSSM 1..C_NUM_STAGES
  ------------------------------------------------------------------------------
  --           | SSM1  |      | SSM2  |      | SSM3  |      [..]  | SSN6  |
  -- start(1)->|START_I|  +-->|START_I|  +-->|START_I|  +-->[..]->|START_I|
  --  idle(1)<-|IDLE_O |  | +-|IDLE_O |  | +-|IDLE_O |  | +-[..]+-|IDLE_O |
  --           |IDLE_I |<-|-+ |IDLE_I |<-|-+ |IDLE_I |<-|-+ [..]  |IDLE_I |<- not OUTIF_ALMOST_FULL
  --           |START_O|--+   |START_O|--+   |START_O|--+   [..]  |START_O|-> start(7) (not used)
  ------------------------------------------------------------------------------

  GEN_CTRL_SSM : for i in 1 to C_NUM_STAGES generate

    U_S_CTRL_SSM : entity work.singlesm
      port map (

        CLK => clk,
        RST => rst,
        -- from/to SM(m-1)
        START_I => start(i),
        IDLE_O  => idle(i),
        -- from/to SM(m+1)
        IDLE_I  => idle(i + 1),
        START_O => start(i + 1),
        -- from/to processing block
        PB_RDY_I   => ready_pb(i),
        PB_START_O => start_pb(i),
        -- state out
        FSM_O => fsm_state(i)
      );

  end generate GEN_CTRL_SSM;

  --########################### COMBINATORIAL LOGIC ############################
  idle(C_NUM_STAGES + 1) <= not OUTIF_ALMOST_FULL;

  FDCT_SM_SETTINGS <= reg(1);
  ZIG_SM_SETTINGS  <= reg(2);
  QUA_SM_SETTINGS  <= reg(3);
  RLE_SM_SETTINGS  <= reg(4);
  HUF_SM_SETTINGS  <= reg(5);
  BS_SM_SETTINGS   <= reg(6);

  -- SingleStateMachine 1 controls FDCT
  FDCT_START  <= start_pb(1);
  ready_pb(1) <= FDCT_READY;

  -- SingleStateMachine 2 controls ZigZag
  ZIG_START   <= start_pb(2);
  ready_pb(2) <= ZIG_READY;

  -- SingleStateMachine 3 controls Quantizer
  QUA_START   <= start_pb(3);
  ready_pb(3) <= QUA_READY;

  -- SingleStateMachine 4 controls Run-Length Encoding
  RLE_START   <= start_pb(4);
  ready_pb(4) <= RLE_READY;

  -- SingleStateMachine 5 controls Huffman Encoding
  HUF_START   <= start_pb(5);
  ready_pb(5) <= HUF_READY;

  -- SingleStateMachine 6 controls Byte Stuffer
  BS_START    <= start_pb(6);
  ready_pb(6) <= BS_READY;

  --########################## PROCESSES #######################################

  GEN_REG_SM : for i in 1 to C_NUM_STAGES generate

    P_REG1 : process (CLK, RST) is
    begin

      if (RST = '1') then
        reg(i) <= C_SM_SETTINGS; -- iniitialize to 0
      elsif (CLK'event and CLK = '1') then
        if (start(i) = '1') then
          if (i = 1) then
            reg(i).x_cnt   <= rsm.x_cnt;
            reg(i).y_cnt   <= rsm.y_cnt;
            reg(i).cmp_idx <= rsm.cmp_idx;
          else
            reg(i) <= reg(i - 1);
          end if;
        end if;
      end if;

    end process P_REG1;

  end generate GEN_REG_SM;

  P_MAIN_SM : process (CLK, RST) is
  begin

    if (rst = '1') then
      main_state <= IDLES;

      JPEG_READY   <= '0';
      JPEG_BUSY    <= '0';
      JFIF_EOI     <= '0';
      OUT_MUX_CTRL <= '0';
      JFIF_START   <= '0';

      rsm.x_cnt   <= (others => '0');
      rsm.y_cnt   <= (others => '0');
      rsm.cmp_idx <= (others => '0');

      start(1)        <= '0';
      start1_d        <= '0';
      out_mux_ctrl_s  <= '0';
      out_mux_ctrl_s2 <= '0';
    elsif (CLK'event and CLK = '1') then
      start(1)        <= '0';
      start1_d        <= start(1);
      JPEG_READY      <= '0';
      JFIF_START      <= '0';
      out_mux_ctrl_s2 <= out_mux_ctrl_s;
      OUT_MUX_CTRL    <= out_mux_ctrl_s2;

      case main_state is

        -- IDLE
        when IDLES =>
          if (SOF = '1') then
            main_state <= JFIF;

            rsm.x_cnt <= (others => '0');
            rsm.y_cnt <= (others => '0');

            JFIF_START <= '1';
            JFIF_EOI   <= '0';

            out_mux_ctrl_s <= '0';
          end if;

        -- JFIF
        when JFIF =>
          if (JFIF_READY = '1') then
            main_state <= HORIZ;

            out_mux_ctrl_s <= '1';
          end if;

        -- HORIZ
        when HORIZ =>
          if (rsm.x_cnt < unsigned(IMG_SIZE_X)) then
            main_state <= COMP;
          else
            main_state <= VERT;

            rsm.x_cnt <= (others => '0');
          end if;

        -- COMP
        when COMP =>
          if (idle(1) = '1' and start(1) = '0') then
            if (rsm.cmp_idx < unsigned(C_CMP_MAX)) then
              start(1) <= '1';
            else
              main_state <= HORIZ;

              rsm.x_cnt   <= rsm.x_cnt + 16;
              rsm.cmp_idx <= (others => '0');
            end if;
          end if;

        -- VERT
        when VERT =>
          if (rsm.y_cnt < unsigned(IMG_SIZE_Y) - 8) then
            main_state <= HORIZ;

            rsm.x_cnt <= (others => '0');
            rsm.y_cnt <= rsm.y_cnt + 8;
          else
            --if (idle(C_NUM_STAGES downto 1) = (C_NUM_STAGES  downto 1 => '1')) then
            if (idle(C_NUM_STAGES downto 1) = (C_NUM_STAGES - 1 downto 0 => '1')) then
              main_state <= EOI;

              JFIF_EOI   <= '1';
              JFIF_START <= '1';

              out_mux_ctrl_s <= '0';
            end if;
          end if;

        -- VERT
        when EOI =>
          if (JFIF_READY = '1') then
            main_state <= IDLES;

            JPEG_READY <= '1';
          end if;

        -- others
        when others =>
          main_state <= IDLES;

      end case;

      -- increase component index in register state machine
      if (start1_d = '1') then
        rsm.cmp_idx <= rsm.cmp_idx + 1;
      end if;

      if (main_state = IDLES) then
        JPEG_BUSY <= '0';
      else
        JPEG_BUSY <= '1';
      end if;
    end if;

  end process P_MAIN_SM;

end architecture RTL;
