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
  --use work.NORM_COMM_PKG.all;

----------------------------- ENTITY -------------------------------------------

entity CTRLSM is
  port (
    CLK                 : in    std_logic;
    RST                 : in    std_logic;

    -- Intermittent Enhancment Ports
    BUS_MSYNC           : in    std_logic; --master sync signal
    BUS_RX              : in    std_logic_vector(C_BUS_BIT_WIDTH - 1 downto 0);
    BUS_TX              : out   std_logic_vector(C_BUS_BIT_WIDTH - 1 downto 0);
    BUS_SSYNC           : out   std_logic; --slave sync signal

    -- output IF
    OUTIF_ALMOST_FULL   : in    std_logic;

    -- HOST IF
    SOF                 : in    std_logic;
    IMG_SIZE_X          : in    std_logic_vector(15 downto 0);
    IMG_SIZE_Y          : in    std_logic_vector(15 downto 0);
    JPEG_READY          : out   std_logic;
    JPEG_BUSY           : out   std_logic;

    -- JFIF GEN
    JFIF_START          : out   std_logic;
    JFIF_READY          : in    std_logic;
    JFIF_EOI            : out   std_logic;

    -- FDCT
    FDCT_START          : out   std_logic;
    FDCT_READY          : in    std_logic;
    FDCT_SM_SETTINGS    : out   T_SM_SETTINGS;

    -- ZIGZAG
    ZIG_START           : out   std_logic;
    ZIG_READY           : in    std_logic;
    ZIG_SM_SETTINGS     : out   T_SM_SETTINGS;

    -- QUANTIZER
    QUA_START           : out   std_logic;
    QUA_READY           : in    std_logic;
    QUA_SM_SETTINGS     : out   T_SM_SETTINGS;

    -- RLE
    RLE_START           : out   std_logic;
    RLE_READY           : in    std_logic;
    RLE_SM_SETTINGS     : out   T_SM_SETTINGS;

    -- HUFFMAN
    HUF_START           : out   std_logic;
    HUF_READY           : in    std_logic;
    HUF_SM_SETTINGS     : out   T_SM_SETTINGS;

    -- BYTE STUFFER
    BS_START            : out   std_logic;
    BS_READY            : in    std_logic;
    BS_SM_SETTINGS      : out   T_SM_SETTINGS;

    -- OUT MUX
    OUT_MUX_CTRL        : out   std_logic
  );
end entity CTRLSM;

----------------------------- ARCHITECTURE -------------------------------------

architecture RTL of CTRLSM is

  --########################### TYPES ##########################################

  type ctrlsm_state_t is (
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

  -- number of stages of ctrlsm
  constant C_NUM_STAGES                : integer := 6;

  type bus_state_t is (
    -- wait for the master to signal state restore
    WAIT_STATE_RESTORE,
    -- perform state restore
    STATE_RESTORE,
    -- wait for master to stignal babkup store
    WAIT_STATE_STORE,
    -- perform a backup of the current state
    STATE_STORE
  );

  type fsm_state_arr_t is array(C_NUM_STAGES downto 1) of std_logic_vector(1 downto 0);

  type arr_sm_settings_t is array(C_NUM_STAGES  downto 1) of t_sm_settings;

  --########################### FUNCIONS #######################################

  -- encode ctrlsm_state to be sent on BUS

  function encode_state (
    state : ctrlsm_state_t
  )
  return std_logic_vector is
    variable ret : std_logic_vector(BUS_TX'length - 1 downto 0);
  begin
    ret := std_logic_vector(to_unsigned(ctrlsm_state_t'pos(state), BUS_TX'length));
    return  ret;
  end function;

  -- decode a state comming from BUS

  function decode_state (
    encoded_state : std_logic_vector(BUS_TX'length - 1 downto 0)
  )
  return ctrlsm_state_t is
  begin
    return ctrlsm_state_t'val(to_integer(unsigned(encoded_state)));
  end function;

  -- encode an x and y counters of a sm_settings type, to be sent to BUS

  function enc_sm_settings_xy_cnt (
    reg : t_sm_settings
  )
  return std_logic_vector is
    variable ret : std_logic_vector(BUS_TX'length - 1 downto 0);
  begin
    ret := std_logic_vector(reg.y_cnt & reg.x_cnt);
    return ret;
  end function;

  -- decode x and y counters of a sm_setting type coming from BUS

  function dec_sm_settings_xy_cnt (
    enc_xy_cnt : std_logic_vector(BUS_TX'length - 1 downto 0)
  )
  return t_sm_settings is
    variable ret : t_sm_settings;
  begin
    ret := (
            unsigned(enc_xy_cnt(ret.x_cnt'length - 1 downto 0)),
            unsigned(enc_xy_cnt(BUS_RX'length - 1 downto ret.y_cnt'length)),
            (others => '0')
          );

    return ret;
  end function;

  -- encode all compare indexes of reg (arr_sm_settings_t) to be sent onto BUS

  function enc_reg_cmp_idx (
    reg : arr_sm_settings_t
  )
  return std_logic_vector is
    variable ret : std_logic_vector(BUS_TX'length - 1 downto 0);
  begin
    ret := (others => '0');
    for i in 0 to reg'length - 1 loop
      -- WARNING: logical shift of reg.cmp_idx and resize shall not cut data
      ret( ((i + 1) * reg(0).cmp_idx'length) - 1 downto (i * reg(0).cmp_idx'length) )
                                        := std_logic_vector(reg(i + 1).cmp_idx);
    end loop;
    return  ret;
  end function;

  -- decode all compare indexes of reg (arr_sm_settings_t) coming from BUS

  function dec_reg_cmp_idx (
    encoded_data : std_logic_vector(BUS_TX'length - 1 downto 0);
    reg_numb :natural
  )
  return unsigned is
    -- variable tmp             : unsigned(BUS_TX'length - 1 downto 0);
    variable tmp_sm_settings : t_sm_settings;
    variable ret             : unsigned (tmp_sm_settings.cmp_idx'length - 1 downto 0);
  begin

    -- tmp := shift_right(unsigned(encoded_data),
    --                   (reg_number - 1) * tmp_sm_settings.cmp_idx'length);
    -- ret := tmp (tmp_sm_settings.cmp_idx'length - 1 downto 0);
    ret := unsigned(encoded_data(((reg_numb * tmp_sm_settings.cmp_idx'length) - 1)
                                 downto ((reg_numb - 1) * tmp_sm_settings.cmp_idx'length)));
    return ret;
  end function;

  -- encode rsm compare index to be sent onto BUS

  function enc_rsm_cmp_idx (
    rsm : t_sm_settings
  )
  return std_logic_vector is
    variable ret : std_logic_vector(BUS_TX'length - 1 downto 0);
  begin
    ret := std_logic_vector(resize(rsm.cmp_idx, BUS_TX'length));

    return  ret;
  end function;

  -- decode rsm compare index coming from BUS

  function dec_rsm_cmp_idx (
    encoded_rsm_cmp_idx : std_logic_vector(BUS_TX'length - 1 downto 0)
  )
  return unsigned is
    --variable tmp             : unsigned(BUS_TX'length - 1 downto 0);
    variable tmp_sm_settings : t_sm_settings;
    variable ret             : unsigned (tmp_sm_settings.cmp_idx'length - 1 downto 0);
  begin

    -- tmp := shift_right(unsigned(encoded_rsm_cmp_idx),
    --                   tmp_sm_settings.cmp_idx'length);
    ret := unsigned(encoded_rsm_cmp_idx(tmp_sm_settings.cmp_idx'length - 1 downto 0));
    return ret;
  end function;

  --########################### CONSTANTS ######################################

  constant C_CMP_MAX                   : std_logic_vector(2 downto 0) := "100";
  constant C_SM_SETTINGS               : t_sm_settings :=
  (
    (others => '0'),
    (others => '0'),
    (others => '0')
  );
  -- Store/restore timings are handled by a counter. Processes concerned with storing and restoring data from BUS sync
  -- on this counter.
  -- The below constants represent starting values for each store/restore state.
  -- While the value represents the start of the state it represents the number of clocks the previous state needs to complete.
  constant C_BUS_CNTR_MAIN_STATE_VAL   : natural := 0;
  constant C_BUS_CNTR_RSM_XY_CNT_VAL   : natural := C_BUS_CNTR_MAIN_STATE_VAL + 1;                                                                      -- storing/saving main state takes 1 clk
  constant C_BUS_CNTR_REG_XY_CNT_VAL   : natural := C_BUS_CNTR_RSM_XY_CNT_VAL + 1;                                                                      -- storing/saving rsm_xy_cnt takes 1 clk
  constant C_BUS_CNTR_RSM_CMP_INDX_VAL : natural := C_BUS_CNTR_REG_XY_CNT_VAL + C_NUM_STAGES;                                                           -- storing/saving reg_xy_cnt takes C_NUM_STAGES clks
  constant C_BUS_CNTR_REG_CMP_INDX_VAL : natural := C_BUS_CNTR_RSM_CMP_INDX_VAL + 1;                                                                    -- storing/saving rsm_cmp_index takes 1 clk
  constant C_BUS_CNTR_END_VAL          : natural := C_BUS_CNTR_REG_CMP_INDX_VAL + 1;                                                                    -- storing/saving reg_cmp_index takes 1 clk

  --########################### SIGNALS ########################################

  signal bus_ssync_s                   : std_logic;

  signal main_state                    : ctrlsm_state_t;

  signal idle                          : std_logic_vector(C_NUM_STAGES + 1 downto 1);
  signal start                         : std_logic_vector(C_NUM_STAGES + 1 downto 1);
  signal start_pb                      : std_logic_vector(C_NUM_STAGES downto 1);
  signal ready_pb                      : std_logic_vector(C_NUM_STAGES downto 1);

  signal rsm                           : t_sm_settings;
  signal reg                           : arr_sm_settings_t;

  signal start1_d                      : std_logic;
  signal out_mux_ctrl_s                : std_logic;
  signal out_mux_ctrl_s2               : std_logic;

  signal fsm_state                     : fsm_state_arr_t;

  signal bus_state                     : bus_state_t;
  signal bus_future_state              : bus_state_t;

  signal state_restore_on_s            : std_logic;
  signal state_store_on_s              : std_logic;

  signal bus_counter_init_s            : std_logic;
  signal bus_counter_ce_s              : std_logic;
  signal bus_counter_tc_s              : std_logic;
  signal bus_counter_s                 : natural;

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

  CTRL_SSM_GEN : for i in 1 to C_NUM_STAGES generate

    SSM_GEN : entity work.singlesm
      port map (

        CLK => CLK,
        RST => RST,
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

  end generate CTRL_SSM_GEN;

  BUS_COUNTER_E : entity work.counter
    generic map (
      MAX         => C_BUS_CNTR_END_VAL,
      INIT_VALUE  => 0,
      INCREASE_BY => 1
    )
    port map (
      CLK   => CLK,
      RST   => RST,
      INIT  => bus_counter_init_s,
      CE    => bus_counter_ce_s,
      TC    => bus_counter_tc_s,
      VALUE => bus_counter_s
    );

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

  BUS_SSYNC <= bus_ssync_s;

  state_restore_on_s <= '1' when (bus_state = WAIT_STATE_RESTORE AND BUS_MSYNC = '1')
                                 OR bus_state = STATE_RESTORE else
                        '0';
  state_store_on_s   <= '1' when (bus_state = WAIT_STATE_STORE AND BUS_MSYNC = '1')
                                 OR bus_state = STATE_STORE else
                        '0';
  --########################## PROCESSES #######################################

  BUS_FSM_SEQ : process (CLK, RST) is
  begin

    if (RST = '1') then
      bus_state <= WAIT_STATE_RESTORE;
    elsif (CLK'event and CLK = '1') then
      bus_state <= bus_future_state;
    end if;

  end process BUS_FSM_SEQ;

  BUS_FSM_COMB : process (bus_state, BUS_MSYNC, bus_counter_tc_s) is
  begin

    -- Defaults
    bus_future_state   <= bus_state;
    bus_ssync_s        <= '0';
    bus_counter_init_s <= '0';
    bus_counter_ce_s   <= '0';

    case bus_state is

      when WAIT_STATE_RESTORE =>
        bus_counter_init_s <= '1';

        if (BUS_MSYNC = '1') then
          bus_future_state <= STATE_RESTORE;
        end if;

      when STATE_RESTORE =>
        bus_counter_ce_s <= '1';

        if (BUS_MSYNC = '0') then
          bus_future_state <= WAIT_STATE_STORE;
        end if;

      when WAIT_STATE_STORE =>

        bus_counter_init_s <= '1';

        if (BUS_MSYNC = '1') then
          bus_future_state <= STATE_STORE;
        end if;

      when STATE_STORE =>
        bus_counter_ce_s <= '1';
        bus_ssync_s      <= '1';

        if (bus_counter_tc_s = '1') then
          bus_future_state <= WAIT_STATE_STORE;
        end if;

    end case;

  end process BUS_FSM_COMB;

  P_BUS_TX_CNTRL : process (CLK, RST) is
  begin

    if (RST = '1') then
      BUS_TX <= (others => '0');
    elsif (CLK'event AND CLK = '1') then
      if (state_store_on_s = '1') then
        if (bus_counter_s >= C_BUS_CNTR_MAIN_STATE_VAL AND bus_counter_s < C_BUS_CNTR_RSM_XY_CNT_VAL) then
          BUS_TX <= encode_state(main_state);
        elsif (bus_counter_s >= C_BUS_CNTR_RSM_XY_CNT_VAL AND bus_counter_s < C_BUS_CNTR_REG_XY_CNT_VAL) then
          BUS_TX <= enc_sm_settings_xy_cnt(rsm);
        elsif (bus_counter_s >= C_BUS_CNTR_REG_XY_CNT_VAL AND bus_counter_s < C_BUS_CNTR_RSM_CMP_INDX_VAL) then
          BUS_TX <= enc_sm_settings_xy_cnt(reg(bus_counter_s - C_BUS_CNTR_REG_XY_CNT_VAL + 1));
        elsif (bus_counter_s >= C_BUS_CNTR_RSM_CMP_INDX_VAL AND bus_counter_s < C_BUS_CNTR_REG_CMP_INDX_VAL) then
          BUS_TX <= enc_rsm_cmp_idx(rsm);
        elsif (bus_counter_s >= C_BUS_CNTR_REG_CMP_INDX_VAL AND bus_counter_s < C_BUS_CNTR_END_VAL) then
          BUS_TX <= enc_reg_cmp_idx(reg);
        else
          BUS_TX <= (others => '0');
        end if;
      else
        BUS_TX <= (others => '0');
      end if;
    end if;

  end process P_BUS_TX_CNTRL;

  GEN_REG_SM : for i in 1 to C_NUM_STAGES generate

    P_REG : process (CLK, RST) is
    begin

      if (RST = '1') then
        reg(i) <= C_SM_SETTINGS;                  -- iniitialize to 0
      elsif (CLK'event and CLK = '1') then
        if (state_restore_on_s = '1') then        -- Init For reg from NV_REG
          if (bus_counter_s = C_BUS_CNTR_REG_XY_CNT_VAL + i - 1) then
            reg(i) <= dec_sm_settings_xy_cnt (BUS_RX);
          elsif (bus_counter_s = C_BUS_CNTR_REG_CMP_INDX_VAL) then
            reg(i).cmp_idx <= dec_reg_cmp_idx(BUS_RX, i);
          end if;
        elsif (state_store_on_s = '0') then
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
      end if;

    end process P_REG;

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
      if (state_restore_on_s = '1') then
        -- main_state and rsm are restored from nv_reg (transmitted through BUS)
        if (bus_counter_s = C_BUS_CNTR_MAIN_STATE_VAL) then
          main_state <= decode_state(BUS_RX);
        elsif (bus_counter_s = C_BUS_CNTR_RSM_XY_CNT_VAL) then
          rsm <= dec_sm_settings_xy_cnt(BUS_RX);
        elsif (bus_counter_s = C_BUS_CNTR_RSM_CMP_INDX_VAL) then
          rsm.cmp_idx <= dec_rsm_cmp_idx(BUS_RX);
        end if;
      elsif (state_store_on_s = '0') then
        -- normal operation of CtrlSM
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
              -- if (idle(C_NUM_STAGES downto 1) = (C_NUM_STAGES - 1 downto 0 => '1')) then
              if (idle(C_NUM_STAGES downto 1) = (C_NUM_STAGES  downto 1 => '1')) then
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
    end if;

  end process P_MAIN_SM;

end architecture RTL;
