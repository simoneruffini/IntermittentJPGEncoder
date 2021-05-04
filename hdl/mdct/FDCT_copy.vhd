--------------------------------------------------------------------------------
-- Engineer:  Michal Krepa
--            Simone Ruffini [simone.ruffini@tutanota.com]
--
-- Create Date:     01/03/2009
-- Design Name:     FDCT
-- Module Name:     FDCT.vhd - RTL
-- Project Name:    JPEG_ENC
-- Description:     This module perforoms 2D Discrete Cosine Transform and outputs
-- the result to a ZIG ZAG partitioner
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

entity FDCT is
  port (
    CLK                : in    std_logic;
    RST                : in    std_logic;
    -- CTRL
    START_PB           : in    std_logic;
    READY_PB           : out   std_logic;
    FDCT_SM_SETTINGS   : in    T_SM_SETTINGS;

    -- BUF_FIFO
    BF_FIFO_RD         : out   std_logic;
    BF_FIFO_Q          : in    std_logic_vector(23 downto 0);
    BF_FIFO_HF_FULL    : in    std_logic;

    -- ZIG ZAG
    ZZ_BUF_SEL         : in    std_logic;
    ZZ_RD_ADDR         : in    std_logic_vector(5 downto 0);
    ZZ_DATA            : out   std_logic_vector(11 downto 0);
    ZZ_RDEN            : in    std_logic;

    -- HOST
    IMG_SIZE_X         : in    std_logic_vector(15 downto 0);
    IMG_SIZE_Y         : in    std_logic_vector(15 downto 0);
    SOF                : in    std_logic
  );
end entity FDCT;

----------------------------- ARCHITECTURE -------------------------------------

architecture RTL of FDCT is

  --########################### TYPES ##########################################

  --########################### FUNCTIONS ######################################

  --########################### CONSTANTS ######################################

  -- Encode Real numbers without using IEEE std
  --
  -- Positive real numbers are encoded in the range they have, so if we have a 8 bit vector
  -- that will represents unsigned real numbers we have that:
  -- 0.0 = x00 and 1.0 = xFF, everything else is in between.
  -- E.g. 0.45 : in 8 bits we have 2^9 possible values so .45 will be in the 45%
  -- of it => 0.45 * 2^9 = 230
  --
  -- What if we want to encode negative and positive numbers using 2 complement notation?
  -- Firs we use odd size vectors and then we apply the same apporach.
  -- For a 3 bit vector from b000 to b0111 we have positive numbers and from
  -- b100 to b111 we have negative numbers. We need to use only the positive part
  -- of the vector because the 3bit is used "for encoding the sign".
  -- This means that in a 3 bit vector that will encode real numbers we have not
  -- 2^4 possible values but just 2^3. This is the only gotcha then the encoding
  -- procedes in the same way as with positive reals (look above)

  -- Constants used to change color space from RGB to Y'CbCr for the JPEG algorithm
  -- see https://en.wikipedia.org/wiki/YCbCr#JPEG_conversion
  --
  -- Constants used in this equation have format 14 bits of precision plus 1 sign bit on MSB.

  constant C_Y_1        : signed(14 downto 0) := to_signed(4899,  15); -- round(0.299 * 2^14)
  constant C_Y_2        : signed(14 downto 0) := to_signed(9617,  15); -- round(0.587 * 2^14)
  constant C_Y_3        : signed(14 downto 0) := to_signed(1868,  15); -- round(0.114 * 2^14)
  constant C_CB_1       : signed(14 downto 0) := to_signed(-2764, 15); -- -1 * round(0.1687 * 2^14)
  constant C_CB_2       : signed(14 downto 0) := to_signed(-5428, 15); -- -1 * round(0.3313 * 2^14)
  constant C_CB_3       : signed(14 downto 0) := to_signed(8192,  15); -- -1 * round(0.5 * 2^14)
  constant C_CR_1       : signed(14 downto 0) := to_signed(8192,  15); -- round(0.5 * 2^14)
  constant C_CR_2       : signed(14 downto 0) := to_signed(-6860, 15); -- -1 * round(0.4187 * 2^14)
  constant C_CR_3       : signed(14 downto 0) := to_signed(-1332, 15); -- -1 * round(0.0813 * 2^14)

  --########################### SIGNALS ########################################

  signal mdct_data_in   : std_logic_vector(7 downto 0);
  signal mdct_idval     : std_logic;
  signal mdct_odval     : std_logic;
  signal mdct_data_out  : std_logic_vector(11 downto 0);
  signal odv1           : std_logic;
  signal dcto1          : std_logic_vector(11 downto 0);

  signal rd_en          : std_logic;
  signal x_pixel_cnt    : unsigned(15 downto 0);
  signal y_line_cnt     : unsigned(15 downto 0);
  signal input_rd_cnt   : unsigned(6 downto 0);
  signal cmp_idx        : unsigned(2 downto 0);
  signal cur_cmp_idx    : unsigned(2 downto 0);
  signal cur_cmp_idx_d1 : unsigned(2 downto 0);                        -- cur_cmp_idx 9 stage delay
  signal cur_cmp_idx_d2 : unsigned(2 downto 0);
  signal cur_cmp_idx_d3 : unsigned(2 downto 0);
  signal cur_cmp_idx_d4 : unsigned(2 downto 0);
  signal cur_cmp_idx_d5 : unsigned(2 downto 0);
  signal cur_cmp_idx_d6 : unsigned(2 downto 0);
  signal cur_cmp_idx_d7 : unsigned(2 downto 0);
  signal cur_cmp_idx_d8 : unsigned(2 downto 0);
  signal cur_cmp_idx_d9 : unsigned(2 downto 0);
  signal eoi_fdct       : std_logic;                                   -- end of image for fdct
  signal start_int      : std_logic;
  signal start_int_d    : std_logic_vector(4 downto 0); -- 5 stage delay for start_int
  signal bf_dval        : std_logic;
  signal bf_dval_m1     : std_logic;
  signal bf_dval_m2     : std_logic;
  signal bf_dval_m3     : std_logic;
  signal bf_fifo_rd_s   : std_logic;
  signal rd_started     : std_logic;

  signal fram1_data     : std_logic_vector(23 downto 0);
  signal fram1_q        : std_logic_vector(23 downto 0);
  signal fram1_we       : std_logic;
  signal fram1_waddr    : std_logic_vector(6 downto 0); -- data of where next pixel will be saved from BUF_FIFO
  signal fram1_raddr    : std_logic_vector(6 downto 0);
  signal fram1_rd       : std_logic;
  signal fram1_rd_d     : std_logic_vector(8 downto 0); -- 9 stage delay for fram1_rd
  signal fram1_pix_cnt  : unsigned(2 downto 0);
  signal fram1_line_cnt : unsigned(2 downto 0);
  signal fram1_q_vld    : std_logic;

  signal y_reg_1        : signed(23 downto 0);
  signal y_reg_2        : signed(23 downto 0);
  signal y_reg_3        : signed(23 downto 0);
  signal cb_reg_1       : signed(23 downto 0);
  signal cb_reg_2       : signed(23 downto 0);
  signal cb_reg_3       : signed(23 downto 0);
  signal cr_reg_1       : signed(23 downto 0);
  signal cr_reg_2       : signed(23 downto 0);
  signal cr_reg_3       : signed(23 downto 0);
  signal y_reg          : signed(23 downto 0);
  signal cb_reg         : signed(23 downto 0);
  signal cr_reg         : signed(23 downto 0);
  signal r_s            : signed(8 downto 0);
  signal g_s            : signed(8 downto 0);
  signal b_s            : signed(8 downto 0);
  signal y_8bit         : unsigned(7 downto 0);
  signal cb_8bit        : unsigned(7 downto 0);
  signal cr_8bit        : unsigned(7 downto 0);

  signal fifo1_rd       : std_logic;
  signal fifo1_wr       : std_logic;
  --signal fifo1_q        : std_logic_vector(11 downto 0);
  --signal fifo1_q_dval   : std_logic;
  signal fifo1_datao        : std_logic_vector(11 downto 0);
  signal fifo1_datao_dval   : std_logic;
  signal fifo1_full     : std_logic;
  signal fifo1_empty    : std_logic;
  signal fifo1_count    : std_logic_vector(9 downto 0);
  signal fifo1_rd_cnt   : unsigned(5 downto 0);
  signal fifo_data_in   : std_logic_vector(11 downto 0);
  signal fifo_rd_arm    : std_logic;

  signal dbuf_data      : std_logic_vector(11 downto 0);
  signal dbuf_q_z1      : std_logic_vector(11 downto 0);
  signal dbuf_q         : std_logic_vector(11 downto 0);
  signal dbuf_we        : std_logic;
  signal dbuf_waddr     : std_logic_vector(6 downto 0);
  signal dbuf_raddr     : std_logic_vector(6 downto 0);

  signal writing_en     : std_logic;
  signal wr_cnt         : unsigned(5 downto 0);
  signal xw_cnt         : unsigned(2 downto 0); -- Generates the address where MDCT frame in FIFO1 is transposed to DBUF 
  signal yw_cnt         : unsigned(2 downto 0); -- Generates the address where MDCT frame in FIFO1 is transposed to DBUF 

  --########################### ARCHITECTURE BEGIN #############################

begin

  --########################### ENTITY DEFINITION ##############################

  --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  -- Frame RAM1, size: 128x24 bits, holds pixel data from BUFFIFO. 
  -- FRAM1 contains RGB or fake YCbCr values (depending on BUF_FIFO, see C_YUV_INPUT)
  -- RGB2YCBCR, subsampling and channel selection are done on the fly and fed to FDCT.
  --
  -- Why FRAM1?
  -- FDCT with Y1Y2CbCr needs 4 reads (1 for each channel), impossible from BUF_FIFO
  -- Why 128x24 bits?
  -- 4:2:2 subsampling is used hence the Minimum Coded Unit is 16x8 pixels, 1 pixel 24 bit
  -- 
  -- In 4:2:2 chroma subsampling: 
  -- - Luma (Y) is not subsampled 
  -- - Chroma (Cb/Cr) is subsampled line-wise, every 2 consecutive pixels 1 is created
  -- FDCT needs 8x8 blocks after subsampling, 16 pixels per line because of choroma subsamp.
  -- The additional luma is used too: first 8x8 block is Y1 second 8x8 block is Y2
  -- 
  --       FRAM1                    INPUT TO FDCT
  --    1  2    15 16            1   2      7   8
  -- 1 [x][x]...[x][x]        | [xx][xx]...[xx][xx] |
  -- 2 [x][x]...[x][x]        | [xx][xx]...[xx][xx] |
  --        ......     -...-> |        .....        | X 4 (Y1,Y2,Cb,Cr), xx is the "subsampled" value 
  -- 7 [x][x]...[x][x]        | [xx][xx]...[xx][xx] |
  -- 8 [x][x]...[x][x]        | [xx][xx]...[xx][xx] | 
  --
  --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  E_FRAM1 : entity work.ramz
    generic map (

      RAMADDR_W => 7, -- 2^7 = 128
      RAMDATA_W => 24
    )
    port map (

      D     => fram1_data,
      WADDR => fram1_waddr,
      RADDR => fram1_raddr,
      WE    => fram1_we,
      CLK   => CLK,

      Q => fram1_q
    );

  --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  -- FDCT with input level shift
  --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  E_MDCT : entity work.mdct
    port map (

      CLK   => CLK,
      RST   => RST,
      DCTI  => mdct_data_in,
      IDV   => mdct_idval,
      ODV   => mdct_odval,
      DCTO  => mdct_data_out,
      ODV1  => odv1,
      DCTO1 => dcto1
    );

  --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  -- FIFO1
  --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  E_FIFO1 : entity work.fifo
    generic map (

      DATA_WIDTH => 12,
      ADDR_WIDTH => 9
    )
    port map (

      RST   => RST,
      CLK   => CLK,
      RINC  => fifo1_rd,
      WINC  => fifo1_wr,
      DATAI => fifo_data_in,

      DATAO  => fifo1_datao, -- fifo1_q,
      FULLO  => fifo1_full,
      EMPTYO => fifo1_empty,
      COUNT  => fifo1_count
    );

  -------------------------------------------------------------------
  -- DBUF
  -------------------------------------------------------------------
  E_DBUF : entity work.ramz
    generic map (

      RAMADDR_W => 7,
      RAMDATA_W => 12
    )
    port map (

      D     => dbuf_data,
      WADDR => dbuf_waddr,
      RADDR => dbuf_raddr,
      WE    => dbuf_we,
      CLK   => CLK,

      Q => dbuf_q
    );

  --########################### COMBINATORIAL LOGIC ############################

  BF_FIFO_RD <= bf_fifo_rd_s;

  ZZ_DATA <= dbuf_q;

  -- 24bit word splitter from FRAM1_d`, extracts RGB, adds an additional 0 for sign
  r_s <= signed('0' & fram1_q(7 downto 0));
  g_s <= signed('0' & fram1_q(15 downto 8));
  b_s <= signed('0' & fram1_q(23 downto 16));

  y_8bit  <= unsigned(y_reg(21 downto 14));
  cb_8bit <= unsigned(cb_reg(21 downto 14));
  cr_8bit <= unsigned(cr_reg(21 downto 14));

  fram1_we   <= bf_dval;
  fram1_data <= BF_FIFO_Q;

  fram1_q_vld <= fram1_rd_d(5);

  mdct_idval <= fram1_rd_d(8);


  fifo1_wr     <= mdct_odval;
  fifo_data_in <= mdct_data_out;

  dbuf_data  <= fifo1_datao; --fifo1_q;
  dbuf_we    <= fifo1_datao_dval; --fifo1_q_dval;
  dbuf_waddr <= (not ZZ_BUF_SEL) & std_logic_vector(yw_cnt & xw_cnt);
  dbuf_raddr <= ZZ_BUF_SEL & ZZ_RD_ADDR;

  --########################## PROCESSES #######################################

  --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  -- FRAM1 write address manager, data comes from BUF_FIFO.
  --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  P_FRAM1_ACC : process (CLK, RST) is
  begin

    if (RST = '1') then
      fram1_waddr <= (others => '0');
    elsif (CLK'event and CLK = '1') then
      if (fram1_we = '1') then
        fram1_waddr <= std_logic_vector(unsigned(fram1_waddr) + 1);
      end if;
    end if;

  end process P_FRAM1_ACC;

  --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  -- RGB to YCbCr conversion
  --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  P_RGB2YCBCR : process (CLK, RST) is
  begin

    if (RST = '1') then
      y_reg_1  <= (others => '0');
      y_reg_2  <= (others => '0');
      y_reg_3  <= (others => '0');
      cb_reg_1 <= (others => '0');
      cb_reg_2 <= (others => '0');
      cb_reg_3 <= (others => '0');
      cr_reg_1 <= (others => '0');
      cr_reg_2 <= (others => '0');
      cr_reg_3 <= (others => '0');
      y_reg    <= (others => '0');
      cb_reg   <= (others => '0');
      cr_reg   <= (others => '0');
    elsif (CLK'event and CLK = '1') then
      -- RGB input
      if (C_YUV_INPUT = '0') then
        -- We are multipling "fake" real numbers with natural ones (8 bit R,G,B)
        -- The result of this multiplication has to be extracted
        y_reg_1 <= r_s * C_Y_1;
        y_reg_2 <= g_s * C_Y_2;
        y_reg_3 <= b_s * C_Y_3;

        cb_reg_1 <= r_s * C_CB_1;
        cb_reg_2 <= g_s * C_CB_2;
        cb_reg_3 <= b_s * C_CB_3;

        cr_reg_1 <= r_s * C_CR_1;
        cr_reg_2 <= g_s * C_CR_2;
        cr_reg_3 <= b_s * C_CR_3;

        y_reg  <= y_reg_1 + y_reg_2 + y_reg_3;
        cb_reg <= cb_reg_1 + cb_reg_2 + cb_reg_3 + to_signed(128 * 16384, cb_reg'length);
        cr_reg <= cr_reg_1 + cr_reg_2 + cr_reg_3 + to_signed(128 * 16384, cr_reg'length);
      -- YCbCr input
      -- R-G-B misused as Y-Cb-Cr
      else
        y_reg_1  <= '0' & r_s & "00000000000000";
        cb_reg_1 <= '0' & g_s & "00000000000000";
        cr_reg_1 <= '0' & b_s & "00000000000000";

        y_reg  <= y_reg_1;
        cb_reg <= cb_reg_1;
        cr_reg <= cr_reg_1;
      end if;
    end if;

  end process P_RGB2YCBCR;

  --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  -- Intermidiate (I)RAM read process
  --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  P_IRAM_READ: process (CLK, RST) is
  begin

    if (RST = '1') then
      rd_en          <= '0';
      x_pixel_cnt    <= (others => '0');
      y_line_cnt     <= (others => '0');
      input_rd_cnt   <= (others => '0');
      cmp_idx        <= (others => '0');
      cur_cmp_idx    <= (others => '0');
      cur_cmp_idx_d1 <= (others => '0');
      cur_cmp_idx_d2 <= (others => '0');
      cur_cmp_idx_d3 <= (others => '0');
      cur_cmp_idx_d4 <= (others => '0');
      cur_cmp_idx_d5 <= (others => '0');
      cur_cmp_idx_d6 <= (others => '0');
      cur_cmp_idx_d7 <= (others => '0');
      cur_cmp_idx_d8 <= (others => '0');
      cur_cmp_idx_d9 <= (others => '0');
      eoi_fdct       <= '0';
      start_int      <= '0';
      bf_fifo_rd_s   <= '0';
      bf_dval        <= '0';
      bf_dval_m1     <= '0';
      bf_dval_m2     <= '0';
      fram1_rd       <= '0';
      fram1_rd_d     <= (others => '0');
      start_int_d    <= (others => '0');
      fram1_raddr    <= (others => '0');
      fram1_line_cnt <= (others => '0');
      fram1_pix_cnt  <= (others => '0');
    elsif (CLK'event and CLK = '1') then
      cur_cmp_idx_d1 <= cur_cmp_idx;
      cur_cmp_idx_d2 <= cur_cmp_idx_d1;
      cur_cmp_idx_d3 <= cur_cmp_idx_d2;
      cur_cmp_idx_d4 <= cur_cmp_idx_d3;
      cur_cmp_idx_d5 <= cur_cmp_idx_d4;
      cur_cmp_idx_d6 <= cur_cmp_idx_d5;
      cur_cmp_idx_d7 <= cur_cmp_idx_d6;
      cur_cmp_idx_d8 <= cur_cmp_idx_d7;
      cur_cmp_idx_d9 <= cur_cmp_idx_d8;

      start_int      <= '0';

      bf_dval_m3 <= bf_fifo_rd_s;
      bf_dval_m2 <= bf_dval_m3;
      bf_dval_m1 <= bf_dval_m2;
      bf_dval    <= bf_dval_m1;

      -- 9 bit shift register: dealyes fram1 read signal by 9
      fram1_rd_d  <= fram1_rd_d(fram1_rd_d'length - 2 downto 0) & fram1_rd;
      -- 5 bit shift register: dealyes start int signal by 5
      start_int_d <= start_int_d(start_int_d'length - 2 downto 0) & start_int;

      --------------------------------------------------------------------------
      -- SOF or internal self-start (after a 16x8 read from zig-zag)
      if (SOF = '1' or start_int = '1') then
        input_rd_cnt <= (others => '0');
        -- enable BUF_FIFO/FRAM1 reading
        rd_started <= '1';

        -- component index
        if (cmp_idx = 4 - 1) then
          cmp_idx <= (others => '0');
          -- horizontal block counter
          if (x_pixel_cnt = unsigned(IMG_SIZE_X) - 16) then
            x_pixel_cnt <= (others => '0');
            -- vertical block counter
            if (y_line_cnt = unsigned(IMG_SIZE_Y) - 8) then
              y_line_cnt <= (others => '0');
              -- set end of image flag
              eoi_fdct <= '1';
            else
              y_line_cnt <= y_line_cnt + 8;
            end if;
          else
            x_pixel_cnt <= x_pixel_cnt + 16;
          end if;
        else
          cmp_idx <= cmp_idx + 1;
        end if;

        cur_cmp_idx <= cmp_idx;
      end if;

      --------------------------------------------------------------------------
      -- wait until FIFO becomes half full but only for component 0
      -- as we read buf FIFO only during component 0
      if (rd_started = '1' and (BF_FIFO_HF_FULL = '1' or cur_cmp_idx > 1)) then
        rd_en      <= '1';
        rd_started <= '0';
      end if;

      bf_fifo_rd_s <= '0';
      fram1_rd     <= '0';
      --------------------------------------------------------------------------
      -- stall reading from input FIFO and writing to output FIFO
      -- when output FIFO is almost full
      if (rd_en = '1' and unsigned(fifo1_count) < 512 - 64 and
          (BF_FIFO_HF_FULL = '1' or cur_cmp_idx > 1)) then
        -- read request goes to BUF_FIFO only for component 0.
        if (cur_cmp_idx < 2) then
          bf_fifo_rd_s <= '1';
        end if;

        -- count number of samples read from input in one run
        if (input_rd_cnt = 64 - 1) then
          rd_en <= '0';
          -- internal restart
          start_int <= '1' and not eoi_fdct;
          eoi_fdct  <= '0';
        else
          input_rd_cnt <= input_rd_cnt + 1;
        end if;
        -- FRAM read enable
        fram1_rd <= '1';
      end if;

      --------------------------------------------------------------------------
      -- increment FRAM1 read address according to subsampling
      -- idea is to extract 8x8 from 16x8 block
      -- there are two luminance blocks: left and right
      -- there is 2:1 subsampled Cb block
      -- there is 2:1 subsampled Cr block
      -- subsampling done as simple decimation by 2 wo/ averaging
      if (SOF = '1') then
        fram1_raddr    <= (others => '0');
        fram1_line_cnt <= (others => '0');
        fram1_pix_cnt  <= (others => '0');
      elsif (start_int_d(4) = '1') then
        fram1_line_cnt <= (others => '0');
        fram1_pix_cnt  <= (others => '0');

        case cur_cmp_idx_d4 is

          -- Y1, Cr, Cb
          when "000" | "010" | "011" =>
            fram1_raddr <= (others => '0');
          -- Y2
          when "001" =>
            fram1_raddr <= std_logic_vector(to_unsigned(64, fram1_raddr'length));
          when others =>
            null;

        end case;

      elsif (fram1_rd_d(4) = '1') then
        ------------------------------------------------------------------------
        -- pixel and line counter handler
        if (fram1_pix_cnt = 8 - 1) then
          fram1_pix_cnt <= (others => '0');
          if (fram1_line_cnt = 8 - 1) then
            fram1_line_cnt <= (others => '0');
          else
            fram1_line_cnt <= fram1_line_cnt + 1;
          end if;
        else
          fram1_pix_cnt <= fram1_pix_cnt + 1;
        end if;

        case cur_cmp_idx_d6 is

          when "000" | "001" =>
            fram1_raddr <= std_logic_vector(unsigned(fram1_raddr) + 1);
          when "010" | "011" =>
            if (fram1_pix_cnt = 4 - 1) then
              fram1_raddr <= std_logic_vector('1' & fram1_line_cnt & "000");
            elsif (fram1_pix_cnt = 8 - 1) then
              if (fram1_line_cnt = 8 - 1) then
                fram1_raddr <= '0' & "000" & "000";
              else
                fram1_raddr <= std_logic_vector('0' & (fram1_line_cnt + 1) & "000");
              end if;
            else
              fram1_raddr <= std_logic_vector(unsigned(fram1_raddr) + 2);
            end if;
          when others =>
            null;

        end case;

      end if;
    end if;

  end process P_IRAM_READ;

  --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  -- MUX1, selects correct channel for MDCT
  -- 
  -- It routes proper byte from 24 bits input data. Data(7:0) is used for cur_cmp_idx=0,1, data(15:8) for cur_cmp_idx=2, data(23:16) for
-- cur_cmp_idx=3. 
  --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  P_MUX1 : process (CLK, RST) is
  begin

    if (RST = '1') then
      mdct_data_in <= (others => '0');
    elsif (CLK'event and CLK = '1') then

      case cur_cmp_idx_d9 is

        when "000" | "001" =>
          mdct_data_in <= std_logic_vector(y_8bit);
        when "010" =>
          mdct_data_in <= std_logic_vector(cb_8bit);
        when "011" =>
          mdct_data_in <= std_logic_vector(cr_8bit);
        when others =>
          null;

      end case;

    end if;

  end process P_MUX1;

  --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  -- FIFO1 read controller
  --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  P_FIFO_RD_CTRL : process (CLK, RST) is
  begin

    if (RST = '1') then
      fifo1_rd     <= '0';
      fifo_rd_arm  <= '0';
      fifo1_rd_cnt <= (others => '0');
      fifo1_datao_dval <= '0'; --fifo1_q_dval <= '0';
    elsif (CLK'event and CLK = '1') then
      fifo1_rd <= '0';

      fifo1_datao_dval <= fifo1_rd; --fifo1_q_dval <= fifo1_rd;

      if (START_PB = '1') then
        fifo_rd_arm  <= '1';
        fifo1_rd_cnt <= (others => '0');
      end if;

      if (fifo_rd_arm = '1') then
        if (fifo1_rd_cnt = 64 - 1) then
          fifo_rd_arm <= '0';
          fifo1_rd    <= '1';
        elsif (fifo1_empty = '0') then
          fifo1_rd     <= '1';
          fifo1_rd_cnt <= fifo1_rd_cnt + 1;
        end if;
      end if;
    end if;

  end process P_FIFO_RD_CTRL;

  --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  -- Write counter
  --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  P_WR_CNT : process (CLK, RST) is
  begin

    if (RST = '1') then
      wr_cnt     <= (others => '0');
      READY_PB   <= '0';
      xw_cnt     <= (others => '0');
      yw_cnt     <= (others => '0');
      writing_en <= '0';
    elsif (CLK'event and CLK = '1') then
      READY_PB <= '0';

      if (START_PB = '1') then
        wr_cnt     <= (others => '0');
        xw_cnt     <= (others => '0');
        yw_cnt     <= (others => '0');
        writing_en <= '1';
      end if;

      if (writing_en = '1') then
        if (fifo1_datao_dval = '1') then --if (fifo1_q_dval = '1') then
          if (wr_cnt = 64 - 1) then
            wr_cnt     <= (others => '0');
            READY_PB   <= '1';
            writing_en <= '0';
          else
            wr_cnt <= wr_cnt + 1;
          end if;

          ----------------------------------------------------------------------
          -- Counters that compute the address for the transpositions of the frame
          if (yw_cnt = 8 - 1) then
            yw_cnt <= (others => '0');
            xw_cnt <= xw_cnt + 1;
          else
            yw_cnt <= yw_cnt + 1;
          end if;
        end if;
      end if;
    end if;

  end process P_WR_CNT;

end architecture RTL;

