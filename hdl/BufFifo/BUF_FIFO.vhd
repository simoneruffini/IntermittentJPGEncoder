--------------------------------------------------------------------------------
-- Company:
-- Engineer:  Michal Krepa
--            Ahmet Tekyildiz [ahmet.tekyildiz@opencores.org]
--            Simone Ruffini [simone.ruffini@tutanota.com]
--
-- Create Date:     11/03/2009
-- Design Name:     BUF_FIFO
-- Module Name:     BUF_FIFO.vhd - RTL
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
-- Revision 01 - Ahmet Tekyildiz
--  * New BUF_FIFO which needs circa 9.5 line buffer but achieves performance very close to old design with ~16 lines (8 extra lines).
--    So it heavily reduces on-chip RAM utilization without performance sacrifice.
-- Revision 02 - Simone Ruffini
--  * Refactoring + comments
-- Additional Comments:
--
--------------------------------------------------------------------------------

----------------------------- PACKAGES/LIBRARIES -------------------------------

library IEEE;
  use IEEE.STD_LOGIC_1164.ALL;
  use IEEE.NUMERIC_STD.ALL;   -- arithmetic functions with Signed or Unsigned values

-- User libraries

library work;
  use work.JPEG_PKG.ALL;

----------------------------- ENTITY -------------------------------------------

entity BUF_FIFO is
  port (
    CLK                : in    std_logic;
    RST                : in    std_logic;
    -- HOST PROG
    IMG_SIZE_X         : in    std_logic_vector(15 downto 0);
    IMG_SIZE_Y         : in    std_logic_vector(15 downto 0);
    SOF                : in    std_logic;

    -- HOST DATA
    IRAM_WREN          : in    std_logic;
    IRAM_WDATA         : in    std_logic_vector(C_PIXEL_BITS - 1 downto 0);
    FIFO_ALMOST_FULL   : out   std_logic;

    -- FDCT
    FDCT_FIFO_RD       : in    std_logic;
    FDCT_FIFO_Q        : out   std_logic_vector(23 downto 0);
    FDCT_FIFO_HF_FULL  : out   std_logic
  );
end entity BUF_FIFO;

----------------------------- ARCHITECTURE -------------------------------------

architecture RTL of BUF_FIFO is

  --########################### SIGNALS ########################################
  -- constants
  constant c_num_lines      : integer := 8;

  -- signals
  signal ram_dout           : std_logic_vector(C_PIXEL_BITS - 1 downto 0);
  signal ram_din            : std_logic_vector(C_PIXEL_BITS - 1 downto 0);

  signal ram_wr_addr        : unsigned(log2(C_MAX_LINE_WIDTH * c_num_lines) - 1 downto 0);
  signal ram_wr_en          : std_logic;
  signal ram_rd_addr        : unsigned(log2(C_MAX_LINE_WIDTH * c_num_lines) - 1 downto 0);

  signal lut_wr_addr        : unsigned(log2(C_MAX_LINE_WIDTH) - 1 downto 0);
  signal lut_wr_en          : std_logic;
  signal lut_rd_addr        : unsigned(log2(C_MAX_LINE_WIDTH) - 1 downto 0);

  signal lut_dout           : std_logic_vector(15 downto 0);
  signal lut_din            : unsigned(15 downto 0);

  signal lut_wr_addr2       : unsigned(log2(C_MAX_LINE_WIDTH) - 1 downto 0);
  signal lut_wr_en2         : std_logic;
  signal lut_rd_addr2       : unsigned(log2(C_MAX_LINE_WIDTH) - 1 downto 0);

  signal lut_dout2          : std_logic_vector(15 downto 0);
  signal lut_din2           : unsigned(15 downto 0);

  signal result             : std_logic_vector(31 downto 0);                          -- the number of pixels in input
  signal threshold          : std_logic_vector(31 downto 0);                          -- pixel threshold when fifo is considered full (7*image_size_x)

  signal wr_counter         : unsigned(15 downto 0);                                  -- increased by every pixel written, resets after a full 8x8 pixel block line write
  signal rd_counter         : unsigned(15 downto 0);                                  -- increased by every pixel read, resets after a full 8x8 pixel block line read

  signal wr_mod             : unsigned(15 downto 0);                                  -- starts at 0, increased by every 8x8 pixel block line written, resets after rising_edge(SOF)
  signal rd_mod             : unsigned(15 downto 0);                                  -- starts at 1, increased by every 8x8 pixel block line read, resets after rising_edge(SOF)

  signal wr_counter_total   : unsigned(31 downto 0);                                  -- goes from 0 to image_size_x * image_size_y
  signal rd_counter_total   : unsigned(31 downto 0);                                  -- goes from 0 to image_size_x * image_size_y
  signal lut_upd_cntr       : unsigned(31 downto 0);                                  -- counter used to update lut values when near end (last 8x8 pixel block line)
  signal lut_upd_cntr2      : unsigned(31 downto 0);                                  -- counter used to update lut2 values when near end (last 8x8 pixel block line)

  signal init_table_wr      : std_logic;                                              -- flag, when 0 means write process is on, set by a SOF transition
  signal init_table_rd      : std_logic;                                              -- flag, when 0 means read process is on, set by a SOF transition

  signal lut_dout_samp      : unsigned(15 downto 0);                                  -- lut data out sampled
  signal lut_dout_samp2     : unsigned(15 downto 0);
  signal lut_upd_val_tmp    : unsigned(15 downto 0);
  signal lut_upd_val_tmp2   : unsigned(15 downto 0);

  signal ram_dout_samp      : std_logic_vector(23 downto 0);                          -- used to sample ram during reading

  signal fifo_almost_full_i : std_logic;                                              -- this signal is used by the host interface to halt push of new pixels to BUF_FIFO

  --########################### FUNCIONS #######################################

  impure function magicsplitter (
    lut_dout_samp : unsigned;
    out_size      : natural
  )
  return unsigned is
    variable up  : natural;                                                           --implicit max 2^32
    variable dwn : natural;                                                           --implicit  max 2^32
  begin
    up  := to_integer(lut_dout_samp(5 downto 3)) * to_integer(unsigned(IMG_SIZE_X));  -- equal to ((lut_dout_samp srl 3 ) AND (2 downto 0 => '1',others=>'0')) * unsigned(IMG_SIZE_X)
    dwn := to_integer(lut_dout_samp(15 downto 6)) * 8;                                -- equal to ((lut_dout_samp srl 6 ) AND (0=>'1',1=>'1',2=>'1',others=>'0')) * unsigned(IMG_SIZE_X)
    return to_unsigned(up + dwn, out_size);
  end function;

  --########################### ARCHITECTURE BEGIN #############################

begin

  --########################### ENTITY DEFINITION ############################
  U_SUB_RAMZ : entity work.sub_ramz
    generic map (

      RAMADDR_W => log2( C_MAX_LINE_WIDTH*c_num_lines ),
      RAMDATA_W => C_PIXEL_BITS
    )
    port map (

      D     => ram_din,
      WADDR => std_logic_vector(ram_wr_addr),
      RADDR => std_logic_vector(ram_rd_addr),
      WE    => ram_wr_en,
      clk   => CLK,
      Q     => ram_dout
    );

  MULTIPLIER : entity work.multiplier
    port map (
      clk        => clk,
      rst        => rst,
      IMG_SIZE_X => IMG_SIZE_X,
      IMG_SIZE_Y => IMG_SIZE_Y,
      RESULT     => result,
      THRESHOLD  => threshold
    );

  U_SUB_RAMZ_WR_ADRESS_LUT : entity work.sub_ramz_lut
    generic map (

      RAMADDR_W => log2( C_MAX_LINE_WIDTH ),
      RAMDATA_W => 16
    )
    port map (

      D     => std_logic_vector(lut_din),
      WADDR => std_logic_vector(lut_wr_addr),
      RADDR => std_logic_vector(lut_rd_addr),
      WE    => lut_wr_en,
      clk   => clk,
      Q     => lut_dout
    );

  U_SUB_RAMZ_RD_ADRESS_LUT : entity work.sub_ramz_lut
    generic map (

      RAMADDR_W => log2( C_MAX_LINE_WIDTH ),
      RAMDATA_W => 16
    )
    port map (

      D     => std_logic_vector(lut_din2),
      WADDR => std_logic_vector(lut_wr_addr2),
      RADDR => std_logic_vector(lut_rd_addr2),
      WE    => lut_wr_en2,
      clk   => clk,
      Q     => lut_dout2
    );

  --########################### COMBINATORIAL LOGIC ############################
  FIFO_ALMOST_FULL <= fifo_almost_full_i;

  --########################### PROCESSES ######################################
  HOST_INTERFACE_FILL_RAM_P : process (clk, rst) is
  begin

    if (rst = '1') then
      wr_counter       <= (others => '0');
      wr_counter_total <= (others => '0');
      wr_mod           <= (others => '0');
      ram_wr_addr      <= (others => '0');                                               --main ram wr address

      ram_wr_en       <= '0';
      init_table_wr   <= '0';
      lut_dout_samp   <= (others => '0');
      lut_upd_val_tmp <= (others => '0');
      lut_wr_addr     <= (others => '0');
      lut_rd_addr     <= (others => '0');
      lut_wr_en       <= '0';
      lut_upd_cntr    <= (others => '0');
    elsif (clk'event and clk = '1') then
      if (init_table_wr = '0') then                                                      -- always zero unless all pixels are read
        -- if hos interface enables write on fifo
        if (IRAM_WREN = '1') then
          -- if we are after 1 line of 8x8 pixel blocks and we've just read 8 pixels horizzontal from host
          if (wr_mod /= 0 and wr_counter mod 8 = "000") then
            -- MODIFIED INCREASE
            ram_wr_addr     <= magicsplitter(lut_dout_samp, log2(C_MAX_LINE_WIDTH * c_num_lines));
            lut_upd_val_tmp <= magicsplitter(lut_dout_samp, 16);
          -- old messy method from ahmet
          -- ram_wr_addr <=  resize(lut_dout_samp(5 downto 3) * unsigned(IMG_SIZE_X), log2(C_MAX_LINE_WIDTH*C_NUM_LINES))
          --                                                   +
          --                 resize(lut_dout_samp(15 downto 6) * 8, log2(C_MAX_LINE_WIDTH*C_NUM_LINES));
          -- lut_upd_val_tmp <=  resize(lut_dout_samp(5 downto 3) * unsigned(IMG_SIZE_X), 16) + resize(lut_dout_samp(15 downto 6) * 8, 16);
          else                                                                           -- if we are writing the fiRST line of 8x8 block pixels then go on by inreasing the address to ram (ram_wr_addr)
            -- NORMAL INCREASE
            if (wr_counter = 0) then
              ram_wr_addr <= (others => '0');
            else
              ram_wr_addr <= ram_wr_addr + 1;
            end if;
          end if;

          -- ASK data from WR_LUT
          if (wr_mod /= 0 and wr_counter mod 8 = "011") then                             -- if we are after 1 line of 8x8 and on the 4 pixel of pixel_line
            if (wr_counter = unsigned(IMG_SIZE_X) * 8 - 5) then                          --and if we are on the 3 pixel of the last block of pixel_line
              --  1...|0 1 2 3 4 5 6 7|
              --  2...|0 1 2 3 4 5 6 7|
              --  3...|0 1 2 3 4 5 6 7|
              --  4...|0 1 2 3 4 5 6 7|
              --  5...|0 1 2 3 4 5 6 7|
              --  6...|0 1 2 3 4 5 6 7|
              --  7...|0 1 2 3 4 5 6 7|
              --  8...|0 1 2 [3] 4 5 6 7|
              lut_rd_addr <= (others => '0');                                            --read lut from beginning, preparing for next row
            else
              -- |0 1 2 [3] 4 5 6 7||0 1 2 [3] 4 5 6 7||0 1 2 [3] 4 5 6 7|
              --   wr_counter=3       wr_counter=11       wr_counter=19
              lut_rd_addr <= resize((wr_counter + 5) / 8, log2(C_MAX_LINE_WIDTH));       --goes to next address of U_SUB_RAMZ_WR_ADRESS_LUT, effectively an lut_rd_addr++
            end if;
          end if;

          -- GET data from WR_LUT
          if (wr_mod /= 0 and wr_counter mod 8 = "101") then                             -- if we are after 1 line of 8x8 and on the 6 pixel of a block in pixel_line
            -- this because the request of this infromation is done on pixel 4
            lut_dout_samp <= unsigned(lut_dout);
            lut_wr_en     <= '1';

            if (wr_mod = unsigned(IMG_SIZE_Y) / 8 - 1) then                              -- if we are at the final 8x8 block line and (from previous condtion) on the 6th pixel of a block in pixel_line
              -- meaning we are approaching the end of the picture
              lut_wr_addr  <= resize(lut_upd_cntr, log2(C_MAX_LINE_WIDTH));
              lut_din      <= resize(lut_upd_cntr * 8, 16);
              lut_upd_cntr <= lut_upd_cntr + 1;                                          -- can increase from 0 to 8 lines * C_MAX_LINE_WIDTH/8 blocks = C_MAX_LINE_WIDTH
            -- this upper thing rewrites the LUT as from counter_8.txt
            else
              lut_din <= lut_upd_val_tmp;
              if (wr_counter = unsigned(IMG_SIZE_X) * 8 - 2 - 1) then                    -- if we are at the 6th pixel in the last pixel line of the last 8x8 block of a block line that is neither the fiRST or last one (block line)
                lut_wr_addr <= (others => '0');                                          -- pust lut_upd_val_tmp in 0000
              else                                                                       -- if we are in any 6th pixel of any 8x8 block that is not in either the fiRST or last block line
                lut_wr_addr <= resize((wr_counter + 3) / 8 - 1, log2(C_MAX_LINE_WIDTH)); -- (wr_counter + 3) / 8 -1 =
                -- puts data_tmp in the next cell in WR_LUT, something like lut_wr_addr ++
                -- i think this process preapares the position where to put the next pixel that the buffer will accept
              end if;
            end if;
          else
            lut_wr_en <= '0';
          end if;

          -- CONSTANT SIGNALS
          ram_wr_en <= '1';                                                              -- main ram write always one while hostInterface sends data
          ram_din   <= IRAM_WDATA;                                                       -- push pixels line by line to main ram from hostInterface

          wr_counter_total <= wr_counter_total  + 1;                                     -- main counter pixel

          -- if all pixels are read reinitialize
          if (wr_counter_total = unsigned(result) - 1) then
            init_table_wr <= '1';
            lut_upd_cntr  <= (others => '0');
          end if;

          -- if a block line of 8x8 pixels from host was read
          -- wr counter counts from 0 to 8 full pixel lines, aka a full 8x8 block line
          if (wr_counter = unsigned(IMG_SIZE_X) * 8 - 1) then
            wr_counter <= (others => '0');
            wr_mod     <= wr_mod + 1;                                                    --increase counter to next line of 8x8 blocks from host image
          else
            wr_counter <= wr_counter + 1;
          end if;
        else
          ram_wr_en <= '0';
        end if;

        -- start of frame stuff: like a second reset
        if (SOF = '1') then
          wr_counter       <= (others => '0');
          wr_counter_total <= (others => '0');
          wr_mod           <= (others => '0');
          ram_wr_addr      <= (others => '0');

          ram_wr_en       <= '0';
          init_table_wr   <= '0';
          lut_dout_samp   <= (others => '0');
          lut_upd_val_tmp <= (others => '0');
          lut_wr_addr     <= (others => '0');
          lut_rd_addr     <= (others => '0');
          lut_wr_en       <= '0';
          lut_upd_cntr    <= (others => '0');
        end if;
      end if;
    end if;

  end process HOST_INTERFACE_FILL_RAM_P;

  FIFO_FULLNES_P : process (clk, rst) is
  begin

    if (rst = '1') then
      fifo_almost_full_i <= '0';
      FDCT_FIFO_HF_FULL  <= '0';
    elsif (clk'event and clk = '1') then
      if (fifo_almost_full_i = '0' and (wr_counter_total - rd_counter_total) = unsigned(IMG_SIZE_X) * 8 - 2) then -- if the number of written pixels inside the buffer is equal to the ones already read + a full 8x8 block line buffer then fifo is almost full
        fifo_almost_full_i <= '1';
      end if;

      if (fifo_almost_full_i = '1' and (wr_counter_total - rd_counter_total) < unsigned(threshold)) then
        fifo_almost_full_i <= '0';
      end if;

      if (wr_counter = unsigned(IMG_SIZE_X) * 8 - 1) then
        FDCT_FIFO_HF_FULL <= '1';
      end if;

      if (rd_counter = unsigned(IMG_SIZE_X) * 8 - 1) then
        FDCT_FIFO_HF_FULL  <= '0';
        fifo_almost_full_i <= '0';
      end if;

      if (SOF = '1') then
        fifo_almost_full_i <= '0';
        FDCT_FIFO_HF_FULL  <= '0';
      end if;
    end if;

  end process FIFO_FULLNES_P;

  FDCT_READ_REAM_P : process (clk, rst) is
  begin

    if (rst = '1') then
      FDCT_FIFO_Q      <= (others => '0');
      ram_dout_samp    <= (others => '0');
      rd_counter       <= (others => '0');
      rd_counter_total <= (others => '0');
      rd_mod           <= x"0001";
      ram_rd_addr      <= (others => '0');
      init_table_rd    <= '0';
      lut_dout_samp2   <= (others => '0');
      lut_upd_val_tmp2 <= (others => '0');
      lut_wr_addr2     <= (others => '0');
      lut_rd_addr2     <= (others => '0');
      lut_wr_en2       <= '0';
      lut_upd_cntr2    <= (others => '0');
    elsif (clk'event and clk = '1') then
      if (init_table_rd = '0') then
        if (FDCT_FIFO_RD = '1') then
          if (rd_counter mod 8 = "000") then
            ram_rd_addr      <= magicsplitter(lut_dout_samp2, log2(C_MAX_LINE_WIDTH * c_num_lines));
            lut_upd_val_tmp2 <= magicsplitter(lut_dout_samp2, 16);
          -- old messy method of ahmet
          -- ram_rd_addr <= resize(lut_dout_samp2(5 downto 3) * unsigned(IMG_SIZE_X),
          --                       log2(C_MAX_LINE_WIDTH*C_NUM_LINES)) + resize(lut_dout_samp2(15 downto 6) * 8, log2(C_MAX_LINE_WIDTH*C_NUM_LINES));
          -- lut_upd_val_tmp2 <=  resize(lut_dout_samp2(5 downto 3) * unsigned(IMG_SIZE_X), 16) + resize(lut_dout_samp2(15 downto 6) * 8, 16);
          else
            ram_rd_addr <= ram_rd_addr + 1;
          end if;

          if (rd_counter mod 8 = "011") then
            if (rd_counter = unsigned(IMG_SIZE_X) * 8 - 5) then
              lut_rd_addr2 <= (others => '0');
            else
              lut_rd_addr2 <= resize(unsigned(rd_counter + 5) / 8, log2(C_MAX_LINE_WIDTH));
            end if;
          end if;

          if (rd_counter mod 8 = "101") then
            lut_dout_samp2 <= unsigned(lut_dout2);
            lut_wr_en2     <= '1';
            if (rd_mod = unsigned(IMG_SIZE_Y) / 8) then
              lut_din2      <= resize(lut_upd_cntr2 * 8, 16);
              lut_wr_addr2  <= resize(lut_upd_cntr2, log2(C_MAX_LINE_WIDTH));
              lut_upd_cntr2 <= lut_upd_cntr2 + 1;
            else
              lut_din2 <= lut_upd_val_tmp2;
              if (rd_counter = unsigned(IMG_SIZE_X) * 8 - 3) then
                lut_wr_addr2 <= (others => '0');
              else
                lut_wr_addr2 <= resize(unsigned(rd_counter + 3) / 8 - 1, log2(C_MAX_LINE_WIDTH));
              end if;
            end if;
          else
            lut_wr_en2 <= '0';
          end if;

          rd_counter_total <= rd_counter_total  + 1;

          if (rd_counter_total = unsigned(result) - 1) then
            init_table_rd <= '1';
            lut_upd_cntr2 <= (others => '0');
          end if;

          if (rd_counter = unsigned(IMG_SIZE_X) * 8 - 1) then
            rd_counter <= (others => '0');
            rd_mod     <= rd_mod + 1;
          else
            rd_counter <= rd_counter + 1;
          end if;
        end if;

        if (SOF = '1') then
          FDCT_FIFO_Q      <= (others => '0');
          ram_dout_samp    <= (others => '0');
          rd_counter       <= (others => '0');
          rd_counter_total <= (others => '0');
          rd_mod           <= x"0001";
          ram_rd_addr      <= (others => '0');
          init_table_rd    <= '0';
          lut_dout_samp2   <= (others => '0');
          lut_upd_val_tmp2 <= (others => '0');
          lut_wr_addr2     <= (others => '0');
          lut_rd_addr2     <= (others => '0');
          lut_wr_en2       <= '0';
          lut_upd_cntr2    <= (others => '0');
        end if;
      end if;

      ram_dout_samp <= ram_dout;
      if (C_PIXEL_BITS = 16) then
        FDCT_FIFO_Q <= (ram_dout_samp(15 downto 11) & "000" &
                        ram_dout_samp(10 downto 5) & "00" &
                        ram_dout_samp(4 downto 0) & "000");
      else
        FDCT_FIFO_Q <= ram_dout_samp;
      end if;
    end if;

  end process FDCT_READ_REAM_P;

end architecture RTL;
