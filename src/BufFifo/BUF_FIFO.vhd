-------------------------------------------------------------------------------
-- File Name : BUF_FIFO.vhd
--
-- Project   : JPEG_ENC
--
-- Module    : BUF_FIFO
--
-- Content   : Input FIFO Buffer
--
-- Description : 
--
-- Spec.     : 
--
-- Author    : Michal Krepa
--
-------------------------------------------------------------------------------
-- History :
-- 20090311: (MK): Initial Creation.
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
----------------------------------- LIBRARY/PACKAGE ---------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- generic packages/libraries:
-------------------------------------------------------------------------------
library ieee;
	use IEEE.STD_LOGIC_1164.all;
  use ieee.numeric_std.all;

-------------------------------------------------------------------------------
-- user packages/libraries:
-------------------------------------------------------------------------------
library work;
  use work.JPEG_PKG.all;
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
----------------------------------- ENTITY ------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
entity BUF_FIFO is
  port 
  (
        CLK                : in  std_logic;
        RST                : in  std_logic;
        -- HOST PROG
        img_size_x         : in  std_logic_vector(15 downto 0);
        img_size_y         : in  std_logic_vector(15 downto 0);
        sof                : in  std_logic;
        
        -- HOST DATA
        iram_wren          : in  std_logic;
        iram_wdata         : in  std_logic_vector(C_PIXEL_BITS-1 downto 0);
        fifo_almost_full   : out std_logic;
        
        -- FDCT
        fdct_fifo_rd       : in  std_logic;
        fdct_fifo_q        : out std_logic_vector(23 downto 0);
        fdct_fifo_hf_full  : out std_logic
    );
end entity BUF_FIFO;

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
----------------------------------- ARCHITECTURE ------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
architecture RTL of BUF_FIFO is

  
  --constant C_NUM_LINES    : integer := 8 + C_EXTRA_LINES;
  -- No Exstra Lines
  constant C_NUM_LINES    : integer := 8;
  
  --signal ramq             : STD_LOGIC_VECTOR(C_PIXEL_BITS-1 downto 0);
  signal q                : STD_LOGIC_VECTOR(C_PIXEL_BITS-1 downto 0);
  --signal ramd             : STD_LOGIC_VECTOR(C_PIXEL_BITS-1 downto 0);
  signal ram_data         : STD_LOGIC_VECTOR(C_PIXEL_BITS-1 downto 0);
  
  signal wr_ptr           : unsigned(log2(C_MAX_LINE_WIDTH*C_NUM_LINES)-1 downto 0);
  signal ram_write        : STD_LOGIC;
  signal rd_ptr           : unsigned(log2(C_MAX_LINE_WIDTH*C_NUM_LINES)-1 downto 0);
  
  signal wr_addr          : unsigned(log2(C_MAX_LINE_WIDTH)-1 downto 0);
  signal we               : STD_LOGIC;
  signal rd_addr          : unsigned(log2(C_MAX_LINE_WIDTH)-1 downto 0);
  
  signal data_out         : STD_LOGIC_VECTOR(15 downto 0);
  --signal ramd             : STD_LOGIC_VECTOR(C_PIXEL_BITS-1 downto 0);
  signal data_in          : unsigned(15 downto 0);
  
  
  signal wr_addr2         : unsigned(log2(C_MAX_LINE_WIDTH)-1 downto 0);
  signal we2              : STD_LOGIC;
  signal rd_addr2         : unsigned(log2(C_MAX_LINE_WIDTH)-1 downto 0);
  
  signal data_out2        : STD_LOGIC_VECTOR(15 downto 0);
  --signal ramd             : STD_LOGIC_VECTOR(C_PIXEL_BITS-1 downto 0);
  signal data_in2         : unsigned(15 downto 0);
  
  signal pix_inblk_cnt    : unsigned(3 downto 0);
  signal pix_inblk_cnt_d1 : unsigned(3 downto 0);
  signal line_inblk_cnt   : unsigned(2 downto 0);
  
  signal read_block_cnt   : unsigned(12 downto 0);
  signal read_block_cnt_d1: unsigned(12 downto 0);
  signal write_block_cnt  : unsigned(12 downto 0);
  
  signal ramraddr_int     : unsigned(16+log2(C_NUM_LINES)-1 downto 0);
  signal raddr_base_line  : unsigned(16+log2(C_NUM_LINES)-1 downto 0);
  signal raddr_tmp        : unsigned(15 downto 0);
  --signal ramwaddr_d1      : unsigned(ramwaddr'range);
  
  signal line_lock        : unsigned(log2(C_NUM_LINES)-1 downto 0);
  
  signal memwr_line_cnt   : unsigned(log2(C_NUM_LINES)-1 downto 0);
  
  signal memrd_offs_cnt   : unsigned(log2(C_NUM_LINES)-1+1 downto 0);
  signal memrd_line       : unsigned(log2(C_NUM_LINES)-1 downto 0);
  
  signal wr_line_idx      : unsigned(15 downto 0);
  signal rd_line_idx      : unsigned(15 downto 0);
  
  signal image_write_end  : std_logic;  
  
  signal result 				  : std_logic_vector(31 downto 0); -- the number of pixels in input
  signal threshold				: std_logic_vector(31 downto 0); -- pixel threshold when fifo is considered full
  
  signal wr_counter       : unsigned(15 downto 0);
  signal rd_counter       : unsigned(15 downto 0);
  
  signal wr_mod           : unsigned(15 downto 0);
  signal rd_mod           : unsigned(15 downto 0);
  
  signal wr_counter_total : unsigned(31 downto 0);
  signal rd_counter_total : unsigned(31 downto 0);
  signal counter          : unsigned(31 downto 0);
  signal counter2         : unsigned(31 downto 0);
  
  signal init_table_wr    : std_logic;
  signal init_table_rd    : std_logic;
  
  signal do1              : unsigned(15 downto 0);
  signal do2              : unsigned(15 downto 0);
  signal data_temp        : unsigned(15 downto 0);
  signal data_temp2       : unsigned(15 downto 0);
  
  signal temp 				    : std_logic_vector(23 downto 0);
	
	signal fifo_almost_full_i: std_logic;
	
	
  
-------------------------------------------------------------------------------
-- Architecture: begin
-------------------------------------------------------------------------------
begin  
  -------------------------------------------------------------------
  -- RAM for SUB_FIFOs
  -------------------------------------------------------------------
  fifo_almost_full <= fifo_almost_full_i;
  
  U_SUB_RAMZ : entity work.SUB_RAMZ
  generic map 
  (
           RAMADDR_W => log2( C_MAX_LINE_WIDTH*C_NUM_LINES ),
           RAMDATA_W => C_PIXEL_BITS        
  )   
  port map 
  (      
        d            => ram_data,               
        waddr        => std_logic_vector(wr_ptr),     
        raddr        => std_logic_vector(rd_ptr),     
        we           => ram_write,     
        clk          => clk,     
        
        q            => q     
  ); 
  
  MULTIPLIER : entity work.multiplier
  PORT MAP(
		CLK => CLK,
		RST => RST,
		img_size_x => img_size_x,
		img_size_y => img_size_y,
		result => result,
		threshold => threshold
	);
	
	U_SUB_RAMZ_WR_ADRESS_LUT : entity work.SUB_RAMZ_LUT
  generic map 
  (
           RAMADDR_W => log2( C_MAX_LINE_WIDTH ),
           RAMDATA_W => 16        
  )   
  port map 
  (      
        d            => std_logic_vector(data_in),               
        waddr        => std_logic_vector(wr_addr),     
        raddr        => std_logic_vector(rd_addr),     
        we           => we,     
        clk          => CLK,     
        
        q            => data_out
  ); 
  
  U_SUB_RAMZ_RD_ADRESS_LUT : entity work.SUB_RAMZ_LUT
  generic map 
  (
           RAMADDR_W => log2( C_MAX_LINE_WIDTH ),
           RAMDATA_W => 16        
  )   
  port map 
  (      
        d            => std_logic_vector(data_in2),               
        waddr        => std_logic_vector(wr_addr2),     
        raddr        => std_logic_vector(rd_addr2),     
        we           => we2,     
        clk          => CLK,     
        
        q            => data_out2
  ); 
  
  
  HOST_INTERFACE_FILL_RAM_P: process (CLK, RST)
  begin
    if (RST = '1') then
      wr_counter <= (others => '0');
      wr_counter_total <= (others => '0');
      wr_mod <= (others => '0');
      wr_ptr <= (others => '0'); --main ram wr address
      
      ram_write <= '0';
      init_table_wr <= '0';
      do1 <= (others => '0');
      data_temp <= (others => '0');
      wr_addr <= (others => '0');
      rd_addr <= (others => '0');
      we <= '0';
      counter <= (others => '0');
    elsif (CLK'event and CLK = '1') then
      if (init_table_wr = '0') then -- always zero unless all pixels are read

        -- if hos interface enables write on fifo
        if (iram_wren = '1') then
            
          -- if we are after 1 line of 8x8 pixel blocks and we've just read 8 pixels horizzontal from host
          if (wr_mod /= 0 and wr_counter mod 8 = "000") then
              -- MODIFIED INCREASE
              wr_ptr <= resize(do1(5 downto 3) * unsigned(img_size_x), log2(C_MAX_LINE_WIDTH*C_NUM_LINES)) + resize(do1(15 downto 6) * 8, log2(C_MAX_LINE_WIDTH*C_NUM_LINES));
              data_temp <=  resize(do1(5 downto 3) * unsigned(img_size_x), 16) + resize(do1(15 downto 6) * 8, 16);
            
            --wr_ptr <= do1 / 8 mod 8 *  unsigned(img_size_x) + do1 / 8 / 8 * 8;
            --data_temp <= do1 / 8 mod 8 *  unsigned(img_size_x) + do1 / 8 / 8 * 8;
          else -- if we are writing the first line of 8x8 block pixels then go on by inreasing the address to ram (wr_ptr) 
            -- NORMAL INCREASE
            if (wr_counter = 0) then
              wr_ptr <= (others => '0');
            else
              wr_ptr <= wr_ptr + 1;
            end if;
          end if;
            
          -- ASK data from WR_LUT
          if (wr_mod /= 0 and wr_counter mod 8 = "011") then -- if we are after 1 line of 8x8 and on the 4 pixel of pixel_line
            if (wr_counter = unsigned(img_size_x) * 8 - 5) then --and if we are on the 3 pixel of the last block of pixel_line
              --  1...|0 1 2 3 4 5 6 7|
              --  2...|0 1 2 3 4 5 6 7|
              --  3...|0 1 2 3 4 5 6 7|
              --  4...|0 1 2 3 4 5 6 7|
              --  5...|0 1 2 3 4 5 6 7|
              --  6...|0 1 2 3 4 5 6 7|
              --  7...|0 1 2 3 4 5 6 7|
              --  8...|0 1 2 [3] 4 5 6 7|
              rd_addr <= (others => '0'); --read lut from beginning, preparing for next row
            else
              -- |0 1 2 [3] 4 5 6 7||0 1 2 [3] 4 5 6 7||0 1 2 [3] 4 5 6 7|
              --   wr_counter=3       wr_counter=11       wr_counter=19
              rd_addr <= resize((wr_counter + 5) / 8, log2(C_MAX_LINE_WIDTH)); --goes to next address of U_SUB_RAMZ_WR_ADRESS_LUT, effectively an rd_addr++
            end if;
          end if;
          
          -- GET data from WR_LUT
          if (wr_mod /= 0 and wr_counter mod 8 = "101") then -- if we are after 1 line of 8x8 and on the 6 pixel of a block in pixel_line 
                                                             -- this because the request of this infromation is done on pixel 4
            do1 <= unsigned(data_out);
            we <= '1';
        
            if (wr_mod = unsigned(img_size_y) / 8 - 1) then -- if we are at the final 8x8 block line and (from previous condtion) on the 6th pixel of a block in pixel_line
                                                            -- meaning we are approaching the end of the picture 
              wr_addr <= resize(counter, log2(C_MAX_LINE_WIDTH));
              data_in <= resize(counter * 8, 16);
              counter <= counter + 1; -- can increase from 0 to 8 lines * C_MAX_LINE_WIDTH/8 blocks = C_MAX_LINE_WIDTH
              -- this upper thing rewrites the LUT as from counter_8.txt
            else
              data_in <= data_temp;
              if (wr_counter = unsigned(img_size_x) * 8 - 2 -1) then -- if we are at the 6th pixel in the last pixel line of the last 8x8 block of a block line that is neither the first or last one (block line)
                wr_addr <= (others => '0'); -- pust data_temp in 0000
              else -- if we are in any 6th pixel of any 8x8 block that is not in either the first or last block line
                wr_addr <= resize((wr_counter + 3) / 8 - 1, log2(C_MAX_LINE_WIDTH)); -- (wr_counter + 3) / 8 -1 = 
                -- puts data_tmp in the next cell in WR_LUT, something like wr_addr ++
                -- i think this process preapares the position where to put the next pixel that the buffer will accept
              end if;
            end if;
          else
            we <= '0';
          end if;

          -- CONSTANT SIGNALS
          ram_write <= '1'; -- main ram write always one while hostInterface sends data
          ram_data <= iram_wdata; -- push pixels line by line to main ram from hostInterface

          wr_counter_total <= wr_counter_total  + 1; -- main counter pixel

          -- if all pixels are read reinitialize 
          if (wr_counter_total = unsigned(result) - 1) then 
            init_table_wr <= '1';
            counter <= (others => '0');
          end if;
                          
          -- if a block line of 8x8 pixels from host was read
          -- wr counter counts from 0 to 8 full pixel lines, aka a full 8x8 block line
          if (wr_counter = unsigned(img_size_x) * 8 - 1)	then		
            wr_counter <= (others => '0');
            wr_mod <= wr_mod + 1; --increase counter to next line of 8x8 blocks from host image
          else
            wr_counter <= wr_counter + 1;
          end if;
        else
          ram_write <= '0';
        end if;
       
        -- start of frame stuff: like a second reset
        if sof = '1' then
          wr_counter <= (others => '0');
          wr_counter_total <= (others => '0');
          wr_mod <= (others => '0');
          wr_ptr <= (others => '0');
          
          ram_write <= '0';
          init_table_wr <= '0';
          do1 <= (others => '0');
          data_temp <= (others => '0');
          wr_addr <= (others => '0');
          rd_addr <= (others => '0');
          we <= '0';
          counter <= (others => '0');
        end if;
        
      end if;
      
    end if;
  end process;
  
	FIFO_FULLNES_P: process (CLK, RST)
	begin
		if (RST = '1') then
			fifo_almost_full_i <= '0';
			fdct_fifo_hf_full <= '0';
		elsif (CLK'event and CLK = '1') then
				if (fifo_almost_full_i = '0' and wr_counter_total  = rd_counter_total + unsigned(img_size_x)*8-2) then
				
					fifo_almost_full_i <= '1';
					
				end if;
				
				if (fifo_almost_full_i = '1' and wr_counter_total < rd_counter_total + unsigned(threshold) ) then
				
					fifo_almost_full_i <= '0';
					
				end if;
				
				if (wr_counter = unsigned(img_size_x) * 8 - 1) then
				
					fdct_fifo_hf_full <= '1';
				end if;
				
				if (rd_counter = unsigned(img_size_x) * 8 - 1) then
			
					fdct_fifo_hf_full <= '0';
					fifo_almost_full_i <= '0';
				end if;
        
        if sof = '1' then
          fifo_almost_full_i <= '0';
			    fdct_fifo_hf_full <= '0';
        end if;
        
		end if;
	end process;
	
	
	
	FDCT_READ_REAM_P: process (CLK, RST)
	begin
		if (RST = '1') then
			fdct_fifo_q <= (others => '0');
			temp <= (others => '0');
			rd_counter <= (others => '0');
			rd_counter_total <= (others => '0');
			rd_mod <= x"0001";
			rd_ptr <= (others => '0');
			init_table_rd <= '0';
			do2 <= (others => '0');
			data_temp2 <= (others => '0');
			wr_addr2 <= (others => '0');
			rd_addr2 <= (others => '0');
			we2 <= '0';
			counter2 <= (others => '0');
		elsif (CLK'event and CLK = '1') then
			if (init_table_rd = '0') then
				if (fdct_fifo_rd = '1') then
					if (rd_counter mod 8 = "000") then
						--rd_ptr <=  resize(do2(5 downto 3) *  unsigned(img_size_x) + do2(15 downto 7) * 8, log2(C_MAX_LINE_WIDTH*C_NUM_LINES));
						--data_temp2 <=  resize(do2(5 downto 3) *  unsigned(img_size_x) + do2(15 downto 7) * 8, 16);
						rd_ptr <= resize(do2(5 downto 3) * unsigned(img_size_x), log2(C_MAX_LINE_WIDTH*C_NUM_LINES)) + resize(do2(15 downto 6) * 8, log2(C_MAX_LINE_WIDTH*C_NUM_LINES));
						data_temp2 <=  resize(do2(5 downto 3) * unsigned(img_size_x), 16) + resize(do2(15 downto 6) * 8, 16);
					else
						rd_ptr <= rd_ptr + 1;
					end if;
					
					if (rd_counter mod 8 = "011") then
						if (rd_counter = unsigned(img_size_x) * 8 - 5) then
							rd_addr2 <= (others => '0');
						else
							rd_addr2 <= resize(unsigned(rd_counter + 5) / 8, log2(C_MAX_LINE_WIDTH));
						end if;
					end if;
					
					if (rd_counter mod 8 = "101") then
						do2 <= unsigned(data_out2);
						we2 <= '1';
						if (rd_mod = unsigned(img_size_y) / 8) then
							data_in2 <= resize(counter2 * 8, 16);
							wr_addr2 <= resize(counter2, log2(C_MAX_LINE_WIDTH));
							counter2 <= counter2 + 1;
						else
							data_in2 <= data_temp2;
							if (rd_counter = unsigned(img_size_x) * 8 - 3) then
								wr_addr2 <= (others => '0');
							else
							
								wr_addr2 <= resize(unsigned(rd_counter + 3) / 8 - 1, log2(C_MAX_LINE_WIDTH));
								
							end if;
						end if;
					else
						we2 <= '0';
					end if;
					
					rd_counter_total <= rd_counter_total  + 1;
			
					if (rd_counter_total = unsigned(result) - 1) then
					
						init_table_rd <= '1';
						counter2 <= (others => '0');
					end if;
					
					if (rd_counter = unsigned(img_size_x) * 8 - 1) then
				
						rd_counter <= (others => '0');
						rd_mod <= rd_mod + 1;
					
					else
						rd_counter <= rd_counter + 1;
					end if;
				
					
				 
				
				end if;
			
				if sof = '1' then
          fdct_fifo_q <= (others => '0');
          temp <= (others => '0');
          rd_counter <= (others => '0');
          rd_counter_total <= (others => '0');
          rd_mod <= x"0001";
          rd_ptr <= (others => '0');
          init_table_rd <= '0';
          do2 <= (others => '0');
          data_temp2 <= (others => '0');
          wr_addr2 <= (others => '0');
          rd_addr2 <= (others => '0');
          we2 <= '0';
          counter2 <= (others => '0');
        end if;
			
			
--				 fdct_fifo_q <= (temp(15 downto 11) & "000" & 
--                 temp(10 downto 5) & "00" & 
--                 temp(4 downto 0) & "000") when C_PIXEL_BITS = 16 else 
--                 std_logic_vector(resize(unsigned(temp), 24));
			end if;
		
			temp <= q;
			if (C_PIXEL_BITS = 16) then
				
				fdct_fifo_q <= (temp(15 downto 11) & "000" & 
				  temp(10 downto 5) & "00" & 
				  temp(4 downto 0) & "000");
			else
				fdct_fifo_q <= temp;	
			end if;	

		end if;
	end process;
  
end architecture RTL;
-------------------------------------------------------------------------------
-- Architecture: end
-------------------------------------------------------------------------------