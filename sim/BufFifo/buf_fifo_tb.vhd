----------------------------------------------------------------------------------
-- Company: 
-- Engineer: Simone Ruffini [simone.ruffini@tutanota.com]
-- 
-- Create Date: 03/16/2021 02:49:33 PM
-- Design Name: 
-- Module Name: buf_fifo_tb - Behavioral
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
----------------------------------------------------------------------------------


----------------------------- PACKAGES/LIBRARIES -------------------------------
library IEEE;
  use IEEE.STD_LOGIC_1164.ALL;
  use IEEE.NUMERIC_STD.ALL;   -- arithmetic functions with Signed or Unsigned values

-- User libraries
library work;
    use work.JPEG_PKG.ALL;

entity buf_fifo_tb is
--  Port ( );
end buf_fifo_tb;

----------------------------- ARCHITECTURE -------------------------------------
architecture Behavioral of buf_fifo_tb is


----------------------------- CONSTANTS ---------------------------------------
constant MASTER_CLK_SPEED_HZ                : INTEGER := 1000000; -- Master clk speed in HZ.

----------------------------- COMMON SIGNALS -----------------------------------
signal MASTER_CLK         : std_logic;
signal MASTER_RST         : std_logic;

signal img_size_x         : std_logic_vector(15 downto 0);
signal img_size_y         : std_logic_vector(15 downto 0);
signal sof                : std_logic;
signal iram_wren          : std_logic;
signal iram_wdata         : std_logic_vector(C_PIXEL_BITS-1 downto 0);
signal fifo_almost_full   : std_logic;
signal fdct_fifo_rd       : std_logic;
signal fdct_fifo_q        : std_logic_vector(23 downto 0);
signal fdct_fifo_hf_full  : std_logic;

begin
  --######################### ENTITY DECALARATION #############################

  CLK_GEN_INSTANCE: entity work.ClkGen
  generic map (
    CLK_HZ            => MASTER_CLK_SPEED_HZ
  )
  port map (
    CLK               => MASTER_CLK,
    RST               => MASTER_RST
  );

	BUF_FIFO_INSTANCE: entity work.BUF_FIFO
	port map (
    -- inputs
    CLK               => MASTER_CLK,
    RST               => MASTER_RST,
    img_size_x        => img_size_x,
    img_size_y        => img_size_y,
    sof               => sof,
    iram_wdata        => iram_wdata,
    iram_wren         => iram_wren,
    fdct_fifo_rd      => fdct_fifo_rd,
    -- outputs
    fifo_almost_full  => fifo_almost_full,
    fdct_fifo_q       => fdct_fifo_q,
    fdct_fifo_hf_full => fdct_fifo_hf_full
	);

  --######################## PROCESSES ########################################
  
  TESTBENCH: process(MASTER_CLK,MASTER_RST)
    variable internal_counter: integer RANGE 0 to 2 ** iram_wdata'length;
  begin
    if MASTER_RST = '1' then
      sof           <= '0';
      img_size_x    <= std_logic_vector(to_unsigned(32,img_size_x'length));
      img_size_y    <= std_logic_vector(to_unsigned(32,img_size_y'length));
      iram_wdata    <= (others => '0'); 
      iram_wren     <= '0'; 
      fdct_fifo_rd  <= '0'; 
      -- variables
      internal_counter := 0;
    elsif rising_edge(MASTER_CLK) then 

      sof <= '0';
			fdct_fifo_rd <= '0';
			      
      if fifo_almost_full = '0' then 
				internal_counter := internal_counter + 1; 
      	if internal_counter = 10 then
        	sof <= '1';
				elsif internal_counter >= 10  then
					iram_wren <= '1';
					iram_wdata <= std_logic_vector(to_unsigned(internal_counter,iram_wdata'length));
				end if;
			else
				iram_wren <= '0';
			end if;
			
			if fdct_fifo_hf_full = '1' then
				fdct_fifo_rd <='1';
			end if;
      
    end if;
   end process;
end Behavioral;
