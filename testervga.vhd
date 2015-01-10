-- Donald Wight and Alejandro Velazques
-- Lab 9 Keyboard to VGA Character Interface
-- EECE 343 Fall 2014

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
entity testervga is
	port ( kb_data, kb_clk, clk_25, clk_50, reset, enable : in std_logic;
			 seg_1a, seg_1b, seg_1c, seg_1d, seg_1e, seg_1f, seg_1g,
			 seg_2a, seg_2b, seg_2c, seg_2d, seg_2e, seg_2f, seg_2g : out std_logic;
			 VGA_Red,VGA_Green,VGA_Blue,VGA_HSync,VGA_VSync,video_blank_out,Video_clock_out:OUT std_logic);
			 end testervga;
architecture test of testervga is
	signal pixel_row_sig, pixel_column_sig								:	STD_LOGIC_VECTOR(9 DOWNTO 0);
	signal scanned, m1, m2, m3, m4, m5, m6, m7, m8 					:	std_logic_vector (7 downto 0);
	signal address 															: 	std_logic_vector ( 5 downto 0);
	signal scanned_ready, mux_out, red_sig,green_sig,blue_sig,
			 vert_sync_sig, horiz_sync_sig, video_ons	 				: 	std_logic;
	COMPONENT vga_sync
		PORT(	clock_50Mhz, red, green, blue		: IN	STD_LOGIC;
					red_out, green_out, blue_out, horiz_sync_out, 
					vert_sync_out, video_on, pixel_clock	: OUT	STD_LOGIC;
					pixel_row, pixel_column: OUT STD_LOGIC_VECTOR(9 DOWNTO 0));
		END COMPONENT;
	component Char_ROM IS
		PORT(	character_address																		: IN	STD_LOGIC_VECTOR(5 DOWNTO 0);
				font_row, font_col																	: IN 	STD_LOGIC_VECTOR(2 DOWNTO 0);
				clk 																						: in std_logic;
				rom_mux_output																			: OUT	STD_LOGIC;
				mux_1, mux_2, mux_3, mux_4, mux_5, mux_6, mux_7, mux_8					: out std_LOGIC_VECTOR(7 downto 0));
	END component;
	COMPONENT ball
		PORT(pixel_row, pixel_column						: IN std_logic_vector(9 DOWNTO 0);
			video_on												:in std_logic;
        Red,Green, Blue										: OUT std_logic;
        Vert_sync												: IN std_logic;
		  m_1, m_2, m_3, m_4, m_5, m_6, m_7, m_8		: in std_LOGIC_VECTOR(7 downto 0));
		END COMPONENT;
	component seg_7_display
		port ( char_in : in std_logic_vector (3 downto 0);
				 scan_code : in std_logic;
				 seg_a, seg_b, seg_c, seg_d, seg_e, seg_f, seg_g :out std_logic);
	end component;
	component character_out
		port( keyboard_clk, keyboard_data, clock_25Mhz,
				reset, read		:	in	std_logic;
				scan_code		:	out std_logic_vector (7 downto 0);
				scan_ready 		:	out std_logic);
	end component;
	component add 
		port ( clk							: in std_logic;
				 input_scan				: in std_logIC_VECTOR (7 downto 0);
				 output_address			: out std_logic_vector (5 downto 0));
	end component;
	begin
	char	:	character_out 	port map (kb_clk, kb_data, clk_25 ,reset, enable, scanned, scanned_ready);
	seg1  :	seg_7_display 	port map (scanned (7 downto 4), scanned_ready, seg_2a, seg_2b, seg_2c, seg_2d, seg_2e, seg_2f, seg_2g);
	seg2  :	seg_7_display 	port map (scanned (3 downto 0), scanned_ready, seg_1a, seg_1b, seg_1c, seg_1d, seg_1e, seg_1f, seg_1g);
	sync0 :	vga_sync 		port map (clk_50,red_sig,green_sig,blue_sig,VGA_Red,VGA_Green,VGA_Blue,
												 horiz_sync_sig, vert_sync_sig,video_ons,Video_clock_out,pixel_row_sig, 
												 pixel_column_sig);
   ball0 :	ball				port map (pixel_row_sig, pixel_column_sig, video_ons ,red_sig,green_sig,blue_sig, horiz_sync_sig, m1, m2, m3, m4, m5,m6, m7, m8);
	rom	:	Char_ROM		 	port map (address, pixel_row_sig(9 downto 7), pixel_column_sig(9 downto 7), clk_50, mux_out, m1, m2, m3, m4, m5,m6, m7, m8);
	dec	: 	add				port map (clk_50, scanned, address);
	VGA_VSync<=vert_sync_sig;
	VGA_HSync<=horiz_sync_sig;
	video_blank_out <= video_ons;
end test;
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
entity add is
	port ( clk				 : 	in std_logic;
			 input_scan 	 : 	in std_logic_vector (7 downto 0);
			 output_address : 	out std_logic_vector (5 downto 0));
end add;
architecture do of add is
	begin
		process (input_scan)
		begin
				case input_scan is
					--a
					when "00011100" =>
						output_address <= "000000";
						
					--b
					when "00110010" =>
						output_address <= "000001";
					--c
					when "00100001" =>
						output_address <= "000010";
					--d
					when "00100011" =>
						output_address <= "000011";
					--e
					when "00100100" =>
						output_address <= "000100";
					--f
					when "00101011" =>
						output_address <= "000101";
					--g
					when "00110100" =>
						output_address <= "000110";
					--h
					when "00110011" =>
						output_address <= "000111";
					--i
					when "01000011" =>
						output_address <= "001000";
					--j
					when "00111011" =>
						output_address <= "001001";
					--k
					when "01000010" =>
						output_address <= "001010";
					--l
					when "01001011" =>
						output_address <= "001011";
					--m
					when "00111010" =>
						output_address <= "001100";
					--n
					when "00110001" =>
						output_address <= "001101";
					--o
					when "01000100" =>
						output_address <= "001110";	
					--p
					when "01001101" =>
						output_address <= "001111";
					--q
					when "00010101" =>
						output_address <= "010000";
					--r
					when "00101101" =>
						output_address <= "010001";
					--s
					when "00011011" =>
						output_address <= "010010";
					--t
					when "00101100" =>
						output_address <= "010011";
					--u
					when "00111100" =>
						output_address <= "010100";
					when "00101010" =>
						output_address <= "010101";
					--w
					when "00011101" =>
						output_address <= "010110";
					--x
					when "00100010" =>
						output_address <= "010111";
					--y
					when "00110101" =>
						output_address <= "011000";
					--z
					when "00011010" =>
						output_address <= "011001";
					--0
					when "01000101" =>
						output_address <= "011010";
					--1
					when "00010110" =>
						output_address <= "011011";
					--2
					when "00011110" =>
						output_address <= "011100";
					--3
					when "00100110" =>
						output_address <= "011101";
					--4
					when "00100101" =>
						output_address <= "011110";
					--5
					when "00101110" =>
						output_address <= "011111";
					--6
					when "00110110" =>
						output_address <= "100000";
					--7
					when "00111101" =>
						output_address <= "100001";
					--8
					when "00111110" =>
						output_address <= "100010";
					--9
					when "01000110" =>
						output_address <= "100011";
					WHEN OTHers =>
						output_address <="100101";
					end case;
			end process;
	end do;
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
entity seg_7_display is
	port( char_in : std_logic_vector (3 downto 0);
			scan_code : std_logic;
			seg_a, seg_b, seg_c, seg_d, seg_e, seg_f, seg_g :out std_logic);
end seg_7_display;

architecture func of seg_7_display is
	signal segment_data1 : std_logic_vector (6 downto 0);
	begin
		process (char_in)
			begin
				case char_in is
					when "0000"=>
				segment_data1<="1111110";
				when "0001"=>
				segment_data1<="0110000";
				when "0010"=>
				segment_data1<="1101101";
				when "0011"=>
				segment_data1<="1111001";
				when "0100"=>
				segment_data1<="0110011";
				when "0101" =>
				segment_data1 <="1011011";
				when "0110" =>
				segment_data1<="1011111";
				when "0111"=> 
				segment_data1<= "1110000";
				when "1000" => 
				segment_data1<= "1111111";
				when "1001" =>
				segment_data1 <= "1100111";
				when "1010" => 
				segment_data1 <= "1110111";
				when "1011" =>
				segment_data1<= "0011111";
				when "1100" =>
				segment_data1 <= "1001110";
				when "1101" => 
				segment_data1 <="0111101";
				when "1110" =>
				segment_data1 <="1001111";
				when others =>
				segment_data1 <= "1000111";
				end case;
	end process;
	seg_a <=not segment_data1(6);
	seg_b <=not segment_data1(5);
	seg_c <=not segment_data1(4);
	seg_d <=not segment_data1(3);
	seg_e <=not segment_data1(2);
	seg_f <=not segment_data1(1);
	seg_g <=not segment_data1(0);
end func;

--
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
entity character_out is
	-- keyboard_clk is one line on  from the keynoard
	-- keyboard data is the 2nd line that comes in as a serial input
	-- scan code is they keyboard data that we will work with for character development
	-- scan ready is to ensure each command is read only once/ when it goes high we read else we dont
	-- read is used to clear the scan ready 
	port( keyboard_clk, keyboard_data, clock_25Mhz,
			reset, read		:	in	std_logic;
			scan_code		:out std_logic_vector (7 downto 0);
			scan_ready 		:out std_logic);
end character_out;

architecture beh of character_out is
	-- incnt is used to read in the 8 bits of data
	-- shift in is used to read in the data in a serial fashion shifting them in
	-- read char is used to communicate between 
	signal incnt							:	std_logic_vector (3 downto 0);
	signal shiftin							:	std_logic_vector (8 downto 0);
	signal read_char						:	std_logic;
	signal inflag, ready_set			:	std_logic;
	signal keyboard_clk_filtered		:	std_logic;
	signal filter							:	std_logic_vector (7 downto 0);
	
	begin
		process (read, ready_set)
			begin
				if read = '1' then
					scan_ready <= '0';
				elsif ready_set'EVENT and ready_set = '1' then
					scan_ready <= '1';
				end if;
		end process;
		
		--process will filter the clk signal from 
		--keyboard using shift reg and two and gates
		clk_filter: process
			begin
				wait until clock_25Mhz'EVENT and clock_25Mhz = '1';
				filter(6 downto 0) <= filter (7 downto 1);
				filter(7) <= keyboard_clk;
				if filter = "00000000" then
					keyboard_clk_filtered <= '0';
				elsif filter = "11111111" then
					keyboard_clk_filtered <= '1';
				end if;
		end process clk_filter;
		
		-- Process will read in serial scan code from
		-- keyboard
		process
			begin
				wait until (keyboard_clk_filtered'EVENT and keyboard_clk_filtered = '1');
				if reset = '1' then
					incnt		 				<= "0000";
					read_char 				<= '0';
				else
					if keyboard_data = '0' and read_char = '0' then
						read_char 			<= '1';
						ready_set 			<= '0';
					else
						-- shift next 8 data bits to get scan code
						if read_char = '1' then
							if incnt < "1001" then
								incnt 					<= incnt +1;
								shiftin (7 downto 0) <= shiftin (8 downto 1);
								shiftin(8) 				<= keyboard_data;
								ready_set 	<= '0';
								--end of scan code char so we set
								--flags to exit loop
							else
								scan_code 	<= shiftin(7 downto 0);
								read_char 	<= '0';
								ready_set 	<='1';
								incnt 		<= "0000";
							end if;
						end if;
					end if;
				end if;
			end process;
end beh;


--
library IEEE;
use  IEEE.STD_LOGIC_1164.all;
use  IEEE.STD_LOGIC_ARITH.all;
use  IEEE.STD_LOGIC_UNSIGNED.all;
LIBRARY lpm;
USE lpm.lpm_components.ALL;
ENTITY Char_ROM IS
	PORT(	character_address														: IN	STD_LOGIC_VECTOR(5 DOWNTO 0);
			font_row, font_col													: IN 	STD_LOGIC_VECTOR(2 DOWNTO 0);
			clk																		: in std_logic;
			rom_mux_output															: OUT	STD_LOGIC;
			mux_1, mux_2, mux_3, mux_4, mux_5, mux_6, mux_7, mux_8	: out std_logic_vector (7 downto 0));
END Char_ROM;

ARCHITECTURE a OF Char_ROM IS
	SIGNAL	rom_data: STD_LOGIC_VECTOR(7 DOWNTO 0);
	SIGNAL	rom_address: STD_LOGIC_VECTOR(8 DOWNTO 0);
	signal 	fonts			: std_logic_vector(2 downto 0):="000";
BEGIN
-- Small 8 by 8 Character Generator ROM for Video Display
-- Each character is 8 8-bits words of pixel data
 char_gen_rom: lpm_rom
      GENERIC MAP ( lpm_widthad => 9,
        lpm_numwords => 512,
        lpm_outdata => "UNREGISTERED",
        lpm_address_control => "REGISTERED",
-- Reads in mif file for character generator font data 
			lpm_file => "chars.mif",
         lpm_width => 8)
      PORT MAP ( address => rom_address, inclock => clk, q => rom_data);
		rom_address <= character_address & fonts;
-- Mux to pick off correct rom data bit from 8-bit word
-- for on screen character generation
rom_mux_output <= rom_data ( (CONV_INTEGER(NOT font_row(2 downto 0))));
	process --(character_address)
	begin
		wait until clk'EVENT and clk = '1';
		if (fonts = "000") then
			mux_1<=rom_Data;
			fonts <= "001";
		elsif fonts = "001" then	
			mux_2<=rom_Data;
			fonts <= "010";
		elsif fonts = "010" then
			mux_3<=rom_Data;
			fonts <= "011";
		elsif fonts = "011" then
			mux_4<=rom_Data;
			fonts <= "100"; 
		elsif fonts = "100" then
			mux_5<=rom_Data;
			fonts <= "101";
		elsif fonts = "101" then
			mux_6<=rom_Data;
			fonts <= "110";
		elsif fonts = "110" then
			mux_7<=rom_Data;
			fonts <= "111";
		elsif fonts = "111" then
			mux_8<=rom_Data;
			fonts <= "000";
		end if;
	end process;

END a;
--
--LIBRARY ieee;
--USE ieee.std_logic_1164.all;
--USE ieee.std_logic_arith.all;
--USE ieee.std_logic_unsigned.all;
--PACKAGE Char_Rom_Package IS
--COMPONENT Char_Rom
--PORT(	character_address			: IN	STD_LOGIC_VECTOR(5 DOWNTO 0);
--			font_row, font_col			: IN 	STD_LOGIC_VECTOR(2 DOWNTO 0);
--			clk							: in std_logic;
--			rom_mux_output	: OUT	STD_LOGIC);
--END COMPONENT;
--END Char_Rom_Package;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.all;
USE  IEEE.STD_LOGIC_ARITH.all;
USE  IEEE.STD_LOGIC_UNSIGNED.all; 

ENTITY ball IS
   PORT(pixel_row, pixel_column		: IN std_logic_vector(9 DOWNTO 0);
		  video_on							: in std_logic;
        Red,Green, Blue					: OUT std_logic;
        Vert_sync							: IN std_logic;
		  m_1, m_2, m_3, m_4, m_5, m_6, m_7, m_8 : in std_logic_vector (7 downto 0));		
END ball;

architecture behavior of ball is 
--signal sel 	: std_logic_vector (7 downto 0); 
BEGIN           
	RGB_Display: Process (pixel_column, pixel_row)
	variable sel 	: std_logic_vector (7 downto 0);
	BEGIN
				-- Set Ball_on ='1' to display ball
		if (vert_sync = '1') then
		if pixel_row >= 200 and  pixel_row <= 204  then
				sel := m_1;
		elsif pixel_row >= 205 and pixel_row <= 209 then
				sel := m_2;
		elsif pixel_row >= 210 and pixel_row <= 214 then
			sel := m_3;
		elsif pixel_row >= 215 and pixel_row <= 219 then
			sel := m_4;
		elsif pixel_row >= 220 and pixel_row <= 224 then
			sel := m_5;
		elsif pixel_row >= 225 and pixel_row <= 229 then
			sel := m_6;
		elsif pixel_row >= 230 and pixel_row <= 234 then
			sel := m_7;
		elsif pixel_row >= 235 and pixel_row <= 239 then
			sel := m_8;
		else 
			sel := "11111111";
		end if;
		end if;
		if pixel_row >=200 and pixel_row<=239 then
			if pixel_column >= 320 and pixel_column <=324 then
				Red<=  sel(0);
				Blue <= sel(0);
				Green <=  sel(0);
			elsif pixel_column >=325 and pixel_column <=329 then
				Red<=  sel(1);
				Blue <= sel(1);
				Green <=  sel(1);
			elsif pixel_column >=330 and pixel_column <=334 then
				Red<=  sel(2);
				Blue <= sel(2);
				Green <=  sel(2);
			elsif pixel_column >=335 and pixel_column <=339 then
				Red<=  sel(3);
				Blue <= sel(3);
				Green <=  sel(3);
			elsif pixel_column >=340 and pixel_column <=344 then
				Red<=  sel(4);
				Blue <= sel(4);
				Green <=  sel(4);
			elsif pixel_column >=345 and pixel_column <=349 then
				Red<=  sel(5);
				Blue <= sel(5);
				Green <=  sel(5);
			elsif pixel_column >=350 and pixel_column <=354 then
				Red<=  sel(6);
				Blue <= sel(6);
				Green <=  sel(6);
			elsif pixel_column >=355 and pixel_column <=359 then
				Red<=  sel(7);
				Blue <= sel(7);
				Green <=  sel(7);
			else
				Red <= '0';
				Blue <= '0';
				Green<= '0';
			end if;
		else 
			Red <= '0';
			Blue <= '0';
			Green<= '0';
		end if;
	end process RGB_Display;
END behavior;

LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_arith.all;
USE ieee.std_logic_unsigned.all;
PACKAGE ball_package IS
COMPONENT ball
 PORT(pixel_row, pixel_column		: IN std_logic_vector(9 DOWNTO 0);
        Red,Green,Blue 				: OUT std_logic;
        Vert_sync, Horiz_sync, input_on	   : IN std_logic;
		  m_1, m_2, m_3, m_4, m_5, m_6, m_7, m_8 : in std_logic_vector (7 downto 0));
END COMPONENT;
END ball_package;

library IEEE;
use  IEEE.STD_LOGIC_1164.all;
use  IEEE.STD_LOGIC_ARITH.all;
use  IEEE.STD_LOGIC_UNSIGNED.all;
-- Module Generates Video Sync Signals for Video Montor Interface
-- RGB and Sync outputs tie directly to monitor conector pins
ENTITY VGA_SYNC IS
	PORT(	clock_50Mhz, red, green, blue		: IN	STD_LOGIC;
			red_out, green_out, blue_out, horiz_sync_out, 
			vert_sync_out, video_on, pixel_clock	: OUT	STD_LOGIC;
			pixel_row, pixel_column: OUT STD_LOGIC_VECTOR(9 DOWNTO 0));
END VGA_SYNC;

ARCHITECTURE a OF VGA_SYNC IS
	SIGNAL horiz_sync, vert_sync,clock_25Mhz : STD_LOGIC;
	SIGNAL video_on_int, video_on_v, video_on_h : STD_LOGIC;
	SIGNAL h_count, v_count :STD_LOGIC_VECTOR(9 DOWNTO 0);
--
-- To select a different screen resolution, clock rate, and refresh rate
-- pick a set of new video timing constant values from table at end of code section
-- enter eight new sync timing constants below and
-- adjust PLL frequency output to pixel clock rate from table
-- using MegaWizard to edit video_PLL.vhd
-- Horizontal Timing Constants  
	CONSTANT H_pixels_across: 	Natural := 640;
	CONSTANT H_sync_low: 		Natural := 664;
	CONSTANT H_sync_high: 		Natural := 760;
	CONSTANT H_end_count: 		Natural := 800;
-- Vertical Timing Constants
	CONSTANT V_pixels_down: 	Natural := 480;
	CONSTANT V_sync_low: 		Natural := 491;
	CONSTANT V_sync_high: 		Natural := 493;
	CONSTANT V_end_count: 		Natural := 525;
--
BEGIN
--
---- PLL below is used to generate the pixel clock frequency
---- Uses UP 3's 48Mhz USB clock for PLL's input clock

-- video_on is high only when RGB pixel data is being displayed
-- used to blank color signals at screen edges during retrace
video_on_int <= video_on_H AND video_on_V;
-- output pixel clock and video on for external user logic

video_on <= video_on_int;

CLOCK_25M : PROCESS(clock_50Mhz)
BEGIN
   IF (clock_50Mhz' EVENT AND clock_50Mhz='1') THEN
		clock_25MHz<= clock_25MHz XOR '1';
   END IF;
END PROCESS CLOCK_25M;
	
pixel_clock<=clock_25Mhz;
	
a1: PROCESS
BEGIN
	
WAIT UNTIL(clock_25Mhz'EVENT) AND (clock_25Mhz='1');
--Generate Horizontal and Vertical Timing Signals for Video Signal
-- H_count counts pixels (#pixels across + extra time for sync signals)
-- 
--  Horiz_sync  ------------------------------------__________--------
--  H_count     0                 #pixels            sync low      end
--
	IF (h_count = H_end_count) THEN
   		h_count <= "0000000000";
	ELSE
   		h_count <= h_count + 1;
	END IF;

--Generate Horizontal Sync Signal using H_count
	IF (h_count <= H_sync_high) AND (h_count >= H_sync_low) THEN
 	  	horiz_sync <= '0';
	ELSE
 	  	horiz_sync <= '1';
	END IF;

--V_count counts rows of pixels (#pixel rows down + extra time for V sync signal)
--  
--  Vert_sync      -----------------------------------------------_______------------
--  V_count         0                        last pixel row      V sync low       end
--
	IF (v_count >= V_end_count) AND (h_count >= H_sync_low) THEN
   		v_count <= "0000000000";
	ELSIF (h_count = H_sync_low) THEN
   		v_count <= v_count + 1;
	END IF;

-- Generate Vertical Sync Signal using V_count
	IF (v_count <= V_sync_high) AND (v_count >= V_sync_low) THEN
   		vert_sync <= '0';
	ELSE
  		vert_sync <= '1';
	END IF;

-- Generate Video on Screen Signals for Pixel Data
-- Video on = 1 indicates pixel are being displayed
-- Video on = 0 retrace - user logic can update pixel
-- memory without needing to read memory for display
	IF (h_count < H_pixels_across) THEN
   		video_on_h <= '1';
   		pixel_column <= h_count;
	ELSE
	   	video_on_h <= '0';
	END IF;

	IF (v_count <= V_pixels_down) THEN
   		video_on_v <= '1';
   		pixel_row <= v_count;
	ELSE
   		video_on_v <= '0';
	END IF;
		horiz_sync_out <= horiz_sync;
		vert_sync_out <= vert_sync;
		red_out <= red AND video_on_int;
		green_out <= green AND video_on_int;
		blue_out <= blue AND video_on_int;

END PROCESS a1;
END a;
--
LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_arith.all;
USE ieee.std_logic_unsigned.all;
PACKAGE vga_sync_package IS
COMPONENT vga_sync
PORT(	clock_50Mhz, red, green, blue		: IN	STD_LOGIC;
			red_out, green_out, blue_out, horiz_sync_out, 
			vert_sync_out, video_on, pixel_clock	: OUT	STD_LOGIC;
			pixel_row, pixel_column: OUT STD_LOGIC_VECTOR(9 DOWNTO 0));
END COMPONENT;
END vga_sync_package;