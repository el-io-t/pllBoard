module fsm(
	input clk,
	input rstn,
	input start,
	input [15:0] scan_cycle,
//	input clk_in_pll,

	output mem_en,
	output reg [3:0] mem_wea,
	output reg [31:0] mem_addr,
	output reg [31:0] data_to_mem,
	input  [31:0] data_from_mem,

	output reg phi,
	output reg phi_b,
	output reg scan_in,
	output reg load,
	output load_scanchain,
	output defval,
	
	//output reg clk_out_pll,
	output pll_ce, 		// Chip Enable. Logic high powers up device, depends on status of power up pins
	output pll_dat, 	// Serial Data Input. Data written to the registers of the PLL board. The serial data is loaded MSB first with the three LSBs as the control bits.
	output pll_le, 		// Load Enable. When LE goes high data stored in the 32 bit shift register is loaded onto the designated register on the PLL board
    output reg clk_out_pll,
        
	output reg flag_finish
);
 

// constant value	
assign mem_en		 	= 1'b1;
assign load_scanchain	= 1'b0;
assign defval		 	= 1'b0;

// State definition
localparam IDLE			= 3'b000;
localparam PRE_START   	= 3'b001;
localparam FETCH_DATA  	= 3'b010;
localparam SCAN_IN	 	= 3'b011;	
localparam RESET_MEM   	= 3'b100;

// PLL States
localparam PLL_IDLE		 	= 4'b0000;
localparam PLL_SEND_REG0	= 4'b0001;
localparam PLL_SEND_REG1	= 4'b0010;
localparam PLL_SEND_REG2	= 4'b0011;
localparam PLL_SEND_REG3	= 4'b0100;
localparam PLL_SEND_REG4	= 4'b0101;
localparam PLL_SEND_REG5	= 4'b0110;
localparam PLL_DONE		 	= 4'b0111;
localparam PLL_TRANS		= 4'b1000;

// Other parameters
localparam LAST_ADDR  = 32'd24;
localparam SCAN_WIDTH = 750;
reg [5:0] pll_bit_cntr_d, pll_bit_cntr_q;		// PLL bit counter
reg [2:0] cntr_d, cntr_q;						// PLL register counter
reg clk_pll_hold_q, clk_pll_hold_d;				// PLL clock hold
reg pll_dat_q, pll_dat_d;						// PLL data register
reg pll_le_q, pll_le_d;							// PLL LE input
reg rst_bit_cntr_q, rst_bit_cntr_d;
reg [2:0] clk_counter;

// State
reg [2:0] state;
reg [2:0] state_next;
reg [3:0] state_pll_d;
reg [3:0] state_pll_q;

// Internal signals
reg [31:0] MEM0;

//PLL Register Values
reg [31:0] pll_reg0 = 32'h500000;		//	00000000010100000000000000000_000
reg [31:0] pll_reg1 = 32'h8008011;		//	00001000000000001000000000010_001
reg [31:0] pll_reg2 = 32'h4E42;			// 	00000000000000000100111001000_010
reg [31:0] pll_reg3 = 32'h4B3;			//	00000000000000000000010010110_011
reg [31:0] pll_reg4 = 32'h9C803C;		//	00000000100111001000000000111_100
reg [31:0] pll_reg5 = 32'h580005;		//	00000000010110000000000000000_101

// PLL wire assignments
assign pll_ce	= 1'b1;					// Chip Enable
assign pll_dat	= pll_dat_q;			// Serial Data Input
assign pll_le	= pll_le_q;				// Load Enable

//Clock for PLL
always @(posedge clk or negedge rstn) begin
    if (!rstn) begin
        clk_counter <= 3'b000;
        clk_out_pll <= 0;
    end
    
    else if (clk_counter == 3'b100) begin
        clk_out_pll <= !clk_out_pll;
        clk_counter <= 3'b000;
    end
    
    else begin
        clk_counter <= clk_counter + 3'b001;
    end
    
end

//	PLL Setup: Positive clock behavior
always @(posedge clk_out_pll or negedge rstn) begin
	if (!rstn) begin
		pll_dat_q		<= 1'b0;
		state_pll_q		<= PLL_IDLE;
		cntr_q			<= 3'b000;
		clk_pll_hold_q	<= 1'b1;
		rst_bit_cntr_q	<= 1'b0;
	end
	else begin
		clk_pll_hold_q	<= clk_pll_hold_d;
		pll_dat_q		<= pll_dat_d;
		state_pll_q		<= state_pll_d;
		cntr_q			<= cntr_d;
		rst_bit_cntr_q	<= rst_bit_cntr_d;
	end
end

//	PLL Setup: Negative clock behavior
always @(negedge clk_out_pll or negedge rstn) begin
	if (!rstn) begin
		pll_le_q		<= 1'b0;
		pll_bit_cntr_q	<= 6'b011111;
	end
	else begin
		pll_bit_cntr_q	<= pll_bit_cntr_d;
		pll_le_q		<= pll_le_d;
	end
end

always @(*) begin
	clk_pll_hold_d	= clk_pll_hold_q;
	pll_dat_d		= pll_dat_q;
	state_pll_d		= state_pll_q;
	cntr_d			= cntr_q;
	pll_bit_cntr_d	= pll_bit_cntr_q;
	pll_le_d		= pll_le_q;
	rst_bit_cntr_d	= rst_bit_cntr_q;
	
	case(state_pll_q)
		PLL_IDLE: begin
			if (clk_pll_hold_q == 1'b1)
				clk_pll_hold_d = 1'b0;
			else if(clk_pll_hold_q == 1'b0)
				state_pll_d = PLL_SEND_REG5;
		end

		PLL_SEND_REG5: begin
			if (pll_le_q == 1'b0) begin
				pll_dat_d = pll_reg5[pll_bit_cntr_q];
				if (pll_bit_cntr_q > 6'b000000 && pll_bit_cntr_q < 6'b100000)
					pll_bit_cntr_d = pll_bit_cntr_d - 1'b1;
				else if (pll_bit_cntr_q == 6'b000000) begin
					cntr_d = cntr_d + 1;
					state_pll_d	= PLL_TRANS;
				end
			end
		end
		PLL_TRANS: begin
			pll_le_d = 1'b1;
			if (pll_le_q == 1'b1 && cntr_q == 3'b001) begin
				pll_le_d = 1'b0;
				pll_bit_cntr_d = 6'b011111;
				state_pll_d = PLL_SEND_REG4;
			end
			else if (pll_le_q == 1'b1 && cntr_q == 3'b010) begin
				pll_le_d = 1'b0;
				pll_bit_cntr_d = 6'b011111;
				state_pll_d = PLL_SEND_REG3;
			end
			else if (pll_le_q == 1'b1 && cntr_q == 3'b011) begin
				pll_le_d = 1'b0;
				pll_bit_cntr_d = 6'b011111;
				state_pll_d = PLL_SEND_REG2;
			end
			else if (pll_le_q == 1'b1 && cntr_q == 3'b100) begin
				pll_le_d = 1'b0;
				pll_bit_cntr_d = 6'b011111;
				state_pll_d = PLL_SEND_REG1;
			end
			else if (pll_le_q == 1'b1 && cntr_q == 3'b101) begin
				pll_le_d = 1'b0;
				pll_bit_cntr_d = 6'b011111;
				state_pll_d = PLL_SEND_REG0;
			end
			else if (pll_le_q == 1'b1 && cntr_q == 3'b110) begin
				pll_le_d = 1'b0;
				pll_bit_cntr_d = 6'b011111;
				state_pll_d = PLL_DONE;
			end
		end
		PLL_SEND_REG4: begin
			pll_le_d = 1'b0;
			if (rst_bit_cntr_q == 1'b0) begin
				pll_bit_cntr_d = 6'b011111;
				rst_bit_cntr_d = 1'b1;
			end
			if (pll_le_q == 1'b0 && rst_bit_cntr_q == 1'b1) begin
				pll_dat_d = pll_reg4[pll_bit_cntr_q];
				if (pll_bit_cntr_q > 6'b000000 && pll_bit_cntr_q < 6'b100000)
					pll_bit_cntr_d = pll_bit_cntr_d - 1'b1;
				else if (pll_bit_cntr_q == 6'b000000) begin
					cntr_d = cntr_d + 1;
					rst_bit_cntr_d = 1'b0;
					state_pll_d	= PLL_TRANS;
				end
			end
		end
		PLL_SEND_REG3: begin
			pll_le_d = 1'b0;
			if (rst_bit_cntr_q == 1'b0) begin
				pll_bit_cntr_d = 6'b011111;
				rst_bit_cntr_d = 1'b1;
			end
			if (pll_le_q == 1'b0) begin
				pll_dat_d = pll_reg3[pll_bit_cntr_q];
				if (pll_bit_cntr_q > 6'b000000 && pll_bit_cntr_q < 6'b100000)
					pll_bit_cntr_d = pll_bit_cntr_d - 1'b1;
				else if (pll_bit_cntr_q == 6'b000000) begin
					cntr_d = cntr_d + 1;
					rst_bit_cntr_d = 1'b0;
					state_pll_d	= PLL_TRANS;
				end
			end
		end
		PLL_SEND_REG2: begin
			pll_le_d = 1'b0;
			if (rst_bit_cntr_q == 1'b0) begin
				pll_bit_cntr_d = 6'b011111;
				rst_bit_cntr_d = 1'b1;
			end
			if (pll_le_q == 1'b0) begin
				pll_dat_d = pll_reg2[pll_bit_cntr_q];
				if (pll_bit_cntr_q > 6'b000000 && pll_bit_cntr_q < 6'b100000)
					pll_bit_cntr_d = pll_bit_cntr_d - 1'b1;
				else if (pll_bit_cntr_q == 6'b000000) begin
					cntr_d = cntr_d + 1;
					rst_bit_cntr_d = 1'b0;
					state_pll_d	= PLL_TRANS;
				end
			end
		end
		PLL_SEND_REG1: begin
			pll_le_d = 1'b0;
			if (rst_bit_cntr_q == 1'b0) begin
				pll_bit_cntr_d = 6'b011111;
				rst_bit_cntr_d = 1'b1;
			end
			if (pll_le_q == 1'b0) begin
				pll_dat_d = pll_reg1[pll_bit_cntr_q];
				if (pll_bit_cntr_q > 6'b000000 && pll_bit_cntr_q < 6'b100000)
					pll_bit_cntr_d = pll_bit_cntr_d - 1'b1;
				else if (pll_bit_cntr_q == 6'b000000) begin
					cntr_d = cntr_d + 1;
					rst_bit_cntr_d = 1'b0;
					state_pll_d	= PLL_TRANS;
				end
			end
		end
		PLL_SEND_REG0: begin
			pll_le_d = 1'b0;
			if (rst_bit_cntr_q == 1'b0) begin
				pll_bit_cntr_d = 6'b011111;
				rst_bit_cntr_d = 1'b1;
			end
			if (pll_le_q == 1'b0) begin
				pll_dat_d = pll_reg0[pll_bit_cntr_q];
				if (pll_bit_cntr_q > 6'b000000 && pll_bit_cntr_q < 6'b100000)
					pll_bit_cntr_d = pll_bit_cntr_d - 1'b1;
				else if (pll_bit_cntr_q == 6'b000000) begin
					cntr_d = cntr_d + 1;
					rst_bit_cntr_d = 1'b0;
					state_pll_d	= PLL_TRANS;
				end
			end
		end
		PLL_DONE: begin
			pll_le_d = 1'b1;
		end

		// Have to increment counter to keep track of which register we are on
	endcase
end  

/*
 *	Is everything below relevant ONLY to the canceller configuration?
 */

// update state
always @(posedge clk or negedge rstn) begin
	if (!rstn)
		state <= IDLE;
	else
		state <= state_next;
end

// calculate next state
always @(*) begin
	if ((state == IDLE) && (start == 1'b1) && (state_pll_q == PLL_DONE))
		state_next <= PRE_START;
	else if ((state == PRE_START) && (MEM0 == 32'hFFFFFFFF))
		state_next <= FETCH_DATA;
	else if ((state == FETCH_DATA) && (mem_addr == 4*LAST_ADDR))
		state_next <= SCAN_IN;
	else if ((state == SCAN_IN) && (scan_counter == 4*SCAN_WIDTH-1) && (scan_cycle_counter == scan_cycle))
		state_next <= RESET_MEM;
	else if (state == RESET_MEM)
		state_next <= IDLE;
	else
		state_next <= state;
end

//buffer the control signals
reg [2:0]  state_last;
reg [31:0] mem_addr_d1;
reg [3:0]  mem_wea_d1;

always @(posedge clk or negedge rstn) begin
	if (!rstn) begin
		state_last	<= 0;
		mem_addr_d1	<= 0;
		mem_wea_d1	<= 0;
	end
	else begin
		state_last  <= state;
		mem_addr_d1 <= mem_addr;
		mem_wea_d1  <= mem_wea;
	end
end


// Get the data at address 0 (start signal)
always @(posedge clk or negedge rstn) begin
	if (!rstn)
		MEM0 <= 32'd0;
	else if ((state == PRE_START) && (mem_addr_d1 == 32'd0) && (mem_wea_d1 == 4'h0))
		MEM0 <= data_from_mem;
	else
		MEM0 <= 32'd0;
end

// Buffer the scanin data from mem
reg [32*24-1:0] scanin_data;
always @(posedge clk or negedge rstn) begin
	if (!rstn)
		scanin_data <= 0;
	else if ((state_last == FETCH_DATA) && (mem_addr_d1 <= 4*LAST_ADDR))
		scanin_data <= {scanin_data[32*23-1:0], data_from_mem};
	else if ((state == SCAN_IN) && (scan_counter[1:0] == 3) && ((scan_cycle_counter == scan_cycle)))
		scanin_data <= scanin_data << 1;
	else
		scanin_data <= scanin_data;
end

// controls the memory interface
always @(posedge clk or negedge rstn) begin
	if (!rstn) begin
		mem_wea			<= 4'h0;
		mem_addr		<= 32'd0;
		data_to_mem 	<= 32'd0;
	end
	else if (state == IDLE) begin
		mem_wea			<= 4'h0;
		mem_addr	  	<= 32'd0;
		data_to_mem   	<= 32'd0;
	end
	else if (state == PRE_START) begin // generate a read request
		mem_wea			<= 4'h0;
		mem_addr	  	<= 32'd0;
		data_to_mem   	<= 32'd0;
	end
	else if (state == FETCH_DATA) begin // load the scanin data
		mem_wea			<= 4'h0;
		mem_addr	  	<= mem_addr + 4;
		data_to_mem   	<= 32'd0;
	end
	else if (state == RESET_MEM) begin
		mem_wea			<= 4'hF;
		mem_addr		<= 0;
		data_to_mem 	<= 32'd0;
	end
end

/*
// indicate whether it is the fisrt scan clk
reg initial_count;
always @ (posedge clk or negedge rstn)
if (!rstn)
	initial_count <= 0;
else if ((state==SCAN_IN) &&	 )
*/

// scanchain counter
reg unsigned [15:0] scan_counter;
always @(posedge clk or negedge rstn) begin
	if (!rstn)
		scan_counter <= 0;
	else if ((state_last == SCAN_IN) && (scan_cycle_counter == scan_cycle)  && (scan_counter < 4*SCAN_WIDTH))
		scan_counter <= scan_counter + 1;
	else if ((state_last == SCAN_IN) && (scan_counter < 4*SCAN_WIDTH))
		scan_counter <= scan_counter;
	else
		scan_counter <= 0;
end

//scan chain cycle counter (equivalently generate a slow clock)
reg unsigned [15:0] scan_cycle_counter;
always @(posedge clk or negedge rstn) begin
	if (!rstn)
		scan_cycle_counter <= 0;
	else if (scan_cycle_counter == scan_cycle)
		scan_cycle_counter <= 0;
	else if (state_last == SCAN_IN)
		scan_cycle_counter <= scan_cycle_counter + 1;
end

//control the scanchain signal
always @(posedge clk or negedge rstn) begin
	if (!rstn) begin
		phi		<= 0;
		phi_b   <= 0;
		scan_in <= 0;
		load	<= 0;
	end
	else if ((state_last == SCAN_IN) && (state == SCAN_IN) && (scan_counter[1:0] == 0)) begin
		phi		<= 1;
		phi_b   <= 0;
		scan_in <= scanin_data[32*24-1];
		load	<= 0;
	end
	else if ((state_last == SCAN_IN) && (scan_counter[1:0] == 1)) begin
		phi	 	<= 0;
		phi_b   <= 0;
		scan_in <= scanin_data[32*24-1];
		load	<= 0;
	end
	else if ((state_last == SCAN_IN) && (scan_counter[1:0] == 2)) begin
		phi	 	<= 0;
		phi_b   <= 1;
		scan_in <= scanin_data[32*24-1];
		load	<= 0;
	end
	else if ((state_last == SCAN_IN) && (scan_counter[1:0] == 3)) begin
		phi	 	<= 0;
		phi_b   <= 0;
		scan_in <= scanin_data[32*24-1];
		load	<= 0;
	end
	else if (state_pll_q == PLL_DONE && pll_le == 1'b1) begin
		phi	 	<= 0;
		phi_b   <= 0;
		scan_in <= 0;
		load	<= 1;
	end
end

//generate finish signal
always @(posedge clk or negedge rstn) begin
	if (!rstn)
		flag_finish <= 0;
	else if (state_last == RESET_MEM)
		flag_finish <= 1;
	else if (((state == PRE_START) && (MEM0 == 32'hFFFFFFFF)) || (state == FETCH_DATA) || (state == SCAN_IN))
		flag_finish <= 0;
	else
		flag_finish <= flag_finish;
end


endmodule