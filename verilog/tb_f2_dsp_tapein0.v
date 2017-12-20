//This is a tesbench generated with scala generator
//Things you want to control from the simulator cmdline must be parameters
module tb_f2_dsp_tapein0 #( parameter g_infile  = "./A.txt",
                      parameter g_outfile = "./Z.txt",
                      parameter g_Rs      = 160.0e6,
                      parameter g_Rs_DSP  = 20e6
                      );
//timescale 1ps this should probably be a global model parameter 
parameter integer c_Ts=1/(g_Rs*1e-12);
parameter integer c_ratio=g_Rs/(2.0*g_Rs_DSP);
parameter RESET_TIME = 5*c_Ts;
reg signed [15:0] io_iptr_A_real = 0;
reg signed [15:0] io_iptr_A_imag = 0;
reg io__Z;
reg clock;
reg io_clock_DSP;
reg reset;
wire signed [15:0] io_Z_real;
wire signed [15:0] io_Z_imag;

integer StatusI, StatusO, infile, outfile;
integer count;
integer din1,din2;

initial count = 0;
initial clock = 1'b0;
initial io_clock_DSP = 1'b0;
initial reset = 1'b0;
initial outfile = $fopen(g_outfile,"w"); // For writing
always #(c_Ts)clock = !clock ;
always @(posedge clock) begin 
    if (count%c_ratio == 0) begin
        io_clock_DSP =! io_clock_DSP;
    end 
    count++;
end

always @(posedge io_clock_DSP) begin 
    //Print only valid values 
    if (~($isunknown( io_Z_real)) &&   ~($isunknown( io_Z_imag))) begin
        $fwrite(outfile, "%d\t%d\n", io_Z_real, io_Z_imag);
    end
    else begin
        $fwrite(outfile, "%d\t%d\n", 0, 0);
    end 
end

f2_dsp_tapein0 DUT( // @[:@3.2]
.clock(clock), // @[:@4.4]
.reset(reset), // @[:@5.4]
.io_clock_DSP(io_clock_DSP), // @[:@6.4]
.io_iptr_A_real(io_iptr_A_real), // @[:@6.4]
.io_iptr_A_imag(io_iptr_A_imag), // @[:@6.4]
.io_Z_real(io_Z_real), // @[:@6.4]
.io_Z_imag(io_Z_imag) // @[:@6.4]
);

initial #0 begin
    reset=1;
    #RESET_TIME
    reset=0;
    
    infile = $fopen(g_infile,"r"); // For reading
    while (!$feof(infile)) begin

            @(posedge clock) 
             StatusI=$fscanf(infile, "%d\t%d\n", din1, din2);
             io_iptr_A_real <= din1;
             io_iptr_A_imag <= din2;
    end
    $fclose(infile);
    $fclose(outfile);
    $finish;
end
endmodule