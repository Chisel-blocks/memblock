// This is the testbench of a third order CIC-filter for programmable decimator
//
// Intially written by Marko Kosunen 20180110
// Last modification by Marko Kosunen, marko.kosunen@aalto.fi, 12.01.2018 11:39
package cic3_interpolator

import chisel3._
import java.io.{File, FileWriter, BufferedWriter}
import com.gilt.handlebars.scala.binding.dynamic._
import com.gilt.handlebars.scala.Handlebars

//Testbench.
object tb_cic3_interpolator {
  def main(args: Array[String]): Unit = {
    object tbvars {
      val dutmod = "cic3_interpolator" 
      val n = 16
      val resolution=32
      val ulimit=resolution-n-1
      val gainbits= 10
      val gainlimit=gainbits-1
      val clk2="clockslow"
      val sig1="integscale"
    }
    val name= this.getClass.getSimpleName.split("\\$").last
    val tb = new BufferedWriter(new FileWriter("./verilog/"+name+".v"))
    //Simple template that uses handlebars to input buswidth definition
    //Identified some common structures tah could be generated with functions
    val textTemplate="""//This is a tesbench generated with scala generator
                    |//Things you want to control from the simulator cmdline must be parameters
                    |module tb_{{dutmod}} #( parameter g_infile  = "./A.txt",
                    |                      parameter g_outfile = "./Z.txt",
                    |                      parameter g_integscale = 1023,
                    |                      parameter g_Rs      = 640.0e6,
                    |                      parameter g_Rs_slow = 80.0e6
                    |                      );
                    |//timescale 1ps this should probably be a global model parameter 
                    |parameter integer c_Ts=1/(g_Rs*1e-12);
                    |parameter integer c_ratio=g_Rs/g_Rs_slow;
                    |parameter RESET_TIME = 5*c_Ts;
                    |//These registers always needed
                    |reg clock;
                    |reg reset;
                    |
                    |//Registers for additional clocks
                    |reg io_{{clk2}};
                    |
                    |//Registers for inputs
                    |reg signed [{{ulimit}}:0] io_iptr_A_real = 0;
                    |reg signed [{{ulimit}}:0] io_iptr_A_imag = 0;
                    |reg signed [{{gainlimit}}:0] io_integscale;
                    |
                    |//resisters for outputs
                    |wire signed [{{ulimit}}:0] io_Z_real;
                    |wire signed [{{ulimit}}:0] io_Z_imag;
                    |
                    |//File IO parameters
                    |integer StatusI, StatusO, infile, outfile;
                    |integer count;
                    |integer din1,din2;
                    |
                    |//Initializtions
                    |initial count = 0;
                    |initial count = 0;
                    |initial clock = 1'b0;
                    |initial io_{{clk2}}= 1'b0;
                    |initial reset = 1'b0;
                    |initial io_{{sig1}} =$realtobits($itor(g_integscale));
                    |initial outfile = $fopen(g_outfile,"w"); // For writing
                    | 
                    |//Clock definitions
                    |always #(c_Ts)clock = !clock ;
                    |always @(posedge clock) begin 
                    |    if (count%(c_ratio/2) == 0) begin
                    |        io_{{clk2}} =! io_{{clk2}};
                    |    end 
                    |    count++;
                    |end
                    |
                    |//File out 
                    |always @(posedge io_{{clk2}}) begin 
                    |    //Print only valid values 
                    |    if (~($isunknown( io_Z_real)) &&   ~($isunknown( io_Z_imag))) begin
                    |        $fwrite(outfile, "%d\t%d\n", io_Z_real, io_Z_imag);
                    |    end
                    |    else begin
                    |        $fwrite(outfile, "%d\t%d\n", 0, 0);
                    |    end 
                    |end
                    |
                    |//DUT definition
                    |{{dutmod}} DUT( 
                    |   .clock(clock),
                    |   .reset(reset),
                    |   .io_{{clk2}}(io_{{clk2}}), 
                    |   .io_{{sig1}}(io_{{sig1}}), 
                    |   .io_iptr_A_real(io_iptr_A_real), 
                    |   .io_iptr_A_imag(io_iptr_A_imag), 
                    |   .io_Z_real(io_Z_real), 
                    |   .io_Z_imag(io_Z_imag) 
                    |);
                    |
                    |//The simulation run (how to make parallel file reads?)
                    |initial #0 begin
                    |    io_{{sig1}} = g_integscale;
                    |    //io_{{sig1}} =10'd1023;
                    |    reset=1;
                    |    #RESET_TIME
                    |    reset=0;
                    |    
                    |    infile = $fopen(g_infile,"r"); // For reading
                    |    while (!$feof(infile)) begin
                    |            @(posedge clock) 
                    |             StatusI=$fscanf(infile, "%d\t%d\n", din1, din2);
                    |             io_iptr_A_real <= din1;
                    |             io_iptr_A_imag <= din2;
                    |    end
                    |    $fclose(infile);
                    |    $fclose(outfile);
                    |    $finish;
                    |end
                    |endmodule""".stripMargin('|')

  val testbench=Handlebars(textTemplate)
  tb write testbench(tbvars)
  tb.close()
  }
}

