// See LICENSE for license details.
// Use handlebars for template generation
//
//Start with a static tb and try to genererate a gnerator for it
package halfband_interpolator

import chisel3._
import java.io.{File, FileWriter, BufferedWriter}
import com.gilt.handlebars.scala.binding.dynamic._
import com.gilt.handlebars.scala.Handlebars

//Testbench.
object tb_halfband_interpolator {
  def main(args: Array[String]): Unit = {
    val name= this.getClass.getSimpleName.split("\\$").last
    val tb = new BufferedWriter(new FileWriter("./verilog/"+name+".v"))
    object tbvars {
      val oname=name
      val dutmod = "halfband_interpolator" 
      val n = 16
      val resolution=32
      val ulimit=resolution-n-1
      val gainbits= 10
      val gainlimit=gainbits-1
      val clk0="clock_low"
      val sig0="scale"
    }
    //simple template that uses handlebars to input buswidth definition
    val textTemplate="""//This is a tesbench generated with scala generator
                    |//Things you want to control from the simulator cmdline must be parameters
                    |module tb_halfband_interpolator #( parameter g_infile  = "./A.txt",
                    |                      parameter g_outfile = "./Z.txt",
                    |                      parameter g_scale   = 1,
                    |                      parameter g_Rs_high = 160.0e6
                    |                      );
                    |//timescale 1ps this should probably be a global model parameter 
                    |parameter integer c_Ts=1/(g_Rs_high*1e-12);
                    |parameter integer c_ratio0=2.0;
                    |parameter RESET_TIME = 5*c_Ts;
                    |
                    |//These registers always needed
                    |reg clock;
                    |reg reset;
                    |
                    |//Registers for additional clocks
                    |reg io_{{clk0}};
                    |
                    |//Registers for inputs
                    |reg signed [{{ulimit}}:0] io_iptr_A_real = 0;
                    |reg signed [{{ulimit}}:0] io_iptr_A_imag = 0;
                    |reg signed [{{gainlimit}}:0] io_{{sig0}};
                    |
                    |//Resisters for outputs
                    |wire signed [{{ulimit}}:0] io_Z_real;
                    |wire signed [{{ulimit}}:0] io_Z_imag;
                    |
                    |//File IO parameters
                    |integer StatusI, StatusO, infile, outfile;
                    |integer count0;
                    |integer din1,din2;
                    |
                    |//Initializations
                    |initial count0 = 0;
                    |initial clock = 1'b0;
                    |initial io_{{clk0}}= 1'b0;
                    |initial reset = 1'b0;
                    |initial outfile = $fopen(g_outfile,"w"); // For writing
                    |
                    |//Clock definitions
                    |always #(c_Ts)clock = !clock ;
                    |
                    |always @(posedge clock) begin 
                    |    if (count0%c_ratio0/2 == 0) begin
                    |        io_{{clk0}} =! io_{{clk0}};
                    |    end 
                    |    count0++;
                    |end
                    |
                    |always @(posedge io_{{clk0}}) begin 
                    |    //Print only valid values 
                    |    if (~($isunknown( io_Z_real)) &&   ~($isunknown( io_Z_imag))) begin
                    |        $fwrite(outfile, "%d\t%d\n", io_Z_real, io_Z_imag);
                    |    end
                    |    else begin
                    |        $fwrite(outfile, "%d\t%d\n", 0, 0);
                    |    end 
                    |end
                    |
                    |halfband_interpolator DUT( 
                    |    .clock(clock),
                    |    .reset(reset),
                    |    .io_{{clk0}}(io_{{clk0}}), 
                    |    .io_{{sig0}}(io_{{sig0}}), 
                    |    .io_iptr_A_real(io_iptr_A_real), 
                    |    .io_iptr_A_imag(io_iptr_A_imag), 
                    |    .io_Z_real(io_Z_real), 
                    |    .io_Z_imag(io_Z_imag) 
                    |);
                    |
                    |initial #0 begin
                    |    io_{{sig0}} = g_scale;
                    |    reset=1;
                    |    #RESET_TIME
                    |    reset=0;
                    |    
                    |    infile = $fopen(g_infile,"r"); // For reading
                    |    while (!$feof(infile)) begin
                    |
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

