// See LICENSE for license details.
// Use handlebars for template generation
//
//Start with a static tb and try to genererate a gnerator for it
package halfband

import chisel3._
import java.io.{File, FileWriter, BufferedWriter}
import com.gilt.handlebars.scala.binding.dynamic._
import com.gilt.handlebars.scala.Handlebars

//Testbench.
object tb_halfband {
  def main(args: Array[String]): Unit = {
    object tbvars {
      val ulimit = 15
      val clk2="clkp2"
    }
    val name= this.getClass.getSimpleName.split("\\$").last
    val tb = new BufferedWriter(new FileWriter("./verilog/"+name+".v"))
    //simple template that uses handlebars to input buswidth definition
    val textTemplate="""//This is a tesbench generated with scala generator
                    |//Things you want to control from the simulator cmdline must be parameters
                    |module tb_f2_dsp_tapein0 #( parameter g_infile  = "./A.txt",
                    |                      parameter g_outfile = "./Z.txt",
                    |                      parameter g_Rs      = 160.0e6,
                    |                      parameter g_Rs_p2   = 80e6
                    |                      );
                    |//timescale 1ps this should probably be a global model parameter 
                    |parameter integer c_Ts=1/(g_Rs*1e-12);
                    |parameter integer c_ratio=g_Rs/(2.0*g_Rs_p2);
                    |parameter RESET_TIME = 5*c_Ts;
                    |reg signed [{{ulimit}}:0] io_iptr_A_real = 0;
                    |reg signed [{{ulimit}}:0] io_iptr_A_imag = 0;
                    |reg io_Z;
                    |reg clock;
                    |reg io_{{clk2}};
                    |reg reset;
                    |wire signed [{{ulimit}}:0] io_Z_real;
                    |wire signed [{{ulimit}}:0] io_Z_imag;
                    |
                    |integer StatusI, StatusO, infile, outfile;
                    |integer count;
                    |integer din1,din2;
                    |
                    |initial count = 0;
                    |initial clock = 1'b0;
                    |initial io_clock_DSP = 1'b0;
                    |initial reset = 1'b0;
                    |initial outfile = $fopen(g_outfile,"w"); // For writing
                    |always #(c_Ts)clock = !clock ;
                    |always @(posedge clock) begin 
                    |    if (count%c_ratio == 0) begin
                    |        io_{{clk2}} =! io_{{clk2}};
                    |    end 
                    |    count++;
                    |end
                    |
                    |always @(posedge io_clock_DSP) begin 
                    |    //Print only valid values 
                    |    if (~($isunknown( io_Z_real)) &&   ~($isunknown( io_Z_imag))) begin
                    |        $fwrite(outfile, "%d\t%d\n", io_Z_real, io_Z_imag);
                    |    end
                    |    else begin
                    |        $fwrite(outfile, "%d\t%d\n", 0, 0);
                    |    end 
                    |end
                    |
                    |halfband DUT( 
                    |.clock(clock),
                    |.reset(reset),
                    |.io_clockp2(io_{{clk2}}), 
                    |.io_iptr_A_real(io_iptr_A_real), 
                    |.io_iptr_A_imag(io_iptr_A_imag), 
                    |.io_Z_real(io_Z_real), 
                    |.io_Z_imag(io_Z_imag) 
                    |);
                    |
                    |initial #0 begin
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

