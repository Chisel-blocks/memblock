// See LICENSE for license details.
// Use handlebars for template generation
//
//Start with a static tb and try to genererate a gnerator for it
package clkdiv_n_2_4_8

import chisel3._
import java.io.{File, FileWriter, BufferedWriter}
import com.gilt.handlebars.scala.binding.dynamic._
import com.gilt.handlebars.scala.Handlebars

//Testbench.
object tb_clkdiv_n_2_4_8 {
  def main(args: Array[String]): Unit = {
    val name= this.getClass.getSimpleName.split("\\$").last
    val tb = new BufferedWriter(new FileWriter("./verilog/"+name+".v"))
    object tbvars {
      val oname=name
      val dutmod = "clkdiv_n_2_4_8" 
      val n = 8
      val sig0 = "Ndiv"
      val sig1 = "reset_clk"
      val sig2 = "clkpn"
      val sig3 = "clkp2n"
      val sig4 = "clkp4n"
      val sig5 = "clkp8n"
      val sig0limit = n-1
    }
    //simple template that uses handlebars to input buswidth definition
    val textTemplate="""//This is a tesbench generated with scala generator
                    |//Things you want to control from the simulator cmdline must be parameters
                    |module {{oname}} #( parameter g_Rs_high  = 16*8*20.0e6
                    |                      );
                    |//timescale 1ps this should probably be a global model parameter 
                    |parameter integer c_Ts=1/(g_Rs_high*1e-12);
                    |parameter RESET_TIME = 50*c_Ts;
                    |parameter SIM_TIME = 1024*c_Ts;
                    |
                    |//These registers always needed
                    |reg clock;
                    |reg reset;
                    |
                    |//Registers for additional clocks
                    |
                    |//Registers for inputs
                    |reg [{{sig0limit}}:0] io_{{sig0}};
                    |reg io_{{sig1}};
                    |
                    |//Resisters for outputs
                    |
                    |//File IO parameters
                    |
                    |//Initializations
                    |initial clock = 1'b0;
                    |initial reset = 1'b0;
                    |initial io_{{sig1}} = 1'b0;
                    |
                    |//Clock definitions
                    |always #(c_Ts)clock = !clock ;
                    |
                    |//DUT definition
                    |{{dutmod}} DUT ( 
                    |    .clock(clock),
                    |    .reset(reset),
                    |    .io_{{sig0}}(io_{{sig0}}), 
                    |    .io_{{sig1}}(io_{{sig1}}), 
                    |    .io_{{sig2}}(io_{{sig2}}), 
                    |    .io_{{sig3}}(io_{{sig3}}), 
                    |    .io_{{sig4}}(io_{{sig4}}), 
                    |    .io_{{sig5}}(io_{{sig5}}) 
                    |   );
                    |
                    |initial #0 begin
                    |    io_{{sig0}} = 8'd4;
                    |    io_{{sig1}} = 1;
                    |    reset=1;
                    |    #RESET_TIME
                    |    reset=0;
                    |    #RESET_TIME
                    |    io_{{sig1}} = 0;
                    |    #SIM_TIME
                    |    $finish;
                    |end
                    |endmodule""".stripMargin('|')
  val testbench=Handlebars(textTemplate)
  tb write testbench(tbvars)
  tb.close()
  }
}


