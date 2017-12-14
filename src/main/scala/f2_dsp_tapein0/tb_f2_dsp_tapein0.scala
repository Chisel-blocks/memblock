// See LICENSE for license details.
//Look at handlebars
//
//Start with a static tb and try to genererate a gnerator for it
package f2_dsp_tapein0

import chisel3._
import java.io.{File, FileWriter, BufferedWriter}

//Testbench.
object tb_f2_dsp_tapein0 {
  def main(args: Array[String]): Unit = {
    //How to extract the filepath?
    //How to extract the dutname
    //val iotestersOM = chisel3.iotesters.Driver.optionsManager
    //val targetDir = iotestersOM.targetDirName
    val name= this.getClass.getSimpleName.split("\\$").last
    val tb = new BufferedWriter(new FileWriter("./verilog/"+name+".v"))
    val textTemplate= """//This is a tesbench generated with scala generator
                    |//Things you want to control from the simulator cmdline must be parameters
                    |module tb_inverter #( parameter g_infile  = "./A.txt",
                    |                      parameter g_outfile = "./Z.txt",
                    |                      parameter g_Rs      = 100.0e6
                    |                      );
                    |//timescale 1ps this should probably be a global model parameter 
                    |parameter c_Ts=1/(g_Rs*1e-12);

                    |reg iptr_A;
                    |reg clock;
                    |wire reset;
                    |
                    |wire Z;
                    |integer StatusI, StatusO, infile, outfile;
                    |
                    |initial clk = 1'b0;
                    |always #(c_Ts)clk = !clk ;
                    |
                    |tb_f2_dsp_tapein0 DUT( .A(iptr_A), .Z(Z) );
                    |
                    |
                    |initial #0 begin
                    |    infile = $fopen(g_infile,"r"); // For reading
                    |    outfile = $fopen(g_outfile,"w"); // For writing
                    |    while (!$feof(infile)) begin
                    |            @(posedge clk) StatusI=$fscanf(infile,"%b\n",iptr_A);
                    |            @(negedge clk) $fwrite(outfile,"%b\n",Z);
                    |    end
                    |    $finish;
                    |end
                    |endmodule""".stripMargin('|')
  tb write textTemplate
  tb.close()
  }
}

