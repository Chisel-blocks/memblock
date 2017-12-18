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
                    |                      parameter g_Rs      = 160.0e6,
                    |                      parameter g_RS_DSP  = 20e6
                    |                      );
                    |//timescale 1ps this should probably be a global model parameter 
                    |parameter c_Ts=1/(g_Rs*1e-12);
                    |parameter c_ratio=g_Rs/g_Rs_DSP
                    |parameter RESET_TIME = 5*c_Ts
                    |reg signed [15:0] io_iptr_Areal = 0;
                    |reg signed [15:0] io_iptr_Aimag = 0;
                    |reg io__Z;
                    |reg clock;
                    |reg reset;
                    |reg io_clock_DSP;
                    |wire reset;
                    |wire Z;
                    |integer StatusI, StatusO, infile, outfile;
                    |integer count
                    |
                    |initial clock = 1'b0;
                    |initial io_clk_DSP = 1'b0;
                    |initial reg = 1'b0;
                    |initial outfile = $fopen(g_outfile,"w"); // For writing
                    |always #(c_Ts)clock = !clock ;
                    |always @(posedge clock) begin 
                    |    if (count % c_ratio ==0) begin
                    |        io_clock_DSP =! io_clock_DSP;
                    |    end 
                    |    count++
                    |end
                    |
                    |always @(posedge io_clock_dsp) begin 
                    |    //if (count % c_ratio ==0) begin
                    |        $fwrite(fileOutID, "%d\t%d\n", io_dout_real, io_dout_imag);
                    |    //end 
                    |end
                    |
                    |
                    |
                    |//tb_f2_dsp_tapein0 DUT(.clock(clock), 
                    |//.reset(reset),.io_clock_DSP(io_clock_DSP),
                    |// .A(iptr_A), .Z(Z) );
                    |
                    |
                    |initial #0 begin
                    |    reset=1
                    |    #RESET_TIME
                    |    reset=0
                    |    
                    |    infile = $fopen(g_infile,"r"); // For reading
                    |    while (!$feof(infile)) begin

                    |            @(posedge clock) 
                    |             StatusI=$fscanf(fileInID, "%d\t%d\n", din1, din2);
                    |             io_iptr_Areal <= din1;
                    |             io_iptr_Aimag <= din2
                    |    end
                    |    $fclose(fileInID);
                    |    $fclose(fileOutID);
                    |    $finish;
                    |end
                    |endmodule""".stripMargin('|')
  tb write textTemplate
  tb.close()
  }
}

