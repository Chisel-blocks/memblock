// See LICENSE for license details.
// Use handlebars for template generation
//
//Start with a static tb and try to genererate a gnerator for it
// This uses clkdiv_n_2_4_8 verilog. You need to compile it separately

package f2_rx_path

import chisel3._
import java.io.{File, FileWriter, BufferedWriter}
import com.gilt.handlebars.scala.binding.dynamic._
import com.gilt.handlebars.scala.Handlebars

//Testbench.
object tb_f2_rx_path {
    def extpargen(): String={
        val extpars=Seq(("g_infile","\"./A.txt\""), 
                        ("g_outfile","\"./Z.txt\""),
                        ("g_Rs_high","16*8*20.0e6"),
                        ("g_Rs_low ","20.0e6"),
                        ("g_scale0 ","1"),
                        ("g_scale1 ","1"),
                        ("g_scale2 ","1"),
                        ("g_scale3 ","1"),
                        ("g_mode   ","4"))

        var externalpars=("//Things you want to control from the simulator cmdline must be parameters \nmodule {{oname}} #("
                                +extpars.map{ case (par,value) => "parameter %s = %s,\n            ".format(par,value)}.mkString)
        externalpars=externalpars.patch(externalpars.lastIndexOf(','),"",1)+");"
         externalpars
    }

    def main(args: Array[String]): Unit = {
        val name= this.getClass.getSimpleName.split("\\$").last
        val tb = new BufferedWriter(new FileWriter("./verilog/"+name+".v"))
        object tbvars {
          val oname=name
          val dutmod = "f2_dsp_rx_path" 
          val n = 16
          val resolution=32
          val ulimit=resolution-n-1
          val gainbits= 10
          val gainlimit=gainbits-1
          val clk0="decimator_controls_cic3clockslow"
          val clk1="decimator_controls_hb1clock_low"
          val clk2="decimator_controls_hb2clock_low"
          val clk3="decimator_controls_hb3clock_low"
          val sig0="decimator_controls_cic3integscale"
          val sig1="decimator_controls_hb1scale"
          val sig2="decimator_controls_hb2scale"
          val sig3="decimator_controls_hb3scale"
          val sig4="decimator_controls_mode"
          val sig4limit=2
        }
        val header="//This is a tesbench generated with scala generator\n"
        val extpars=extpargen()
        val textTemplate=header+ extpars+"""
                        |//timescale 1ps this should probably be a global model parameter 
                        |parameter integer c_Ts=1/(g_Rs_high*1e-12);
                        |parameter integer c_ratio0=g_Rs_high/(8*g_Rs_low);
                        |parameter integer c_ratio1=g_Rs_high/(4*g_Rs_low);
                        |parameter integer c_ratio2=g_Rs_high/(2*g_Rs_low);
                        |parameter integer c_ratio3=g_Rs_high/(g_Rs_low);
                        |parameter RESET_TIME = 5*c_Ts;
                        |
                        |//These registers always needed
                        |reg clock;
                        |reg reset;
                        |
                        |//register to set the clock division ratio
                        |reg [7:0] io_Ndiv;
                        |reg io_reset_clk;
                        |
                        |//Registers for inputs
                        |reg signed [{{ulimit}}:0] io_iptr_A_real = 0;
                        |reg signed [{{ulimit}}:0] io_iptr_A_imag = 0;
                        |reg signed [{{gainlimit}}:0] io_{{sig0}};
                        |reg signed [{{gainlimit}}:0] io_{{sig1}};
                        |reg signed [{{gainlimit}}:0] io_{{sig2}};
                        |reg signed [{{gainlimit}}:0] io_{{sig3}};
                        |reg signed [{{sig4limit}}:0] io_{{sig4}};
                        |
                        |//Resisters for outputs
                        |wire signed [{{ulimit}}:0] io_Z_0_real;
                        |wire signed [{{ulimit}}:0] io_Z_0_imag;
                        |wire signed [{{ulimit}}:0] io_Z_1_real;
                        |wire signed [{{ulimit}}:0] io_Z_1_imag;
                        |
                        |//Wires for additional clocks
                        |wire io_{{clk0}};
                        |wire io_{{clk1}};
                        |wire io_{{clk2}};
                        |wire io_{{clk3}};

                        |//File IO parameters
                        |integer StatusI, StatusO, infile, outfile;
                        |integer count0;
                        |integer count1;
                        |integer count2;
                        |integer count3;
                        |integer din1,din2;
                        |
                        |//Initializations
                        |initial count0 = 0;
                        |initial count1 = 0;
                        |initial count2 = 0;
                        |initial count3 = 0;
                        |initial clock = 1'b0;
                        |initial reset = 1'b0;
                        |initial outfile = $fopen(g_outfile,"w"); // For writing
                        |
                        |//Clock definitions
                        |always #(c_Ts)clock = !clock ;
                        |//always @(posedge clock) begin 
                        |//    if (count0%c_ratio0/2 == 0) begin
                        |//        io_{{clk0}} =! io_{{clk0}};
                        |//    end 
                        |//    count0++;
                        |//end
                        |//always @(posedge clock) begin 
                        |//    if (count1%c_ratio1/2 == 0) begin
                        |//        io_{{clk1}} =! io_{{clk1}};
                        |//    end 
                        |//    count1++;
                        |//end
                        |//always @(posedge clock) begin 
                        |//    if (count2%c_ratio2/2 == 0) begin
                        |//        io_{{clk2}} =! io_{{clk2}};
                        |//    end 
                        |//    count2++;
                        |//end
                        |//always @(posedge clock) begin 
                        |//    if (count3%c_ratio3/2 == 0) begin
                        |//        io_{{clk3}} =! io_{{clk3}};
                        |//    end 
                        |//    count3++;
                        |//end
                        | 
                        |//always @(posedge io_{{clk0}}) begin 
                        |//always @(posedge io_{{clk1}}) begin 
                        |//always @(posedge io_{{clk2}}) begin 
                        |always @(posedge io_{{clk3}}) begin 
                        |    //Print only valid values 
                        |    if (~($isunknown( io_Z_0_real)) &&   ~($isunknown( io_Z_0_imag)) && ~($isunknown( io_Z_1_real)) && ~($isunknown( io_Z_1_imag))) begin
                        |        $fwrite(outfile, "%d\t%d\t%d\t%d\n", io_Z_0_real, io_Z_0_imag, io_Z_1_real, io_Z_1_imag);
                        |    end
                        |    else begin
                        |        $fwrite(outfile, "%d\t%d\t%d\t%d\n",0,0,0,0);
                        |    end 
                        |end
                        |
                        |//Clock divider model
                        |clkdiv_n_2_4_8 clockdiv( // @[:@3.2]
                        |  .clock(clock), // @[:@4.4]
                        |  .reset(reset), // @[:@5.4]
                        |  .io_Ndiv(io_Ndiv), // @[:@6.4]
                        |  .io_reset_clk(io_reset_clk), // @[:@6.4]
                        |  .io_clkpn (io_{{clk0}}), // @[:@6.4]
                        |  .io_clkp2n(io_{{clk1}}), // @[:@6.4]
                        |  .io_clkp4n(io_{{clk2}}), // @[:@6.4]
                        |  .io_clkp8n(io_{{clk3}})// @[:@6.4]
                        |);
                        |
                        |//DUT definition
                        |{{dutmod}} DUT ( // @[:@3740.2]
                        |    .clock(clock), // @[:@3741.4]
                        |    .reset(reset), // @[:@3742.4]
                        |    .io_{{clk0}}(io_{{clk0}}), // @[:@3743.4]
                        |    .io_{{clk1}}(io_{{clk1}}), // @[:@3743.4]
                        |    .io_{{clk2}}(io_{{clk2}}), // @[:@3743.4]
                        |    .io_{{clk3}}(io_{{clk3}}), // @[:@3743.4]
                        |    .io_{{sig0}}(io_{{sig0}}), // @[:@3743.4]
                        |    .io_{{sig1}}(io_{{sig1}}), // @[:@3743.4]
                        |    .io_{{sig2}}(io_{{sig2}}), // @[:@3743.4]
                        |    .io_{{sig3}}(io_{{sig3}}), // @[:@3743.4]
                        |    .io_{{sig4}}(io_{{sig4}}), // @[:@3743.4]
                        |    .io_iptr_A_real, // @[:@3743.4]
                        |    .io_iptr_A_imag, // @[:@3743.4]
                        |    .io_Z_0_real, // @[:@3743.4]
                        |    .io_Z_0_imag, // @[:@3743.4]
                        |    .io_Z_1_real, // @[:@3743.4]
                        |    .io_Z_1_imag // @[:@3743.4]
                        |   );
                        |
                        |initial #0 begin
                        |    io_{{sig0}} = g_scale0;
                        |    io_{{sig1}} = g_scale1;
                        |    io_{{sig2}} = g_scale2;
                        |    io_{{sig3}} = g_scale3;
                        |    io_{{sig4}} = g_mode;
                        |    io_Ndiv= c_ratio0;
                        |    reset=1;
                        |    io_reset_clk=1;
                        |    #RESET_TIME
                        |    reset=0;
                        |    io_reset_clk=0;
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

