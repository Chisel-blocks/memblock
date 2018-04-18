// See LICENSE for license details.
// Use handlebars for template generation
//
//Start with a static tb and try to genererate a gnerator for it
// This uses clkdiv_n_2_4_8 verilog. You need to compile it separately

package f2_dsp_tapein6

import chisel3._
import java.io.{File, FileWriter, BufferedWriter}
import com.gilt.handlebars.scala.binding.dynamic._
import com.gilt.handlebars.scala.Handlebars

//Testbench.
object tb_f2_dsp_tapein6 {
    def extpargen(): String={
        val extpars=Seq(("g_infile","\"./A.txt\""), 
                        ("g_outfile","\"./Z.txt\""),
                        ("g_Rs_high","16*8*20.0e6"),
                        ("g_Rs_low ","20.0e6"),
                        ("g_scale0 ","1"),
                        ("g_scale1 ","1"),
                        ("g_scale2 ","1"),
                        ("g_scale3 ","1"),
                        ("g_user_index","0"),
                        ("g_antenna_index","0"),
                        ("g_rx_output_mode","0"),
                        ("g_input_mode","0"),
                        ("g_mode   ","4"),
                        ("g_adc_fifo_mode   ","0"))

        var externalpars=("//Things you want to control from the simulator cmdline must be parameters \nmodule {{oname}} #("
                                +extpars.map{ case (par,value) => "parameter %s = %s,\n            ".format(par,value)}.mkString)
        externalpars=externalpars.patch(externalpars.lastIndexOf(','),"",1)+");"
         externalpars
    }
    // This is insane
    // This must be done by a method processing direction-name-width tuples
    def main(args: Array[String]): Unit = {
        val name= this.getClass.getSimpleName.split("\\$").last
        val tb = new BufferedWriter(new FileWriter("./verilog/"+name+".v"))
        object tbvars {
          val oname=name
          val dutmod = "f2_dsp_tapein6" 
          val n = 16
          val inputn = 9
          val inlimit = 8
          val ulimit= 15
          val gainbits= 10
          val decimator_modebits= 3
          val rx_output_modebits= 3
          val input_modebits= 3
          val adc_fifo_modebits= 1
          val indexbits= 2
          val gainlimit=gainbits-1
          val clk0="decimator_controls_0_cic3clockslow"
          val clk1="decimator_controls_0_hb1clock_low"
          val clk2="decimator_controls_0_hb2clock_low"
          val clk3="decimator_controls_0_hb3clock_low"
          val clk4="decimator_controls_1_cic3clockslow"
          val clk5="decimator_controls_1_hb1clock_low"
          val clk6="decimator_controls_1_hb2clock_low"
          val clk7="decimator_controls_1_hb3clock_low"
          val clk8="decimator_controls_2_cic3clockslow"
          val clk9="decimator_controls_2_hb1clock_low"
          val clk10="decimator_controls_2_hb2clock_low"
          val clk11="decimator_controls_2_hb3clock_low"
          val clk12="decimator_controls_3_cic3clockslow"
          val clk13="decimator_controls_3_hb1clock_low"
          val clk14="decimator_controls_3_hb2clock_low"
          val clk15="decimator_controls_3_hb3clock_low"
          val clk16="clock_symrate"
          val clk17="clock_symratex4"
          val clk18="clock_outfifo_deq"
          val clk19="clock_infifo_enq"
          val clk20="adc_clocks_0"
          val clk21="adc_clocks_1"
          val clk22="adc_clocks_2"
          val clk23="adc_clocks_3"
          val sig0="decimator_controls_0_cic3integscale"
          val sig1="decimator_controls_0_hb1scale"
          val sig2="decimator_controls_0_hb2scale"
          val sig3="decimator_controls_0_hb3scale"
          val sig4="decimator_controls_0_mode"
          val sig4limit=2
          val sig5="decimator_controls_1_cic3integscale"
          val sig6="decimator_controls_1_hb1scale"
          val sig7="decimator_controls_1_hb2scale"
          val sig8="decimator_controls_1_hb3scale"
          val sig9="decimator_controls_1_mode"
          val sig10="decimator_controls_2_cic3integscale"
          val sig11="decimator_controls_2_hb1scale"
          val sig12="decimator_controls_2_hb2scale"
          val sig13="decimator_controls_2_hb3scale"
          val sig14="decimator_controls_2_mode"
          val sig15="decimator_controls_3_cic3integscale"
          val sig16="decimator_controls_3_hb1scale"
          val sig17="decimator_controls_3_hb2scale"
          val sig18="decimator_controls_3_hb3scale"
          val sig19="decimator_controls_3_mode"
          val sig20="user_index"
          val sig21="antenna_index"
          val indexlimit=1
          val sig22="reset_index_count"
          val sig23="reset_outfifo"
          val sig24="reset_infifo"
          val rx_omodelimit=2
          val sig25="rx_output_mode"
          val inputmodelimit=2
          val sig26="input_mode"
          val sig27="adc_fifo_mode"
          val sig28="iptr_A_0_real"
          val sig29="iptr_A_0_imag"
          val sig30="iptr_A_1_real"
          val sig31="iptr_A_1_imag"
          val sig32="iptr_A_2_real"
          val sig33="iptr_A_2_imag"
          val sig34="iptr_A_3_real"
          val sig35="iptr_A_3_imag"
          val sig36="Z_ready"
          val sig37="Z_valid"
          val sig38="ofifo_bits_data_0_real"
          val sig39="ofifo_bits_data_0_imag"
          val sig40="ofifo_bits_data_1_real"
          val sig41="ofifo_bits_data_1_imag"
          val sig42="ofifo_bits_data_2_real"
          val sig43="ofifo_bits_data_2_imag"
          val sig44="ofifo_bits_data_3_real"
          val sig45="ofifo_bits_data_3_imag"
          val sig46="iptr_fifo_ready"
          val sig47="iptr_fifo_valid"
          val sig48="iptr_fifo_bits_data_0_real"
          val sig49="iptr_fifo_bits_data_0_imag"
          val sig50="iptr_fifo_bits_data_1_real"
          val sig51="iptr_fifo_bits_data_1_imag"
          val sig52="iptr_fifo_bits_data_2_real"
          val sig53="iptr_fifo_bits_data_2_imag"
          val sig54="iptr_fifo_bits_data_3_real"
          val sig55="iptr_fifo_bits_data_3_imag"
          val sig56="reset_adcfifo"
          //(type,name,upperlimit,lowerlimit, assign,init)    
          val ioseq=Seq(("clock","clock"),
                        ("reset","reset"),
                        ("wire","clkpn"),
                        ("wire","clkp2n"),
                        ("wire","clkp4n"),
                        ("wire","clkp8n"),
                        ("dclk","decimator_clocks_cic3clockslow","clkpn"),
                        ("dclk","decimator_clocks_hb1clock_low","clkp2n"),
                        ("dclk","decimator_clocks_hb2clock_low","clkp4n"),
                        ("dclk","decimator_clocks_hb3clock_low","clkp8n"),
                        ("dclk","clock_symrate","clkp8n"),
                        ("dclk","clock_symratex4","clkp2n"),
                        ("dclk","clock_outfifo_deq","clock_symrate"),
                        ("dclk","clock_infifo_enq","clock_symrate"),
                        ("dclk","adc_clocks_0","clock"),
                        ("dclk","adc_clocks_1","clock"),
                        ("dclk","adc_clocks_2","clock"),
                        ("dclk","adc_clocks_3","clock"),
                        ("reg","Ndiv",7,0),
                        ("reg","reset_clk"),
                        ("in","decimator_controls_0_cic3integscale",gainbits-1,0),
                        ("in","decimator_controls_0_hb1scale",gainbits-1,0),
                        ("in","decimator_controls_0_hb2scale",gainbits-1,0),
                        ("in","decimator_controls_0_hb3scale",gainbits-1,0),
                        ("in","decimator_controls_0_mode",decimator_modebits-1,0),
                        ("in","decimator_controls_1_cic3integscale",gainbits-1,0),
                        ("in","decimator_controls_1_hb1scale",gainbits-1,0),
                        ("in","decimator_controls_1_hb2scale",gainbits-1,0),
                        ("in","decimator_controls_1_hb3scale",gainbits-1,0),
                        ("in","decimator_controls_1_mode",decimator_modebits-1,0),
                        ("in","decimator_controls_2_cic3integscale",gainbits-1,0),
                        ("in","decimator_controls_2_hb1scale",gainbits-1,0),
                        ("in","decimator_controls_2_hb2scale",gainbits-1,0),
                        ("in","decimator_controls_2_hb3scale",gainbits-1,0),
                        ("in","decimator_controls_2_mode",decimator_modebits-1,0),
                        ("in","decimator_controls_3_cic3integscale",gainbits-1,0),
                        ("in","decimator_controls_3_hb1scale",gainbits-1,0),
                        ("in","decimator_controls_3_hb2scale",gainbits-1,0),
                        ("in","decimator_controls_3_hb3scale",gainbits-1,0),
                        ("in","decimator_controls_3_mode",decimator_modebits-1,0),
                        ("in","user_index",indexbits-1,0),
                        ("in","antenna_index",indexbits-1,0),
                        ("in","reset_index_count"),
                        ("in","reset_outfifo"),
                        ("in","reset_adcfifo"),
                        ("in","reset_infifo"),
                        ("in","rx_output_mode",rx_output_modebits-1,0),
                        ("in","input_mode",input_modebits-1,0),
                        ("in","adc_fifo_mode",adc_fifo_modebits-1,0),
                        ("in","iptr_A_0_real",inputn-1,0),
                        ("in","iptr_A_0_imag",inputn-1,0),
                        ("in","iptr_A_1_real",inputn-1,0),
                        ("in","iptr_A_1_imag",inputn-1,0),
                        ("in","iptr_A_2_real",inputn-1,0),
                        ("in","iptr_A_2_imag",inputn-1,0),
                        ("in","iptr_A_3_real",inputn-1,0),
                        ("in","iptr_A_3_imag",inputn-1,0),
                        ("outs","ofifo_bits_data_0_real",n-1,0),
                        ("outs","ofifo_bits_data_0_imag",n-1,0),
                        ("outs","ofifo_bits_data_1_real",n-1,0),
                        ("outs","ofifo_bits_data_1_imag",n-1,0),
                        ("outs","ofifo_bits_data_2_real",n-1,0),
                        ("outs","ofifo_bits_data_2_imag",n-1,0),
                        ("outs","ofifo_bits_data_3_real",n-1,0),
                        ("outs","ofifo_bits_data_3_imag",n-1,0),
                        ("out","ofifo_bits_index",1,0),
                        ("in","ofifo_ready"),
                        ("out","ofifo_valid"),
                        ("out","iptr_fifo_ready"),
                        ("in","iptr_fifo_valid"),
                        ("in","iptr_fifo_bits_data_0_real",n-1,0),
                        ("in","iptr_fifo_bits_data_0_imag",n-1,0),
                        ("in","iptr_fifo_bits_data_1_real",n-1,0),
                        ("in","iptr_fifo_bits_data_1_imag",n-1,0),
                        ("in","iptr_fifo_bits_data_2_real",n-1,0),
                        ("in","iptr_fifo_bits_data_2_imag",n-1,0),
                        ("in","iptr_fifo_bits_data_3_real",n-1,0),
                        ("in","iptr_fifo_bits_data_3_imag",n-1,0),
                        ("in","iptr_fifo_bits_index",1,0)
                        )
        }
        val header="//This is a tesbench generated with scala generator\n"
        val extpars=extpargen()
        var dutdef="""//DUT definition%n    %s DUT (""".format(tbvars.dutmod)+
                     tbvars.ioseq.map{ 
                         case ("reg",name) => ""
                         case ("reg",name,ul,dl)  => ""
                         case ("wire",name) => ""
                         case ("reset",name) => ".%s(%s),\n    ".format(name,name)
                         case ("clock",name) => ".%s(%s),\n    ".format(name,name)
                         case ("dclk",name,assign) => ".io_%s(io_%s),\n    ".format(name,name)
                         case (dir,name) => ".io_%s(io_%s),\n    ".format(name,name)
                         case (dir,name,ul,dl) => ".io_%s(io_%s),\n    ".format(name,name)
                         case (dir,name,ul,dl,init) => ".io_%s(io_%s),\n    ".format(name,name)
                         case (dir,name,ul,dl,init,assign) => ".io_%s(io_%s),\n    ".format(name,name)
                     }.mkString
        dutdef=dutdef.patch(dutdef.lastIndexOf(','),"",1)+");"

        val regdef="""//Registers for inputs %n""".format() +
                     tbvars.ioseq.map{ 
                         case ("reset",name) => "reg %s;\n".format(name)
                         case ("clock",name) => "reg %s;\n".format(name)
                         case ("in"|"reg",name) => "reg io_%s;\n".format(name)
                         case ("in"|"reg",name,ul,dl) => "reg [%s:%s] io_%s;\n".format(ul,dl,name)
                         case ("in"|"reg",name,ul,dl,init) => "reg [%s:%s] io_%s;\n".format(ul,dl,name)
                         case ("in"|"reg",name,ul,dl,init,assign) => "reg [%s:%s] io_%s;\n".format(ul,dl,name)
                         case ("ins"|"regs",name,ul,dl) => "reg signed [%s:%s] io_%s;\n".format(ul,dl,name)
                         case ("ins"|"regs",name,ul,dl,init) => "reg signed [%s:%s] io_%s;\n".format(ul,dl,name)
                         case _ => ""
                     }.mkString

        val wiredef="""//Wires for outputs %n""".format() +
                     tbvars.ioseq.map{ 
                         case ("dclk",name) => "wire io_%s;\n".format(name)
                         case ("dclk",name,assign) => "wire io_%s;\n".format(name)
                         case ("out",name) => "wire io_%s;\n".format(name)
                         case ("out"|"wire",name) => "wire io_%s;\n".format(name)
                         case ("out"|"wire",name,ul,dl) => "wire [%s:%s] io_%s;\n".format(ul,dl,name)
                         case ("out"|"wire",name,ul,dl,init) => "wire [%s:%s] io_%s;\n".format(ul,dl,name)
                         case ("out"|"wire",name,ul,dl,init,assign) => "wire io_%s;\n".format(name)
                         case ("outs"|"wires",name,ul,dl) => "wire signed [%s:%s] io_%s;\n".format(ul,dl,name)
                         case ("outs"|"wires",name,ul,dl,init) => "wire signed [%s:%s] io_%s;\n".format(ul,dl,name)
                         case _ => ""
                     }.mkString

        val assdef="""//Assignments %n""".format()+
                     tbvars.ioseq.map{ 
                         case ("dclk",name,"clock") => "assign io_%s=%s;\n".format(name,"clock")
                         case ("dclk",name,assign) => "assign io_%s=io_%s;\n".format(name,assign)
                         case ("out",name,ul,dl,init,assign) => "assign io_%s=io_%s;\n".format(name,assign)
                         case ("in",name,ul,dl,init,assign) => "assign io_%s=io_%s;\n".format(name,assign)
                         case _ => ""
                     }.mkString


        val textTemplate=header+ extpars+"""
                        |//timescale 1ps this should probably be a global model parameter 
                        |parameter integer c_Ts=1/(g_Rs_high*1e-12);
                        |parameter integer c_ratio0=g_Rs_high/(8*g_Rs_low);
                        |parameter integer c_ratio1=g_Rs_high/(4*g_Rs_low);
                        |parameter integer c_ratio2=g_Rs_high/(2*g_Rs_low);
                        |parameter integer c_ratio3=g_Rs_high/(g_Rs_low);
                        |parameter RESET_TIME = 16*c_Ts;
                        |
                        |""".stripMargin('|')+regdef+wiredef+assdef+
                        """|
                        |//File IO parameters
                        |integer StatusI, StatusO, infile, outfile;
                        |integer din1,din2,din3,din4,din5,din6,din7,din8;
                        |
                        |//Initializations
                        |initial clock = 1'b0;
                        |initial reset = 1'b0;
                        |initial outfile = $fopen(g_outfile,"w"); // For writing
                        |
                        |//Clock definitions
                        |always #(c_Ts)clock = !clock ;
                        | 
                        |//Read this with Ouput fifo enquque clk
                        |always @(posedge io_{{clk18}}) begin 
                        |    //Print only valid values 
                        |    if (~$isunknown(io_{{sig38}})) begin
                        |        $fwrite(outfile, "%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\n", 
                        |                         io_ofifo_bits_data_0_real, io_ofifo_bits_data_0_imag, 
                        |                         io_ofifo_bits_data_1_real, io_ofifo_bits_data_1_imag, 
                        |                         io_ofifo_bits_data_2_real, io_ofifo_bits_data_2_imag, 
                        |                         io_ofifo_bits_data_3_real, io_ofifo_bits_data_3_imag);
                        |    end
                        |    else begin
                        |        $fwrite(outfile, "%d\t%d\t%d\t%d\t\%d\t%d\t%d\t%d\n",0,0,0,0,0,0,0,0);
                        |    end 
                        |end
                        |
                        |//Clock divider model
                        |clkdiv_n_2_4_8 clockdiv( // @[:@3.2]
                        |  .clock(clock), // @[:@4.4]
                        |  .reset(reset), // @[:@5.4]
                        |  .io_Ndiv(io_Ndiv), // @[:@6.4]
                        |  .io_reset_clk(io_reset_clk), // @[:@6.4]
                        |  .io_clkpn (io_clkpn), // @[:@6.4]
                        |  .io_clkp2n(io_clkp2n), // @[:@6.4]
                        |  .io_clkp4n(io_clkp4n), // @[:@6.4]
                        |  .io_clkp8n(io_clkp8n)// @[:@6.4]
                        |);
                        |
                        |""".stripMargin('|')+dutdef+
                        """
                        |
                        |initial #0 begin
                        |    io_{{sig0}} = g_scale0;
                        |    io_{{sig1}} = g_scale1;
                        |    io_{{sig2}} = g_scale2;
                        |    io_{{sig3}} = g_scale3;
                        |    io_{{sig4}} = g_mode;
                        |    io_{{sig5}} = g_scale0;
                        |    io_{{sig6}} = g_scale1;
                        |    io_{{sig7}} = g_scale2;
                        |    io_{{sig8}} = g_scale3;
                        |    io_{{sig9}} = g_mode;
                        |    io_{{sig10}} = g_scale0;
                        |    io_{{sig11}} = g_scale1;
                        |    io_{{sig12}} = g_scale2;
                        |    io_{{sig13}} = g_scale3;
                        |    io_{{sig14}} = g_mode;
                        |    io_{{sig15}} = g_scale0;
                        |    io_{{sig16}} = g_scale1;
                        |    io_{{sig17}} = g_scale2;
                        |    io_{{sig18}} = g_scale3;
                        |    io_{{sig19}} = g_mode;
                        |    io_{{sig20}} = g_user_index;
                        |    io_{{sig21}} = g_antenna_index;
                        |    io_{{sig25}} = g_rx_output_mode;
                        |    io_{{sig27}} = g_adc_fifo_mode;
                        |    io_Ndiv= c_ratio0;
                        |    //Resets
                        |    reset=1;
                        |    io_reset_clk=1;
                        |    io_reset_adcfifo=1;
                        |    io_{{sig22}} = 1;
                        |    io_{{sig23}} = 1;
                        |    io_{{sig24}} = 1;
                        |    io_ofifo_ready = 1;
                        |    #RESET_TIME
                        |    reset=0;
                        |    io_reset_clk=0;
                        |    #(16*RESET_TIME)
                        |    io_reset_adcfifo=0;
                        |    io_{{sig22}} = 0;
                        |    io_{{sig23}} = 0;
                        |    io_{{sig24}} = 0;
                        |
                        |    infile = $fopen(g_infile,"r"); // For reading
                        |    while (!$feof(infile)) begin
                        |            @(posedge clock) 
                        |             StatusI=$fscanf(infile, "%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\n",
                        |                             din1, din2, din3, din4,din5, din6, din7, din8);
                        |             io_iptr_A_0_real <= din1;
                        |             io_iptr_A_0_imag <= din2;
                        |             io_iptr_A_1_real <= din3;
                        |             io_iptr_A_1_imag <= din4;
                        |             io_iptr_A_2_real <= din5;
                        |             io_iptr_A_2_imag <= din6;
                        |             io_iptr_A_3_real <= din7;
                        |             io_iptr_A_3_imag <= din8;
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

