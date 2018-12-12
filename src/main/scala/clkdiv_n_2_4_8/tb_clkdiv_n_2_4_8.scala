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
    // This is insane
    // This must be done by a method processing direction-name-width tuples
    object tbvars {
        val oname=name
        val dutmod = "clkdiv_n_2_4_8" 
        val n = 8

        val paramseq=Seq(("g_Rs_high","16*8*20.0e6"),
                      ("g_Ndiv","16"),
                      ("g_shift","0"))

       //(type,name,upperlimit,lowerlimit, assign,init)    
       //("None","None","None","None","None","None")
       val ioseq=Seq(("clock","clock","None","None","None","None"),
                     ("reset","reset","None","None","None",1),
                     ("in","io_reset_clk",0,0,"None",1),
                     ("in","io_Ndiv",n-1,0,"None","g_Ndiv"),
                     ("in","io_shift",1,0,"None","g_shift"),
                     ("out","io_clkpn","None","None","None","None"),
                     ("out","io_clkp2n","None","None","None","None"),
                     ("out","io_clkp4n","None","None","None","None"),
                     ("out","io_clkp8n","None","None","None","None")
                     )
   }
    val header="//This is a tesbench generated with scala generator\n"
    var extpars="""//Things you want to control from the simulator cmdline must be parameters %nmodule %s #(""".format(tbvars.oname)+
                   tbvars.paramseq.map{ 
                       case (par,value) => "parameter %s = %s,\n            ".format(par,value)
                   }.mkString
    extpars=extpars.patch(extpars.lastIndexOf(','),"",1)+");"
    var dutdef="""//DUT definition%n    %s DUT (""".format(tbvars.dutmod)+
                 tbvars.ioseq.map{ 
                     case ("reg",name,ul,dl,assingn,init)  => ""
                     case ("wire",name,ul,dl,assingn,init)  => ""
                     case ("reset"|"clock",name,ul,dl,assign,init)  => ".%s(%s),\n    ".format(name,name)
                     case (dir,name,ul,dl,assign,init) => ".%s(%s),\n    ".format(name,name)
                     case _ => ""
                 }.mkString
    dutdef=dutdef.patch(dutdef.lastIndexOf(','),"",1)+");"
 
    val regdef="""//Registers for inputs %n""".format() +
                 tbvars.ioseq.map{ 
                     case ("clock",name,ul,dl,assign,init)  => "reg %s;\n".format(name)
                     case ("reset",name,ul,dl,assign,init) => "reg %s;\n".format(name)
                     case ("in"|"reg",name,"None","None",assign,init) => "reg %s;\n".format(name)
                     case ("in"|"reg",name,ul,dl,assign,init) => "reg [%s:%s] %s;\n".format(ul,dl,name)
                     case ("ins"|"regs",name,ul,dl,assign,init) => "reg signed [%s:%s] %s;\n".format(ul,dl,name)
                     case _ => ""
                 }.mkString
 
    val wiredef="""//Wires for outputs %n""".format() +
                 tbvars.ioseq.map{ 
                     case ("dclk"|"out"|"wire",name,"None","None",assign,init) => "wire %s;\n".format(name)
                     case ("out"|"wire",name,ul,dl,assign,init) => "wire [%s:%s] %s;\n".format(ul,dl,name)
                     case ("outs"|"wires",name,ul,dl,assign,init) => "wire signed [%s:%s] %s;\n".format(ul,dl,name)
                     case _ => ""
                 }.mkString
 
    val assdef="""//Assignments %n""".format()+
                 tbvars.ioseq.map{ 
                     case ("dclk"|"out"|"in",name,ul,dl,"None",init) => ""
                     case ("dclk"|"out"|"in",name,ul,dl,"clock",init) => "assign %s=clock;\n".format(name)
                     case ("dclk"|"out"|"in",name,ul,dl,"reset",init) => "assign %s=reset;\n".format(name)
                     case ("dclk"|"out"|"in",name,ul,dl,assign,init) => "assign %s=%s;\n".format(name,assign)
                     case _ => ""
                 }.mkString
 
    val initialdef="""%n%n//Initial values %ninitial #0 begin%n""".format()+
                 tbvars.ioseq.map{ 
                     case ( dir,name,ul,dl,assign,"None") => ""
                     case ("reset",name,ul,dl,assign,init) => "    %s=%s;\n".format(name,init)
                     case ("reset" | "in" | "wire" | "reg" |"wires" | "regs" ,name,ul,dl,assign,init) => "    %s=%s;\n".format(name,init)
                     case _ => ""
                 }.mkString
                 
 
 
    val textTemplate=header+ extpars+"""
                    |//timescale 1ps this should probably be a global model parameter 
                    |parameter integer c_Ts=1/(g_Rs_high*1e-12);
                    |parameter RESET_TIME = 16*c_Ts;
                    |
                    |""".stripMargin('|')+regdef+wiredef+assdef+
                    """|
                    |//File IO parameters
                    |
                    |//Initializations
                    |initial clock = 1'b0;
                    |initial reset = 1'b0;
                    |
                    |//Clock definitions
                    |always #(c_Ts)clock = !clock ;
                    |
                    |""".stripMargin('|')+dutdef+initialdef+
                    """
                    |    #RESET_TIME
                    |    reset=0;
                    |    io_reset_clk=0;
                    |    #(4096*RESET_TIME)
                    |    $finish;
                    |end
                    |endmodule""".stripMargin('|')
    //val testbench=Handlebars(textTemplate)
    //tb write testbench(tbvars)
    tb write textTemplate
    tb.close()
  }
}



