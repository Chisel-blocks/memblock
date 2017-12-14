// See LICENSE for license details.
//Look at handlebars
//
//Start with a static tb and try to genererate a gnerator for it
package f2_dsp_tapein0

import chisel3._
import java.io.{File, FileWriter, BufferedWriter}

class f2_dsp_tapein0 extends Module {
  val io = IO(new Bundle {
    val iptr_A        = Input(UInt(16.W))
    val _Z             = Output(UInt(16.W))
  })

  val x  = Reg(UInt())

  x := io.iptr_A
  io._Z := x
}

//This gives you verilog
object f2_dsp_tapein0 extends App {
  chisel3.Driver.execute(args, () => new f2_dsp_tapein0)
}

