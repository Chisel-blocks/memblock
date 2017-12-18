// See LICENSE for license details.
//Look at handlebars
//
//Start with a static tb and try to genererate a gnerator for it
package f2_dsp_tapein0

import chisel3.experimental._
import chisel3._

class f2_dsp_tapein0 extends Module {
  val io = IO(new Bundle {
    val clock_DSP     = Input(Clock())
    val iptr_Areal    = Input(UInt(16.W))
    val iptr_Aimag    = Input(UInt(16.W))
    val Zreal         = Output(UInt(16.W))
    val Zimag         = Output(UInt(16.W))
  })

  val inregreal  = RegNext(io.iptr_Areal)
  val inregimag  = RegNext(io.iptr_Aimag)

  withClock (io.clock_DSP){
    io.Zreal := RegNext(inregreal)
    io.Zimag := RegNext(inregimag)
  }

}

//This gives you verilog
object f2_dsp_tapein0 extends App {
  chisel3.Driver.execute(args, () => new f2_dsp_tapein0)
}

