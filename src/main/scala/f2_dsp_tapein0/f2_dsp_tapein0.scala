// See LICENSE for license details.
//
//Start with a static tb and try to genererate a gnerator for it
package f2_dsp_tapein0

import chisel3.experimental._
import chisel3._
import datatypes._

class f2_dsp_tapein0 (n: Int=16) extends Module {
  val io = IO(new Bundle {
    val clock_DSP     = Input(Clock())
    val iptr_A        = new complexIn(n=n)
    val Z             = Flipped(new complexIn(n=n))
  })

  val inreg  = RegNext(io.iptr_A)

  withClock (io.clock_DSP){
    io.Z := RegNext(inreg)
  }

}

//This gives you verilog
object f2_dsp_tapein0 extends App {
  chisel3.Driver.execute(args, () => new f2_dsp_tapein0)
}

