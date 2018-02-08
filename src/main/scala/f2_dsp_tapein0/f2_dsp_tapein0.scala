// See LICENSE for license details.
//
//Start with a static tb and try to genererate a gnerator for it
package f2_dsp_tapein0
import chisel3._
import chisel3.util._
import chisel3.experimental._
import dsptools._
import dsptools.numbers._
import f2_decimator._


class f2_dsp_io(n: Int=16, users: Int=2 ) extends Bundle {
    val decimator_controls = new f2_decimator_controls(gainbits=10)
    val iptr_A      = Input(DspComplex(SInt(n.W), SInt(n.W)))
    val Z           = Output(Vec(users,DspComplex(SInt(n.W), SInt(n.W))))

}

class f2_dsp_tapein0 (n: Int=16, users: Int=2) extends Module {
  val io = IO( new f2_dsp_io(n=n,users=users)
  )

  val decimator  = Module ( new  f2_decimator (n=16, resolution=32, coeffres=16, gainbits=10))
  io.decimator_controls<>decimator.io.controls
  decimator.io.iptr_A:=io.iptr_A
  
  for ( i <- 0 to users-1 ) { 
      //How to use Valid?
      io.Z(i):=decimator.io.Z
  }
}

//This gives you verilog
object f2_dsp_tapein0 extends App {
  chisel3.Driver.execute(args, () => new f2_dsp_tapein0)
}

