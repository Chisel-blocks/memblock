// See LICENSE for license details.
//
//Start with a static tb and try to genererate a gnerator for it
package f2_rx_path_tapein6
import chisel3._
import chisel3.util._
import chisel3.experimental._
import dsptools._
import dsptools.numbers._
import f2_decimator._


class f2_rx_path_io(val n: Int=16, val users: Int=4 ) extends Bundle {
    val decimator_clocks = new f2_decimator_clocks()
    val decimator_controls = new f2_decimator_controls(gainbits=10)
    val iptr_A      = Input(DspComplex(SInt(n.W), SInt(n.W)))
    val Z           = Output(Vec(users,DspComplex(SInt(n.W), SInt(n.W))))
    //override def cloneType = (new f2_rx_path_io(n,users)).asInstanceOf[this.type]
}

class f2_rx_path_tapein6 (n: Int=16, users: Int=4) extends Module {
  val io = IO( new f2_rx_path_io(n=n,users=users)
  )

  val decimator  = Module ( new  f2_decimator (n=16, resolution=32, coeffres=16, gainbits=10))
  io.decimator_controls<>decimator.io.controls
  io.decimator_clocks<>decimator.io.clocks
  decimator.io.iptr_A:=io.iptr_A
  io.Z.map(_:=decimator.io.Z)
}

//This gives you verilog
object f2_rx_path_tapein6 extends App {
  chisel3.Driver.execute(args, () => new f2_rx_path_tapein6)
}

