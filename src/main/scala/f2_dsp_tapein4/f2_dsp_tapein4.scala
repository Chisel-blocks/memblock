// See LICENSE for license details.
//
//Start with a static tb and try to genererate a gnerator for it
package f2_dsp_tapein4
import chisel3._
import chisel3.util._
import chisel3.experimental._
import dsptools._
import dsptools.numbers._
import f2_decimator._
import f2_rx_path_tapein4._


class f2_dsp_io(n: Int=16, antennas: Int=4, users: Int=4 ) extends Bundle {
    val decimator_controls = Vec(antennas,new f2_decimator_controls(gainbits=10))
    val iptr_A      = Input(Vec(antennas,DspComplex(SInt(n.W), SInt(n.W))))
    val Z           = Output(Vec(antennas,Vec(users,DspComplex(SInt(n.W), SInt(n.W)))))
    override def cloneType = (new f2_dsp_io(n,antennas,users)).asInstanceOf[this.type]
}

class f2_dsp_tapein4 (n: Int=16, antennas: Int=4, users: Int=4) extends Module {
  val io = IO( new f2_dsp_io(n=n,users=users)
  )

  val rx_path  = VecInit(Seq.fill(antennas){ Module ( new  f2_rx_path_tapein4 (n=n, users=users)).io })
  
  for ( i <- 0 to antennas-1){ 
      io.decimator_controls(i)<>rx_path(i).decimator_controls
      rx_path(i).iptr_A:=io.iptr_A(i)
      for ( k <- 0 to users-1 ) { 
          //How to use Valid?
          io.Z(i)(k):=rx_path(i).Z(k)
      }
  }
}

//This gives you verilog
object f2_dsp_tapein4 extends App {
  chisel3.Driver.execute(args, () => new f2_dsp_tapein4)
}

