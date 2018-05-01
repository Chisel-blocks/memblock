// See LICENSE for license details.
//
//Start with a static tb and try to genererate a gnerator for it
package prog_delay
import chisel3._
import chisel3.util._
import chisel3.experimental._
import dsptools._
import dsptools.numbers._

class prog_delay_io[ T <: Data]( proto: T, val maxdelay: Int=64) extends Bundle {
   requireIsChiselType(proto)
   val iptr_A= Input(proto)
   val optr_Z= Output(proto)
   val select= Input(UInt(log2Ceil(maxdelay).W))
   override def cloneType = (new prog_delay_io(proto,maxdelay)).asInstanceOf[this.type]
}


//Class of programmable
class prog_delay[ T <: Data ] (val proto: T, val maxdelay: Int=64)extends Module{
    val io=IO(
        new prog_delay_io(proto,maxdelay)
       )
    // Both work, but asTypeOf should provide less "problems"
    //val zero=Ring[T].zero
    val zero=0.U.asTypeOf(proto)
    val inreg=RegInit(zero)
    inreg:=io.iptr_A
    val regarray=Reg(Vec(maxdelay,proto))
    val oreg=RegInit(zero)
    oreg:=regarray(io.select)
    io.optr_Z:=oreg
}
