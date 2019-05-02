// See LICENSE for license details.
// Initially written by Marko Kosunen  20180429
package memblock 
import chisel3._
import chisel3.util._
import chisel3.experimental._
import dsptools._
import dsptools.numbers._
import scala.math._

class testmemproto_zpad[T <:Data] (
      val  proto : T,
      val zpad   : Int=0 // Optional zero-padding of the output
    ) extends Bundle {
        val signal   = proto
        val zeropad  = UInt(zpad.W)
        // Here we need .cloneType for some reason. Does not work without it
        // Proto is an argument of type T. Proto.cloneType returns a new instance of proto?
        override def cloneType = (new testmemproto_zpad(proto.cloneType,zpad)).asInstanceOf[this.type]
 }

class testmemproto[T <:Data] (
      val  proto : T
    ) extends Bundle {
        val signal   = proto
        // Here we need .cloneType for some reason. Does not work without it
        // Proto is an argument of type T. Proto.cloneType returns a new instance of proto?
        override def cloneType = (new testmemproto(proto.cloneType)).asInstanceOf[this.type]
 }


class memblock[T <:Data] (
        proto            : T,
        memsize          : Int=scala.math.pow(2,13).toInt,
        zpad             : Int=0
    ) extends Module {
    val io = IO( new Bundle { 
            val write_addr = Input(UInt(log2Ceil(memsize).W))
            val read_addr  = Input(UInt(log2Ceil(memsize).W))
            val read_val   = Output(proto)
            val write_val  = Input(proto)
    } )

    if (zpad > 0) {
        val memproto= new testmemproto_zpad(proto, zpad=zpad)
        val write_val=RegInit(0.U.asTypeOf(memproto))
        val mem =SyncReadMem(memsize, memproto)
        val write_addr =RegInit(0.U(log2Ceil(memsize).W))
        val read_addr =RegInit(0.U(log2Ceil(memsize).W))
        val read_val =RegInit(0.U.asTypeOf(memproto))
        write_addr:=io.write_addr
        write_val.zeropad:= 0.U.asTypeOf(memproto.zeropad)
        write_val.signal:=io.write_val
        read_addr:=io.read_addr
        mem.write(write_addr,write_val)
        read_val:=mem.read(read_addr)
        io.read_val:=read_val.signal
    } else {
        val memproto= new testmemproto(proto)
        val write_val=RegInit(0.U.asTypeOf(memproto))
        val mem =SyncReadMem(memsize, memproto)
        val write_addr =RegInit(0.U(log2Ceil(memsize).W))
        val read_addr =RegInit(0.U(log2Ceil(memsize).W))
        val read_val =RegInit(0.U.asTypeOf(memproto))
        write_addr:=io.write_addr
        //write_val.zeropad:= 0.U.asTypeOf(memproto.zeropad)
        write_val.signal:=io.write_val
        read_addr:=io.read_addr
        mem.write(write_addr,write_val)
        read_val:=mem.read(read_addr)
        io.read_val:=read_val.signal
    }
}
//This gives you verilog
object memblock extends App {
  val proto=DspComplex(FixedPoint(8.W,4.BP))
  chisel3.Driver.execute(args, () => 
          new memblock(proto, memsize=scala.math.pow(2,13).toInt, zpad=1 )
  )
}


