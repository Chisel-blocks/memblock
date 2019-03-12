package dcpipe

import chisel3._
import chisel3.util._
import chisel3.experimental._
import dsptools._
import dsptools.numbers._

// Initially written by Marko Kosunen  20190308
// Maybe this is a Queue

class dcpipe[T <:Data] (
        proto            : T,
        latency          : Int=16
    ) extends Module {
    val io = IO( new Bundle { 
        val enq = Flipped(new DecoupledIO(proto.cloneType))
        val deq = new DecoupledIO(proto.cloneType)
    } )

    val datapipe= Module( new Pipe(proto.cloneType,latency=latency)).io
    val readypipe=Module(new Pipe(Bool(), latency=latency)).io
    datapipe.enq.bits:=io.enq.bits
    datapipe.enq.valid:=io.enq.valid
    readypipe.enq.valid:=true.B
    readypipe.enq.bits:=io.deq.ready
    io.enq.ready:=readypipe.deq.bits
    io.deq.bits:=datapipe.deq.bits
    io.deq.valid:=datapipe.deq.valid
}


//This gives you verilog
object dcpipe extends App {
  val proto=UInt(16.W)
  chisel3.Driver.execute(args, () => new dcpipe(proto, latency=16))
}


