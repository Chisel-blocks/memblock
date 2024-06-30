// See LICENSE for license details.
// Initially written by Marko Kosunen  20180429
package memblock 
import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import dsptools._
import dsptools.numbers._
import scala.math._


class memblock[T <:Data] (
        proto            : T,
        memsize          : Int=scala.math.pow(2,13).toInt
    ) extends Module {
    val io = IO( new Bundle { 
            val write_en   = Input(Bool())
            val write_addr = Input(UInt(log2Ceil(memsize).W))
            val write_val  = Input(proto)
            val read_addr  = Input(UInt(log2Ceil(memsize).W))
            val read_val   = Output(proto)
    } )
     
        val write_val=RegInit(0.U.asTypeOf(proto.cloneType))
        val write_enable=RegInit(0.U.asBool)
        val mem =SyncReadMem(memsize, proto.cloneType)
        val write_addr =RegInit(0.U(log2Ceil(memsize).W))
        val read_addr =RegInit(0.U(log2Ceil(memsize).W))
        val read_val =RegInit(0.U.asTypeOf(proto.cloneType))
        write_addr:=io.write_addr
        write_enable:=io.write_en
        write_val:=io.write_val
        read_addr:=io.read_addr
        // [ ToDo] Create structural alternatives to control whether the operation
        // Aims for single or dual port memory
        when(write_enable){ 
            mem.write(write_addr,write_val)
        }
        read_val:=mem.read(read_addr)
        io.read_val:=read_val
}

/** This gives you verilog */
object memblock extends App {
    val annos = Seq(ChiselGeneratorAnnotation(() => new memblock(
        proto=DspComplex(UInt(16.W),UInt(16.W)), memsize=4096
    )))
    (new ChiselStage).execute(args, annos)
}

