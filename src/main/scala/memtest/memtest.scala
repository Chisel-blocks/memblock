// See LICENSE for license details.
// Initially written by Marko Kosunen  20180429
package memtest 
import chisel3._
import chisel3.util._
import chisel3.experimental._
import dsptools._
import dsptools.numbers._

class memtest (
        n                : Int=16,
        users            : Int=4,
        memsize          : Int=scala.math.pow(2,13).toInt
    ) extends Module {
    val io = IO( new Bundle { 
            val write_addr = Input(UInt(log2Ceil(memsize).W))
            val read_addr  = Input(UInt(log2Ceil(memsize).W))
            val read_val   = Output(UInt(n.W)
            val write_val  = Input(UInt(n.W))
    } )
    // Need a memory with write from scan, read to scan, and 
    // To map this to SRAM, write address must be syncroniozed
    // All addressing through write_addri, enables throuhg write_en
    val mem      =SyncReadMem(memsize, UInt(n.W))
    val write_addr =RegInit(0.U(log2Ceil(memsize).W))
    val read_addr =RegInit(0.U(log2Ceil(memsize).W))
    //val write_en =RegInit(Bool())
    val write_val=RegInit(0.U(n.W))
    val read_val =RegInit(0.U(n.W))
    write_addr:=io.write_addr
    write_val:=io.write_val
    read_addr:=io.read_addr
    // Every clock cycle we write to memory, if write is enabled
    //when ( write_en===true.B) {
        mem.write(write_addr,write_val)
    //}.otherwise {
        read_val:=mem.read(read_addr)
    //}
  io.read_val:=read_val 
   
}
//This gives you verilog
object memtest extends App {
  chisel3.Driver.execute(args, () => new memtest(n=16, memsize=scala.math.pow(2,13).toInt ))
}


