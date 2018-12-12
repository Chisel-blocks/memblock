// See LICENSE for license details.
// Initially written by Marko Kosunen  20180429

package edge_detector
import chisel3._
import chisel3.util._
import chisel3.experimental._
import dsptools._
import dsptools.numbers._

class edge_detector ()  extends Module {
    val io = IO( new Bundle {
        val A = Input(Bool())
        val rising     = Output(Bool())
        val falling    = Output(Bool())
        val transition = Output(Bool())
    })
    val reg_curr=RegInit(false.B)
    val reg_prev=RegInit(false.B)
    reg_curr:=io.A
    reg_prev:=reg_curr

    //Use this in parallel with registered inputs 
    io.rising    := (~reg_prev & reg_curr)
    io.falling   := (reg_prev & ~reg_curr)
    io.transition:= (~reg_prev & reg_curr) | (reg_prev & !reg_curr)
}

//This gives you verilog
object edge_detector extends App {
  chisel3.Driver.execute(args, () => new edge_detector())
}


