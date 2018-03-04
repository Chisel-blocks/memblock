// Clk divider. Initiallyl  written by Marko Kosunen
// Divides input clock by N, 2N , 4N and 8N
// Last modification by Marko Kosunen, marko.kosunen@aalto.fi, 03.03.2018 23:17
package clkdiv_n_2_4_8

import chisel3.experimental._
import chisel3._
import halfband_BW_045_N_40._
import dsptools._
import dsptools.numbers._
import breeze.math.Complex
class regcustom

class clkdiv_n_2_4_8 (n: Int=8) extends Module {
    val io = IO(new Bundle {
        val Ndiv       = Input(UInt(n.W))
        val reset_clk  = Input(Bool())
        val clkpn      = Output(Bool())
        val clkp2n     = Output(Bool())
        val clkp4n     = Output(Bool())
        val clkp8n     = Output(Bool())
    })

    val en  = Wire(Bool()) 
    en  := !io.reset_clk 

    val count=RegInit(0.U(n.W))
    val sregs=RegInit(Vec(Seq.fill(4)(false.B)))
    val clocks=sregs.map(x =>x.asClock)

    when ( en ) {
        when (count === io.Ndiv) {
            count := count+1.U(1.W)
            sregs(0) := true.B
        } .otherwise {
         count:=0.U
         sregs(0):= false.B
        }
    }
    val enN = RegInit(false.B) 
    enN := en

    //Enable registers. Could be looped, this is for clarity
    val en2 = withClockAndReset(clocks(0),en)(RegInit(false.B))
    val en4 = withClockAndReset(clocks(1),en)(RegInit(false.B))
    val en8 = withClockAndReset(clocks(2),en)(RegInit(false.B))

    //Chaining the enables
    en2 := enN
    en4 := en2
    en8 := en4
    val enchain = Seq(enN,en2,en4,en8)
    val allzp = Seq.fill(4)(Wire(Bool()))
    
    //Three stages for division
    allzp(0):=true.B
    val outregs=RegInit(Vec(Seq.fill(4)(false.B)))
    outregs(0):=sregs(0)
    for ( i <- 1 to 3) {
       sregs(i):= (enchain(i) & allzp(i-i)) ^ sregs(i)
       allzp(i) := allzp(i-1) & (!sregs(i))
       //Pure registers at the output
       outregs(i):=sregs(i)
    }

    io.clkpn  := outregs(0)
    io.clkp2n := outregs(1)
    io.clkp4n := outregs(2)
    io.clkp8n := outregs(3)
}


//This gives you verilog
object clkdiv_n_2_4_8 extends App {
  chisel3.Driver.execute(args, () => new clkdiv_n_2_4_8(n=8) )
}


