// Clk divider. Initiallyl  written by Marko Kosunen
// Divides input clock by N, 2N , 4N and 8N
// Last modification by Marko Kosunen, marko.kosunen@aalto.fi, 04.03.2018 18:13
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
    
    val creg=RegInit(false.B)
    val cclock = creg.asClock
    val sregs=withClock(cclock)(RegInit(Vec(Seq.fill(4)(false.B))))

    //wires to implement asynchronous reset
    val clocks=sregs.map(x =>x.asClock)

    when ( en ) {
        when (count === io.Ndiv) {
            count:=0.U
            creg := true.B
        } .otherwise {
         count := count+1.U(1.W)
         creg:= false.B
        }
    }
    val enN = RegInit(false.B) 
    enN := en

    //Enable registers. We need to delay the enable by one clock pulse in order
    // to get the feedbacks reseted
    val en2 = withClock(cclock)(RegInit(false.B))
    val en2del = withClock(cclock)(RegInit(false.B))
    //val enout = withClock(cclock)(RegInit(false.B))
    val en4 = withClock(clocks(1))(RegInit(false.B))
    val en8 = withClock(clocks(2))(RegInit(false.B))
    val reset_out = Wire(Bool())

    //Chaining the enables
    en2 := (enN & en)
    en2del := en2 & en
    reset_out := ! (en2del)
    en4 := en2del & en
    en8 := en4 & en
    val enchain = Seq(enN,en2del,en4,en8)

    // Monitors if the all previous stages are zero
    val allzp = Wire(Vec(4,Bool()))
    allzp(0) := false.B
    for ( i <- 1 to 3) {
        allzp(i):= allzp(i-1) && !sregs(i)
        //allzp(i):= false.B
    }
    val outregs=withReset(reset_out)(RegInit(Vec(Seq.fill(4)(false.B))))
    outregs(0):=creg

    for ( i <- 1 to 3) {
       allzp(0):= true.B //First stage always toggles
       when (en2) { 
           when ( (enchain(i) && allzp(i-1)) ) {
              sregs(i):= ! sregs(i)
           } .otherwise { 
               sregs(i):=sregs(i)
           }
       } .otherwise { 
           sregs(i)    := false.B  
     }
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


