// Clk divider. Initiallyl  written by Marko Kosunen
// Divides input clock by N, 2N , 4N and 8N
// Last modification by Marko Kosunen, marko.kosunen@aalto.fi, 17.05.2018 18:45
package clkdiv_n_2_4_8

import chisel3.experimental._
import chisel3._
import dsptools._
import dsptools.numbers._
import breeze.math.Complex

class clkdiv_n_2_4_8 (n: Int=8) extends Module {
    val io = IO(new Bundle {
        val Ndiv       = Input(UInt(n.W))
        val reset_clk  = Input(Bool())
        val shift      = Input(UInt(2.W))
        val clkpn      = Output(Bool())
        val clkp2n     = Output(Bool())
        val clkp4n     = Output(Bool())
        val clkp8n     = Output(Bool())
    })

    val en  = Wire(Bool()) 
    en  := !io.reset_clk 
    val r_shift=RegInit(0.U.asTypeOf(io.shift))
    val stateregisters=RegInit(VecInit(Seq.fill(4)(false.B)))
    val count=RegInit(0.U(n.W))

    //Sync the shift
    r_shift:=io.shift
    when ( en ) {
        when (count === io.Ndiv-1) {
            count:=0.U
            stateregisters(0) := true.B
        } .otherwise {
         count := count+1.U(1.W)
         stateregisters(0):= false.B
        }
    }
    val enN = RegInit(false.B) 
    enN := en

    //Enable registers. We need to delay the enable by one clock pulse in order
    // to get the feedbacks reseted
    val en2 = RegInit(false.B)
    val en4 = RegInit(false.B)
    val en8 = RegInit(false.B)

    //Chaining the enables
    when (stateregisters(0)) { 
        en2 := (enN &&  en )
    }
    when (stateregisters(1) ){
        en4 := en2
    }
    when (stateregisters(2) ){
        en8 := en4
    }
    
    val enchain = Seq(enN,en2,en4,en8)

    // Monitors if the all previous stages are zero
    val allzp = Wire(Vec(4,Bool()))
    allzp(0) :=  stateregisters(0) 
    for ( i <- 1 to 3) {
        allzp(i):= allzp(i-1) && !stateregisters(i)
        //allzp(i):= false.B
    }
    val outregs   =RegInit(VecInit(Seq.fill(4)(false.B)))
    
    outregs(0):=stateregisters(0)
    for ( i <- 1 to 3) {
       when (en) { 
           when ( (enchain(i) && allzp(i-1)) ) {
              stateregisters(i):= ! stateregisters(i)
           } .otherwise { 
               stateregisters(i):=stateregisters(i)
           }
       } .otherwise { 
           stateregisters(i)    := false.B  
     }
     //Pure registers at the output
     outregs(i):=stateregisters(i)
    }

    val w_clkpn=Wire(Bool())
    when ( io.Ndiv-1=== 0.U(n.W) ) {
        w_clkpn := clock.asUInt
    } .otherwise {
        w_clkpn  := RegNext(RegNext(outregs(0)))
    }
    
    when (r_shift===0.U) {
        io.clkpn := w_clkpn
        io.clkp2n:=RegNext(outregs(1)) 
        io.clkp4n:=RegNext(outregs(2))
        io.clkp8n:=RegNext(outregs(3))
    } .elsewhen( r_shift===1.U) {
        io.clkpn := clock.asUInt
        io.clkp2n:= w_clkpn
        io.clkp4n:=RegNext(outregs(1))
        io.clkp8n:=RegNext(outregs(2))
    } .elsewhen( r_shift===2.U) {
        io.clkpn := clock.asUInt
        io.clkp2n:= clock.asUInt
        io.clkp4n:=w_clkpn
        io.clkp8n:=RegNext(outregs(1))
    } .elsewhen( r_shift===3.U) {
        io.clkpn := clock.asUInt
        io.clkp2n:= clock.asUInt
        io.clkp4n:= clock.asUInt
        io.clkp8n:= w_clkpn
    } .otherwise {
        io.clkpn := w_clkpn
        io.clkp2n:=RegNext(outregs(1)) 
        io.clkp4n:=RegNext(outregs(2))
        io.clkp8n:=RegNext(outregs(3))
    }
}


//This gives you verilog
object clkdiv_n_2_4_8 extends App {
  chisel3.Driver.execute(args, () => new clkdiv_n_2_4_8(n=8) )
}


