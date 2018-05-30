// These are the third order CIC-filter for programmable decimator
// Usually works with integer decimation ratios when initial oversampling ration is 
// greater than eight
//
// Intially written by Marko Kosunen 20180503
// Last modification by Marko Kosunen, marko.kosunen@aalto.fi, 30.05.2018 00:34
package cic3_interpolator

import chisel3._
import chisel3.experimental._
import chisel3.util._
import dsptools._
import dsptools.numbers._
import breeze.math.Complex

class cic3_interpolator (n: Int=16, resolution: Int=28, gainbits: Int=10) extends Module {
    val io = IO(new Bundle {
        val clockfast       = Input(Clock())
        val derivscale      = Input(UInt(gainbits.W))
        val derivshift      = Input(UInt(log2Ceil(resolution).W))
        val iptr_A          = Input(DspComplex(SInt(n.W), SInt(n.W)))
        val Z               = Output(DspComplex(SInt(n.W), SInt(n.W)))
    })

    //Here we should pay attention to scaling
    // Registers for sampling rate reduction
    //val slowregs  = RegInit(Vec.fill(4)(DspComplex.wire(0.S(resolution.W), 0.S(resolution.W)))) //works
    val slowregs  = RegInit(VecInit(Seq.fill(4)(DspComplex.wire(0.S(resolution.W), 0.S(resolution.W))))) //works
    val minusregs = RegInit(VecInit(Seq.fill(4)(DspComplex.wire(0.S(resolution.W), 0.S(resolution.W))))) //works
    for (i<- 0 to 3) {
      if (i <=0) {
          //Must be another way to do this
          slowregs(i).real:=RegNext(io.iptr_A.real) 
          slowregs(i).imag:=RegNext(io.iptr_A.imag)
          minusregs(i):=slowregs(i)
      } else {
          slowregs(i):=slowregs(i-1)-minusregs(i-1)
          minusregs(i):=slowregs(i)
      }
    }
    
     withClock (io.clockfast){
     val integregs  = RegInit(VecInit(Seq.fill(4)(DspComplex.wire(0.S(resolution.W), 0.S(resolution.W))))) //works }
      for (i<- 0 to 3) {
        if (i <=0){
            integregs(i).real:=slowregs(3).real*io.derivscale << io.derivshift
            integregs(i).imag:=slowregs(3).imag*io.derivscale << io.derivshift
        } else { 
            integregs(i):=integregs(i-1)+integregs(i)
        }
      }
      io.Z.real := RegNext(integregs(3).real(resolution-1,resolution-n).asSInt)
      io.Z.imag := RegNext(integregs(3).imag(resolution-1,resolution-n).asSInt)
    }
}
//This is the object to provide verilog
object cic3_interpolator extends App {
  //Convert coeffs to integers with 16 bit resolution
  chisel3.Driver.execute(args, () => new cic3_interpolator() )
}

