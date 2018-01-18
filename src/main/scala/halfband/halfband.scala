//These are the ahlf-band filters for the F2 decimator
package halfband

import chisel3.experimental._
import chisel3._
import halfband_BW_045_N_40._
import dsptools._
import dsptools.numbers._
import breeze.math.Complex

class halfband (n: Int=16, resolution: Int=32, coeffs: Seq[Int]=Seq(-1,2,-3,4,-5), gainbits: Int=10) extends Module {
    val io = IO(new Bundle {
        val clock_low       = Input(Clock())
        val scale           = Input(UInt(gainbits.W))
        val iptr_A          = Input(DspComplex(SInt(n.W), SInt(n.W)))
        val Z               = Output(DspComplex(SInt(n.W), SInt(n.W)))
    })

    val czero  = DspComplex(0.S(resolution.W),0.S(resolution.W)) //Constant complex zero
    //val scale = 8.S //Output scaling
    val inregs  = Reg(Vec(2, DspComplex(SInt(n.W), SInt(n.W)))) //registers for sampling rate reduction
    //Would like to do this with foldLeft but could't figure out how.
        for (i<- 0 to 1) {
            if (i <=0) inregs(i):=io.iptr_A 
            else inregs(i):=inregs(i-1)
        }
    //The half clock rate domain
    withClock (io.clock_low){
        val slowregs  = Reg(Vec(2, DspComplex(SInt(n.W), SInt(n.W)))) //registers for sampling rate reduction
        (slowregs,inregs).zipped.map(_:=_)
        
        // Transposed direct form subfilters. Folding left for the synthesizer
        val sub1coeffs=coeffs.indices.filter(_ %2==0).map(coeffs(_)) //Even coeffs for Fir1
        println(sub1coeffs)
        val subfil1= sub1coeffs.map(tap => slowregs(0)*tap).foldLeft(czero)((current,prevreg)=>RegNext(current+prevreg))
        
        val sub2coeffs=coeffs.indices.filter(_ %2==1).map(coeffs(_)) //Odd coeffs for Fir 2
        println(sub2coeffs)
        val subfil2= sub2coeffs.map(tap => slowregs(1)*tap).foldLeft(czero)((current,prevreg)=>RegNext(current+prevreg))
        
        
        io.Z.real := ((subfil1.real+subfil2.real)*io.scale)(resolution-1,resolution-n).asSInt
        io.Z.imag := ((subfil1.imag+subfil2.imag)*io.scale)(resolution-1,resolution-n).asSInt
    }
}



//This is the object to provide verilog
object halfband extends App {
  //Convert coeffs to integers with 16 bit resolution
  val coeffres=16
  val taps = halfband_BW_045_N_40.H.map(_ * (math.pow(2,coeffres-1)-1)).map(_.toInt)
  chisel3.Driver.execute(args, () => new halfband(coeffs=taps) )
}


