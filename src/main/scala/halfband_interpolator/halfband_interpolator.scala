//These are the half-band filters for the F2 decimator
package halfband_interpolator

import chisel3.experimental._
import chisel3._
import halfband_BW_045_N_40._
import dsptools._
import dsptools.numbers._
import breeze.math.Complex

class halfband_interpolator (n: Int=16, resolution: Int=32, coeffs: Seq[Int]=Seq(-1,2,-3,4,-5), gainbits: Int=10) extends Module {
    val io = IO(new Bundle {
        val clock_high      = Input(Clock())
        val scale           = Input(UInt(gainbits.W))
        val iptr_A          = Input(DspComplex(SInt(n.W), SInt(n.W)))
        val Z               = Output(DspComplex(SInt(n.W), SInt(n.W)))
    })

    val czero  = DspComplex(0.S(resolution.W),0.S(resolution.W)) //Constant complex zero
    val inregs  = RegInit(VecInit(Seq.fill(2)(DspComplex.wire(0.S(n.W), 0.S(n.W))))) //registers for sampling rate reduction
    //Would like to do this with foldLeft but could't figure out how.
    inregs.map(_:=io.iptr_A)

    //The half clock rate domain
        val slowregs  = RegInit(VecInit(Seq.fill(2)(DspComplex.wire(0.S(n.W), 0.S(n.W))))) //registers for sampling rate reduction
        (slowregs,inregs).zipped.map(_:=_)

        val sub1coeffs=coeffs.indices.filter(_ %2==0).map(coeffs(_)) //Even coeffs for Fir1
        println(sub1coeffs)
        val tapped1=sub1coeffs.reverse.map(tap => slowregs(0)*tap)
        val registerchain1=RegInit(VecInit(Seq.fill(tapped1.length+1)(DspComplex.wire(0.S(resolution.W), 0.S(resolution.W)))))
        for ( i <- 0 to tapped1.length-1) {
            if (i==0) {
                registerchain1(i+1):=tapped1(i)
            } else {
                registerchain1(i+1):=registerchain1(i)+tapped1(i)
            }
        }
        val subfil1=registerchain1(tapped1.length)

        // Transposed direct form subfilters. Folding left for the synthesizer oneliner, no reset for registers
        //val subfil1= sub1coeffs.reverse.map(tap => slowregs(0)*tap).foldLeft(czero)((current,prevreg)=>RegNext(current+prevreg))
        
        val sub2coeffs=coeffs.indices.filter(_ %2==1).map(coeffs(_)) //Odd coeffs for Fir 2
        println(sub2coeffs)
        val tapped2=sub2coeffs.reverse.map(tap => slowregs(1)*tap)
        val registerchain2=RegInit(VecInit(Seq.fill(tapped2.length+1)(DspComplex.wire(0.S(resolution.W), 0.S(resolution.W)))))
        for ( i <- 0 to tapped2.length-1) {
            if (i==0) {
                registerchain2(i+1):=tapped2(i)
            } else {
                registerchain2(i+1):=registerchain2(i)+tapped2(i)
            }
        }
        val subfil2=registerchain2(tapped2.length)

    //The double clock rate domain
    withClock (io.clock_high){
        val outreg=RegInit(DspComplex.wire(0.S(n.W), 0.S(n.W)))
        //Slow clock sampled with fast one to control the ouput multiplexer
        val clkreg=Wire(Bool())
        clkreg:=RegNext(clock.asUInt)
        when (clkreg===false.B) { 
            outreg.real := (subfil1.real*io.scale)(resolution-1,resolution-n).asSInt
            outreg.imag := (subfil1.imag*io.scale)(resolution-1,resolution-n).asSInt
        }.elsewhen (clkreg===true.B) { 
            outreg.real := (subfil2.real*io.scale)(resolution-1,resolution-n).asSInt
            outreg.imag := (subfil2.imag*io.scale)(resolution-1,resolution-n).asSInt
        }
        io.Z:= outreg
    }
}



//This is the object to provide verilog
object halfband_interpolator extends App {
  //Convert coeffs to integers with 16 bit resolution
  val coeffres=16
  val taps = halfband_BW_045_N_40.H.map(_ * (math.pow(2,coeffres-1)-1)).map(_.toInt)
  chisel3.Driver.execute(args, () => new halfband_interpolator(coeffs=taps) )
}


