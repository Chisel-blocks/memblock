//These are the ahlf-band filters for the F2 decimator
package halfband

import chisel3.experimental._
import chisel3._
import datatypes._
import halfband_BW_045_N_40._
import chisel3.experimental._
import chisel3._
import datatypes._
import halfband_BW_045_N_40._
import dsptools._
import dsptools.numbers._
import breeze.math.Complex

class halfband (n: Int=16, resolution: Int=32, coeffs: Seq[Int]=Seq(-1,2,-3,4,-5)) extends Module {
    val io = IO(new Bundle {
        val clockp2       = Input(Clock())
        val iptr_A          = Input(DspComplex(SInt(n.W), SInt(n.W)))
        val Z               = Output(DspComplex(SInt(n.W), SInt(n.W)))
    })

  //Output scaling
  val scale = 2.S
  //I guess lots of stuff could be reduced by creating a function for the subfilter
  val sub1coeffs=coeffs.indices.filter(_ %2==0).map(coeffs(_)) //Even coeffs
  val sub2coeffs=coeffs.indices.filter(_ %2==1).map(coeffs(_)) //Odd coeffs
  println(sub1coeffs)
  println(sub2coeffs)

  val sub1stages=sub1coeffs.length //number of register stages in first subfir
  val sub2stages=sub2coeffs.length //number of register stages in second subfir

  val inregs  = Reg(Vec(2, DspComplex(SInt(n.W), SInt(n.W)))) //registers for sampling rate reduction
  inregs(0):=io.iptr_A
  inregs(1):=inregs(0)

  //The half clock rate domain
  withClock (io.clockp2){
    val slowregs  = Reg(Vec(2, DspComplex(SInt(n.W), SInt(n.W)))) //registers for sampling rate reduction
    slowregs(0):=inregs(0)
    slowregs(1):=inregs(1)

    // Transposed direct form subfilters. Folding left for the synthesizer
    // Remains to be seen if it is clever enough
    val sub1regs  = Reg(Vec(sub1stages+1, DspComplex(SInt(resolution.W), SInt(resolution.W))))
    sub1regs(0).real:= 0.S(resolution.W)
    sub1regs(0).imag:= 0.S(resolution.W)
    for (i <- 0 to sub1stages-1) {
      sub1regs(i+1):=slowregs(0)*sub1coeffs(i)+sub1regs(i)
      //sub1regs(i+1).real:=slowregs(0).real*sub1coeffs(i)+sub1regs(i).real
      //sub1regs(i+1).imag:=slowregs(0).imag*sub1coeffs(i)+sub1regs(i).imag
    }

    val sub2regs  = Reg(Vec(sub2stages+1, DspComplex(SInt(resolution.W), SInt(resolution.W))))
    sub2regs(0).real:= 0.S(resolution.W)
    sub2regs(0).imag:= 0.S(resolution.W)
    for (i <- 0 to sub2stages-1) {
      sub2regs(i+1):=slowregs(1)*sub2coeffs(i)+sub2regs(i)
      //sub2regs(i+1).real:=slowregs(1).real*sub2coeffs(i)+sub2regs(i).real
      //sub2regs(i+1).imag:=slowregs(1).imag*sub2coeffs(i)+sub2regs(i).imag
    }
    
    //val sumnode = sub1regs(sub1stages)+sub2regs(sub2stages)
    io.Z.real := ((sub1regs(sub1stages).real+sub2regs(sub2stages).real)*scale)(resolution-1,resolution-n).asSInt
    io.Z.imag := ((sub1regs(sub1stages).imag+sub2regs(sub2stages).imag)*scale)(resolution-1,resolution-n).asSInt
  }
}



//This is the object to provide verilog
object halfband extends App {
  //Convert coeffs to integers with 16 bit resolution
  val coeffres=16
  val taps = halfband_BW_045_N_40.H.map(_ * (math.pow(2,coeffres-1)-1)).map(_.toInt)
  //val taps = F2halfbands.hb3.map(_ * (math.pow(2,coeffres-1)-1)).map(_.toInt)
  chisel3.Driver.execute(args, () => new halfband(coeffs=taps) )
}


