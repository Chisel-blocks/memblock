//These are the ahlf-band filters for the F2 decimator
package f2_decimator
import chisel3._
import chisel3.util._
import chisel3.experimental._
import dsptools._
import dsptools.numbers._
//import breeze.math.Complex
import halfband_BW_045_N_40._
import halfband_BW_0225_N_8._
import halfband_BW_01125_N_6._
import halfband._
import cic3._

class f2_decimator (n: Int=16, resolution: Int=32, coeffres: Int=16, gainbits: Int=10) extends Module {
    val io = IO(new Bundle {
        val cic3clockslow   = Input(Clock())
        val hb1clock_low    = Input(Clock())
        val hb2clock_low    = Input(Clock())
        val hb3clock_low    = Input(Clock())
        val cic3integscale  = Input(UInt(gainbits.W))
        val hb1scale        = Input(UInt(gainbits.W))
        val hb2scale        = Input(UInt(gainbits.W))
        val hb3scale        = Input(UInt(gainbits.W))
        //val modeselect      = Input(UInt(3.W))
        val iptr_A          = Input(DspComplex(SInt(n.W), SInt(n.W)))
        val Z               = Output(DspComplex(SInt(n.W), SInt(n.W)))
    })

    val czero  = DspComplex(0.S(resolution.W),0.S(resolution.W)) //Constant complex zero
    //val coeffres=16 //halfband filter coefficient resolution
    val bypass :: two :: four :: eight :: more :: Nil = Enum(5)
    val state = two
    
    val cic3= Module( new cic3(n=n,resolution=resolution,gainbits=gainbits))
    val hb1 = withClock(io.cic3clockslow)(Module( new halfband( n=16, resolution=32,coeffs=halfband_BW_01125_N_6.H.map(_ * (math.pow(2,coeffres-1)-1)).map(_.toInt))))
    val hb2 = withClock(io.hb1clock_low)(Module( new halfband( n=16, resolution=32,coeffs=halfband_BW_0225_N_8.H.map(_ * (math.pow(2,coeffres-1)-1)).map(_.toInt))))
    val hb3 = withClock(io.hb2clock_low)(Module( new halfband( n=16, resolution=32,coeffs=halfband_BW_045_N_40.H.map(_ * (math.pow(2,coeffres-1)-1)).map(_.toInt))))
    

    cic3.io.clockslow :=io.cic3clockslow
    cic3.io.integscale:=io.cic3integscale
    cic3.io.iptr_A    :=io.iptr_A

    hb1.io.clock_low :=io.hb1clock_low
    hb1.io.scale     :=io.hb1scale
    hb1.io.iptr_A    :=cic3.io.Z

    hb2.io.clock_low :=io.hb2clock_low
    hb2.io.scale     :=io.hb2scale
    hb2.io.iptr_A    :=hb1.io.Z

    hb3.io.clock_low :=io.hb3clock_low
    hb3.io.scale     :=io.hb3scale
    hb3.io.iptr_A    :=hb2.io.Z

    //withClock (io.cic3clockslow){
    //    io.Z:= cic3.io.Z
    //}
    //withClock (io.hb1clock_low){
    //    io.Z:= hb1.io.Z
    //}
    //withClock (io.hb2clock_low){
    //    io.Z:= hb2.io.Z
    //}
    withClock (io.hb3clock_low){
        io.Z:= hb3.io.Z
    }
        

    //switch(state){
    //    is(bypass) {
    //           io.Z:= io.iptr_A
    //    }
    //    is(two) {
    //       withClock (io.cic3clockslow){
    //           io.Z:= cic3.io.Z
    //       }
    //    }
    //    is(four) {
    //       withClock (io.hb1clock_low){
    //           io.Z:= hb1.io.Z
    //       }
    //    }
    //    is(eight) {
    //       withClock (io.hb2clock_low){
    //           io.Z:= hb2.io.Z
    //       }
    //    }
    //    is(more) {
    //        withClock (io.hb3clock_low){
    //            io.Z:= hb3.io.Z
    //        }
    //    }

    //} 

}

//This is the object to provide verilog
object f2_decimator extends App {
  //Convert coeffs to integers with 16 bit resolution
  //val coeffres=16
  chisel3.Driver.execute(args, () => new f2_decimator() )
}


