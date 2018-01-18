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
        val mode            = Input(UInt(3.W))
        val iptr_A          = Input(DspComplex(SInt(n.W), SInt(n.W)))
        val Z               = Output(DspComplex(SInt(n.W), SInt(n.W)))
    })

    val czero  = DspComplex(0.S(resolution.W),0.S(resolution.W)) //Constant complex zero
    //val coeffres=16 //halfband filter coefficient resolution
    val bypass :: two :: four :: eight :: more :: Nil = Enum(5)
    //Select state
    val state=RegInit(more)
    
    //Decoder for the modes
    when(io.mode===0.U){
        state := bypass
    } .elsewhen(io.mode===1.U) {
        state := two
    } .elsewhen(io.mode===2.U) {
        state:=four
    } .elsewhen(io.mode===3.U) {
        state:=eight
    } .elsewhen(io.mode===4.U) {
        state:=more
    }.otherwise {
        state := bypass
    }

    
    val cic3reset = Wire(Bool())
    cic3reset     :=reset.toBool
    val cic3= withClockAndReset(clock,cic3reset)(Module( new cic3(n=n,resolution=resolution,gainbits=gainbits)))

    val hb1reset = Wire(Bool())
    hb1reset     :=reset.toBool
    val hb1 = withClockAndReset(io.cic3clockslow,hb1reset)(Module( new halfband( n=16, resolution=32,coeffs=halfband_BW_01125_N_6.H.map(_ * (math.pow(2,coeffres-1)-1)).map(_.toInt))))

    val hb2reset = Wire(Bool())
    hb2reset     :=reset.toBool
    val hb2 = withClockAndReset(io.hb1clock_low,hb2reset)(Module( new halfband( n=16, resolution=32,coeffs=halfband_BW_0225_N_8.H.map(_ * (math.pow(2,coeffres-1)-1)).map(_.toInt))))

    val hb3reset = Wire(Bool())
    hb3reset    :=reset.toBool
    val hb3 = withClockAndReset(io.hb2clock_low,hb3reset)(Module( new halfband( n=16, resolution=32,coeffs=halfband_BW_045_N_40.H.map(_ * (math.pow(2,coeffres-1)-1)).map(_.toInt))))

    //Default is to bypass
    cic3.io.clockslow :=io.cic3clockslow
    cic3.io.integscale:=io.cic3integscale
    hb1.io.clock_low  :=io.hb1clock_low
    hb1.io.scale      :=io.hb1scale
    hb2.io.clock_low  :=io.hb2clock_low
    hb2.io.scale      :=io.hb2scale
    hb3.io.clock_low  :=io.hb3clock_low
    hb3.io.scale      :=io.hb3scale
    cic3.io.iptr_A    :=io.iptr_A
    hb1.io.iptr_A     :=cic3.io.Z
    hb2.io.iptr_A     :=hb1.io.Z
    hb3.io.iptr_A     :=hb2.io.Z
    io.Z              :=RegNext(io.iptr_A) 
    
    //Modes
    switch(state) {
        is(bypass) {
            cic3reset        :=true.B 
            hb1reset         :=true.B
            hb2reset         :=true.B
            hb3reset         :=true.B
            io.Z             :=RegNext(io.iptr_A)
        }
        is(two) {
            cic3reset        :=true.B 
            hb1reset         :=true.B
            hb2reset         :=true.B
            hb3reset         :=reset.toBool
            hb3.io.iptr_A    :=io.iptr_A
            io.Z             :=hb3.io.Z
        }
        is(four) {
            cic3reset        :=true.B 
            hb1reset         :=true.B
            hb2reset         :=reset.toBool
            hb3reset         :=reset.toBool
            hb2.io.iptr_A    :=io.iptr_A
            io.Z             :=hb3.io.Z
        }
        is(eight) {
            cic3.reset       :=true.B
            hb1.reset        :=reset.toBool 
            hb2.reset        :=reset.toBool
            hb3reset         :=reset.toBool
            hb1.io.iptr_A    :=io.iptr_A
            io.Z             :=hb3.io.Z
        }
        is(more) {
            cic3reset        :=reset.toBool 
            hb1reset         :=reset.toBool
            hb2reset         :=reset.toBool
            hb3reset         :=reset.toBool
            cic3.io.iptr_A   :=io.iptr_A
            hb1.io.iptr_A    :=cic3.io.Z
            hb2.io.iptr_A    :=hb1.io.Z
            hb3.io.iptr_A    :=hb2.io.Z
            io.Z             :=hb3.io.Z
        }
    }
}
//This is the object to provide verilog
object f2_decimator extends App {
  //Convert coeffs to integers with 16 bit resolution
  //val coeffres=16
  chisel3.Driver.execute(args, () => new f2_decimator() )
}


