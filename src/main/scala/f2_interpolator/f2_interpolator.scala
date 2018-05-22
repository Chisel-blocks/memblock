//These are the ahlf-band filters for the F2 decimator
package f2_interpolator
import chisel3._
import chisel3.util._
import chisel3.experimental._
import dsptools._
import dsptools.numbers._
//import breeze.math.Complex
import halfband_BW_045_N_40._
import halfband_BW_0225_N_8._
import halfband_BW_01125_N_6._
import halfband_interpolator._
import cic3_interpolator._

class f2_interpolator_clocks extends Bundle {
        val cic3clockfast   = Input(Clock())
        val hb1clock_low    = Input(Clock())
        val hb1clock_high    = Input(Clock())
        val hb2clock_high    = Input(Clock())
        val hb3clock_high    = Input(Clock())
}

class f2_interpolator_controls(val gainbits: Int) extends Bundle {
        val cic3derivscale  = Input(UInt(gainbits.W))
        val hb1scale        = Input(UInt(gainbits.W))
        val hb2scale        = Input(UInt(gainbits.W))
        val hb3scale        = Input(UInt(gainbits.W))
        val mode            = Input(UInt(3.W))
}

class f2_interpolator_io(val n: Int, val gainbits: Int) extends Bundle {
        val clocks          = new f2_interpolator_clocks
        val controls        = new f2_interpolator_controls(gainbits=gainbits)
        val iptr_A          = Input(DspComplex(SInt(n.W), SInt(n.W)))
        val Z               = Output(DspComplex(SInt(n.W), SInt(n.W)))
}

class f2_interpolator (n: Int=16, resolution: Int=32, coeffres: Int=16, gainbits: Int=10) extends Module {
    val io = IO(new f2_interpolator_io(n=n,gainbits=gainbits)
    )

    //State definitions
    val bypass :: two :: four :: eight :: more :: Nil = Enum(5)
    //Select state
    val state=withClock(io.clocks.hb1clock_low) (RegInit(bypass))
    
    //Decoder for the modes
    when(io.controls.mode===0.U){
        state := bypass
    } .elsewhen(io.controls.mode===1.U) {
        state := two
    } .elsewhen(io.controls.mode===2.U) {
        state:=four
    } .elsewhen(io.controls.mode===3.U) {
        state:=eight
    } .elsewhen(io.controls.mode===4.U) {
        state:=more
    }.otherwise {
        state := bypass
    }

    
    //Reset initializations
    val hb1reset = Wire(Bool())
    hb1reset    :=reset.toBool
    val hb1 = withClockAndReset(io.clocks.hb1clock_low,hb1reset)(Module( 
        new halfband_interpolator( 
            n=n, resolution=resolution,coeffs=halfband_BW_045_N_40.H.map(_ * (math.pow(2,coeffres-1)-1)).map(_.toInt)
        )
    ))


    val hb2reset = Wire(Bool())
    hb2reset     :=reset.toBool
    val hb2 = withClockAndReset(io.clocks.hb1clock_high,hb2reset)(Module( 
        new halfband_interpolator( 
            n=n, resolution=resolution,coeffs=halfband_BW_0225_N_8.H.map(_ * (math.pow(2,coeffres-1)-1)).map(_.toInt)
        )
    ))

    val hb3reset = Wire(Bool())
    hb3reset     :=reset.toBool
    val hb3 = withClockAndReset(io.clocks.hb2clock_high,hb3reset)(Module(
        new halfband_interpolator(
            n=n, resolution=resolution,coeffs=halfband_BW_01125_N_6.H.map(_ * (math.pow(2,coeffres-1)-1)).map(_.toInt)
        )
    ))

    val cic3reset = Wire(Bool())
    cic3reset     :=reset.toBool
    val cic3= withClockAndReset(io.clocks.hb3clock_high,cic3reset)(Module(
        new cic3_interpolator(n=n,resolution=resolution,gainbits=gainbits)
    ))

    //Default is to bypass
    hb1.io.clock_high :=io.clocks.hb1clock_high
    hb1.io.scale      :=io.controls.hb1scale
    hb2.io.clock_high :=io.clocks.hb2clock_high
    hb2.io.scale      :=io.controls.hb2scale
    hb3.io.clock_high :=io.clocks.hb3clock_high
    hb3.io.scale      :=io.controls.hb3scale
    cic3.io.clockfast :=io.clocks.cic3clockfast
    cic3.io.derivscale:=io.controls.cic3derivscale
    hb1.io.iptr_A     :=io.iptr_A
    hb2.io.iptr_A     :=hb1.io.Z
    hb3.io.iptr_A     :=hb2.io.Z
    cic3.io.iptr_A    :=hb3.io.Z
    io.Z              :=withClock(io.clocks.hb1clock_low) (RegNext(io.iptr_A)) 
    
    //Modes
    switch(state) {
        is(bypass) {
            cic3reset        :=true.B 
            hb1reset         :=true.B
            hb2reset         :=true.B
            hb3reset         :=true.B
            io.Z             :=withClock(io.clocks.hb1clock_low) (RegNext(io.iptr_A))
        }
        is(two) {
            hb1.io.iptr_A    :=io.iptr_A
            hb1reset         :=reset.toBool
            hb2reset         :=true.B
            hb3reset         :=true.B
            cic3reset        :=true.B 
            io.Z             :=hb1.io.Z
        }
        is(four) {
            hb1.io.iptr_A    :=io.iptr_A
            hb1reset         :=reset.toBool
            hb2reset         :=reset.toBool
            hb3reset         :=true.B
            cic3reset        :=true.B 
            hb2.io.iptr_A    :=hb1.io.Z
            io.Z             :=hb2.io.Z
        }
        is(eight) {
            hb1.io.iptr_A    :=io.iptr_A
            hb1reset         :=reset.toBool
            hb2reset         :=reset.toBool
            hb3reset         :=reset.toBool
            cic3reset        :=true.B 
            hb2.io.iptr_A    :=hb1.io.Z
            hb3.io.iptr_A    :=hb2.io.Z
            io.Z             :=hb3.io.Z
        }
        is(more) {
            hb1.io.iptr_A    :=io.iptr_A
            hb1reset         :=reset.toBool
            hb2reset         :=reset.toBool
            hb3reset         :=reset.toBool
            cic3reset        :=reset.toBool
            hb2.io.iptr_A    :=hb1.io.Z
            hb3.io.iptr_A    :=hb2.io.Z
            cic3.io.iptr_A   :=hb3.io.Z
            io.Z             :=cic3.io.Z
        }
    }
}
//This is the object to provide verilog
object f2_interpolator extends App {
  //Convert coeffs to integers with 16 bit resolution
  //val coeffres=16
  chisel3.Driver.execute(args, () => new f2_interpolator() )
}

