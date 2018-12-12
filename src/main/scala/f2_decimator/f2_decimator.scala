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

class f2_decimator_clocks extends Bundle {
        val cic3clockslow   = Input(Clock())
        val hb1clock_low    = Input(Clock())
        val hb2clock_low    = Input(Clock())
        val hb3clock_low    = Input(Clock())
}

class f2_decimator_controls(val resolution: Int, val gainbits: Int) extends Bundle {
        val cic3integscale  = Input(UInt(gainbits.W))
        val cic3integshift  = Input(UInt(log2Ceil(resolution).W))
        val reset_loop      = Input(Bool())
        val hb1scale        = Input(UInt(gainbits.W))
        val hb2scale        = Input(UInt(gainbits.W))
        val hb3scale        = Input(UInt(gainbits.W))
        val mode            = Input(UInt(3.W))
        //override def cloneType = (new f2_decimator_controls(gainbits)).asInstanceOf[this.type]
}

class f2_decimator_io(n: Int, resolution: Int, gainbits: Int) extends Bundle {
        val clocks          = new f2_decimator_clocks
        val controls        = new f2_decimator_controls(resolution=resolution,gainbits=gainbits)
        val iptr_A          = Input(DspComplex(SInt(n.W), SInt(n.W)))
        val Z               = Output(DspComplex(SInt(n.W), SInt(n.W)))
        override def cloneType = (new f2_decimator_io(n,resolution,gainbits)).asInstanceOf[this.type]
}

class f2_decimator (n: Int=16, resolution: Int=32, coeffres: Int=16, gainbits: Int=10) extends Module {
    val io = IO(new f2_decimator_io(n=n,resolution=resolution,gainbits=gainbits)
    )

    //State definitions
    val bypass :: two :: four :: eight :: more :: Nil = Enum(5)
    //Select state
    val state=RegInit(bypass)
    
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
    val cic3reset = Wire(Bool())
    cic3reset     :=reset.toBool()
    val cic3= withClockAndReset(clock,cic3reset)(Module( new cic3(n=n,resolution=resolution,gainbits=gainbits)))

    val hb1reset = Wire(Bool())
    hb1reset     :=reset.toBool
    val hb1 = withClockAndReset(io.clocks.cic3clockslow,hb1reset)(Module( new halfband( n=16, resolution=32,coeffs=halfband_BW_01125_N_6.H.map(_ * (math.pow(2,coeffres-1)-1)).map(_.toInt))))

    val hb2reset = Wire(Bool())
    hb2reset     :=reset.toBool
    val hb2 = withClockAndReset(io.clocks.hb1clock_low,hb2reset)(Module( new halfband( n=16, resolution=32,coeffs=halfband_BW_0225_N_8.H.map(_ * (math.pow(2,coeffres-1)-1)).map(_.toInt))))

    val hb3reset = Wire(Bool())
    hb3reset    :=reset.toBool
    val hb3 = withClockAndReset(io.clocks.hb2clock_low,hb3reset)(Module( new halfband( n=16, resolution=32,coeffs=halfband_BW_045_N_40.H.map(_ * (math.pow(2,coeffres-1)-1)).map(_.toInt))))

    //Default is to bypass
    cic3.io.clockslow :=io.clocks.cic3clockslow
    cic3.io.integscale:=io.controls.cic3integscale
    cic3.io.integshift:=io.controls.cic3integshift
    hb1.io.clock_low  :=io.clocks.hb1clock_low
    hb1.io.scale      :=io.controls.hb1scale
    hb2.io.clock_low  :=io.clocks.hb2clock_low
    hb2.io.scale      :=io.controls.hb2scale
    hb3.io.clock_low  :=io.clocks.hb3clock_low
    hb3.io.scale      :=io.controls.hb3scale
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
            cic3reset        :=io.controls.reset_loop 
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

