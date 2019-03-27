// See LICENSE for license details.
//
//Start with a static tb and try to genererate a gnerator for it
package f2_rx_path
import chisel3._
import chisel3.util._
import chisel3.experimental.{FixedPoint,withClock, withReset, withClockAndReset}
import dsptools._
import dsptools.numbers._
import freechips.rocketchip.util._
import f2_decimator._
import prog_delay._
import clkmux._

class rx_path_adc_ioctrl (
        val inputn    : Int=9,
        val n         : Int=16,
        val users     : Int=4,
        val progdelay : Int=64,
        val finedelay : Int=32,
        val weightbits: Int=10
    ) extends Bundle {
        val adc_fifo_lut_mode  = UInt(3.W)
        val inv_adc_clk_pol    = Bool()
        val reset_adcfifo      = Bool()
        val adc_lut_write_addr = UInt(inputn.W)
        val adc_lut_write_val  = DspComplex(SInt(inputn.W), SInt(inputn.W))
        val adc_lut_write_en   = Bool()
        val adc_lut_reset      = Bool()
        val user_delays        = Vec(users,UInt(log2Ceil(progdelay).W))
        val user_weights       = Vec(users,DspComplex(SInt(weightbits.W),SInt(weightbits.W)))
        val fine_delays        = UInt(log2Ceil(finedelay).W)
}

// How to extract the "resolution" of a type?
class f2_rx_path_io (
        val inputn    : Int=9, 
        val n         : Int=16,  
        val resolution: Int=32,  
        val users     : Int=4,
        val progdelay : Int=64,
        val finedelay : Int=32,
        val weightbits: Int=10

    ) extends Bundle {
    val decimator_clocks   = new f2_decimator_clocks()
    val decimator_controls = new f2_decimator_controls(resolution=resolution,gainbits=10)
    val adc_clock          = Input(Clock())
    val adc_ioctrl         = Input(new rx_path_adc_ioctrl(inputn=inputn, n=n,
                                        users=users,progdelay=progdelay,
                                        finedelay=finedelay))
    val iptr_A             = Input(DspComplex(SInt(inputn.W), SInt(inputn.W)))
    val Z                  = Output(Vec(users,DspComplex(SInt(n.W), SInt(n.W))))
    val bypass_out         = Output(Vec(users,DspComplex(SInt(n.W), SInt(n.W))))
    val bypass_Ndiv        = Input(UInt(8.W)) //Division at maximum with nuber of users
                                         // Consistent with the clockdivider
    val bypass_clock       = Input(Clock())  //Clock for bypass mode
}

class f2_rx_path (
        inputn: Int=9, 
        n         : Int=16, 
        resolution: Int=32, 
        users     : Int=4,
        progdelay : Int=64,
        finedelay : Int=32,
        weightbits: Int=10
    ) extends Module {
    val io = IO( new f2_rx_path_io(inputn=inputn,users=users,progdelay=progdelay))

    val adcproto=DspComplex(SInt(n.W),SInt(n.W))  
    val iclk=Wire(Bool())
    val input_clockmux= Module( new clkmux() ).io

    // Two options for input synchronization
    iclk := !io.adc_clock.asUInt
    val invclk = iclk.asClock 
    input_clockmux.c0:=io.adc_clock
    input_clockmux.c1:=invclk
    input_clockmux.sel:=io.adc_ioctrl.inv_adc_clk_pol
    val synced=withClock(input_clockmux.co){RegInit(0.U.asTypeOf(adcproto))}

    //Assigns io.iptr_A. to LSB end with sign bit extension
    synced:=io.iptr_A

    val decimator  = Module ( 
        new  f2_decimator (
            n=n, 
            resolution=resolution, 
            coeffres=16, 
            gainbits=10)
        ).io
    io.decimator_controls<>decimator.controls
    io.decimator_clocks<>decimator.clocks
    val adcfifodepth=16
    val adcfifo = Module (new AsyncQueue(adcproto,depth=adcfifodepth)).io
    adcfifo.enq_clock:=io.adc_clock
    adcfifo.enq.valid:=true.B
    adcfifo.enq_reset:=io.adc_ioctrl.reset_adcfifo
    adcfifo.deq_reset:=io.adc_ioctrl.reset_adcfifo
    adcfifo.deq_clock:=clock
    adcfifo.deq.ready:=true.B

    // Adc delay compensation
    // Fast Clock
    val adcdelay=Module( new prog_delay(adcproto, maxdelay=finedelay)).io
    
    adcdelay.iptr_A:=adcfifo.deq.bits
    adcdelay.select<>io.adc_ioctrl.fine_delays

    //ADC lookup tables
    // Fast Clock
    val adclut_real= SyncReadMem(scala.math.pow(2,9).toInt,SInt(inputn.W))
    val adclut_imag= SyncReadMem(scala.math.pow(2,9).toInt,SInt(inputn.W))
    val w_lutoutdata= withReset(io.adc_ioctrl.adc_lut_reset){RegInit(DspComplex.wire(0.S(inputn.W),0.S(inputn.W)))}
    // Fast Clock
    val w_lutreadaddress= RegInit(DspComplex.wire(0.S(inputn.W),0.S(inputn.W)))

    //Input selection wire
    val w_inselect = Wire(DspComplex(SInt(inputn.W), SInt(inputn.W)))

    when (io.adc_ioctrl.adc_fifo_lut_mode===0.U) {
        //Bypass FIFO and LUT
        w_inselect:=synced
        adcfifo.enq.bits:= synced
        w_lutreadaddress.real:= io.adc_ioctrl.adc_lut_write_addr.asSInt
        w_lutreadaddress.imag:= io.adc_ioctrl.adc_lut_write_addr.asSInt
    } .elsewhen (io.adc_ioctrl.adc_fifo_lut_mode===1.U) {
        //LUT bypassed, FIFO active
        adcfifo.enq.bits:=synced
        w_inselect:=adcdelay.optr_Z
    } .elsewhen (io.adc_ioctrl.adc_fifo_lut_mode===2.U) {
       //FIFO active, LUt active
       adcfifo.enq.bits:=synced
       w_lutreadaddress:=adcdelay.optr_Z
       w_inselect:=w_lutoutdata
    } .elsewhen (io.adc_ioctrl.adc_fifo_lut_mode===3.U) {
       //FIFO active, LUT active, LUT first
       //Sync problem assumed
       w_lutreadaddress:=synced
       adcfifo.enq.bits:=w_lutoutdata
       w_inselect:=adcdelay.optr_Z
    } .elsewhen (io.adc_ioctrl.adc_fifo_lut_mode===4.U) {
       //LUT active, FIFO bypassed
       //Sync problem assumed
       adcfifo.enq.bits:= synced
       w_lutreadaddress:=synced
       w_inselect:=w_lutoutdata
    } .otherwise {
       adcfifo.enq.bits:= synced
       w_inselect:=adcdelay.optr_Z
    }
    
    //Enabled write
    when (io.adc_ioctrl.adc_lut_write_en===true.B) {
        //With master clock
            adclut_real.write(io.adc_ioctrl.adc_lut_write_addr,io.adc_ioctrl.adc_lut_write_val.real)
            adclut_imag.write(io.adc_ioctrl.adc_lut_write_addr,io.adc_ioctrl.adc_lut_write_val.imag)
    } .otherwise {
        //With master clock
            w_lutoutdata.real:=adclut_real.read(w_lutreadaddress.real.asUInt)
            w_lutoutdata.imag:=adclut_imag.read(w_lutreadaddress.imag.asUInt)
    }

    //Reset signals to control the bypass mode
    // When the decimator is bypassed, also the rest of the DSP is
    // Shut down
    val reset_bypass_register=Wire(Bool())
    val reset_decimator_input=Wire(Bool())
    val bypass_serdes_register=withReset(reset_bypass_register){
        RegInit(0.U.asTypeOf(io.bypass_out))}

    val bypass_counter=withReset(reset_bypass_register){
        RegInit(0.U.asTypeOf(io.bypass_Ndiv))
    }
    //Count the index for the bypass mode cant exceed users
    when ( bypass_counter < users) {
        when ( bypass_counter < io.bypass_Ndiv-1.U) {
            bypass_counter:=bypass_counter+1.U
        } .otherwise {
            bypass_counter:=0.U
        }
    } .otherwise {
        bypass_counter:=0.U
    }
    

    when (io.decimator_controls.mode===0.U) {
        //mode 0 bypasses the decimator
        reset_bypass_register:=false.B
        reset_decimator_input:=true.B
        bypass_serdes_register(bypass_counter):=w_inselect
    } .otherwise {
        reset_bypass_register:=true.B
        reset_decimator_input:=false.B
    }
        io.bypass_out:=withClock(io.bypass_clock){RegNext(bypass_serdes_register)}
    //RX input assignments        
    val reg_decim_in=withReset(reset_decimator_input){
        RegInit(0.U.asTypeOf(w_inselect))
    }
    reg_decim_in:=w_inselect
    decimator.iptr_A:=reg_decim_in

    //Gives a possibility to tune each user separately
    //even they are currently only one data stream
    val userdelay= Seq.fill(users){ 
        withClock(io.decimator_clocks.hb3clock_low)(
            Module( new prog_delay(adcproto, maxdelay=progdelay)).io
        )
    }
    
    userdelay.map(_.iptr_A:=decimator.Z)
    (userdelay,io.adc_ioctrl.user_delays).zipped.map(_.select:=_)
    
    //Weighting of the users. Will be raeplaced by Beamformer
    val weighted_users=Seq.fill(users){
        withClock(io.decimator_clocks.hb3clock_low){
            Reg(DspComplex(SInt(n.W), SInt(n.W)))
        }
    }
     
    // We do the Fixed point arithmetic here because 
    // we only need FP to handle gain, so input does not need ot be FP
    // Complex multiplication did not work as expected
    //w_weighted_users(i):=( userdelay(i).optr_Z * io.adc_ioctrl.user_weights(i) 
    // Must extend the widths to the RESULT WIDTH before multiplication
    val w_weights=Seq.fill(users){
        // Integer part is just the sign
        Wire(DspComplex(
            FixedPoint((n+weightbits).W,(weightbits-1).BP),
            FixedPoint((weightbits).W,(weightbits-1).BP)
        ))
    }
    val w_delays=Seq.fill(users){
        // Sum of fractional bits must eventually equal weightbits after multiplication
        Wire(DspComplex(FixedPoint((n+weightbits).W,1.BP),FixedPoint(n.W,1.BP)))
    }

    val w_weighted_users=Seq.fill(users){
        // Sum of fractional bits must eventually equal weightbits after multiplication
        Wire(DspComplex(
            FixedPoint((n+weightbits).W,(weightbits).BP),
            FixedPoint((n+weightbits).W,(weightbits).BP)
        ))
    }

    for (i <- 0 until users ) {
        w_weights(i).real:=io.adc_ioctrl.user_weights(i).real
            .asFixedPoint((weightbits-1).BP)
        w_weights(i).imag:=io.adc_ioctrl.user_weights(i).imag
            .asFixedPoint((weightbits-1).BP)
        w_delays(i).real:=userdelay(i).optr_Z.real.asFixedPoint(1.BP)
        w_delays(i).imag:=userdelay(i).optr_Z.imag.asFixedPoint(1.BP)
        w_weighted_users(i):= w_delays(i) * w_weights(i) 
        weighted_users(i).real:=w_weighted_users(i).real.floor.asSInt 
        weighted_users(i).imag:=w_weighted_users(i).imag.floor.asSInt 
    }
    //Assign output
    io.Z:=weighted_users
    //This is the bypass from the luts-section
    // Fast clock
}
//This gives you verilog
object f2_rx_path extends App {
  chisel3.Driver.execute(args, () => new f2_rx_path)
}

