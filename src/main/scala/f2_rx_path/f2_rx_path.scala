// See LICENSE for license details.
//
//Start with a static tb and try to genererate a gnerator for it
package f2_rx_path
import chisel3._
import chisel3.util._
import chisel3.experimental._
import dsptools._
import dsptools.numbers._
import freechips.rocketchip.util._
import f2_decimator._
import prog_delay._

class rx_path_adc_ioctrl (
        val inputn    : Int=9,
        val n         : Int=16,
        val users     : Int=4,
        val progdelay : Int=64,
        val finedelay : Int=32,
        val weightbits: Int=10
    ) extends Bundle {
        val adc_fifo_lut_mode  = UInt(3.W)
        val inv_adc_clk_pol = Bool()
        val reset_adcfifo      = Bool()
        val adc_lut_write_addr = UInt(inputn.W)
        val adc_lut_write_val  = DspComplex(SInt(inputn.W), SInt(inputn.W))
        val adc_lut_write_en   = Bool()
        val user_delays        = Vec(users,UInt(log2Ceil(progdelay).W))
        val user_weights       = Vec(users,DspComplex(SInt(weightbits.W),SInt(weightbits.W)))
        val fine_delays        = UInt(log2Ceil(finedelay).W)
}

// How to extract the "resolution" of a type?
class f2_rx_path_io (
        val inputn    : Int=9, 
        val n         : Int=16,  
        val users     : Int=4,
        val progdelay : Int=64,
        val finedelay : Int=32,
        val weightbits: Int=10

    ) extends Bundle {
    val decimator_clocks   = new f2_decimator_clocks()
    val decimator_controls = new f2_decimator_controls(gainbits=10)
    val adc_clock          = Input(Clock())
    val adc_ioctrl         = Input(new rx_path_adc_ioctrl(inputn=inputn, n=n,
                                        users=users,progdelay=progdelay,
                                        finedelay=finedelay))
    val iptr_A             = Input(DspComplex(SInt(inputn.W), SInt(inputn.W)))
    val Z                  = Output(Vec(users,DspComplex(SInt(n.W), SInt(n.W))))
}

class f2_rx_path (
        inputn: Int=9, 
        n         : Int=16, 
        users     : Int=4,
        progdelay : Int=64,
        finedelay : Int=32,
        weightbits: Int=10
    ) extends Module {
    val io = IO( new f2_rx_path_io(inputn=inputn,users=users,progdelay=progdelay))
    val adcproto=DspComplex(SInt(n.W),SInt(n.W))  
    val iclk=Wire(Bool())
    iclk := !io.adc_clock.asUInt
    val invclk = iclk.asClock 

    val inreg=withClock(io.adc_clock){RegInit(0.U.asTypeOf(adcproto))}
    val inreg_inv=withClock(invclk){RegInit(0.U.asTypeOf(adcproto))}
    inreg:=io.iptr_A
    inreg_inv:=io.iptr_A
    val synced = Wire(adcproto)
    when (io.adc_ioctrl.inv_adc_clk_pol === false.B ) {
        synced:=inreg
    }.elsewhen ( io.adc_ioctrl.inv_adc_clk_pol === true.B){ 
        synced:=inreg_inv
    }.otherwise{
        synced:=inreg
    }

    val decimator  = Module ( new  f2_decimator (n=n, resolution=32, coeffres=16, gainbits=10)).io
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
    val adcdelay=Module( new prog_delay(adcproto, maxdelay=finedelay)).io
    
    adcdelay.iptr_A:=adcfifo.deq.bits
    adcdelay.select<>io.adc_ioctrl.fine_delays

    //ADC lookup tables
    val adclut_real= SyncReadMem(scala.math.pow(2,9).toInt,SInt(inputn.W))
    val adclut_imag= SyncReadMem(scala.math.pow(2,9).toInt,SInt(inputn.W))
    val w_lutoutdata= RegInit(DspComplex.wire(0.S(inputn.W),0.S(inputn.W)))
    //val w_lutoutdata = Wire(DspComplex(SInt(inputn.W), SInt(inputn.W)))
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
        adclut_real.write(io.adc_ioctrl.adc_lut_write_addr,io.adc_ioctrl.adc_lut_write_val.real)
        adclut_imag.write(io.adc_ioctrl.adc_lut_write_addr,io.adc_ioctrl.adc_lut_write_val.imag)
    } 
    .otherwise {
        w_lutoutdata.real:=adclut_real.read(w_lutreadaddress.real.asUInt)
        w_lutoutdata.imag:=adclut_imag.read(w_lutreadaddress.imag.asUInt)
    }
    //RX input assignments        
    decimator.iptr_A:=w_inselect

    //Gives a possibility to tune each user separately
    //even they are currently only one data stream
    val userdelay= Seq.fill(users){ 
        withClock(io.decimator_clocks.hb3clock_low)(
            Module( new prog_delay(adcproto, maxdelay=progdelay)).io
        )
    }
    
    userdelay.map(_.iptr_A:=decimator.Z)
    (userdelay,io.adc_ioctrl.user_delays).zipped.map(_.select:=_)
    
    val weighted_users=withClock(io.decimator_clocks.hb3clock_low){
        Reg(Vec(users,DspComplex(SInt(n.W), SInt(n.W))))
    }
    ( weighted_users, 
        ( userdelay,io.adc_ioctrl.user_weights
        ).zipped.map( _.optr_Z * _)
    ).zipped.map(_:=_)
 
    when (io.decimator_controls.mode===0.U) {
        io.Z.map(_:=RegNext(decimator.Z))
    } .otherwise {
        //These are in the same clock domain if
        //the decimator in NOT bypassed
        (io.Z,weighted_users).zipped.map(_:=_)
    }
}
//This gives you verilog
object f2_rx_path extends App {
  chisel3.Driver.execute(args, () => new f2_rx_path)
}

