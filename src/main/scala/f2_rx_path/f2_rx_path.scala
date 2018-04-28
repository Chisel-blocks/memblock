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


// How to extract the "resolution" of a type?
class f2_rx_path_io (
        val inputn   : Int=9, 
        val n        : Int=16,  
        val users    : Int=4,
        val progdelay: Int=64

    ) extends Bundle {
    val decimator_clocks   = new f2_decimator_clocks()
    val decimator_controls = new f2_decimator_controls(gainbits=10)
    val adc_clock          = Input(Clock())
    val reset_adcfifo      = Input(Bool())
    val adc_fifo_lut_mode  = Input(UInt(3.W))
    val adc_lut_write_addr = Input(UInt(inputn.W))
    val adc_lut_write_val  = Input(DspComplex(SInt(inputn.W), SInt(inputn.W)))
    val adc_lut_write_en   = Input(Bool())
    val user_delays        = Input(Vec(users,UInt(log2Ceil(progdelay).W)))
    val iptr_A             = Input(DspComplex(SInt(inputn.W), SInt(inputn.W)))
    val Z                  = Output(Vec(users,DspComplex(SInt(n.W), SInt(n.W))))
}

class f2_rx_path (
        inputn: Int=9, 
        n: Int=16, 
        users: Int=4,
        progdelay: Int=64
    ) extends Module {
    val io = IO( new f2_rx_path_io(inputn=inputn,users=users,progdelay=progdelay))
  
    val decimator  = Module ( new  f2_decimator (n=n, resolution=32, coeffres=16, gainbits=10)).io
    io.decimator_controls<>decimator.controls
    io.decimator_clocks<>decimator.clocks
    val adcproto=DspComplex(SInt(n.W),SInt(n.W))  
    val adcfifodepth=16
    val adcfifo = Module (new AsyncQueue(adcproto,depth=adcfifodepth)).io
    adcfifo.enq_clock:=io.adc_clock
    adcfifo.enq.valid:=true.B
    adcfifo.enq_reset:=io.reset_adcfifo
    adcfifo.deq_reset:=io.reset_adcfifo
    adcfifo.deq_clock:=clock
    adcfifo.deq.ready:=true.B

    //ADC lookup tables
    val adclut_real= Mem(scala.math.pow(2,9).toInt,SInt(inputn.W))
    val adclut_imag= Mem(scala.math.pow(2,9).toInt,SInt(inputn.W))
    val w_lutoutdata= RegInit(DspComplex.wire(0.S(inputn.W),0.S(inputn.W)))
    //val w_lutoutdata = Wire(DspComplex(SInt(inputn.W), SInt(inputn.W)))
    val w_lutreadaddress= RegInit(DspComplex.wire(0.S(inputn.W),0.S(inputn.W)))

    //Input selection wire
    val w_inselect = Wire(DspComplex(SInt(inputn.W), SInt(inputn.W)))

    when (io.adc_fifo_lut_mode===0.U) {
        //Bypass FIFO and LUT
        w_inselect:=io.iptr_A
        adcfifo.enq.bits:= io.iptr_A
        w_lutreadaddress.real:= io.adc_lut_write_addr.asSInt
        w_lutreadaddress.imag:= io.adc_lut_write_addr.asSInt
    } .elsewhen (io.adc_fifo_lut_mode===1.U) {
        //LUT bypassed, FIFO active
        adcfifo.enq.bits:=io.iptr_A
        w_inselect:=adcfifo.deq.bits
    } .elsewhen (io.adc_fifo_lut_mode===2.U) {
       //FIFO active, LUt active
       adcfifo.enq.bits:=io.iptr_A
       w_lutreadaddress:=adcfifo.deq.bits
       w_inselect:=w_lutoutdata
    } .elsewhen (io.adc_fifo_lut_mode===3.U) {
       //FIFO active, LUT active, LUT first
       //Sync problem assumed
       w_lutreadaddress:=io.iptr_A
       adcfifo.enq.bits:=w_lutoutdata
       w_inselect:=adcfifo.deq.bits
    } .elsewhen (io.adc_fifo_lut_mode===4.U) {
       //LUT active, FIFO bypassed
       //Sync problem assumed
       adcfifo.enq.bits:= io.iptr_A
       w_lutreadaddress:=io.iptr_A
       w_inselect:=w_lutoutdata
    } .otherwise {
       adcfifo.enq.bits:= io.iptr_A
       w_inselect:=adcfifo.deq.bits
    }
    
    //Enabled read
    when (io.adc_lut_write_en===true.B) {
        adclut_real.write(io.adc_lut_write_addr,io.adc_lut_write_val.real)
        adclut_imag.write(io.adc_lut_write_addr,io.adc_lut_write_val.imag)
    } 
    .otherwise {
        w_lutoutdata.real:=adclut_real.read(w_lutreadaddress.real.asUInt)
        w_lutoutdata.imag:=adclut_imag.read(w_lutreadaddress.imag.asUInt)
    }
    //RX input assignments        
    decimator.iptr_A:=w_inselect

    //Gives a possibility to tune each user separately
    //even they are currently only one data stream
    val proto=DspComplex(SInt(n.W), SInt(n.W))
    val p_delay= Seq.fill(users){ 
        withClock(io.decimator_clocks.hb3clock_low)(
            Module( new prog_delay(proto, maxdelay=progdelay)).io
        )
    }
    //val p_delay= Module( new prog_delay(proto, maxdelay=progdelay))
    
    p_delay.map(_.iptr_A:=decimator.Z)
    (p_delay,io.user_delays).zipped.map(_.select:=_)

    when (io.decimator_controls.mode===0.U) {
        io.Z.map(_:=RegNext(decimator.Z))
    } .otherwise {
        //These are in the same clock domain if
        //the decimator in NOT bypassed
        (io.Z,p_delay).zipped.map(_:=_.optr_Z)
    }
}
//This gives you verilog
object f2_rx_path extends App {
  chisel3.Driver.execute(args, () => new f2_rx_path)
}

