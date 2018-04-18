// See LICENSE for license details.
//
//Start with a static tb and try to genererate a gnerator for it
package f2_dsp_tapein6
import chisel3._
import chisel3.util._
import chisel3.experimental._
import dsptools._
import dsptools.numbers._
import freechips.rocketchip.util._
import f2_decimator._
import f2_rx_path_tapein6._

//class Z extends Bundle {
//  val w = UInt(4.W)
//  val z = UInt(8.W)
//}

//Creating arrays
//Wires
//class f2_dsp_decimator_controls(gainbits: Int) extends Bundle {
//        val cic3integscale  = Input(UInt(gainbits.W))
//        val hb1scale        = Input(UInt(gainbits.W))
//        val hb2scale        = Input(UInt(gainbits.W))
//        val hb3scale        = Input(UInt(gainbits.W))
//        val mode            = Input(UInt(3.W))
//        override def cloneType = (new f2_decimator_controls(gainbits)).asInstanceOf[this.type]
//}
class iofifosigs(n: Int) extends Bundle {
        val data=Vec(4,DspComplex(SInt(n.W), SInt(n.W)))
        val index=UInt(2.W)
        override def cloneType = (new iofifosigs(n)).asInstanceOf[this.type]
}

class f2_dsp_io(val inputn: Int=9, val n: Int=16, val antennas: Int=4, val users: Int=4) extends Bundle {
    val decimator_clocks   =  new f2_decimator_clocks    
    val decimator_controls = Vec(4,new f2_decimator_controls(gainbits=10))    
    //val iptr_A             = Vec(antennas,Flipped(DecoupledIO(DspComplex.wire(SInt(inputn.W), SInt(inputn.W)))))
    val iptr_A             = Input(Vec(antennas,DspComplex(SInt(inputn.W), SInt(inputn.W))))
    val adc_clocks         = Input(Vec(antennas,Clock()))
    val user_index         = Input(UInt(log2Ceil(users).W)) //W should be log2 of users
    val antenna_index      = Input(UInt(log2Ceil(antennas).W)) //W should be log2 of users
    val clock_symrate      = Input(Clock())
    val clock_symratex4    = Input(Clock())
    val clock_outfifo_deq  = Input(Clock())
    val clock_infifo_enq   = Input(Clock())
    val reset_index_count  = Input(Bool())
    val reset_adcfifo      = Input(Bool())
    val reset_outfifo      = Input(Bool())
    val reset_infifo       = Input(Bool())
    val rx_output_mode     = Input(UInt(3.W))
    val input_mode         = Input(UInt(3.W))
    val adc_fifo_mode      = Input(UInt(1.W))
    val ofifo              =  DecoupledIO(new iofifosigs(n=n))
    val iptr_fifo          =  Flipped(DecoupledIO(new iofifosigs(n=n)))
    // Index to be transmitted. Indicates user, or antenna
    //val index              =  Output(UInt(2.W))
}

class f2_dsp_tapein6 (inputn: Int=9, n: Int=16, antennas: Int=4, users: Int=4, fifodepth: Int=128 ) extends Module {
    val io = IO( new f2_dsp_io(n=n,users=users)
    )
    val iozerovec=VecInit(Seq.fill(4)(DspComplex.wire(0.S(n.W), 0.S(n.W))))

    //RX ADC FIFO's
    val adcproto=DspComplex(SInt(n.W),SInt(n.W))
    val adcfifodepth=16

    //There is a problem with the clone type of CrossingIO it is not defined
    val adcfifo = Seq.fill(antennas){ Module (new AsyncQueue(adcproto,depth=adcfifodepth)).io }
    (adcfifo,io.iptr_A).zipped.map(_.enq.bits:=_)
    (adcfifo,io.adc_clocks).zipped.map(_.enq_clock:=_)
    adcfifo.map(_.enq.valid:=true.B)
    adcfifo.map(_.enq_reset:=io.reset_adcfifo)
    adcfifo.map(_.deq_reset:=io.reset_adcfifo)
    adcfifo.map(_.deq_clock:=clock)
    adcfifo.map(_.deq.ready:=true.B)
    
    //-The RX:s
    // Vec is required to do runtime adressing of an array i.e. Seq is not hardware structure
    val rx_path  = VecInit(Seq.fill(antennas){ Module ( new  f2_rx_path_tapein6 (n=n, users=users)).io })
    //val rx_path  = Seq.fill(antennas){ Module ( new  f2_rx_path_tapein6 (n=n, users=users)).io }
     
    //val w_inselect = Seq.fill(4)(Wire(DspComplex(SInt(inputn.W), SInt(inputn.W))))
    //val w_inselect = Wire(Vec(4,DspComplex(SInt(inputn.W), SInt(inputn.W))))
    val w_inselect = Wire(Vec(4,DspComplex(SInt(inputn.W), SInt(inputn.W))))
    
    when (io.adc_fifo_mode===0.U) {
        (w_inselect,io.iptr_A).zipped.map(_:=_)
    } .elsewhen (io.adc_fifo_mode===1.U) {
       (w_inselect,adcfifo).zipped.map(_:=_.deq.bits)
    } .otherwise {
       w_inselect.map(_ := DspComplex.wire(0.S(inputn.W), 0.S(inputn.W)))
    }

    //RX input assignments        
    (rx_path,io.decimator_controls).zipped.map(_.decimator_controls:=_)
    rx_path.map(_.decimator_clocks:=io.decimator_clocks) 
    (rx_path,w_inselect).zipped.map(_.iptr_A:=_)

    //---Input fifo from serdes
    //Reformulate to RX compliant format.
    val infifo = Module(new AsyncQueue(new iofifosigs(n=n),depth=fifodepth)).io
    val r_iptr_fifo=  withClock(io.clock_symrate)(RegInit(VecInit(Seq.fill(4)(DspComplex.wire(0.S(n.W), 0.S(n.W))))))
    val r_iptr_index= withClock(io.clock_symrate)(RegInit(0.U(2.W))) //For sake of symmetry,dunno if needed
    val zero :: userssum :: Nil = Enum(2)
    val inputmode=RegInit(zero)
    
    infifo.deq_reset:=io.reset_infifo
    infifo.enq_reset:=io.reset_infifo
    infifo.enq_clock:=io.clock_infifo_enq

    infifo.enq<>io.iptr_fifo
    infifo.deq_clock :=io.clock_symrate
    when (io.input_mode===0.U) {
        inputmode := zero
        infifo.deq.ready:=false.B
    } .elsewhen (io.input_mode===1.U ) {
        inputmode := userssum
        infifo.deq.ready:=true.B
    } .otherwise {
        inputmode:=zero
        infifo.deq.ready:=false.B
    }

   //--Serdes Input fifo ends here
    when ( (infifo.deq.valid) && (inputmode===userssum)) { 
        r_iptr_index := infifo.deq.bits.index
        r_iptr_fifo:=infifo.deq.bits.data
    } .elsewhen ( inputmode===zero ) {
        r_iptr_index := 0.U
        r_iptr_fifo:=iozerovec
    } .otherwise {
        r_iptr_index := 0.U
        r_iptr_fifo:=iozerovec
    }

    //This is a 4 element receiver array, later to be concatenated to a bitvector
    val w_Z=  Wire(Vec(4,DspComplex(SInt(n.W), SInt(n.W))))
    val w_index=  Wire(UInt(2.W))
   
    // First we generate all possible output signals, then we just select The one we want.
    //Generate the sum of users
    val sumusersstream = withClockAndReset(io.clock_symrate,io.reset_outfifo)(RegInit(VecInit(Seq.fill(users)(DspComplex.wire(0.S(n.W), 0.S(n.W))))))
    for (user <-0 to users-1){ 
        sumusersstream(user):=rx_path.map( rxpath=> rxpath.Z(user)).foldRight(r_iptr_fifo(user))((usrleft,usrright)=> usrleft+usrright)
    }
  
  
    //All antennas, single user
    val seluser = withClockAndReset(io.clock_symrate,io.reset_outfifo)(RegInit(VecInit(Seq.fill(antennas)(DspComplex.wire(0.S(n.W), 0.S(n.W))))))
    (seluser,rx_path).zipped.map(_:=_.Z(io.user_index)) 

    //All users, single antenna
    val selrx = withClockAndReset(io.clock_symrate,io.reset_outfifo)(RegInit(VecInit(Seq.fill(users)(DspComplex.wire(0.S(n.W), 0.S(n.W))))))
    (selrx,rx_path(io.antenna_index).Z).zipped.map(_:=_) 

    //Single users, single antenna
    val selrxuser = withClockAndReset(io.clock_symrate,io.reset_outfifo)(RegInit(DspComplex.wire(0.S(n.W), 0.S(n.W))))
    selrxuser:=rx_path(io.antenna_index).Z(io.user_index)
  

    //State counter to select the user or branch to the output
    val index=withClockAndReset(io.clock_symratex4,io.reset_outfifo)(RegInit(0.U(2.W)))
    when ( ! io.reset_index_count ) {
        when (index === 3.U) {
            index:=0.U
        } .otherwise {
            index := index+1.U(1.W)
        }
    } .otherwise {
        index := 0.U
    }
  
    // Indexed user stream
    val indexeduserstream = withClockAndReset(io.clock_symratex4,io.reset_outfifo)(RegInit(VecInit(Seq.fill(4)(DspComplex.wire(0.S(n.W), 0.S(n.W))))))
    (indexeduserstream,rx_path).zipped.map(_:=_.Z(index))

    // Indexed RX stream
    val indexedrxstream = withClockAndReset(io.clock_symratex4,io.reset_outfifo)(RegInit(VecInit(Seq.fill(4)(DspComplex.wire(0.S(n.W), 0.S(n.W))))))
    (indexedrxstream,rx_path(index).Z).zipped.map(_:=_)


    //Selection part starts here
    //State definiotions for the selected mode. Just to map numbers to understandable labels
    val bypass :: select_users  :: select_antennas :: select_both :: stream_users :: stream_rx :: stream_sum :: Nil = Enum(7)
    //Select state
    val mode=RegInit(bypass)
    
    //Decoder for the modes
    when(io.rx_output_mode===0.U){
        mode := bypass
    } .elsewhen(io.rx_output_mode===1.U) {
        mode := select_users
    } .elsewhen(io.rx_output_mode===2.U) {
        mode:=select_antennas
    } .elsewhen(io.rx_output_mode===3.U) {
        mode:=select_both
    } .elsewhen(io.rx_output_mode===4.U) {
        mode:=stream_users
    } .elsewhen(io.rx_output_mode===5.U) {
        mode:=stream_rx
    } .elsewhen(io.rx_output_mode===6.U) {
        mode:=stream_sum
    }.otherwise {
        mode := bypass
    }

    // Fifo for ther output
    val proto=UInt((4*2*n+2).W)
    val outfifo = Module(new AsyncQueue(new iofifosigs(n=n),depth=fifodepth)).io

    //Defaults
    outfifo.enq_reset:=io.reset_outfifo 
    outfifo.enq_clock:=io.clock_symratex4
    outfifo.deq_reset:=io.reset_outfifo
    outfifo.deq.ready:=io.ofifo.ready
    outfifo.deq_clock:=io.clock_outfifo_deq
    w_index := withClock(io.clock_symrate)(RegNext(0.U))
    io.ofifo.valid   := outfifo.deq.valid

    //Put something out if nothig else defined
    (w_Z,rx_path).zipped.map(_:=_.Z(0))

    //Clock multiplexing does not work. Use valid to control output rate.
    val validcount  = withClockAndReset(io.clock_symratex4,io.reset_outfifo)(RegInit(0.U(2.W)))
    val validreg =  withClockAndReset(io.clock_symratex4,io.reset_outfifo)(RegInit(false.B))
    //control the valid signaö for the interface
    when ( (mode===bypass) ||  (mode===select_users) ||  (mode===select_antennas) || (mode===select_both) || (mode===stream_sum)  ) {
        // In these modes, the write rate is symrate
        when (validcount===3.U) {
            validcount:=0.U
            validreg := true.B
        } .otherwise {
            validcount:= validcount+1.U(1.W)
            validreg := false.B
        }
    } .elsewhen ( ( mode===stream_users) || (mode===stream_rx) ) {
        // In these modes, the write rate is 4xsymrate
        validreg :=true.B
    } .otherwise {
        //Unknown modes
        validcount := 0.U
        validreg := false.B
    }
    outfifo.enq.valid :=  validreg   


    //Mode operation definitions
    switch(mode) {
        is(bypass) {
            (w_Z,rx_path).zipped.map(_:=_.Z(0))
            w_index := withClockAndReset(io.clock_symrate,io.reset_outfifo)(RegNext(0.U))
        }
        is(select_users) {
            (w_Z,seluser).zipped.map(_:=_)
            w_index := withClockAndReset(io.clock_symrate,io.reset_outfifo)(RegNext(io.user_index))
        }
        is(select_antennas) {
           (w_Z,selrx).zipped.map(_:=_)
            w_index := withClockAndReset(io.clock_symrate,io.reset_outfifo)(RegNext(io.antenna_index))
        }
        is(select_both) {
            (w_Z,Seq(selrxuser)++Seq.fill(3)(DspComplex.wire(0.S,0.S))).zipped.map(_:=_)
            w_index := withClockAndReset(io.clock_symrate,io.reset_outfifo)(RegNext(0.U))
        }
        is(stream_users) {
            w_Z := indexeduserstream    
            w_index := withClockAndReset(io.clock_symratex4,io.reset_outfifo)(RegNext(index))

        }
        is(stream_rx) {
            w_Z := indexedrxstream    
            w_index := withClockAndReset(io.clock_symratex4,io.reset_outfifo)(RegNext(index))
        }
        is(stream_sum) {
            w_Z := sumusersstream    
            w_index := withClock(io.clock_symrate)(RegNext(0.U))
        }
    }
    
    //Here we reformat the output signals to a single bitvector
    when ( outfifo.enq.ready ){
        outfifo.enq.bits.data:=w_Z
        outfifo.enq.bits.index:=index
    } .otherwise {
        outfifo.enq.bits.data:=iozerovec
        outfifo.enq.bits.index:=0.U
    }
    io.ofifo.bits.data :=  outfifo.deq.bits.data
    io.ofifo.bits.index := outfifo.deq.bits.index

}

//This gives you verilog
object f2_dsp_tapein6 extends App {
  chisel3.Driver.execute(args, () => new f2_dsp_tapein6(inputn=9, n=16, antennas=4, users=4, fifodepth=128 ))
}

