// See LICENSE for license details.
//
//Start with a static tb and try to genererate a gnerator for it
package f2_dsp_tapein5
import chisel3._
import chisel3.util._
import chisel3.experimental._
import dsptools._
import dsptools.numbers._
import freechips.rocketchip.util._
import f2_decimator._
import f2_rx_path_tapein5._

class f2_dsp_io(n: Int=16, antennas: Int=4, users: Int=4) extends Bundle {
    val decimator_controls = Vec(antennas,new f2_decimator_controls(gainbits=10))    
    val iptr_A             = Input(Vec(antennas,DspComplex(SInt(n.W), SInt(n.W))))
    val user_index         = Input(UInt(log2Ceil(users).W)) //W should be log2 of users
    val antenna_index      = Input(UInt(log2Ceil(antennas).W)) //W should be log2 of users
    val clock_symrate      = Input(Clock())
    val clock_symratex4    = Input(Clock())
    val clock_outfifo_deq  = Input(Clock())
    val clock_infifo_enq   = Input(Clock())
    val reset_index_count  = Input(Bool())
    val reset_outfifo      = Input(Bool())
    val reset_infifo       = Input(Bool())
    val rx_output_mode     = Input(UInt(3.W))
    val input_mode         = Input(UInt(3.W))
    // Output bundle structure is determined by the sink (serdes), not by parameters
    //val Z                  =  Output(Vec(4,DspComplex(SInt(n.W), SInt(n.W))))
    val Z                  =  DecoupledIO(UInt((4*2*n+2).W))
    val iptr_fifo          =  Flipped(DecoupledIO(UInt((4*2*n+2).W)))
    // Index to be transmitted. Indicates user, or antenna
    //val index              =  Output(UInt(2.W))
    override def cloneType = (new f2_dsp_io(n,antennas,users)).asInstanceOf[this.type]
}

class f2_dsp_tapein5 (n: Int=16, antennas: Int=4, users: Int=4, fifodepth: Int=128 ) extends Module {
    val io = IO( new f2_dsp_io(n=n,users=users)
    )
  
    val rx_path  = VecInit(Seq.fill(antennas){ Module ( new  f2_rx_path_tapein5 (n=n, users=users)).io })

    //RX input assignments        
    for ( i <-0 to antennas-1){ 
        io.decimator_controls(i)<>rx_path(i).decimator_controls
        rx_path(i).iptr_A:=io.iptr_A(i)
    }

    //Input fifo from serdes
    //Reformulate to RX compliant format.
    val iproto=UInt((4*2*n+2).W)
    val infifo = Module(new AsyncQueue(iproto,depth=fifodepth)).io
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

    when ( (infifo.deq.valid) && (inputmode===userssum)) { 
        r_iptr_index := infifo.deq.bits(129,128)
        for ( i <-0 to 3){
            r_iptr_fifo(i).real:=infifo.deq.bits(i*2*n+n-1,i*2*n).asSInt
            r_iptr_fifo(i).imag:=infifo.deq.bits(i*2*n+2*n-1,i*2*n+n).asSInt
        }
    } .elsewhen ( inputmode===zero ) {
        r_iptr_index := 0.U
        for ( i <-0 to 3){
            r_iptr_fifo(i).real:=0.S
            r_iptr_fifo(i).imag:=0.S
        }
    } .otherwise {
        r_iptr_index := 0.U
        for ( i <-0 to 3){
            r_iptr_fifo(i).real:=0.S
            r_iptr_fifo(i).imag:=0.S
        
        }
    }

    //This is a 4 element receiver array, later to be concatenated to a bitvector
    val w_Z=  Wire(Vec(4,DspComplex(SInt(n.W), SInt(n.W))))
    val w_index=  Wire(UInt(2.W))
   
    // First we generate all possible output signals, then we just select The one we want.
    //Generate the sum of users
    val sumusersstream = withClock(io.clock_symrate)(RegInit(VecInit(Seq.fill(users)(DspComplex.wire(0.S(n.W), 0.S(n.W))))))
    for (user <-0 to users-1){ 
        sumusersstream(user):=rx_path.map( rxpath=> rxpath.Z(user)).foldRight(r_iptr_fifo(user))((usrleft,usrright)=> usrleft+usrright)
    }
  
  
    //All antennas, single usere
    val seluser = withClock(io.clock_symrate)(RegInit(VecInit(Seq.fill(antennas)(DspComplex.wire(0.S(n.W), 0.S(n.W))))))
    for (antenna <-0 to antennas-1){ 
        seluser(antenna):=rx_path(antenna).Z(io.user_index)
    }
  
    //All users, single antenna
    val selrx = withClock(io.clock_symrate)(RegInit(VecInit(Seq.fill(users)(DspComplex.wire(0.S(n.W), 0.S(n.W))))))
    for (user <-0 to users-1){ 
        selrx(user):=rx_path(io.antenna_index).Z(user)
    }
  
    //Single users, single antenna
    val selrxuser = withClock(io.clock_symrate)(RegInit(DspComplex.wire(0.S(n.W), 0.S(n.W))))
    selrxuser:=rx_path(io.antenna_index).Z(io.user_index)
  

    //State counter to select the user or branch to the output
    val index=withClock(io.clock_symratex4)(RegInit(0.U(n.W)))
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
    val indexeduserstream = withClock(io.clock_symratex4)(RegInit(VecInit(Seq.fill(4)(DspComplex.wire(0.S(n.W), 0.S(n.W))))))
    for ( rxindex <-0 to 3){
      indexeduserstream(rxindex):=rx_path(rxindex).Z(index)
    }
    // Indexed RX stream
    val indexedrxstream = withClock(io.clock_symratex4)(RegInit(VecInit(Seq.fill(4)(DspComplex.wire(0.S(n.W), 0.S(n.W))))))
    for ( uindex <-0 to 3){
      indexedrxstream(uindex):=rx_path(index).Z(uindex)
    }


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
    val outfifo = Module(new AsyncQueue(proto,depth=fifodepth)).io

    //Defaults
    outfifo.enq_reset:=io.reset_outfifo 
    outfifo.deq_reset:=io.reset_outfifo
    outfifo.deq.ready:=io.Z.ready
    outfifo.deq_clock:=io.clock_outfifo_deq
    outfifo.enq_clock:=io.clock_symratex4
    w_index := withClock(io.clock_symrate)(RegNext(0.U))
    io.Z.valid   := outfifo.deq.valid
    //Put something out if nothig else defined
    for (i <- 0 to 3) {
        w_Z(i) :=rx_path(i).Z(0)
    }

    //Clock multiplexing does not work. Use valid to control output rate.
    val validcount  = withClock(io.clock_symratex4)(RegInit(0.U(2.W)))
    val validreg =  withClock(io.clock_symratex4)(RegInit(false.B))
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
            when ( outfifo.enq.ready ){
                for (i <- 0 to 3) {
                    w_Z(i) :=rx_path(i).Z(0)
                }
                w_index := withClock(io.clock_symrate)(RegNext(0.U))
            }
        }
        is(select_users) {
            when ( outfifo.enq.ready ){
                for (i <- 0 to 3) {
                    w_Z(i) :=seluser(i)
                }
                w_index := withClock(io.clock_symrate)(RegNext(io.user_index))
            }
        }
        is(select_antennas) {
            when ( outfifo.enq.ready ){
                for (i <- 0 to 3) {
                    w_Z(i) :=selrx(i)
                }
                w_index := withClock(io.clock_symrate)(RegNext(io.antenna_index))
            }
        }
        is(select_both) {
            when ( outfifo.enq.ready ){
                w_Z(0) :=selrxuser
                w_Z(1) := DspComplex.wire(0.S,0.S)
                w_Z(2) := DspComplex.wire(0.S,0.S)
                w_Z(3) := DspComplex.wire(0.S,0.S)
                w_index := withClock(io.clock_symrate)(RegNext(0.U))
            }

        }
        is(stream_users) {
            when ( outfifo.enq.ready ){
                w_Z := indexeduserstream    
                w_index := withClock(io.clock_symratex4)(RegNext(index))
            }

        }
        is(stream_rx) {
            when ( outfifo.enq.ready ){
                w_Z := indexedrxstream    
                w_index := withClock(io.clock_symratex4)(RegNext(index))
            }
        }
        is(stream_sum) {
            when ( outfifo.enq.ready ){
                w_Z := sumusersstream    
                w_index := withClock(io.clock_symrate)(RegNext(0.U))
            }
        }
    }
    
    //Here we reformat the output signals to a single bitvector
    val w_concat_Z=  Wire(UInt((4*2*n).W))
    val w_concat_Z_and_index=  Wire(UInt((4*2*n+2).W))
    w_concat_Z:=w_Z.map(x => Cat(x.imag,x.real).asUInt).reduceRight((msb,lsb)=>Cat(msb,lsb).asUInt)
    w_concat_Z_and_index:=Cat(w_index,w_concat_Z).asUInt
    outfifo.enq.bits:=w_concat_Z_and_index
    io.Z.bits := outfifo.deq.bits

}

//This gives you verilog
object f2_dsp_tapein5 extends App {
  chisel3.Driver.execute(args, () => new f2_dsp_tapein5(n=16, antennas=4, users=4, fifodepth=128 ))
}

