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

class f2_dsp_io(n: Int=16, antennas: Int=4, users: Int=4 ) extends Bundle {
    val decimator_controls = Vec(antennas,new f2_decimator_controls(gainbits=10))    
    val iptr_A             = Input(Vec(antennas,DspComplex(SInt(n.W), SInt(n.W))))
    val user_index         = Input(UInt(log2Ceil(users).W)) //W should be log2 of users
    val antenna_index      = Input(UInt(log2Ceil(antennas).W)) //W should be log2 of users
    val clock_symrate      = Input(Clock())
    val clock_symratex4    = Input(Clock())
    val clock_outfifo_deq  = Input(Clock())
    val reset_index_count  = Input(Bool())
    val reset_outfifo      = Input(Bool())
    val output_mode        = Input(UInt(3.W))
    // Output bundle structure is determined by the sink (serdes), not by parameters
    //val Z                  =  Output(Vec(4,DspComplex(SInt(n.W), SInt(n.W))))
    val Z                  =  DecoupledIO(UInt((4*2*n+2).W))
    // Index to be transmitted. Indicates user, or antenna
    //val index              =  Output(UInt(2.W))
    override def cloneType = (new f2_dsp_io(n,antennas,users)).asInstanceOf[this.type]
}

class f2_dsp_tapein5 (n: Int=16, antennas: Int=4, users: Int=4) extends Module {
    val io = IO( new f2_dsp_io(n=n,users=users)
    )
  
    val rx_path  = VecInit(Seq.fill(antennas){ Module ( new  f2_rx_path_tapein5 (n=n, users=users)).io })

    //Input assignments        
    for ( i <- 0 to antennas-1){ 
        io.decimator_controls(i)<>rx_path(i).decimator_controls
        rx_path(i).iptr_A:=io.iptr_A(i)
    }

    //This is a 2 dimensional array, later to beconcatenated to a bitvector
    val w_Z=  Wire(Vec(4,DspComplex(SInt(n.W), SInt(n.W))))
    val w_index=  Wire(UInt(2.W))
   
    //Generate the sum of users
    val sumusersstream = withClock(io.clock_symrate)(RegInit(VecInit(Seq.fill(users)(DspComplex.wire(0.S(n.W), 0.S(n.W))))))
    
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
  
    //All users, single antenna
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
   
   val bypass :: select_users  :: select_antennas :: select_both :: stream_users :: stream_rx :: stream_sum :: Nil = Enum(7)
    //Select state
    val state=RegInit(bypass)
    
    //Decoder for the modes
    when(io.output_mode===0.U){
        state := bypass
    } .elsewhen(io.output_mode===1.U) {
        state := select_users
    } .elsewhen(io.output_mode===2.U) {
        state:=select_antennas
    } .elsewhen(io.output_mode===3.U) {
        state:=select_both
    } .elsewhen(io.output_mode===4.U) {
        state:=stream_users
    } .elsewhen(io.output_mode===5.U) {
        state:=stream_rx
    } .elsewhen(io.output_mode===6.U) {
        state:=stream_sum
    }.otherwise {
        state := bypass
    }

    // Fifo for ther output
    val width=4*2*n+2
    val depth=128
    val proto=UInt(width.W)
    val outfifo = Module(new AsyncQueue(proto,depth=depth))

    //Defaults
    outfifo.io.enq_reset:=io.reset_outfifo 
    outfifo.io.deq_reset:=io.reset_outfifo
    outfifo.io.enq_clock:=io.clock_symrate
    outfifo.io.enq.valid:=withClock(io.clock_symrate)(RegNext(true.B))
    outfifo.io.deq.ready:=io.Z.ready
    outfifo.io.deq_clock:=io.clock_outfifo_deq
    w_index := withClock(io.clock_symrate)(RegNext(0.U))
    io.Z.valid   := true.B
    for (i <- 0 to 3) {
        w_Z(i) :=rx_path(i).Z(0)
    }
    //Modes
    switch(state) {
        is(bypass) {
            when ( outfifo.io.enq.ready ){
                for (i <- 0 to 3) {
                    w_Z(i) :=rx_path(i).Z(0)
                }
                w_index := withClock(io.clock_symrate)(RegNext(0.U))
            }
            outfifo.io.enq_clock:=io.clock_symrate
            outfifo.io.enq.valid:=withClock(io.clock_symrate)(RegNext(true.B))
        }
        is(select_users) {
            when ( outfifo.io.enq.ready ){
                for (i <- 0 to 3) {
                    w_Z(i) :=seluser(i)
                }
                w_index := withClock(io.clock_symrate)(RegNext(io.user_index))
            }
            outfifo.io.enq_clock:=io.clock_symrate
            outfifo.io.enq.valid:=withClock(io.clock_symrate)(RegNext(true.B))
        }
        is(select_antennas) {
            when ( outfifo.io.enq.ready ){
                for (i <- 0 to 3) {
                    w_Z(i) :=selrx(i)
                }
                w_index := withClock(io.clock_symrate)(RegNext(io.antenna_index))
            }
            outfifo.io.enq_clock:=io.clock_symrate
            outfifo.io.enq.valid:=withClock(io.clock_symrate)(RegNext(true.B))
        }
        is(select_both) {
            when ( outfifo.io.enq.ready ){
                w_Z(0) :=selrxuser
                w_Z(1) := DspComplex.wire(0.S,0.S)
                w_Z(2) := DspComplex.wire(0.S,0.S)
                w_Z(3) := DspComplex.wire(0.S,0.S)
                w_index := withClock(io.clock_symrate)(RegNext(0.U))
            }
            outfifo.io.enq_clock:=io.clock_symrate
            outfifo.io.enq.valid:=withClock(io.clock_symrate)(RegNext(true.B))

        }
        is(stream_users) {
            when ( outfifo.io.enq.ready ){
                w_Z := indexeduserstream    
                w_index := withClock(io.clock_symratex4)(RegNext(index))
            }
            outfifo.io.enq_clock:=io.clock_symratex4
            outfifo.io.enq.valid:=withClock(io.clock_symratex4)(RegNext(true.B))

        }
        is(stream_rx) {
            when ( outfifo.io.enq.ready ){
                w_Z := indexedrxstream    
                w_index := withClock(io.clock_symratex4)(RegNext(index))
            }
            outfifo.io.enq_clock:=io.clock_symratex4
            outfifo.io.enq.valid:=withClock(io.clock_symratex4)(RegNext(true.B))
        }
    }
    val w_concat_Z=  Wire(UInt((4*2*n).W))
    val w_concat_Z_and_index=  Wire(UInt((4*2*n+2).W))
    w_concat_Z:=w_Z.map(x => Cat(x.imag,x.real).asUInt).reduceRight((msb,lsb)=>Cat(msb,lsb).asUInt)
    w_concat_Z_and_index:=Cat(w_index,w_concat_Z).asUInt
    outfifo.io.enq.bits:=w_concat_Z_and_index
    

    io.Z.bits := outfifo.io.deq.bits
}

//This gives you verilog
object f2_dsp_tapein5 extends App {
  chisel3.Driver.execute(args, () => new f2_dsp_tapein5)
}

