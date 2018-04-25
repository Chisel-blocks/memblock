// See LICENSE for license details.
package f2_lane_switch
import chisel3._
import chisel3.util._
import chisel3.experimental._
import dsptools._
import dsptools.numbers._
import freechips.rocketchip.util._
import f2_dsp_tapein6._

class f2_lane_switch_io(val n: Int=16, val dspios: Int=7, val serdesios: Int=6) extends Bundle {
    //input comes from Lane and goes to SerDes
    val from_dsp             = Vec(dspios,Flipped(DecoupledIO(new iofifosigs(n=n))))
    val from_dsp_memory      = Vec(dspios,Flipped(DecoupledIO(new iofifosigs(n=n))))
    val to_dsp               = Vec(dspios,DecoupledIO(new iofifosigs(n=n)))
    val to_dsp_mode          = Vec(dspios,Input(UInt(2.W))) //Off/on
    val dsp_to_serdes_address= Vec(serdesios,Input(UInt(log2Ceil(dspios).W))) //every serdes has 7 sources
    val serdes_to_dsp_address= Vec(dspios,Input(UInt(log2Ceil(serdesios).W)))  //7 dsp dspios has 6 serdes sources
    val from_serdes          = Vec(serdesios,Flipped(DecoupledIO(new iofifosigs(n=n))))
    val from_serdes_scan     = Vec(serdesios,Flipped(DecoupledIO(new iofifosigs(n=n))))
    val to_serdes            = Vec(serdesios,DecoupledIO(new iofifosigs(n=n)))
    val to_serdes_mode       = Vec(serdesios,Input(UInt(2.W))) //Off/On/Scan
}

class f2_lane_switch (n: Int=16, dspios: Int=7, serdesios: Int=6 ) extends Module {
    val io = IO( new f2_lane_switch_io(n=n,dspios=dspios,serdesios=serdesios))
    val iozerovec=VecInit(Seq.fill(4)(DspComplex.wire(0.S(n.W), 0.S(n.W))))
   
    //Defaults
    io.from_dsp.map(_.ready:=false.B)
    io.from_dsp_memory.map(_.ready:=false.B)
    io.from_serdes_scan.map(_.ready:=false.B)
    io.from_serdes.map(_.ready:=false.B)
    io.to_dsp.map(_.valid:=false.B)
    io.to_serdes.map(_.valid:=false.B)

    //From SerDes to dsp routing
    for ( i <- 0 to 6) {
        when ( io.to_dsp_mode(i)===0.U) {
            io.to_dsp(i).bits.data:=iozerovec
            io.to_dsp(i).bits.index:=0.U
            io.to_dsp(i).valid:=1.U
        } .elsewhen ( io.to_dsp_mode(i)===1.U) {
            io.to_dsp(i)<>io.from_serdes(io.serdes_to_dsp_address(i))
        } .elsewhen ( io.to_dsp_mode(i)===2.U) {
            io.to_dsp(i)<>io.from_serdes_scan(io.serdes_to_dsp_address(i))
        } .otherwise {
            io.to_dsp(i).bits.data:=iozerovec
            io.to_dsp(i).bits.index:=0.U
            io.to_dsp(i).valid:=1.U
        }

    }
    //From Dsp to SerDes routing
    for ( i <- 0 to 5) {
        when ( io.to_serdes_mode(i)===0.U) {
            io.to_serdes(i).bits.data:=iozerovec
            io.to_serdes(i).bits.index:=0.U
            io.to_serdes(i).valid:=1.U
        } .elsewhen ( io.to_serdes_mode(i)===1.U) {
            io.to_serdes(i)<>io.from_dsp(io.dsp_to_serdes_address(i))
        } .elsewhen ( io.to_serdes_mode(i)===2.U) {
            io.to_serdes(i)<>io.from_dsp_memory(io.dsp_to_serdes_address(i))
        } .otherwise { 
            io.to_serdes(i).bits.data:=iozerovec
            io.to_serdes(i).bits.index:=0.U
            io.to_serdes(i).valid:=1.U
       }
   }
}

//This gives you verilog
object f2_lane_switch extends App {
  chisel3.Driver.execute(args, () => new f2_lane_switch(n=16, dspios=7, serdesios=6))
}

