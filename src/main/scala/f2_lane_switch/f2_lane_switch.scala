// See LICENSE for license details.
package f2_lane_switch
import chisel3._
import chisel3.util._
import chisel3.experimental._
import dsptools._
import dsptools.numbers._
import freechips.rocketchip.util._
import f2_rx_dsp._

class f2_lane_switch_io(
    val n         : Int=16,
    val users     : Int=4,
    val todspios  : Int=4,
    val fromdspios: Int=1,
    val serdesios : Int=6
) extends Bundle {
    //input comes from Lane and goes to SerDes
    val from_dsp             = Vec(fromdspios,Flipped(DecoupledIO(new iofifosigs(n=n,users=users))))
    val to_dsp               = Vec(todspios,DecoupledIO(new iofifosigs(n=n,users=users)))
    val to_dsp_mode          = Vec(todspios,Input(UInt(2.W))) //Off/on/scan
    val dsp_to_serdes_address= Vec(serdesios,Input(UInt(log2Ceil(fromdspios).W))) //every serdes has 7 sources
    val serdes_to_dsp_address= Vec(todspios,Input(UInt(log2Ceil(serdesios).W)))  //7 dsp dspios has 6 serdes sources
    val from_serdes          = Vec(serdesios,Flipped(DecoupledIO(new iofifosigs(n=n,users=users))))
    val to_serdes            = Vec(serdesios,DecoupledIO(new iofifosigs(n=n,users=users)))
    val to_serdes_mode       = Vec(serdesios,Input(UInt(2.W))) //Off/On/memory
    //These are scans to feed constant to serdes 
    val from_serdes_scan     = Vec(serdesios,Flipped(DecoupledIO(new iofifosigs(n=n,users=users))))
    val from_dsp_scan        = Vec(serdesios,Flipped(DecoupledIO(new iofifosigs(n=n,users=users))))
}

class f2_lane_switch (
        n         : Int =16, 
        users     : Int=4,
        todspios  : Int=4,
        fromdspios: Int=1,
        serdesios : Int=6 
    ) extends Module {

    val io = IO( 
        new f2_lane_switch_io(
            n=n,
            users=users,
            fromdspios=fromdspios,
            todspios=todspios,
            serdesios=serdesios
        )
    )
    //Zeros
    val iofifozero = 0.U.asTypeOf(new iofifosigs(n=n,users=users))
   
    //Defaults
    io.from_serdes_scan.map(_.ready:=false.B)
    io.from_dsp_scan.map(_.ready:=false.B)
    io.from_dsp.map(_.ready:=false.B)
    io.from_serdes.map(_.ready:=false.B)
    io.to_dsp.map(_.valid:=false.B)
    io.to_serdes.map(_.valid:=false.B)

    //From SerDes to dsp routing
    for ( i <- 0 to todspios-1 ) {
        when ( io.to_dsp_mode(i)===0.U) {
            io.to_dsp(i).bits:=iofifozero
            io.to_dsp(i).valid:=1.U
        } .elsewhen ( io.to_dsp_mode(i)===1.U) {
            io.to_dsp(i)<>io.from_serdes(io.serdes_to_dsp_address(i))
        } .elsewhen ( io.to_dsp_mode(i)===2.U) {
            io.to_dsp(i)<>io.from_serdes_scan(io.serdes_to_dsp_address(i))
            io.from_serdes_scan.map(_.ready:=true.B)
        } .otherwise {
            io.to_dsp(i).bits:=iofifozero
            io.to_dsp(i).valid:=1.U
        }

    }
    //From Dsp to SerDes routing
    for ( i <- 0 to serdesios-1) {
        when ( io.to_serdes_mode(i)===0.U) {
            io.to_serdes(i).bits:=iofifozero
            io.to_serdes(i).valid:=1.U
        } .elsewhen ( io.to_serdes_mode(i)===1.U) {
            io.to_serdes(i)<>io.from_dsp(io.dsp_to_serdes_address(i))
        } .elsewhen ( io.to_serdes_mode(i)===2.U) {
            io.to_serdes(i)<>io.from_dsp_scan(io.dsp_to_serdes_address(i))
        } .otherwise { 
            io.to_serdes(i).bits:=iofifozero
            io.to_serdes(i).valid:=1.U
       }
   }
}

//This gives you verilog
object f2_lane_switch extends App {
  chisel3.Driver.execute(args, () => new f2_lane_switch(n=16, todspios=4, fromdspios=1, serdesios=6))
}

