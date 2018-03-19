// See LICENSE for license details.
//
//Start with a static tb and try to genererate a gnerator for it
package f2_dsp_tapein1
import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.experimental._
import dsptools._
import dsptools.numbers._
import f2_decimator._
import ofdm._

//class SyncIO[T <: Data](params: SyncParams[T]) extends Bundle {
//    //val in = Flipped(Valid(params.protoIn))
//    val packetDetect = Output(Bool())
//
//    val autocorrConfig   = new AutocorrConfigIO(params.autocorrParams)
//    val peakDetectConfig = new PeakDetectConfigIO(params.peakDetectParams)
//    val autocorrFF       = Input(params.peakDetectParams.protoEnergyFF)
//    val freqScaleFactor  = Input(params.protoAngle)
//}


class sync_config_io[T <: Data](params: SyncParams[T]) extends Bundle {
    val packetDetect = Output(Bool())
    val autocorrConfig   = new AutocorrConfigIO(params.autocorrParams)
    val peakDetectConfig = new PeakDetectConfigIO(params.peakDetectParams)
    val autocorrFF       = Input(params.peakDetectParams.protoEnergyFF)
    val freqScaleFactor  = Input(params.protoAngle)
}
//
//class f2_dsp_io[T <: Data](n: Int=16, users: Int=2, params: SyncParams[T] ) extends SyncIO(params) {
class f2_dsp_io[T <: Data](n: Int=16, users: Int=2, params: SyncParams[T] ) extends Bundle {
    val decimator_controls = new f2_decimator_controls(gainbits=10)
    val sync_config        = new sync_config_io(params=params)
    val mode               = Input(UInt(2.W))
    val iptr_A             = Input(DspComplex(SInt(n.W), SInt(n.W)))
    val Z                  = Output(Vec(users,DspComplex(SInt(n.W), SInt(n.W))))
}
//Sync_inputs
//  input         clock, // @[:@6008.4]
//  input         reset, // @[:@6009.4]
//  input         io_in_valid, // @[:@6010.4]
//  input  [15:0] io_in_bits_real, // @[:@6010.4]
//  input  [15:0] io_in_bits_imag, // @[:@6010.4]
//  output        io_out_valid, // @[:@6010.4]
//  output [15:0] io_out_bits_real, // @[:@6010.4]
//  output [15:0] io_out_bits_imag, // @[:@6010.4]
//  output        io_packetDetect, // @[:@6010.4]
//  input  [7:0]  io_autocorrConfig_depthApart, // @[:@6010.4]
//  input  [7:0]  io_autocorrConfig_depthOverlap, // @[:@6010.4]
//  input  [31:0] io_peakDetectConfig_energyFF, // @[:@6010.4]
//  input  [31:0] io_peakDetectConfig_energyMult, // @[:@6010.4]
//  input  [31:0] io_peakDetectConfig_accumMult, // @[:@6010.4]
//  input  [31:0] io_peakDetectConfig_energyOffset, // @[:@6010.4]
//  input  [6:0]  io_peakDetectConfig_idlePeriod, // @[:@6010.4]
//  input  [31:0] io_autocorrFF, // @[:@6010.4]
//  input  [31:0] io_freqScaleFactor // @[:@6010.4]

class f2_dsp_tapein1 (n: Int=16, users: Int=2, resolution: Int=32) extends Module {
    //How to define Sint
    val protoIn  = SInt(n.W)
    val protoOut = SInt(n.W)
    val protoCORDIC = SInt(n.W)
    val protoAutocorr = protoIn
    val protoBig = SInt(resolution.W)
    val protoAngle = SInt(resolution.W)
    val stfParams = SyncParams(
    protoIn = DspComplex(protoIn, protoIn),
    protoOut = DspComplex(protoOut, protoOut),
    filterProtos = (protoIn, protoIn, protoIn),
    filterConstructor = (pIn: SInt, pOut: SInt, pCoeff: SInt) => new STF64MatchedFilter(pIn, pOut, pCoeff),
    protoAngle = protoAngle,
        autocorrParams = AutocorrParams(
            protoIn = DspComplex(protoAutocorr, protoAutocorr),
            maxApart = 128,
            maxOverlap = 128
        ),
        peakDetectParams = PeakDetectParams(
            protoCorr = protoCORDIC,
            protoEnergyFF = protoBig,
            protoEnergyMult = protoBig,
            windowSize = 16
        ),
        ncoParams = NCOParams(
            phaseWidth = 32,
            1024,
            {x: UInt => x.asTypeOf(protoAngle)},
            protoFreq = protoAngle,
            protoOut = SInt(resolution.W)
        )
    )
    //val io = IO( new f2_dsp_io(n=n,users=users, params=stfParams))
    val io = IO( new f2_dsp_io(n=n,users=users, params=stfParams))
    //
    //State definitions
    val bypass :: decimate :: detect :: Nil = Enum(3)
    //Select state
    val state=RegInit(bypass)

    //Decoder for the modes
    when(io.mode===0.U){
        state := bypass
    } .elsewhen(io.mode===1.U) {
        state := decimate
    } .elsewhen(io.mode===2.U) {
        state := detect
    }.otherwise {
        state := bypass
    }

    val decimator  = Module ( new  f2_decimator (n=16, resolution=32, coeffres=16, gainbits=10))
    io.decimator_controls<>decimator.io.controls
    decimator.io.iptr_A:=io.iptr_A

    val ofdm_sync = withClock(decimator.io.controls.hb3clock_low)( Module( new Sync(params=stfParams) ))
    ofdm_sync.io.in.bits:=decimator.io.Z

    for ( i <- 0 to users-1 ) { 
        
        switch(state){ 
            is(bypass) {
                io.Z(i):=(RegNext(io.iptr_A))
            }
            is(decimate) {
                io.Z(i):=decimator.io.Z
            }
            is(detect){
                //io.Z(i):=ofdm_sync.io.out.bits
            }
        }
    }
    io.sync_config.packetDetect:=DontCare
}


//This gives you verilog
object f2_dsp_tapein1 extends App {
  chisel3.Driver.execute(args, () => new f2_dsp_tapein1)
}

