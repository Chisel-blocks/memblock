// See LICENSE for license details.
// Initially written by Marko Kosunen  20180429
package memblock 

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import dsptools._
import dsptools.numbers._
import scala.math._


class memblock[T <:Data] (
  proto            : T,
  memsize          : Int=scala.math.pow(2,13).toInt,
  dualport         : Boolean=true
  ) extends Module {
  val io = IO( new Bundle { 
    val write_en   = Input(Bool())
    val write_addr = Input(UInt(log2Ceil(memsize).W))
    val write_val  = Input(proto)
    val read_addr  = Input(UInt(log2Ceil(memsize).W))
    val read_val   = Output(proto)
    }
  )
     
  val write_val=RegInit(0.U.asTypeOf(proto.cloneType))
  val write_enable=RegInit(0.U.asBool)
  val mem =SyncReadMem(memsize, proto.cloneType)
  val write_addr =RegInit(0.U(log2Ceil(memsize).W))
  val read_addr =RegInit(0.U(log2Ceil(memsize).W))
  val read_val =RegInit(0.U.asTypeOf(proto.cloneType))
  write_addr:=io.write_addr
  write_enable:=io.write_en
  write_val:=io.write_val
  read_addr:=io.read_addr
  // [ ToDo] Create structural alternatives to control whether the operation
  // Aims for single or dual port memory
  
  if(dualport){
    when(write_enable){ 
      mem.write(write_addr,write_val)
    }
    read_val:=mem.read(read_addr)
    io.read_val:=read_val
  } else {
    read_val := DontCare
    val rdwr_port = mem(read_addr)
    when (write_enable) { 
      rdwr_port := write_val 
    }.otherwise {
      read_val := rdwr_port
    }
    io.read_val:=read_val
  }
}

/** This gives you verilog */
object memblock extends App {
    // Getopts parses the "Command line arguments for you"  
    def getopts(options : Map[String,String], 
        arguments: List[String]) : (Map[String,String], List[String]) = {
        //This the help
        val usage = """
            |Usage: memblock.memblock [-<option>]
            |
            | Options
            |     proto            [Str]     : Size of the address space. 
            |                                  Default "DspComplex(UInt(16.W),UInt(16.W))"
            |     memsize          [Int]     : Size of the address space. Default 8192
            |     dualport         [Str]     : Use dualport memories. Default "true"
            |     h                          : This help 
          """.stripMargin
        val optsWithArg: List[String]=List(
            "-memsize",
            "-dualport"
        )
        //Handling of flag-like options to be defined 
        arguments match {
            case "-h" :: tail => {
                println(usage)
                val (newopts, newargs) = getopts(options, tail)
                sys.exit()
                (Map("h"->"") ++ newopts, newargs)
            }
            case option :: value :: tail if optsWithArg contains option => {
               val (newopts, newargs) = getopts(
                   options++Map(option.replace("-","") -> value), tail
               )
               (newopts, newargs)
            }
              case argument :: tail => {
                 val (newopts, newargs) = getopts(options,tail)
                 (newopts, argument.toString +: newargs)
              }
            case Nil => (options, arguments)
        }
    }
     
    // Default options
    val defaultoptions : Map[String,String]=Map(
        "memsize"->"8192",
        "dualport"->"true"
        ) 
    // Parse the options
    val (options,arguments)= getopts(defaultoptions,args.toList)

    val annos = Seq(ChiselGeneratorAnnotation(() => new memblock(
        proto=DspComplex(UInt(16.W),UInt(16.W)), 
        options("memsize").toInt, 
        dualport=options("dualport").toBoolean
    ))) 
    (new ChiselStage).execute(arguments.toArray, annos)
}

