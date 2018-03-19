package AsyncQueue

import $cp.lib.rocketchip
import chisel3._
import freechips.rocketchip.util._

//This is the object to provide verilog
object fifo extends App {
    val width 256
    val depth 128
    val proro=UInt(width.W)
  chisel3.Driver.execute(args, () => new AsyncQueue(proto,depth=depth) )
}

