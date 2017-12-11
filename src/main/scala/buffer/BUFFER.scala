// See LICENSE for license details.

package buffer

import chisel3._

/**
  * Compute GCD using subtraction method.
  * Subtracts the smaller from the larger until register y is zero.
  * value in register x is then the GCD
  */
class BUFFER extends Module {
  val io = IO(new Bundle {
    val input        = Input(UInt(16.W))
    val output     = Output(UInt(16.W))
  })

  val x  = Reg(UInt())
  //val y  = Reg(UInt())

  x := io.input
  io.output := x
}


object BUFFER extends App {
  chisel3.Driver.execute(args, () => new BUFFER)
}

