// See LICENSE for license details.
//Look at handlebars
//
//Start with a static tb and try to genererate a gnerator for it
package iodatatypes 

import chisel3.experimental._
import chisel3._

//Basic datatype for complex inputs
//Provide complex outputs by flipping this
//I am sure this exists somewhere
class complexIn (n: Int=16) extends Bundle { 
  val real=Input(UInt(n.W))
  val imag=Input(UInt(n.W))
  override def cloneType = (new complexIn(n)).asInstanceOf[this.type]

}

