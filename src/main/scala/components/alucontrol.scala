// This file contains ALU control logic.

package dinocpu.components

import chisel3._
import chisel3.util._

/**
 * The ALU control unit
 *
 * Input:  aluop, 0 for ld/st, 1 for R-type
 * Input:  funct7, the most significant bits of the instruction
 * Input:  funct3, the middle three bits of the instruction (12-14)
 * Input:  wordinst, True if the instruction *only* operates on 32-bit operands, False otherwise
 * Output: operation, What we want the ALU to do.
 *
 * For more information, see Section 4.4 and A.5 of Patterson and Hennessy.
 * This is loosely based on figure 4.12
 */
class ALUControl extends Module {
  val io = IO(new Bundle {
    val aluop     = Input(Bool())
    val itype     = Input(Bool())
    val funct7    = Input(UInt(7.W))
    val funct3    = Input(UInt(3.W))
    val wordinst  = Input(Bool())

    val operation = Output(UInt(5.W))
  })

  // Your code goes here
  when (io.funct7 === "b0000000".U && io.funct3 === "b000".U) { // add*
    when (io.wordinst === true.B) {    // addw
      io.operation := "b10111".U
    } .otherwise {    //add
      io.operation := "b00111".U
    }
  } .elsewhen (io.funct7 === "b0100000".U && io.funct3 === "b000".U) {   // sub*
    when (io.wordinst === true.B) {    // subw
      io.operation := "b10100".U
    } .otherwise {    //sub
      io.operation := "b00100".U
    }
  } .elsewhen (io.funct7 === "b0000000".U && io.funct3 === "b001".U) {   // sll*
    when (io.wordinst === true.B) {    // sllw
      io.operation := "b11000".U
    } .otherwise {    // sll
      io.operation := "b01000".U
    }
  } .elsewhen (io.funct7 === "b0000000".U && io.funct3 === "b010".U) {   // slt
    io.operation := "b01001".U
  } .elsewhen (io.funct7 === "b0000000".U && io.funct3 === "b011".U) {   // sltu
    io.operation := "b00001".U
  } .elsewhen (io.funct7 === "b0000000".U && io.funct3 === "b100".U) {   // xor
    io.operation := "b00000".U
  } .elsewhen (io.funct7 === "b0000000".U && io.funct3 === "b101".U) {   // srl*
    when (io.wordinst === true.B) {    // srlw
      io.operation := "b10010".U
    } .otherwise {    //srl
      io.operation := "b00010".U
    }
  } .elsewhen (io.funct7 === "b0100000".U && io.funct3 === "b101".U) {   // sra*
    when (io.wordinst === true.B) {    // sraw
      io.operation := "b10011".U
    } .otherwise {    // sra
      io.operation := "b00011".U
    }
  } .elsewhen (io.funct7 === "b0000000".U && io.funct3 === "b110".U) {   // or
    io.operation := "b00101".U
  } .elsewhen (io.funct7 === "b0000000".U && io.funct3 === "b111".U) {   // and
    io.operation := "b00110".U
  } .otherwise {
    io.operation := "b11111".U // invalid operation
  }
}
