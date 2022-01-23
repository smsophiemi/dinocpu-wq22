// This file is where all of the CPU components are assembled into the whole CPU

package dinocpu

import chisel3._
import chisel3.util._
import dinocpu.components._

/**
 * The main CPU definition that hooks up all of the other components.
 *
 * For more information, see section 4.4 of Patterson and Hennessy
 * This follows figure 4.21
 */
class SingleCycleCPU(implicit val conf: CPUConfig) extends BaseCPU {
  // All of the structures required
  val pc         = dontTouch(RegInit(0.U))
  val control    = Module(new Control())
  val registers  = Module(new RegisterFile())
  val aluControl = Module(new ALUControl())
  val alu        = Module(new ALU())
  val immGen     = Module(new ImmediateGenerator())
  val nextpc     = Module(new NextPC())
  val (cycleCount, _) = Counter(true.B, 1 << 30)

  // Should be removed when wired are connected
  // control.io    := DontCare
  // registers.io  := DontCare
  // aluControl.io := DontCare
  // alu.io        := DontCare
  immGen.io     := DontCare
  nextpc.io     := DontCare
  io.dmem       := DontCare

  //FETCH
  io.imem.address := pc
  io.imem.valid := true.B

  val instruction = io.imem.instruction

  val address_adder = Module(new Adder())
  address_adder.io.inputx := pc
  address_adder.io.inputy := 4.U
  pc := address_adder.io.result
  

  // Your code goes here
  control.io.opcode := instruction(6,0)

  val wordinst = control.io.wordinst

  registers.io.readreg1 := instruction(19,15)
  registers.io.readreg2 := instruction(24,20)
  registers.io.writereg := instruction(11,7)
  registers.io.wen := Mux(registers.io.writereg === 0.U, 0.U, 1.U)

  val readdata1 = registers.io.readdata1
  val readdata2 = registers.io.readdata2

  aluControl.io.aluop := 0.U
  aluControl.io.itype := 0.U
  aluControl.io.funct7 := instruction(31,25)
  aluControl.io.funct3 := instruction(14,12)
  aluControl.io.wordinst := wordinst

  val operation = aluControl.io.operation

  alu.io.operation := operation
  alu.io.inputx := readdata1
  alu.io.inputy := readdata2

  val result = alu.io.result

  registers.io.writedata := result
}

/*
 * Object to make it easier to print information about the CPU
 */
object SingleCycleCPUInfo {
  def getModules(): List[String] = {
    List(
      "dmem",
      "imem",
      "control",
      "registers",
      "csr",
      "aluControl",
      "alu",
      "immGen",
      "nextpc"
    )
  }
}
