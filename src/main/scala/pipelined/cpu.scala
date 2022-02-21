// This file is where all of the CPU components are assembled into the whole CPU

package dinocpu.pipelined

import chisel3._
import chisel3.util._
import dinocpu._
import dinocpu.components._

/**
 * The main CPU definition that hooks up all of the other components.
 *
 * For more information, see section 4.6 of Patterson and Hennessy
 * This follows figure 4.49
 */
class PipelinedCPU(implicit val conf: CPUConfig) extends BaseCPU {
  // Everything in the register between IF and ID stages
  class IFIDBundle extends Bundle {
    val instruction = UInt(32.W)
    val pc          = UInt(64.W)
  }

  // Control signals used in EX stage
  class EXControl extends Bundle {
    val itype = Bool()
    val aluop = Bool()
    val src1 = Bool()
    val src2 = UInt(2.W)
    val branch = Bool()
    val jumptype = UInt(2.W)
    val resultselect = Bool()
    val wordinst = Bool()
  }

  // Control signals used in MEM stage
  class MControl extends Bundle {
    val memop = UInt(2.W)
  }

  // Control signals used in WB stage
  class WBControl extends Bundle {
    val toreg = Bool()
    val regwrite = Bool()
  }

  // Data of the the register between ID and EX stages
  class IDEXBundle extends Bundle {
    val pc = UInt(64.W)
    val instruction = UInt(32.W)
    val sextImm = UInt(64.W)

    val readdata1 = UInt(64.W)
    val readdata2 = UInt(64.W)
  }

  // Control block of the IDEX register
  class IDEXControl extends Bundle {
    val ex_ctrl  = new EXControl
    val mem_ctrl = new MControl
    val wb_ctrl  = new WBControl
  }

  // Everything in the register between EX and MEM stages
  class EXMEMBundle extends Bundle {
    val instruction = UInt(32.W)

    val nextpc = UInt(64.W)
    val taken = Bool()

    val result = UInt(64.W)
    val readdata2 = UInt(64.W) // forwarding
  }

  // Control block of the EXMEM register
  class EXMEMControl extends Bundle {
    val mem_ctrl  = new MControl
    val wb_ctrl   = new WBControl
  }

  // Everything in the register between MEM and WB stages
  class MEMWBBundle extends Bundle {
    val instruction = UInt(32.W)
    val readdata = UInt(64.W)
    val result = UInt(64.W)
  }

  // Control block of the MEMWB register
  class MEMWBControl extends Bundle {
    val wb_ctrl = new WBControl
  }

  // All of the structures required
  val pc              = RegInit(0.U(64.W))
  val control         = Module(new Control())
  val registers       = Module(new RegisterFile())
  val aluControl      = Module(new ALUControl())
  val alu             = Module(new ALU())
  val immGen          = Module(new ImmediateGenerator())
  val nextPCmod       = Module(new NextPC())
  val pcPlusFour      = Module(new Adder())
  val forwarding      = Module(new ForwardingUnit())  //pipelined only
  val hazard          = Module(new HazardUnit())      //pipelined only
  val (cycleCount, _) = Counter(true.B, 1 << 30)

  // The four pipeline registers
  val if_id       = Module(new StageReg(new IFIDBundle))

  val id_ex       = Module(new StageReg(new IDEXBundle))
  val id_ex_ctrl  = Module(new StageReg(new IDEXControl))

  val ex_mem      = Module(new StageReg(new EXMEMBundle))
  val ex_mem_ctrl = Module(new StageReg(new EXMEMControl))

  val mem_wb      = Module(new StageReg(new MEMWBBundle))
  // To make the interface of the mem_wb_ctrl register consistent with the other control
  // registers, we create an anonymous Bundle
  val mem_wb_ctrl = Module(new StageReg(new MEMWBControl))

  // Remove when connected
  //control.io    := DontCare
  //registers.io  := DontCare
  //aluControl.io := DontCare
  //alu.io        := DontCare
  //immGen.io     := DontCare
  //nextPCmod.io  := DontCare
  //pcPlusFour.io := DontCare
  //forwarding.io := DontCare
  //hazard.io     := DontCare

  //id_ex.io       := DontCare
  //id_ex_ctrl.io  := DontCare
  //ex_mem.io      := DontCare
  //ex_mem_ctrl.io := DontCare
  //mem_wb.io      := DontCare
  //mem_wb_ctrl.io := DontCare

  // From memory back to fetch. Since we don't decide whether to take a branch or not until the memory stage.
  val next_pc = Wire(UInt(64.W))

  /////////////////////////////////////////////////////////////////////////////
  // FETCH STAGE
  /////////////////////////////////////////////////////////////////////////////

  // Only update the pc if pcstall is false
  pcPlusFour.io.inputx := pc
  pcPlusFour.io.inputy := 4.U
  next_pc := Mux(hazard.io.pcfromtaken, ex_mem.io.data.nextpc, Mux(hazard.io.pcstall, pc, pcPlusFour.io.result))

  // Send the PC to the instruction memory port to get the instruction
  io.imem.address := pc
  io.imem.valid := true.B

  // Fill the IF/ID register
  if_id.io.in.instruction := io.imem.instruction
  if_id.io.in.pc := pc

  // Update during Part III when implementing branches/jump
  if_id.io.valid := Mux(hazard.io.if_id_stall, false.B, true.B)
  if_id.io.flush := Mux(hazard.io.if_id_flush, true.B, false.B)

  /////////////////////////////////////////////////////////////////////////////
  // ID STAGE
  /////////////////////////////////////////////////////////////////////////////

  // Send opcode to control (line 33 in single-cycle/cpu.scala)
  control.io.opcode := if_id.io.data.instruction(6, 0)

  // Grab rs1 and rs2 from the instruction (line 35 in single-cycle/cpu.scala)
  // Send register numbers to the register file
  /*
  registers.io.readreg1 := instruction(19, 15)
  registers.io.readreg2 := instruction(24, 20)
  */
  registers.io.readreg1 := if_id.io.data.instruction(19, 15)
  registers.io.readreg2 := if_id.io.data.instruction(24,20)

  // Send input from this stage to hazard detection unit (Part III and/or Part IV)
  hazard.io.rs1 := if_id.io.data.instruction(19, 15)
  hazard.io.rs2 := if_id.io.data.instruction(24, 20)

  // Send the instruction to the immediate generator (line 45 in single-cycle/cpu.scala)
  /*
  immGen.io.instruction := instruction
  */
  immGen.io.instruction := if_id.io.data.instruction

  // Control block of the IDEX register
  //  - Fill the id_ex register
  id_ex.io.in.pc := if_id.io.data.pc
  id_ex.io.in.instruction := if_id.io.data.instruction
  id_ex.io.in.sextImm := immGen.io.sextImm
  id_ex.io.in.readdata1 := registers.io.readdata1
  id_ex.io.in.readdata2 := registers.io.readdata2

  //  - Set the execution control signals
  id_ex_ctrl.io.in.ex_ctrl.itype := control.io.itype
  id_ex_ctrl.io.in.ex_ctrl.aluop := control.io.aluop
  id_ex_ctrl.io.in.ex_ctrl.src1 := control.io.src1
  id_ex_ctrl.io.in.ex_ctrl.src2 := control.io.src2
  id_ex_ctrl.io.in.ex_ctrl.branch := control.io.branch
  id_ex_ctrl.io.in.ex_ctrl.jumptype := control.io.jumptype
  id_ex_ctrl.io.in.ex_ctrl.resultselect := control.io.resultselect
  id_ex_ctrl.io.in.ex_ctrl.wordinst := control.io.wordinst

  //  - Set the memory control signals
  id_ex_ctrl.io.in.mem_ctrl.memop := control.io.memop
  
  //  - Set the writeback control signals
  id_ex_ctrl.io.in.wb_ctrl.toreg := control.io.toreg
  id_ex_ctrl.io.in.wb_ctrl.regwrite := control.io.regwrite

  // Set the control signals on the id_ex pipeline register (Part III and/or Part IV)
  id_ex.io.valid := true.B
  id_ex.io.flush := Mux(hazard.io.id_ex_flush, true.B, false.B)

  id_ex_ctrl.io.valid := true.B
  id_ex_ctrl.io.flush := Mux(hazard.io.id_ex_flush, true.B, false.B)

  /////////////////////////////////////////////////////////////////////////////
  // EX STAGE
  /////////////////////////////////////////////////////////////////////////////

  // Set the inputs to the hazard detection unit from this stage (SKIP FOR PART I)
  hazard.io.idex_rd :=  id_ex.io.data.instruction(11,7)
  hazard.io.idex_memread := Mux(id_ex_ctrl.io.data.mem_ctrl.memop === 2.U, true.B, false.B)

  // Set the input to the forwarding unit from this stage (SKIP FOR PART I)
  forwarding.io.rs1 := id_ex.io.data.instruction(19,15)
  forwarding.io.rs2 := id_ex.io.data.instruction(24,20)

  // Connect the ALU control wires (line 55 of single-cycle/cpu.scala)
  /*
  aluControl.io.aluop := control.io.aluop
  aluControl.io.itype := control.io.itype
  aluControl.io.funct7 := instruction(31, 25)
  aluControl.io.funct3 := instruction(14, 12)
  aluControl.io.wordinst := control.io.wordinst
  */
  aluControl.io.aluop := id_ex_ctrl.io.data.ex_ctrl.aluop
  aluControl.io.itype := id_ex_ctrl.io.data.ex_ctrl.itype
  aluControl.io.funct7 := id_ex.io.data.instruction(31, 25)
  aluControl.io.funct3 :=  id_ex.io.data.instruction(14, 12)
  aluControl.io.wordinst := id_ex_ctrl.io.data.ex_ctrl.wordinst

  // Connect the NextPC control wires (line 47 of single-cycle/cpu.scala)
  /*
  nextpc.io.branch := control.io.branch
  nextpc.io.jumptype := control.io.jumptype
  */
  nextPCmod.io.branch := id_ex_ctrl.io.data.ex_ctrl.branch
  nextPCmod.io.jumptype := id_ex_ctrl.io.data.ex_ctrl.jumptype

  // Insert the forward inputx mux here (SKIP FOR PART I)
  val readdata_forwardingA = Wire(UInt(64.W))
  val wb_result = Wire(UInt(64.W))
  when(forwarding.io.forwardA === 0.U){
    readdata_forwardingA := id_ex.io.data.readdata1
  }.elsewhen(forwarding.io.forwardA === 1.U){
    readdata_forwardingA := ex_mem.io.data.result
  } .otherwise{
    readdata_forwardingA := wb_result
  }
  
  // Insert the forward inputy mux here (SKIP FOR PART I)
  val readdata_forwardingB = Wire(UInt(64.W))
  when(forwarding.io.forwardB === 0.U){
    readdata_forwardingB := id_ex.io.data.readdata2
  }.elsewhen(forwarding.io.forwardB === 1.U){
    readdata_forwardingB := ex_mem.io.data.result
  } .otherwise {
    readdata_forwardingB := wb_result
  }

  // Input x mux (line 62 of single-cycle/cpu.scala)
  /*
  alu.io.inputx := Mux(control.io.src1, pc, registers.io.readdata1)
  */
  alu.io.inputx := Mux(id_ex_ctrl.io.data.ex_ctrl.src1, id_ex.io.data.pc, readdata_forwardingA)
  
  // Input y mux (line 63 of single-cycle/cpu.scala)
  /*
  alu.io.inputy := MuxCase(0.U, Array((control.io.src2 === 0.U) -> registers.io.readdata2,
                                      (control.io.src2 === 1.U) -> immGen.io.sextImm,
                                      (control.io.src2 === 2.U) -> 4.U))

  */
  alu.io.inputy := MuxCase(0.U, Array((id_ex_ctrl.io.data.ex_ctrl.src2 === 0.U) -> readdata_forwardingB, 
                                      (id_ex_ctrl.io.data.ex_ctrl.src2 === 1.U) -> id_ex.io.data.sextImm, 
                                      (id_ex_ctrl.io.data.ex_ctrl.src2 === 2.U) -> 4.U))

  // Set the ALU operation  (line 61 of single-cycle/cpu.scala)
  /*
  alu.io.operation := aluControl.io.operation
  */
  alu.io.operation := aluControl.io.operation
  // Connect the ALU data wires

  // Connect the NextPC data wires (line 49 of single-cycle/cpu.scala)
  /*
  nextpc.io.inputx := registers.io.readdata1
  nextpc.io.inputy := registers.io.readdata2
  nextpc.io.funct3 := funct3
  nextpc.io.pc := pc
  nextpc.io.imm := immGen.io.sextImm
  */
  nextPCmod.io.inputx := readdata_forwardingA
  nextPCmod.io.inputy := readdata_forwardingB
  nextPCmod.io.funct3 := id_ex.io.data.instruction(14, 12)
  nextPCmod.io.pc:= id_ex.io.data.pc
  nextPCmod.io.imm := id_ex.io.data.sextImm

  // Set the EX/MEM register values
  ex_mem.io.in.instruction := id_ex.io.data.instruction
  
  ex_mem.io.in.nextpc := nextPCmod.io.nextpc
  ex_mem.io.in.taken := nextPCmod.io.taken

  ex_mem.io.in.readdata2 := readdata_forwardingB
  
  // Determine which result to use (the resultselect mux from line 38 of single-cycle/cpu.scala)
  /*
  registers.io.writedata := Mux(control.io.toreg, io.dmem.readdata, 
                              Mux(control.io.resultselect, immGen.io.sextImm, alu.io.result))
  */
  ex_mem.io.in.result := Mux(id_ex_ctrl.io.data.ex_ctrl.resultselect, id_ex.io.data.sextImm, alu.io.result)
  
  /*
  ex_mem.io.in.result := Mux(id_ex_ctrl.io.data.wb_ctrl.toreg, id_ex.io.data.readdata1, 
                          Mux(id_ex_ctrl.io.data.ex_ctrl.resultselect, id_ex.io.data.sextImm, alu.io.result))
*/
  // Set the control signals on the ex_mem pipeline register (Part III and/or Part IV)
  ex_mem.io.valid      := true.B
  ex_mem.io.flush      := Mux(hazard.io.ex_mem_flush, true.B, false.B)

  ex_mem_ctrl.io.valid := true.B
  ex_mem_ctrl.io.flush := Mux(hazard.io.ex_mem_flush, true.B, false.B)

  ex_mem_ctrl.io.in.mem_ctrl.memop :=  id_ex_ctrl.io.data.mem_ctrl.memop
  ex_mem_ctrl.io.in.wb_ctrl.toreg := id_ex_ctrl.io.data.wb_ctrl.toreg
  ex_mem_ctrl.io.in.wb_ctrl.regwrite := id_ex_ctrl.io.data.wb_ctrl.regwrite

  /////////////////////////////////////////////////////////////////////////////
  // MEM STAGE
  /////////////////////////////////////////////////////////////////////////////

  // Set data memory IO (line 67 of single-cycle/cpu.scala)
/*
  io.dmem.address := alu.io.result
  io.dmem.memread := ~control.io.memop(0)
  io.dmem.memwrite := control.io.memop(0)
  io.dmem.valid := control.io.memop(1)
  io.dmem.maskmode := funct3(1, 0)
  io.dmem.sext := ~funct3(2)
  io.dmem.writedata := registers.io.readdata2
  */
  // Data Path
  io.dmem.address := ex_mem.io.data.result
  io.dmem.writedata := ex_mem.io.data.readdata2

  // Control Path
  io.dmem.maskmode := ex_mem.io.data.instruction(13,12)
  when (ex_mem.io.data.instruction(14)) {
    io.dmem.sext := false.B
  } .otherwise {
    io.dmem.sext := true.B
  }
  
  io.dmem.memread := 0.U
  io.dmem.memwrite := 0.U
  io.dmem.valid := false.B
  
  when (ex_mem_ctrl.io.data.mem_ctrl.memop === 2.U) {
    io.dmem.memread := true.B
    io.dmem.valid := true.B
  } .elsewhen(ex_mem_ctrl.io.data.mem_ctrl.memop === 3.U) {
    io.dmem.memwrite := true.B
    io.dmem.valid := true.B
  }
  // Send next_pc back to the fetch stage
  pc := next_pc

  // Send input signals to the hazard detection unit (SKIP FOR PART I)
  hazard.io.exmem_taken := ex_mem.io.data.taken

  // Send input signals to the forwarding unit (SKIP FOR PART I)
  forwarding.io.exmemrd := ex_mem.io.data.instruction(11, 7)
  forwarding.io.exmemrw := ex_mem_ctrl.io.data.wb_ctrl.regwrite

  // Wire the MEM/WB register
  mem_wb.io.in.readdata := io.dmem.readdata
  mem_wb.io.in.result := ex_mem.io.data.result
  mem_wb.io.in.instruction := ex_mem.io.data.instruction

  // Set the control signals on the mem_wb pipeline register
  mem_wb.io.valid := true.B
  mem_wb.io.flush := false.B

  mem_wb_ctrl.io.valid := true.B
  mem_wb_ctrl.io.flush := false.B
  mem_wb_ctrl.io.in.wb_ctrl.toreg :=  ex_mem_ctrl.io.data.wb_ctrl.toreg
  mem_wb_ctrl.io.in.wb_ctrl.regwrite := ex_mem_ctrl.io.data.wb_ctrl.regwrite

  ///////////////////////////////////////////////////////////////////////s//////
  // WB STAGE
  /////////////////////////////////////////////////////////////////////////////

  // Send input signals to the forwarding unit (SKIP FOR PART I)
  forwarding.io.memwbrd := mem_wb.io.data.instruction(11,7)
  forwarding.io.memwbrw := mem_wb_ctrl.io.data.wb_ctrl.regwrite

  // Set the register to be written to
  /*
  registers.io.writereg := instruction(11, 7)
  when (registers.io.writereg =/= 0.U && control.io.regwrite) {
    registers.io.wen := true.B
  } .otherwise {
    registers.io.wen := false.B
  }
  */
  registers.io.writereg := mem_wb.io.data.instruction(11, 7)
  when (registers.io.writereg =/= 0.U && mem_wb_ctrl.io.data.wb_ctrl.regwrite) {
    registers.io.wen := true.B
  } .otherwise {
    registers.io.wen := false.B
  }

  // Set the writeback data mux (line 39 single-cycle/cpu.scala)
  /*
  registers.io.writedata := Mux(control.io.toreg, io.dmem.readdata, 
                            Mux(control.io.resultselect, immGen.io.sextImm, alu.io.result))
  */
  registers.io.writedata := Mux(mem_wb_ctrl.io.data.wb_ctrl.toreg, 
                            mem_wb.io.data.readdata, mem_wb.io.data.result)
  wb_result := Mux(mem_wb_ctrl.io.data.wb_ctrl.toreg, mem_wb.io.data.readdata, mem_wb.io.data.result)
  // Write the data to the register file
}

/*
 * Object to make it easier to print information about the CPU
 */
object PipelinedCPUInfo {
  def getModules(): List[String] = {
    List(
      "imem",
      "dmem",
      "control",
      //"branchCtrl",
      "registers",
      "aluControl",
      "alu",
      "immGen",
      "pcPlusFour",
      //"branchAdd",
      "nextPCmod",
      "forwarding",
      "hazard",
    )
  }
  def getPipelineRegs(): List[String] = {
    List(
      "if_id",
      "id_ex",
      "id_ex_ctrl",
      "ex_mem",
      "ex_mem_ctrl",
      "mem_wb",
      "mem_wb_ctrl"
    )
  }
}