// Lists of different instruction test cases for use with different CPU models

package dinocpu.test

/**
 * This object contains a set of lists of tests. Each list is a different set of
 * instruction types and corresponds to a RISC-V program in resources/risc-v
 *
 * Each test case looks like:
 *  - binary to run in src/test/resources/risc-v
 *  - number of cycles to run for each CPU type
 *  - initial values for registers
 *  - final values to check for registers
 *  - initial values for memory
 *  - final values to check for memory
 *  - extra name information
 */
object InstTests {

  val maxInt = BigInt("FFFFFFFFFFFFFFFF", 16)

  def twoscomp(v: BigInt) : BigInt = {
    if (v < 0) {
      return maxInt + v + 1
    } else {
      return v
    }
  }

    val rtype = List[CPUTestCase](
        CPUTestCase("add1",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> 1234),
                                Map(0 -> 0, 5 -> 1234, 6 -> 1234),
                                Map(), Map()),
        CPUTestCase("add2",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> 1234, 20 -> 5678),
                                Map(0 -> 0, 10 -> 6912),
                                Map(), Map()),
        CPUTestCase("add0",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> 1234, 6 -> 3456),
                                Map(0 -> 0, 5 -> 1234, 6 -> 3456),
                                Map(), Map()),
        CPUTestCase("addw1",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> 1111, 6 -> 2222),
                                Map(0 -> 0, 5 -> 1111, 6 -> 2222, 7 -> 3333),
                                Map(), Map()),
        CPUTestCase("addw2",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> BigInt("00000000ffffffff", 16), 6 -> BigInt("00000000ffffffff", 16)),
                                Map(0 -> 0, 5 -> BigInt("00000000ffffffff", 16), 6 -> BigInt("00000000ffffffff", 16), 7 -> twoscomp(-2)),
                                Map(), Map()),
        CPUTestCase("addw3",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> BigInt("00000000ffffffff", 16), 6 -> 1),
                Map(0 -> 0, 5 -> BigInt("00000000ffffffff", 16), 6 -> 1, 7 -> 0),
                                Map(), Map()),
        CPUTestCase("or",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> 1234, 6 -> 5678),
                                Map(7 -> 5886),
                                Map(), Map()),
        CPUTestCase("sub",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> 1234, 6 -> 5678),
                                Map(7 -> BigInt("FFFFFFFFFFFFEEA4", 16)),
                                Map(), Map()),
        CPUTestCase("subw1",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> 1111, 6 -> 2222),
                                Map(0 -> 0, 5 -> 1111, 6 -> 2222, 7 -> twoscomp(-1111)),
                                Map(), Map()),
        CPUTestCase("subw2",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> BigInt("00000000ffffffff", 16), 6 -> BigInt("00000000ffffffff", 16)),
                                Map(0 -> 0, 5 -> BigInt("00000000ffffffff", 16), 6 -> BigInt("00000000ffffffff", 16), 7 -> 0),
                                Map(), Map()),
        CPUTestCase("subw3",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> BigInt("00000000ffffffff", 16), 6 -> 1),
                Map(0 -> 0, 5 -> BigInt("00000000ffffffff", 16), 6 -> 1, 7 -> twoscomp(-2)),
                                Map(), Map()),
        CPUTestCase("and",
                Map("single-cycle" -> 1,  "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> 1234, 6 -> 5678),
                                Map(7 -> 1026),
                                Map(), Map()),
        CPUTestCase("xor",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(7 -> 1234, 5 -> 5678),
                                Map(5 -> 5678, 7 -> 1234, 6 -> 4860),
                                Map(), Map()),
        CPUTestCase("slt",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(7 -> 1234, 5 -> 5678),
                                Map(5 -> 5678, 7 -> 1234, 6 -> 1),
                                Map(), Map()),
        CPUTestCase("slt1",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(7 -> -1, 5 -> 1),
                                Map(5 -> 1, 6 -> 1),
                                Map(), Map()),
        CPUTestCase("sltu",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(7 -> -1, 5 -> 1),
                                Map(5 -> 1, 6 -> 0),
                                Map(), Map()),
        CPUTestCase("sltu1",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(7 -> 20, 5 -> 100),
                                Map(5 -> 100, 6 -> 1),
                                Map(), Map()),
        CPUTestCase("sll",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(7 -> 32, 5 -> 2),
                                Map(7 -> 32, 5 -> 2, 6 -> 128),
                                Map(), Map()),
        CPUTestCase("sll2",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(7 -> 15, 5 -> 48),
                                Map(7 -> 15, 5 -> 48, 6 -> BigInt("000f000000000000", 16)),
                                Map(), Map()),
        CPUTestCase("sllw1",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(7 -> 32, 5 -> 2),
                                Map(7 -> 32, 5 -> 2, 6 -> 128),
                                Map(), Map()),
        CPUTestCase("sllw2",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(7 -> 32, 5 -> 26),
                                Map(7 -> 32, 5 -> 26, 6 -> BigInt("ffffffff80000000", 16)),
                                Map(), Map()),
        CPUTestCase("sllw3",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(7 -> 32, 5 -> 27),
                                Map(7 -> 32, 5 -> 27, 6 -> 0),
                                Map(), Map()),
        CPUTestCase("sllw4",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(7 -> 32, 5 -> 33),
                                Map(7 -> 32, 5 -> 33, 6 -> 64),
                                Map(), Map()),
        CPUTestCase("srl",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(7 -> 32, 5 -> 2),
                                Map(7 -> 32, 5 -> 2, 6 -> 8),
                                Map(), Map()),
        CPUTestCase("srl2",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(7 -> BigInt("ff00000000000000", 16), 5 -> 260),
                                Map(5 -> 260, 6 -> BigInt("0ff0000000000000", 16)),
                                Map(), Map()),
        CPUTestCase("srlw1",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(7 -> BigInt("00000000f0000000", 16), 5 -> 4),
                                Map(5 -> 4, 6 -> BigInt("000000000f000000", 16)),
                                Map(), Map()),
        CPUTestCase("srlw2",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(7 -> 32, 5 -> 4),
                                Map(5 -> 4, 6 -> 2),
                                Map(), Map()),
        CPUTestCase("srlw3",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(7 -> BigInt("ff00000f0f000000", 16), 5 -> 260),
                                Map(5 -> 260, 6 -> BigInt("0000000000f00000", 16)),
                                Map(), Map()),
        CPUTestCase("srlw4",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(7 -> BigInt("ff00000fff000000", 16), 5 -> 0),
                                Map(5 -> 0, 6 -> BigInt("ffffffffff000000", 16)),
                                Map(), Map()),
        CPUTestCase("sra",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(7 -> twoscomp(-2), 5 -> 31),
                                Map(5 -> 31, 6 -> twoscomp(-1)),
                                Map(), Map()),
        CPUTestCase("sra2",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(7 -> BigInt("ff00000000000000", 16), 5 -> 260),
                                Map(5 -> 260, 6 -> BigInt("fff0000000000000", 16)),
                                Map(), Map()),
        CPUTestCase("sraw1",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(7 -> BigInt("00000000f0000000", 16), 5 -> 4),
                                Map(5 -> 4, 6 -> BigInt("ffffffffff000000", 16)),
                                Map(), Map()),
        CPUTestCase("sraw2",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(7 -> 32, 5 -> 4),
                                Map(5 -> 4, 6 -> 2),
                                Map(), Map()),
        CPUTestCase("sraw3",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(7 -> BigInt("ff000000f0000000", 16), 5 -> 260),
                                Map(5 -> 260, 6 -> BigInt("ffffffffff000000", 16)),
                                Map(), Map()),

  )

    val rtypeMultiCycle = List[CPUTestCase](
        CPUTestCase("addfwd",
                Map("single-cycle" -> 10, "pipelined" -> 14, "pipelined-non-combin" -> 140),
                Map(5 -> 1, 10 -> 0),
                                Map(5 -> 1, 10 -> 10),
                                Map(), Map()),
        CPUTestCase("swapxor",
                Map("single-cycle" -> 3, "pipelined" -> 7, "pipelined-non-combin" -> 70),
                Map(7 -> 5678, 5 -> 1234),
                                Map(5 -> 5678,7->1234),
                                Map(), Map()),
        CPUTestCase("power2",
                Map("single-cycle" -> 3, "pipelined" -> 7, "pipelined-non-combin" -> 70),
                Map(5 -> 512, 6->1, 7->1234),
                                Map(7->1),
                                Map(), Map(), "-512"),
        CPUTestCase("power2",
                Map("single-cycle" -> 3, "pipelined" -> 7, "pipelined-non-combin" -> 70),
                Map(5 -> 1234, 6->1, 7->1234),
                                Map(7->0),
                                Map(), Map(), "-1234"),
        CPUTestCase("power2",
                Map("single-cycle" -> 3, "pipelined" -> 7, "pipelined-non-combin" -> 70),
                Map(5 -> twoscomp(-65536), 6->1, 7->1234),
                                Map(7->0), // This algorithm doesn't work for negative numbers
                                Map(), Map(), "--65536"),
        CPUTestCase("oppsign",
                Map("single-cycle" -> 2, "pipelined" -> 7, "pipelined-non-combin" -> 70),
                Map(5 -> 512, 6->twoscomp(-1024), 7->1234),
                                Map(7->1),
                                Map(), Map(), "-true"),
        CPUTestCase("oppsign",
                Map("single-cycle" -> 2, "pipelined" -> 7, "pipelined-non-combin" -> 70),
                Map(5 -> 512, 6->1024, 7->1234),
                                Map(7->0),
                                Map(), Map(), "-false"),
        CPUTestCase("rotR",
                Map("single-cycle" -> 4, "pipelined" -> 8, "pipelined-non-combin" -> 80),
                Map(5 -> twoscomp(-1), 6->1, 7->1234),
                                Map(7->twoscomp(-1)),
                                Map(), Map())
    )

    val itype = List[CPUTestCase](
        CPUTestCase("addi1",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(),
                                Map(0 -> 0, 10 -> 17),
                                Map(), Map()),
        CPUTestCase("addiw1",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(),
                                Map(10 -> twoscomp(-2047)),
                                Map(), Map()),
        CPUTestCase("addiw2",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> BigInt("00000000ffffffff", 16)),
                                Map(10 -> 1),
                                Map(), Map()),
        CPUTestCase("addiw3",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> BigInt("00000000ffffffff", 16)),
                                Map(10 -> twoscomp(-2)),
                                Map(), Map()),
        CPUTestCase("slli",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> 1),
                                Map(0 -> 0, 5 -> 1, 6 -> 128),
                                Map(), Map()),
		CPUTestCase("slli2",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> 15),
                                Map(0 -> 0, 5 -> 15, 6 -> BigInt("000f000000000000", 16)),
                                Map(), Map()),
        CPUTestCase("slliw1",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> 1),
                                Map(0 -> 0, 5 -> 1, 6 -> 128),
                                Map(), Map()),
        CPUTestCase("slliw2",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> 1024),
                                Map(0 -> 0, 5 -> 1024, 6 -> BigInt("0000000000000000", 16)),
                                Map(), Map()),
        CPUTestCase("slliw3",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> 1),
                                Map(0 -> 0, 5 -> 1, 6 -> BigInt("ffffffff80000000", 16)),
                                Map(), Map()),
        CPUTestCase("srai",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> 1024),
                                Map(0 -> 0, 5 -> 1024, 6 -> 8),
                                Map(), Map()),
        CPUTestCase("srai",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> twoscomp(-1024)),
                                Map(0 -> 0, 5 -> twoscomp(-1024), 6 -> twoscomp(-8)),
                                Map(), Map(), "-negative"),
        CPUTestCase("sraiw1",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> 1024),
                                Map(0 -> 0, 5 -> 1024, 6 -> 8),
                                Map(), Map()),
        CPUTestCase("sraiw2",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> BigInt("fffffffff0000000", 16)),
                                Map(0 -> 0, 5 -> BigInt("fffffffff0000000", 16), 6 -> BigInt("ffffffffffe00000", 16)),
                                Map(), Map()),
        CPUTestCase("srli",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> 128),
                                Map(0 -> 0, 5 -> 128, 6 -> 1),
                                Map(), Map()),
        CPUTestCase("srli2",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> BigInt("f000000000000000", 16)),
                                Map(0 -> 0, 5 -> BigInt("f000000000000000", 16), 6 -> BigInt("0f00000000000000", 16)),
                                Map(), Map()),
        CPUTestCase("srliw1",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> 128),
                                Map(0 -> 0, 5 -> 128, 6 -> 1),
                                Map(), Map()),
        CPUTestCase("srliw2",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> BigInt("f0000000000000f0", 16)),
                                Map(0 -> 0, 5 -> BigInt("f0000000000000f0", 16), 6 -> BigInt("000000000000000f", 16)),
                                Map(), Map()),
        CPUTestCase("srliw3",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> BigInt("f0000000f0000000", 16)),
                                Map(0 -> 0, 5 -> BigInt("f0000000f0000000", 16), 6 -> BigInt("fffffffff0000000", 16)),
                                Map(), Map()),
        CPUTestCase("andi",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> 456),
                                Map(0 -> 0, 5 -> 456, 6 -> 200),
                                Map(), Map()),
        CPUTestCase("ori",
                Map("single-cycle" -> 1,  "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> 456),
                                Map(0 -> 0, 5 -> 456, 6 -> 511),
                                Map(), Map()),
        CPUTestCase("xori",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> 456),
                                Map(0 -> 0, 5 -> 456, 6 -> 311),
                                Map(), Map()),
        CPUTestCase("slti",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> twoscomp(-1)),
                                Map(0 -> 0, 5 -> twoscomp(-1),6->1),
                                Map(), Map()),
        CPUTestCase("sltiu",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(5 -> twoscomp(-1)),
                                Map(0 -> 0, 5 -> twoscomp(-1), 6 -> 0),
                                Map(), Map()),
        CPUTestCase("addi-funct7",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(28 -> 1099),
                                Map(0 -> 0, 5 -> 2123),
                                Map(), Map())
  )

    val itypeMultiCycle = List[CPUTestCase](
        CPUTestCase("addi2",
                Map("single-cycle" -> 2, "pipelined" -> 6, "pipelined-non-combin" -> 60),
                Map(),
                                Map(0 -> 0, 10 -> 17, 11 -> 93),
                                Map(), Map())
    )

    val branch = List[CPUTestCase](
        CPUTestCase("beq",
                Map("single-cycle" -> 3, "pipelined" -> 9, "pipelined-non-combin" -> 90),
                Map(5 -> 1234, 6 -> 1, 7 -> 5678, 28 -> 9012),
                                Map(5 -> 0, 6 -> 1, 7 -> 5678, 28 -> 9012),
                                Map(), Map(), "-False"),
        CPUTestCase("beq",
                Map("single-cycle" -> 3, "pipelined" -> 9, "pipelined-non-combin" -> 90),
                Map(5 -> 1234, 6 -> 1, 7 -> 5678, 28 -> 5678),
                                Map(5 -> 1235, 6 -> 1, 7 -> 5678, 28 -> 5678),
                                Map(), Map(), "-True"),
        CPUTestCase("bne",
                Map("single-cycle" -> 3, "pipelined" -> 9, "pipelined-non-combin" -> 90),
                Map(5 -> 1234, 6 -> 1, 7 -> 5678, 28 -> 5678),
                                Map(5 -> 0, 6 -> 1, 7 -> 5678, 28 -> 5678),
                                Map(), Map(), "-False"),
        CPUTestCase("bne",
                Map("single-cycle" -> 3, "pipelined" -> 9, "pipelined-non-combin" -> 90),
                Map(5 -> 1234, 6 -> 1, 7 -> 5678, 28 -> 9012),
                                Map(5 -> 1235, 6 -> 1, 7 -> 5678, 28 -> 9012),
                                Map(), Map(), "-True"),
        CPUTestCase("blt",
                Map("single-cycle" -> 3, "pipelined" -> 9, "pipelined-non-combin" -> 90),
                Map(5 -> 1234, 6 -> 1, 7 -> 9012, 28 -> 5678),
                                Map(5 -> 0, 6 -> 1, 7 -> 9012, 28 -> 5678),
                                Map(), Map(), "-False"),
        CPUTestCase("blt",
                Map("single-cycle" -> 3, "pipelined" -> 9, "pipelined-non-combin" -> 90),
                Map(5 -> 1234, 6 -> 1, 7 -> 5678, 28 -> 5678),
                                Map(5 -> 0, 6 -> 1, 7 -> 5678, 28 -> 5678),
                                Map(), Map(), "-False-equal"),
        CPUTestCase("blt",
                Map("single-cycle" -> 3, "pipelined" -> 9, "pipelined-non-combin" -> 90),
                Map(5 -> 1234, 6 -> 1, 7 -> 5678, 28 -> 9012),
                                Map(5 -> 1235, 6 -> 1, 7 -> 5678, 28 -> 9012),
                                Map(), Map(), "-True"),
        CPUTestCase("blt",
                Map("single-cycle" -> 3, "pipelined" -> 9, "pipelined-non-combin" -> 90),
                Map(5 -> 1234, 6 -> 1, 7 -> 9012, 28 -> twoscomp(-1)),
                                Map(5 -> 0, 6 -> 1, 7 -> 9012, 28 -> twoscomp(-1)),
                                Map(), Map(), "-False-signed"),
        CPUTestCase("blt",
                Map("single-cycle" -> 3, "pipelined" -> 9, "pipelined-non-combin" -> 90),
                Map(5 -> 1234, 6 -> 1, 7 -> twoscomp(-10000), 28 -> twoscomp(-1000)),
                                Map(5 -> 1235, 6 -> 1, 7 -> twoscomp(-10000), 28 -> twoscomp(-1000)),
                                Map(), Map(), "-True-signed"),
        CPUTestCase("bge",
                Map("single-cycle" -> 3, "pipelined" -> 9, "pipelined-non-combin" -> 90),
                Map(5 -> 1234, 6 -> 1, 7 -> 5678, 28 -> 9012),
                                Map(5 -> 0, 6 -> 1, 7 -> 5678, 28 -> 9012),
                                Map(), Map(), "-False"),
        CPUTestCase("bge",
                Map("single-cycle" -> 3, "pipelined" -> 9, "pipelined-non-combin" -> 90),
                Map(5 -> 1234, 6 -> 1, 7 -> 9012, 28 -> 5678),
                                Map(5 -> 1235, 6 -> 1, 7 -> 9012, 28 -> 5678),
                                Map(), Map(), "-True"),
        CPUTestCase("bge",
                Map("single-cycle" -> 3, "pipelined" -> 9, "pipelined-non-combin" -> 90),
                Map(5 -> 1234, 6 -> 1, 7 -> twoscomp(-1), 28 -> 9012),
                                Map(5 -> 0, 6 -> 1, 7 -> twoscomp(-1), 28 -> 9012),
                                Map(), Map(), "-False-signed"),
        CPUTestCase("bge",
                Map("single-cycle" -> 3, "pipelined" -> 9, "pipelined-non-combin" -> 90),
                Map(5 -> 1234, 6 -> 1, 7 -> twoscomp(-1000), 28 -> twoscomp(-10000)),
                                Map(5 -> 1235, 6 -> 1, 7 -> twoscomp(-1000), 28 -> twoscomp(-10000)),
                                Map(), Map(), "-True-signed"),
        CPUTestCase("bge",
                Map("single-cycle" -> 3, "pipelined" -> 9, "pipelined-non-combin" -> 90),
                Map(5 -> 1234, 6 -> 1, 7 -> 5678, 28 -> 5678),
                                Map(5 -> 1235, 6 -> 1, 7 -> 5678, 28 -> 5678),
                                Map(), Map(), "-True-equal"),
        CPUTestCase("bltu",
                Map("single-cycle" -> 3, "pipelined" -> 9, "pipelined-non-combin" -> 90),
                Map(5 -> 1234, 6 -> 1, 7 -> maxInt, 28 -> 5678),
                                Map(5 -> 0, 6 -> 1, 7 -> maxInt, 28 -> 5678),
                                Map(), Map(), "-False"),
        CPUTestCase("bltu",
                Map("single-cycle" -> 3, "pipelined" -> 9, "pipelined-non-combin" -> 90),
                Map(5 -> 1234, 6 -> 1, 7 -> 5678, 28 -> maxInt),
                                Map(5 -> 1235, 6 -> 1, 7 -> 5678, 28 -> maxInt),
                                Map(), Map(), "-True"),
        CPUTestCase("bgeu",
                Map("single-cycle" -> 3, "pipelined" -> 9, "pipelined-non-combin" -> 90),
                Map(5 -> 1234, 6 -> 1, 7 -> 5678, 28 -> maxInt),
                                Map(5 -> 0, 6 -> 1, 7 -> 5678, 28 -> maxInt),
                                Map(), Map(), "-False"),
        CPUTestCase("bgeu",
                Map("single-cycle" -> 3, "pipelined" -> 9, "pipelined-non-combin" -> 90),
                Map(5 -> 1234, 6 -> 1, 7 -> maxInt, 28 -> 5678),
                                Map(5 -> 1235, 6 -> 1, 7 -> maxInt, 28 -> 5678),
                                Map(), Map(), "-True")
    )

    val branchMultiCycle = List[CPUTestCase](
        CPUTestCase("beqfwd1",
            Map("single-cycle" -> 5, "pipelined" -> 12, "pipelined-non-combin" -> 120),
            Map(5 -> 1234, 6 -> 1, 28 -> 10, 29 -> 5678),
            Map(5 -> 1235, 6 -> 1, 7 -> 5678, 28 -> 5678, 29 -> 5678),
            Map(), Map()),
        CPUTestCase("beqfwd2",
            Map("single-cycle" -> 5, "pipelined" -> 12, "pipelined-non-combin" -> 120),
            Map(5 -> 1234, 6 -> 1, 28 -> 10, 29 -> 5678),
            Map(5 -> 1235, 6 -> 1, 7 -> 5678, 28 -> 5678, 29 -> 5678),
            Map(), Map())
    )

    val memory = List[CPUTestCase](
        CPUTestCase("lw1",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(),
                                Map(5 -> BigInt("ffffffffffffffff", 16)),
                                Map(), Map()),
        CPUTestCase("lb",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(),
                                Map(5 -> BigInt("04", 16)),
                                Map(), Map()),
        CPUTestCase("lh",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(),
                                Map(5 -> BigInt("0304", 16)),
                                Map(), Map()),
        CPUTestCase("lbu",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(),
                                Map(5 -> BigInt("f4", 16)),
                                Map(), Map()),
        CPUTestCase("lhu",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(),
                                Map(5 -> BigInt("f3f4", 16)),
                                Map(), Map()),
        CPUTestCase("lb1",
                Map("single-cycle" -> 1,  "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(),
                                Map(5 -> BigInt("fffffffffffffff4", 16)),
                                Map(), Map()),
        CPUTestCase("lh1",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(),
                                Map(5 -> BigInt("fffffffffffff3f4", 16)),
                                Map(), Map()),
        CPUTestCase("lwu",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(),
                                Map(5 -> BigInt("05060708", 16)),
                                Map(), Map()),
        CPUTestCase("lwu1",
                Map("single-cycle" -> 2, "pipelined" -> 10, "pipelined-non-combin" -> 50),
                Map(),
                                Map(6 -> BigInt("00000000f1f2f3f4", 16)),
                                Map(), Map()),
        CPUTestCase("ld1",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(),
                                Map(5 -> BigInt("f1f2f3f4f5f6f7f8", 16)),
                                Map(), Map()),
        CPUTestCase("ld2",
                Map("single-cycle" -> 2, "pipelined" -> 10, "pipelined-non-combin" -> 50),
                Map(),
                                Map(5 -> BigInt("f1f2f3f4f5f6f7f8", 16)),
                                Map(), Map()),
        CPUTestCase("sw",
                Map("single-cycle" -> 6, "pipelined" -> 10, "pipelined-non-combin" -> 100),
                Map(5 -> 1234),
                                Map(6 -> 1234),
                                Map(), Map(0x100 -> 1234)),
        CPUTestCase("sw2",
                Map("single-cycle" -> 6, "pipelined" -> 10, "pipelined-non-combin" -> 100),
                Map(5 -> twoscomp(-123)),
                                Map(6 -> BigInt("01020304ffffff85", 16)),
                                Map(), Map(0x100 -> 4294967173L, 0x101 -> 0x01020304)),
        CPUTestCase("sb",
                Map("single-cycle" -> 6, "pipelined" -> 10, "pipelined-non-combin" -> 100),
                Map(5 -> 1),
                                Map(6 -> 1),
                                Map(), Map(0x100 -> BigInt("ffffff01", 16))),
        CPUTestCase("sh",
                Map("single-cycle" -> 6, "pipelined" -> 10, "pipelined-non-combin" -> 100),
                Map(5 -> 1),
                                Map(6 -> 1),
                                Map(), Map(0x100 -> BigInt("ffff0001", 16))),
        CPUTestCase("sd1",
                Map("single-cycle" -> 20, "pipelined" -> 30, "pipelined-non-combin" -> 100),
                Map(5 -> BigInt("0102030405060708", 16)),
                                Map(6 -> BigInt("0102030405060708" , 16), 7 -> BigInt("ffffffffffffffff", 16)),
                                Map(), Map()),
        CPUTestCase("sd2",
                Map("single-cycle" -> 10, "pipelined" -> 20, "pipelined-non-combin" -> 100),
                                Map(5 -> 0x100),
                                Map(6 -> 0x110),
                                Map(), Map()),
    )

    val memoryMultiCycle = List[CPUTestCase](
        CPUTestCase("lwfwd",
                Map("single-cycle" -> 2, "pipelined" -> 7, "pipelined-non-combin" -> 70),
                Map(5 -> BigInt("ffffffff", 16), 10 -> 5),
                                Map(5 -> 1, 10 -> 6),
                                Map(), Map()),
        CPUTestCase("swfwd1",
            Map("single-cycle" -> 7, "pipelined" -> 11, "pipelined-non-combin" -> 110),
            Map(5 -> 0, 7->1234),
            Map(5 -> 1234, 6 -> 1234, 7->1234),
            Map(), Map(0x100 -> 1234)),
        CPUTestCase("swfwd2",
            Map("single-cycle" -> 8, "pipelined" -> 12, "pipelined-non-combin" -> 120),
            Map(5 -> 0, 7->1234),
            Map(5 -> 1234, 6 -> 1234, 7->1234),
            Map(), Map(0x100 -> 1234))
    )

  val utype = List[CPUTestCase](
        CPUTestCase("auipc0",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(10 -> 1234),
                                Map(10 -> 0),
                                Map(), Map()),
        CPUTestCase("auipc2",
                Map("single-cycle" -> 2, "pipelined" -> 6, "pipelined-non-combin" -> 60),
                Map(10 -> 1234),
                                Map(10 -> (17 << 12)),
                                Map(), Map()),
        CPUTestCase("lui0",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(10 -> 1234),
                                Map(10 -> 0),
                                Map(), Map()),
        CPUTestCase("lui1",
                Map("single-cycle" -> 1, "pipelined" -> 5, "pipelined-non-combin" -> 50),
                Map(10 -> 1234),
                                Map(10 -> 4096),
                                Map(), Map())
  )

    val utypeMultiCycle = List[CPUTestCase](
        CPUTestCase("auipc1",
            Map("single-cycle" -> 2, "pipelined" -> 6, "pipelined-non-combin" -> 60),
            Map(10 -> 1234),
            Map(10 -> 4),
            Map(), Map()),
        CPUTestCase("auipc3",
            Map("single-cycle" -> 2, "pipelined" -> 6, "pipelined-non-combin" -> 60),
            Map(10 -> 1234),
            Map(10 -> ((17 << 12) + 4)),
            Map(), Map())
    )

  val jump = List[CPUTestCase](
    CPUTestCase("jal",
                Map("single-cycle" -> 2, "pipelined" -> 9, "pipelined-non-combin" -> 90),
                Map(5 -> 1234),
                                Map(0 -> 0, 5 -> 1234, 6 -> 1234, 1 -> 4),
                                Map(), Map()),
    CPUTestCase("jalr0",
                Map("single-cycle" -> 2, "pipelined" -> 9, "pipelined-non-combin" -> 90),
                Map(5 -> 1234, 10 -> 28),
                                Map(0 -> 0, 5 -> 1234, 6 -> 1234, 1 -> 4),
                                Map(), Map()),
    CPUTestCase("jalr1",
                Map("single-cycle" -> 2, "pipelined" -> 9, "pipelined-non-combin" -> 90),
                Map(5 -> 1234, 10 -> 20),
                                Map(0 -> 0, 5 -> 1234, 6 -> 1234, 1 -> 4),
                                Map(), Map())
  )

    val smallApplications = List[CPUTestCase](
        CPUTestCase("fibonacci",
                  Map("single-cycle" -> 300, "pipelined" -> 1000, "pipelined-non-combin" -> 10000),
                  Map(6->11),
                                Map(6->11,5->89),
                                Map(), Map()),
    CPUTestCase("naturalsum",
                   Map("single-cycle" -> 200, "pipelined" -> 500, "pipelined-non-combin" -> 5000),
                Map(),
                                Map(5->55),
                                Map(), Map()),
    CPUTestCase("multiplier",
                                Map("single-cycle" -> 1000, "pipelined" -> 1000, "pipelined-non-combin" -> 10000),
                                Map(5->23,6->20,8->0x1000),
                                Map(5->23*20),
                                Map(), Map()),
    CPUTestCase("divider",
                Map("single-cycle" -> 1000, "pipelined" -> 2000, "pipelined-non-combin" -> 20000),
                Map(5->1260,6->30),
                                Map(7->42),
                                Map(), Map())
    )

    val fullApplications =  List[CPUTestCase](
        CPUTestCase("multiply.riscv",
                                Map("single-cycle" -> 42342, "pipelined" -> 100000, "pipelined-non-combin" -> 1000000),
                                Map(),
                                Map(10->12345678),
                                Map(), Map()),
        CPUTestCase("median.riscv",
                                Map("single-cycle" -> 9433, "pipelined" -> 100000, "pipelined-non-combin" -> 1000000),
                                Map(),
                                Map(10->12345678),
                                Map(), Map()),
        CPUTestCase("qsort.riscv",
                                Map("single-cycle" -> 263290, "pipelined" -> 300000, "pipelined-non-combin" -> 3000000),
                                Map(),
                                Map(10->12345678),
                                Map(), Map()),
        CPUTestCase("rsort.riscv",
                                Map("single-cycle" -> 263290, "pipelined" -> 250000, "pipelined-non-combin" -> 3500000),
                                Map(),
                                Map(10->12345678),
                                Map(), Map()),
        CPUTestCase("towers.riscv",
                                Map("single-cycle" -> 12653, "pipelined" -> 100000, "pipelined-non-combin" -> 1000000),
                                Map(),
                                Map(10->12345678),
                                Map(), Map()),
        CPUTestCase("vvadd.riscv",
                                Map("single-cycle" -> 5484, "pipelined" -> 20000, "pipelined-non-combin" -> 2000000),
                                Map(),
                                Map(10->12345678),
                                Map(), Map())
    )

  // Mapping from group name to list of tests
  val tests = Map(
    "rtype" -> rtype,
    "rtypeMultiCycle" -> rtypeMultiCycle,
        "itype" -> itype,
        "itypeMultiCycle" -> itypeMultiCycle,
    "branch" -> branch,
        "branchMultiCycle" -> branchMultiCycle,
    "memory" -> memory,
        "memoryMultiCycle" -> memoryMultiCycle,
    "utype" -> utype,
        "utypeMultiCycle" -> utypeMultiCycle,
    "jump" -> jump,
        "smallApplications" -> smallApplications
  )

    // All of the tests
    val allTests = rtype ++ rtypeMultiCycle ++ itype ++ itypeMultiCycle ++ branch ++ branchMultiCycle ++
                                 memory ++ memoryMultiCycle ++ utype ++ utypeMultiCycle ++ jump ++ smallApplications

    // Mapping from full name of test to test
    val nameMap = (allTests ++ fullApplications).map(x => x.name() -> x).toMap
}

