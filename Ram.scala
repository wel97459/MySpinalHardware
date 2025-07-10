package MySpinalHardware

import java.nio.file.{Files, Paths}

import spinal.core._

 /* returns max value for number of bit
 */
object max_bits {
  def apply(value: Int): Int = {
    if (value < 0) SpinalError(s"No negative value ($value) on ${this.getClass.getSimpleName}")
    ((0x1 << value) - 1)
  }
}

class RamInit(File: String, AddrDepth: Int = 8) extends Component {
    val io = new Bundle {
        val ena     = in  Bool()
        val wea     = in  Bits(1 bit)
        val addra   = in Bits(AddrDepth bit)
        val douta   = out Bits(8 bit)
        val dina    = in Bits(8 bit)
    }

    def readBin(file : String): Seq[Bits] = Files.readAllBytes(Paths.get(file)).map(b => B(b.toInt & 0xFF))
    def MemRom(initialContent: Seq[Bits]) = new Mem(Bits(8 bit), initialContent.length) init(initialContent)

    val mem = MemRom(readBin(File))

    mem.write(
        enable = io.ena & io.wea.asBool,
        address = io.addra.asUInt,
        data = io.dina
    )

    io.douta := mem.readSync(
        enable = io.ena,
        address = io.addra.asUInt
    )
}

class Ram(AddrDepth: Int = 8, DataDepth: Int = 8) extends Component {
    val io = new Bundle {
        val ena     = in  Bool()
        val wea     = in  Bool()
        val addra   = in Bits(AddrDepth bit)
        val douta   = out Bits(DataDepth bit)
        val dina    = in Bits(DataDepth bit)
    }

    def MemRom(Depth: Int) = new Mem(Bits(8 bit), max_bits(Depth))

    val mem = MemRom(AddrDepth)

    mem.write(
        enable = io.ena & io.wea,
        address = io.addra.asUInt,
        data = io.dina
    )

    io.douta := mem.readSync(
        enable = io.ena,
        address = io.addra.asUInt
    )
}