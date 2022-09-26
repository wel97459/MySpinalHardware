package MyHardware

import java.nio.file.{Files, Paths}

import spinal.core._

class Ram(File: String, AddrDepth: Int = 8) extends Component {
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
