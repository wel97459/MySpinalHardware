package MySpinalHardware

import spinal.core._
import spinal.lib._

case class RGB565toRGB888() extends Component
{
    val io = new Bundle {
        val brightness = in Bits(2 bits)
        val rgb656 = in Bits(16 bits)
        val R8 = out Bits(10 bits)
        val G8 = out Bits(10 bits)
        val B8 = out Bits(10 bits)
    }
    // val R8 = ( B"000" ## io.rgb656(15 downto 11))
    // val G8 = ( B"00" ## io.rgb656(10 downto 5))
    // val B8 = ( B"000" ## io.rgb656(4 downto 0))
    val R8 = (io.rgb656(15 downto 11)  ## io.rgb656(15 downto 13))
    val G8 = (io.rgb656(10 downto 5) ## io.rgb656(10 downto 9))
    val B8 = (io.rgb656(4 downto 0) ## io.rgb656(4 downto 2))
    val R16 = R8.resize(16).asUInt * R8.resize(16).asUInt
    val G16 = G8.resize(16).asUInt * G8.resize(16).asUInt
    val B16 = B8.resize(16).asUInt * B8.resize(16).asUInt
    io.R8 := R16(15 downto 6).asBits >> io.brightness.asUInt
    io.G8 := G16(15 downto 6).asBits >> io.brightness.asUInt
    io.B8 := B16(15 downto 6).asBits >> io.brightness.asUInt
}


case class RGB565toB16() extends Component
{
    val io = new Bundle {
        val R5 = in Bits(5 bits)
        val G6 = in Bits(6 bits)
        val B5 = in Bits(5 bits)
        val rgb656 = out Bits(16 bits)
    }
    val rgb656 = io.R5 ## io.G6 ## io.B5
    io.rgb656 := rgb656

}