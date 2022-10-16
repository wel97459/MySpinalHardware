package MySpinalHardware

import spinal.core._
import spinal.lib._

class Hex2Value extends Component
{
    val io = new Bundle
    {
        val inHex = in UInt(8 bit)
        val outNum = out UInt(4 bit)
    }

    val value = UInt(4 bit)

    when(io.inHex >= '0' && io.inHex<='9'){
        value := (io.inHex - '0')(3 downto 0)
    } elsewhen(io.inHex >= 'a' && io.inHex <= 'f') {
        value := (io.inHex - 'W')(3 downto 0) //Offset by 10
    } elsewhen(io.inHex>='A' && io.inHex<='F') {
        value := (io.inHex - '7')(3 downto 0) //Offset by 10
    } otherwise {
       value := 0x00
    }
 
    io.outNum := value
}

class Value2Hex extends Component
{
    val io = new Bundle
    {
        val inValue = in UInt(4 bit)
        val outHex = out UInt(8 bit)
    }

    val value = UInt(8 bit)

    when(io.inValue>=10 && io.inValue<=15) {
        value := (io.inValue.resize(8) + '7')
    } otherwise {
       value := (io.inValue.resize(8) + '0')
    }
 
    io.outHex := value
}

class Byte2Hex extends Component
{
    val io = new Bundle
    {
        val inValue = in UInt(8 bit)
        val outHexUpper = out UInt(8 bit)
        val outHexLower = out UInt(8 bit)
    }

    val upper = new Value2Hex
    val lower = new Value2Hex
 
    upper.io.inValue := io.inValue(7 downto 4)
    lower.io.inValue := io.inValue(3 downto 0)

    io.outHexUpper := upper.io.outHex
    io.outHexLower := lower.io.outHex
}
