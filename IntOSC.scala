package MyHardware

import spinal.core._
import spinal.lib._

class IntOSC() extends BlackBox {
    val io = new Bundle {
        val CLKHFPU = in  Bool()
        val CLKHFEN = in Bool()
        val CLKHF   = out Bool()
    }
    noIoPrefix()
    setBlackBoxName("SB_HFOSC")
}
