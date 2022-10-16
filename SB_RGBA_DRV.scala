package MySpinalHardware

import spinal.core._
import spinal.lib._

class SB_RGBA_DRV extends BlackBox {
    val io = new Bundle {
        val RGBLEDEN = in Bool()
        val RGB0PWM = in Bool()
        val RGB1PWM = in Bool()
        val RGB2PWM = in Bool()
        val CURREN = in Bool()
        val RGB0 = out Bool()
        val RGB1 = out Bool()
        val RGB2 = out Bool()
    }
    noIoPrefix()
    setBlackBoxName("SB_RGBA_DRV")
}
