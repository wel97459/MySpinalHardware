package MySpinalHardware

import spinal.core._

class SPRAM256KA() extends BlackBox {
    val io = new Bundle {
        val DATAIN = in Bits(16 bits)
        val ADDRESS = in Bits(14 bits)
        val MASKWREN = in Bits(4 bits)
        val WREN = in Bool()
        val CHIPSELECT = in Bool()
        val CLOCK = in Bool()
        val STANDBY = in Bool()
        val SLEEP = in Bool()
        val POWEROFF = in Bool()
        val DATAOUT = out Bits(16 bits)
    }
    noIoPrefix()
    setBlackBoxName("SB_SPRAM256KA")
    // Map the current clock domain to the io.clk pin
    mapClockDomain(clock=io.CLOCK)
}