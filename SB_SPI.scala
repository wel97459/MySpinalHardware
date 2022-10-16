package MySpinalHardware

import spinal.core._
import spinal.lib._

// https://github.com/damdoy/ice40_ultraplus_examples/blob/master/spi_hw/top.v
// https://github.com/damdoy/ice40_ultraplus_examples/blob/master/flash/spi_master.v
class SB_SPI extends BlackBox {
    val io = new Bundle {
        val SBCLKI = in Bool()
        val SBRWI = in Bool()
        val SBSTBI = in Bool()
        val SBADRI0 = in Bool()
        val SBADRI1 = in Bool()
        val SBADRI2 = in Bool()
        val SBADRI3 = in Bool()
        val SBADRI4 = in Bool()
        val SBADRI5 = in Bool()
        val SBADRI6 = in Bool()
        val SBADRI7 = in Bool()
        val SBDATI0 = in Bool()
        val SBDATI1 = in Bool()
        val SBDATI2 = in Bool()
        val SBDATI3 = in Bool()
        val SBDATI4 = in Bool()
        val SBDATI5 = in Bool()
        val SBDATI6 = in Bool()
        val SBDATI7 = in Bool()
        val MI = in Bool()
        val SI = in Bool()
        val SCKI = in Bool()
        val SCSNI = in Bool()
        val SBDATO0 = out Bool()
        val SBDATO1 = out Bool()
        val SBDATO2 = out Bool()
        val SBDATO3 = out Bool()
        val SBDATO4 = out Bool()
        val SBDATO5 = out Bool()
        val SBDATO6 = out Bool()
        val SBDATO7 = out Bool()
        val SBACKO = out Bool()
        val SPIIRQ = out Bool()
        val SPIWKUP = out Bool()
        val SO = out Bool()
        val SOE = out Bool()
        val MO = out Bool()
        val MOE = out Bool()
        val SCKO = out Bool() //inout in the SB verilog library, but output in the VHDL and PDF libs and seemingly in the HW itself
        val SCKOE = out Bool()
        val MCSNO0 = out Bool()
        val MCSNO1 = out Bool()
        val MCSNO2 = out Bool()
        val MCSNO3 = out Bool()
        val MCSNOE0 = out Bool()
        val MCSNOE1 = out Bool()
        val MCSNOE2 = out Bool()
        val MCSNOE3 = out Bool()
    }
    noIoPrefix()
    setBlackBoxName("SB_SPI")
    mapCurrentClockDomain(io.SBCLKI)
}
