package MySpinalHardware

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.core.sim._

import java.io._
import scala.util.control.Breaks

class MFM_Decoder extends Component
{
    val io = new Bundle {
        val index_in = in Bool() 
        val data_in = in Bool()
        val clock = out Bool()
        //val mfm = out Bool()
    }

    val pll = Reg(UInt(8 bits)) init(0)
    val pllLast = Reg(UInt(8 bits)) init(0)
    val pllRst = Reg(UInt(8 bits)) init(0x20)

    when(io.data_in.fall()){
        pllLast := pll
        when(pll >= 0x20 && pllLast >= 0x10){
            pllRst := pll 
        } elsewhen(pll <= 0x10 && pllLast < 0x10){
            pllRst := 0x1f - pll
        } elsewhen(pll > 0x10 && pllLast >= 0x10) {
            pllRst := 0x20 - (pll - 0x10)
        } elsewhen(pll < 0x20 && pllLast < 0x20) {
            //pllRst := 0x10 - pll
        }
    }

    when(pll === 0){
        pll := pllRst
        pllRst := 0x20
    }otherwise{
        pll :=  pll - 1
    }

    io.clock := pll === 0
}

object MFM_Decoder_Test {
    def main(args: Array[String]) {
        SimConfig.withFstWave.compile{
            val dut = new MFM_Decoder()
            dut
        }.doSim { dut =>
            //Fork a process to generate the reset and the clock on the dut
            dut.io.index_in #= true
            dut.io.data_in #= true
            dut.clockDomain.forkStimulus(period = 10)

            val pathToFile = "data/test_floppy.bin"
            val file = new FileInputStream(pathToFile)
            var s = file.read().toInt;

            dut.clockDomain.waitRisingEdge()

            var c = 0;
            val loop = new Breaks;
            loop.breakable {
                while (true) {
                    if((s & 0x10) == 0x10){
                        dut.io.index_in #= true
                    }else{
                        dut.io.index_in #= false
                    }

                    if((s & 0x04) == 0x04){
                        dut.io.data_in #= true
                    }else{
                        dut.io.data_in #= false
                    }


                    s = file.read().toInt
                    if(s == 0) loop.break()

                    dut.clockDomain.waitRisingEdge()
                }
            }
            file.close()
        }
    }
}