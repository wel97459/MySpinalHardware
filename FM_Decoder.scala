package MySpinalHardware

package MySpinalHardware

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.core.sim._

import java.io._
import scala.util.control.Breaks

class FM_Decoder extends Component
{
    val io = new Bundle {
        val index_in = in Bool() 
        val data_in = in Bool()
        val clock = out Bool()
        //val mfm = out Bool()
    }

    val data_decode = Reg(Bits(16 bits)) init(0)
    val d0 = data_decode(14 downto 0) ## B"0"
    val d1 = data_decode(14 downto 0) ## B"1"

    val clock_decode = Reg(Bits(16 bits)) init(0)
    val c1 = clock_decode(14 downto 0) ## B"1"
    val c01 = clock_decode(13 downto 0) ## B"01"

    val counter = Counter(127)
    val countLast = Reg(UInt(7 bits)) init(0)

    // val counterBit = Counter(16)
    
    val count2 = counter.value >= 25 && counter.value <= 35
    val count4 = counter.value >= 55 && counter.value <= 65

    val countLast2 = countLast > 25 && countLast < 35
    val countLast4 = countLast > 55 && countLast < 65

    val state = Reg(Bool()) init(False)  
    val state_start = Reg(Bool()) init(False)  

    // when(data_decode === 0xffff && !state_start){
    
    // }

    when(io.data_in.fall()){
        counter.clear()
        countLast := counter.value

        when(count2){
            state := !state
            when(!state) {
                data_decode := d1
            }
            clock_decode := c1
        }elsewhen(count4){
            state := True
            data_decode := d0
            clock_decode := c01
        }
    }otherwise{
        counter.increment()
    }

    io.clock := False
}

object FM_Decoder_Test {
    def main(args: Array[String]) {
        SimConfig.withFstWave.compile{
            val dut = new FM_Decoder()
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