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
      val data_decode = Reg(Bits(32 bits)) init(0)
      val d01 = data_decode(29 downto 0) ## B"01"
      val d001 = data_decode(28 downto 0) ## B"001"
      val d0001 = data_decode(27 downto 0) ## B"0001"
      val d0000001 = data_decode(24 downto 0) ## B"0000001"

      val counter = Counter(127)
      val countLast = Reg(UInt(7 bits)) init(0)
      
      val count2 = counter.value >= 25 && counter.value <= 35
      val count3 = counter.value >= 40 && counter.value <= 50
      val count4 = counter.value >= 57 && counter.value <= 67
      val count6 = counter.value >= 90 && counter.value <= 100

    //   val countLast2 = countLast > 25 && countLast < 35
    //   val countLast3 = countLast > 40 && countLast < 50
    //   val countLast4 = countLast > 57 && countLast < 67

    when(io.data_in.fall()){
        counter.clear()
        countLast := counter.value

        when(count2){
            data_decode := d01
        }elsewhen(count3){
            data_decode := d001
        }elsewhen(count4){
            data_decode := d0001
        }elsewhen(count6){
            data_decode := d0000001
        }
    }otherwise{
        counter.increment()
    }

    io.clock := False
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