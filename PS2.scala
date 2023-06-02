package MySpinalHardware

import spinal.core._
import spinal.lib._
import spinal.core.sim._

import java.io.FileReader

import scala.util.control.Breaks
import comx35.Config_ECP5_CLI5v6

class PS2 extends Component
{
    val io = new Bundle()
    {
        val clock_in = in Bool()
        val data_in = in Bool()
        val test_out = out Bits(4 bits)
    }

    val test = CounterFreeRun(16);
    io.test_out := test.value.asBits
}


object PS2_TEST_Verilog extends App {
  Config_ECP5_CLI5v6.spinal.generateVerilog(new PS2())
}

object PS2_Test {
    def main(args: Array[String]) {
        SimConfig.withFstWave.compile{
            val dut = new PS2()
            dut
        }.doSim { dut =>
            //Fork a process to generate the reset and the clock on the dut
            dut.clockDomain.forkStimulus(period = 10)

            dut.clockDomain.waitRisingEdge()

            //val pathToFile = "data/test.csv"

            //val reader = new FileReader(pathToFile)
            //val csvReader = new CSVReader(reader)

            //var row: Array[String] = null //csvReader.readNext()

            dut.io.clock_in #= false
            dut.io.data_in #= false
            var c = 0;
            val loop = new Breaks;
            loop.breakable {
                while (true) {
                    // if(row(1) == "1"){
                    //     dut.io.clock_in #= true
                    // }else{
                    //     dut.io.clock_in #= false
                    // }

                    // if(row(2) == "1"){
                    //     dut.io.data_in #= true
                    // }else{
                    //     dut.io.data_in #= false
                    // }


                    //row = csvReader.readNext()
                    if(true) loop.break()

                    dut.clockDomain.waitRisingEdge()
                }
            }
            //reader.close()
        }
    }
}