package MySpinalHardware

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.core.sim._

import java.io.FileReader

import scala.util.control.Breaks
import _root_.com.opencsv._

class PS2_Keyboard extends Component 
{
    val io = new Bundle {
        val clock_in = in Bool()
        val data_in = in Bool()
        val data_out = out Bits(8 bits)
        val valid = out Bool()
    }

    val data = Reg(Bits(11 bits)) init(0)
    val dataLatch = Reg(Bits(8 bits)) init(0)
    val bitCount = Reg(UInt(4 bits)) init(0)
    val bitTimeOut = Reg(UInt(16 bits)) init(0)
    val parity = Reg(Bool()) init(False) 
    
    io.valid := False

    when(bitCount > 0){
        bitTimeOut := bitTimeOut + 1
    }

    when(io.clock_in.rise()){
        data := io.data_in ## data(10 downto 1)
        bitCount := bitCount + 1
        bitTimeOut := 0
        when(io.data_in && bitCount <= 0x8){
            parity := !parity
        }
    }

    when(bitCount === 0xB || bitTimeOut > 100){
        when(data(10) && !data(0) && !parity === data(9)){
            dataLatch := data(8 downto 1)
            io.valid := bitTimeOut < 100
        }
        data := 0 
        bitTimeOut := 0
        bitCount := 0
        parity := False
    }
    io.data_out := dataLatch
}
object PS2_Keyboard_Test {
    def main(args: Array[String]) {
        SimConfig.withFstWave.compile{
            val dut = new PS2_Keyboard()
            dut
        }.doSim { dut =>
            //Fork a process to generate the reset and the clock on the dut
            dut.io.clock_in #= true
            dut.io.data_in #= true
            dut.clockDomain.forkStimulus(period = 10)

            val pathToFile = "data/test.csv"

            val reader = new FileReader(pathToFile)
            val csvReader = new CSVReader(reader)

            var row: Array[String] = csvReader.readNext()


            dut.clockDomain.waitRisingEdge()

            var c = 0;
            val loop = new Breaks;
            loop.breakable {
                while (true) {
                    if(row(1) == "1"){
                        dut.io.clock_in #= true
                    }else{
                        dut.io.clock_in #= false
                    }

                    if(row(2) == "1"){
                        dut.io.data_in #= true
                    }else{
                        dut.io.data_in #= false
                    }


                    row = csvReader.readNext()
                    if(row == null) loop.break()

                    dut.clockDomain.waitRisingEdge()
                }
            }
            reader.close()
        }
    }
}