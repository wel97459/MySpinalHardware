package MySpinalHardware

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.core.sim._

import java.io.FileReader

import scala.util.control.Breaks
import _root_.com.opencsv._

class PS2_Keyboard(Timeout: BigInt) extends Component 
{
    val io = new Bundle {
        val clock_in = in Bool()
        val data_in = in Bool()
        val output = master Stream(Bits(8 bits))
    }

    val data_out = Stream(Bits(8 bits))
    val outFIFO = StreamFifo(
        dataType = Bits(8 bits),
        depth    = 16
    )

    val data = Reg(Bits(11 bits)) init(0)
    val dataLatch = Reg(Bits(8 bits)) init(0)
    val bitCount = Reg(UInt(4 bits)) init(0)
    val bitTimeOut = Counter(Timeout+10)
    val parity = Reg(Bool()) init(False) 
    
    outFIFO.io.push << data_out
    outFIFO.io.pop >> io.output
    data_out.payload := data(8 downto 1)
    data_out.valid := False

    when(bitCount > 0){
        bitTimeOut.increment()
    }

    when(io.clock_in.rise()){
        data := io.data_in ## data(10 downto 1)
        bitCount := bitCount + 1
        bitTimeOut.clear()
        when(io.data_in && bitCount <= 0x8){
            parity := !parity
        }
    }

    when(bitCount === 0xB || bitTimeOut.willOverflow){
        when(data(10) && !data(0) && !parity === data(9) && dataLatch =/= data(8 downto 1)){
            dataLatch := data(8 downto 1)
            data_out.valid := !bitTimeOut.willOverflow && data_out.ready
        }
        data := 0 
        bitTimeOut.clear()
        bitCount := 0
        parity := False
    }
}

class PS2_Keyboard_Decoder(Timeout: BigInt) extends Component {
    val io = new Bundle {
        val clock_in = in Bool()
        val data_in = in Bool()
        val keyCode = master Stream(Bits(8 bits))
    }

    val keyCode_out = Stream(Bits(8 bits))
    val keyCodeFIFO = StreamFifo(
        dataType = Bits(8 bits),
        depth    = 16
    )

    keyCodeFIFO.io.push << keyCode_out
    keyCodeFIFO.io.pop >> io.keyCode
    keyCode_out.payload := 0
    keyCode_out.valid := False


    val fsm = new StateMachine {
        val Init: State = new State with EntryPoint {
            whenIsActive {
                goto(WaitForData)
            }
        }

        val WaitForData: State = new State {
            whenIsActive {


            }
        }
    }
}

object PS2_Keyboard_Test {
    def main(args: Array[String]) {
        SimConfig.withFstWave.compile{
            val dut = new PS2_Keyboard(100)
            dut
        }.doSim { dut =>
            //Fork a process to generate the reset and the clock on the dut
            dut.io.clock_in #= true
            dut.io.data_in #= true
            dut.clockDomain.forkStimulus(period = 10)

            val pathToFile = "data/PS2_abcdef.csv"

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