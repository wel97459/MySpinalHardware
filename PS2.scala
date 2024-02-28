package MySpinalHardware

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.core.sim._

import java.io.FileReader

import scala.util.control.Breaks
//import _root_.com.opencsv._
import spinal.lib.misc.Timer

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
        when(data(10) && !data(0) && !parity === data(9)){
            dataLatch := data(8 downto 1)
            data_out.valid := !bitTimeOut.willOverflow && data_out.ready
        }
        data := 0 
        bitTimeOut.clear()
        bitCount := 0
        parity := False
    }
}

class PS2_Keyboard_Decoder(TimeoutAmt: BigInt) extends Component {
    val io = new Bundle {
        val clock_in = in Bool()
        val data_in = in Bool()
        val keyCode = master Stream(Bits(8 bits))
    }

    val ps2Key = new PS2_Keyboard(100)
    ps2Key.io.clock_in := io.clock_in
    ps2Key.io.data_in := io.data_in
    ps2Key.io.output.ready := False

    val keyCode_out = Stream(Bits(8 bits))
    val keyCodeFIFO = StreamFifo(
        dataType = Bits(8 bits),
        depth    = 16
    )

    keyCodeFIFO.io.push << keyCode_out
    keyCodeFIFO.io.pop >> io.keyCode
    keyCode_out.valid := False

    val Shift = Reg(Bool()) init(False)
    val Ctrl = Reg(Bool()) init(False)

    val DecoderAddr = Reg(Bits(9 bits)) init(0)
    val DecoderAddrNext = Ctrl ## Shift ## ps2Key.io.output.payload(6 downto 0);

    val keyDecoderRom = new RamInit("./data/keys.bin", 9)
    keyDecoderRom.io.addra := DecoderAddr
    keyDecoderRom.io.dina := 0x00
    keyDecoderRom.io.ena := True
    keyDecoderRom.io.wea := 0

    keyCode_out.payload := keyDecoderRom.io.douta
    val fsm = new StateMachine {
        val Init: State = new State with EntryPoint {
            whenIsActive {
                goto(WaitForData)
            }
        }

        val WaitForData: State = new State {
            whenIsActive {
                when(ps2Key.io.output.valid){
                    when(ps2Key.io.output.payload === 0xF0){
                        goto(KeyEnd)
                    }elsewhen(ps2Key.io.output.payload === 0xE0){
                        goto(ExtendedKey)
                    }elsewhen(ps2Key.io.output.payload === 0x12 || ps2Key.io.output.payload === 0x59){
                        Shift := True
                    }elsewhen(ps2Key.io.output.payload === 0x14){
                        Ctrl := True
                    }otherwise{
                        DecoderAddr := DecoderAddrNext
                        goto(KeyPressed)
                    }
                    ps2Key.io.output.ready := True
                }
            }
        }

        val KeyPressed: State = new StateDelay(2) {
            whenCompleted {
                when(keyDecoderRom.io.douta.asUInt >= 0x08){
                    keyCode_out.valid := keyCode_out.ready
                }
                goto(WaitForData)
            }
        }

        val ExtendedKey: State = new State {
            whenIsActive {
                when(ps2Key.io.output.valid){
                    when(ps2Key.io.output.payload === 0xF0){
                        goto(KeyEnd)
                    }otherwise{
                        when(ps2Key.io.output.payload === 0x14){
                            Ctrl := True
                            goto(WaitForData)
                        } otherwise {
                            DecoderAddr := DecoderAddrNext
                            goto(ExtendedKeyPressed)
                        }
                    }
                    ps2Key.io.output.ready := True
                }
            }
        }

        val ExtendedKeyPressed: State = new StateDelay(2) {
            whenCompleted {
                when(keyDecoderRom.io.douta.asUInt =/= 0x00){
                    keyCode_out.valid := keyCode_out.ready
                }
                goto(WaitForData)
            }
        }

        val KeyEnd: State = new State {
            whenIsActive {
                when(ps2Key.io.output.valid){
                    when(ps2Key.io.output.payload === 0x12 || ps2Key.io.output.payload === 0x59){
                        Shift := False
                    }elsewhen(ps2Key.io.output.payload === 0x14){
                        Ctrl := False
                    }
                    ps2Key.io.output.ready := True
                    goto(WaitForData)
                }
            }
        }
    }
}

// object PS2_Keyboard_Test {
//     def main(args: Array[String]) {
//         SimConfig.withFstWave.compile{
//             val dut = new PS2_Keyboard_Decoder(100)
//             dut
//         }.doSim { dut =>
//             //Fork a process to generate the reset and the clock on the dut
//             dut.io.clock_in #= true
//             dut.io.data_in #= true
//             dut.clockDomain.forkStimulus(period = 10)

//             val pathToFile = "data/PS2_abcdef.csv"

//             val reader = new FileReader(pathToFile)
//             val csvReader = new CSVReader(reader)

//             var row: Array[String] = csvReader.readNext()

//             dut.clockDomain.waitRisingEdge()

//             var c = 0;
//             val loop = new Breaks;
//             loop.breakable {
//                 while (true) {
//                     if(row(1) == "1"){
//                         dut.io.clock_in #= true
//                     }else{
//                         dut.io.clock_in #= false
//                     }

//                     if(row(2) == "1"){
//                         dut.io.data_in #= true
//                     }else{
//                         dut.io.data_in #= false
//                     }


//                     row = csvReader.readNext()
//                     if(row == null) loop.break()

//                     dut.clockDomain.waitRisingEdge()
//                 }
//             }
//             reader.close()
//         }
//     }
// }