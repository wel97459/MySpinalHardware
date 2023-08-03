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
        val flux_in = in Bool()

        val data_out = out Bits(8 bits)
        val byteTick = out Bool()
        val header_start = out Bool()
        val data_start = out Bool()
    }

     
    val counter = Reg(UInt(7 bits)) init(0)
    val countLast = Reg(UInt(7 bits)) init(0)

    val countBits = Counter(16)
    val countBytes = Counter(1024)

    val data_decode = Reg(Bits(16 bits)) init(0)
    val d1 = data_decode(14 downto 0) ## !io.flux_in
    
    val clock = data_decode(14) ## data_decode(12) ## data_decode(10) ## data_decode(8) ## data_decode(6) ## data_decode(4) ## data_decode(2) ## data_decode(0)
    val data = data_decode(15) ## data_decode(13) ## data_decode(11) ## data_decode(9) ## data_decode(7) ## data_decode(5) ## data_decode(3) ## data_decode(1)

    val bitTick = False
    val bitTickDelay = RegNext(bitTick) init(False)

    val byteTick = bitTickDelay && countBits === 15
    val dataByteLatched = RegNextWhen(data, byteTick)

    val addressMark = data_decode(1) || data_decode(3) || data_decode(5) || data_decode(10)

    when(bitTickDelay){
        data_decode := d1
    }

    when(io.flux_in.fall()){
        bitTick := True
        counter := 0
    }otherwise{
        when(counter === 40){
            bitTick := True
        }
        counter := counter + 1
    }

    when(bitTickDelay){
        countBits.increment()

        when(countBits.willOverflow){
            countBytes.increment()
        }
    }

    io.data_out := dataByteLatched
    io.header_start := False
    io.data_start := False
    io.byteTick := byteTick
    val fsm = new StateMachine
	{
        val Wait: State = new State with EntryPoint 
        {
           whenIsActive
           {
                when(addressMark.fall()){
                    countBits := 8
                    countBytes.clear()
                    goto(HeaderCheck)
                }
           }
        }

        val HeaderCheck: State = new State
        {
            whenIsActive{
                
                when(byteTick){
                    when(dataByteLatched === 0xFC && countBytes === 0){
                        io.header_start := True
                        goto(Header)
                    }  otherwise{
                        goto(Wait)
                    }
                }
            }
        }


        val Header: State = new State
        {
            whenIsActive{
                when(byteTick && countBytes === 8 && dataByteLatched === 0xFF){
                    countBytes.clear()
                    goto(WaitDataIndex)
                }elsewhen(byteTick && countBytes > 8) {
                    goto(Wait)
                }
            }
        }


        val WaitDataIndex: State = new State
        {
            whenIsActive{
                when(addressMark.fall()){
                    countBits := 8
                }
                when(byteTick && dataByteLatched === 0x00 && countBytes === 15){
                    countBytes.clear()
                    io.data_start := True
                    goto(Data)
                }elsewhen(byteTick && countBytes > 15){
                    goto(Wait)
                }
            }
        }
        val Data: State = new State
        {
            whenIsActive{
                when(byteTick && dataByteLatched === 0xFF && countBytes === 131){
                    goto(Wait)
                }
            }
        }

    }
}

object FM_Decoder_Test {
    def main(args: Array[String]) {
        SimConfig.withFstWave.compile{
            val dut = new FM_Decoder()
            dut
        }.doSim { dut =>
            //Fork a process to generate the reset and the clock on the dut
            dut.io.index_in #= true
            dut.io.flux_in #= true
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
                        dut.io.flux_in #= true
                    }else{
                        dut.io.flux_in #= false
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