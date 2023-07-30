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

    val data_decode = Reg(Bits(8 bits)) init(0)
    val d0 = data_decode(6 downto 0) ## B"0"
    val d1 = data_decode(6 downto 0) ## B"1"

    val counter = Counter(127)
    val countLast = Reg(UInt(7 bits)) init(0)

    val countBits = Counter(8)
    val countBytes = Counter(1024)
    
    val count2 = counter.value >= 25 && counter.value <= 35
    val count4 = counter.value >= 55 && counter.value <= 65

    val countLast2 = countLast > 25 && countLast < 35
    val countLast4 = countLast > 55 && countLast < 65

    val state_read = Reg(Bool()) init(False)  

    val markIndex = False
    val markAddress = False
    val markFF = data_decode === 0xff
    val dataFF00 = !(data_decode === 0x00 || data_decode === 0xff) && countBytes.value =/= 0

    val bitTick = False
    val bitTickDelay = RegNext(bitTick) init(False)
    when(io.data_in.fall()){
        counter.clear()
        countLast := counter.value
        when(count2){
            state_read := !state_read
            when(!state_read) {
                data_decode := d1
                bitTick := True
            }
        }elsewhen(count4){
            state_read := True
            data_decode := d0
            bitTick := True
        }

    }otherwise{
        counter.increment()
    }

    val fsm = new StateMachine
	{
        val Wait: State = new State with EntryPoint 
        {
           whenIsActive
           {
                when(markFF.rise() && bitTickDelay){
                    countBits.clear()
                    countBytes.clear()
                    goto(Count40Index)
                }
           }
        }
        val Count40Index: State = new State
        {
            whenIsActive
            {
                when(bitTickDelay){
                    countBits.increment()
                    when(countBits.willOverflow){
                        countBytes.increment()
                    }

                    when(countBytes.value === 39 && data_decode === 0x00)
                    {
                        countBytes.clear()
                        goto(Count6Index)
                    }elsewhen(countBytes.value > 39 || countBytes.value < 39 && dataFF00){
                        goto(Wait)
                    }
                }
            }
        }
        val Count6Index: State = new State
        {
            whenIsActive
            {
               when(bitTickDelay){
                    countBits.increment()
                    when(countBits.willOverflow){
                        countBytes.increment()
                    }
                    when(countBytes.value === 5 && data_decode === 0xA9)
                    {
                        markIndex := True
                        countBytes.clear()
                        goto(Count26ID)
                    }elsewhen(countBytes.value > 5 || countBytes.value < 5 && dataFF00){
                        goto(Wait)
                    }
                }
            }
        }
        
        val Count26ID: State = new State
        {
            whenIsActive
            {
               when(bitTickDelay){
                    countBits.increment()
                    when(countBits.willOverflow){
                        countBytes.increment()
                    }
                    when(countBytes.value === 25 && data_decode === 0xFE)
                    {
                        markAddress := True
                        countBytes.clear()
                        goto(Header)
                    }elsewhen(countBytes.value > 25 || countBytes.value < 25 && dataFF00){
                        goto(Wait)
                    }
                }
            }
        }
        val Header: State = new State
        {
            whenIsActive{
               when(bitTickDelay){
                    countBits.increment()
                    when(countBits.willOverflow){
                        countBytes.increment()
                    }
                    when(countBytes > 201){
                        goto(Wait)
                    }
                }
            }
        }
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