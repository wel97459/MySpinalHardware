package MySpinalHardware

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import spinal.core.sim._
import scala.util.control.Breaks

class MasterSPI extends Component{
    val io = new Bundle
    {
        val output = slave Stream(Bits(9 bits))
        val output_full = out Bool()
        val input = master Stream(Bits(8 bits))
        val input_full = out Bool()

        val holdOpen = in Bool()
        val sending = out Bool()

        val c = in Bits(16 bits)

        val SPI = new Bundle
        {
            val SCLK = out Bool()
            val CS = out Bool()
            val MOSI = out Bool()
            val MISO = in Bool()
        }
    }

    val spi_data_out = Stream(Bits(9 bits))
    val outFIFO = StreamFifo(
        dataType = Bits(9 bits),
        depth    = 16
    )
    outFIFO.io.push << io.output
    outFIFO.io.pop >> spi_data_out

    val spi_data_in = Stream(Bits(8 bits))
    val inFIFO = StreamFifo(
        dataType = Bits(8 bits),
        depth    = 16
    )
    inFIFO.io.push << spi_data_in
    inFIFO.io.pop >> io.input

    io.output_full := outFIFO.io.availability < 2
    io.input_full := inFIFO.io.availability < 2

    val shiftReg = Reg(Bits(8 bits)) init(0)
    val shiftRegShift = shiftReg(6 downto 0) ## io.SPI.MISO

    val bitCount = Counter(8)
    val dataInFlag = Reg(Bool) init(False)

    val sending = Reg(Bool) init(False)
    io.sending := sending

    val SPI_SCLK = Reg(Bool) init(True)
    val SPI_MOSI = Reg(Bool) init(False)
    val SPI_CS = Reg(Bool) init(True)

    io.SPI.SCLK := SPI_SCLK
    io.SPI.MOSI := SPI_MOSI
    io.SPI.CS := SPI_CS

    spi_data_out.ready := False

    spi_data_in.valid := False
    spi_data_in.payload := shiftRegShift

    val fsm = new StateMachine {
        val Init: State = new State with EntryPoint {
            whenIsActive {
                goto(WaitForData)
            }
        }

        val WaitForData: State = new State {
            whenIsActive {
                when(spi_data_out.valid) {
                    shiftReg := spi_data_out.payload(7 downto 0)
                    dataInFlag := spi_data_out.payload(8)
                    SPI_CS := False
                    bitCount.clear()
                    spi_data_out.ready := True
                    sending := True
                    goto(OutData)
                }otherwise{
                    when(!io.holdOpen){
                        SPI_CS := True
                    }
                    sending := False
                }
            }
        }

        val OutData: State = new State {
            whenIsActive {
                SPI_MOSI := shiftReg(7)
                SPI_SCLK := False
                shiftReg := shiftRegShift
                goto(ShiftData)
            }
        }

        val ShiftData: State = new State {
            when
            whenIsActive {
                bitCount.increment()
                SPI_SCLK := True
                when(bitCount === 7) {
                    when(spi_data_in.ready && dataInFlag){
                        spi_data_in.valid := True
                    }
                    when(spi_data_out.valid && (spi_data_in.ready || !dataInFlag)){
                        shiftReg := spi_data_out.payload(7 downto 0)
                        dataInFlag := spi_data_out.payload(8)
                        bitCount.clear()
                        spi_data_out.ready := True
                        goto(OutData)
                    } otherwise {
                        goto(WaitForData)
                    }
                } otherwise(goto(OutData))
            }
        }
    }
}

case class SlaveSPI() extends Component{
    val io = new Bundle{
        val output = master Stream(Bits(9 bits))
        val output_full = out Bool()
        val input = slave Stream(Bits(8 bits))
        val input_full = out Bool()
        val instruction = out Bool()
        val spi = new Bundle
        {
            val sck = in Bool()
            val ss = in Bool()
            val mosi = in Bool()
            val miso = out Bool()
        }
    }

/***-Defines-***/    

/***-Registers-***/
    val sck = RegNext(RegNext(io.spi.sck))
    val ss = RegNext(RegNext(io.spi.ss))
    val mosi = RegNext(RegNext(io.spi.mosi))

    val dataIn = Reg(Bits(8 bits)) init(0)
    val dataOut = Reg(Bits(8 bits)) init(0)
    val instruction = Reg(Bool()) init(False)

    val bitCounter = Counter(8)
/***-Wires-***/

/***-Streams-***/
    val spi_data_out = Stream(Bits(9 bits))
    val outFIFO = StreamFifo(
        dataType = Bits(9 bits),
        depth    = 32
    )
    outFIFO.io.push << spi_data_out

    val spi_data_in = Stream(Bits(8 bits))
    val inFIFO = StreamFifo(
        dataType = Bits(8 bits),
        depth    = 16
    )
    inFIFO.io.pop >> spi_data_in

    spi_data_out.valid := False
    spi_data_in.ready := False
/***-Blocks-***/

/***-Routing-***/

/***-LutChains-***/
    val dataIn_next = dataIn(6 downto 0) ## mosi
    val dataOut_next = B"0" ## dataIn(7 downto 1) 

/***-IO stuff-***/
    spi_data_out.payload := !instruction ## dataIn_next

    io.output_full := outFIFO.io.availability < 2
    io.input_full := inFIFO.io.availability < 2

    outFIFO.io.pop >> io.output
    inFIFO.io.push << io.input

    io.spi.miso := False //dataOut(7)
    io.instruction := instruction

/***-Logic-***/

    val fsm = new StateMachine {
        val Init: State = new State with EntryPoint {
            whenIsActive {
                goto(WaitForCS)
            }
        }

        val WaitForCS: State = new State {
            whenIsActive {
                when(ss.fall()){
                    instruction := False
                    bitCounter.clear()
                    goto(shiftIn)
                }
            }
        }

        val shiftIn: State = new State {
            whenIsActive {
                when(ss){
                    goto(WaitForCS)
                }elsewhen(sck.fall()){
                    bitCounter.increment()
                }elsewhen(sck.rise()){
                    dataIn := dataIn_next
                    when(bitCounter.willOverflowIfInc){
                        instruction := True
                        spi_data_out.valid := True
                    }
                }
            }
        }
    }
}


object MasterSPI_Test {
    def main(args: Array[String]) {
        SimConfig.withWave.compile{
            val dut = new MasterSPI()
            dut
        }.doSim { dut =>
            //Fork a process to generate the reset and the clock on the dut
            dut.clockDomain.forkStimulus(period = 10)

            dut.clockDomain.waitRisingEdge()

            var c = 0;
            val loop = new Breaks;
            loop.breakable {
                while (true) {
                    dut.io.c #= c
                    dut.io.holdOpen #= true
                    if(c == 2){
                        dut.io.output.payload #= 0xe3
                        dut.io.output.valid #= true
                    }
                    if(c == 3){
                        dut.io.output.valid #= false
                    }
                    if(c == 4){
                        dut.io.output.payload #= 0x101
                        dut.io.output.valid #= true
                    }
                    if(c == 5){
                        dut.io.output.valid #= false
                    }
                    if(c >= 22 && c < 28 || c >= 34 && c < 38){
                        dut.io.SPI.MISO #= true
                    }else{
                        dut.io.SPI.MISO #= false
                    }
                    if(c == 50) loop.break()
                    c+=1
                    dut.clockDomain.waitRisingEdge()
                }
            }
        }
    }
}


object SlaveSPI_Test extends App {
    Config.sim.compile(SlaveSPI()).doSim { dut =>
        //Fork a process to generate the reset and the clock on the dut
        dut.clockDomain.forkStimulus(period = 83)
        var c = 0;
        var cc = 0;
        val b = 0x30;
        val loop = new Breaks;
        dut.io.spi.ss #= true
        loop.breakable {
            while (true) {
                dut.clockDomain.waitRisingEdge()
                if(c > 50){
                    dut.io.spi.ss #= false
                }
                if( c % 4 >= 2 && c > 100){
                    dut.io.spi.sck #= true
                }else{
                    dut.io.spi.sck #= false
                }
                if( c % 4 == 2 && c > 100){

                    dut.io.spi.mosi #= ((b << cc) & 0x80) == 0x80
                    if(cc >= 7){
                        cc=0;
                    }else{
                        cc+=1;
                    }
                }
                c += 1
                if(c > 65536){
                    loop.break;
                }
            }
        }
    }
}