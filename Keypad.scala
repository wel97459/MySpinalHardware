package MySpinalHardware

import spinal.core._
import spinal.lib._
import spinal.core.sim._

import scala.util.control.Breaks

class Keypad extends Component {
    val io = new Bundle {
        val LatchKey = in Bool()
        val Key = in Bits (4 bits)

        val KeyOut = out Bool()

        val KeypadCol = in Bits (4 bits)
        val KeypadRow = out Bits (4 bits)
    }

    val Key = Reg(Bits(4 bits)) init (0)
    when(io.LatchKey) {
        Key := io.Key
    }

    io.KeypadRow(0) := (Key === 0x1 || Key === 0x2 || Key === 0x3 || Key === 0xC)
    io.KeypadRow(1) := (Key === 0x4 || Key === 0x5 || Key === 0x6 || Key === 0xD)
    io.KeypadRow(2) := (Key === 0x7 || Key === 0x8 || Key === 0x9 || Key === 0xE)
    io.KeypadRow(3) := (Key === 0xA || Key === 0x0 || Key === 0xB || Key === 0xF)

    when(Key === 0x1 || Key === 0x4 || Key === 0x7 || Key === 0xA){
        io.KeyOut := io.KeypadCol(0)
    }elsewhen(Key === 0x2 || Key === 0x5 || Key === 0x8 || Key === 0x0){
        io.KeyOut := io.KeypadCol(1)
    }elsewhen(Key === 0x3 || Key === 0x6 || Key === 0x9 || Key === 0xB) {
        io.KeyOut := io.KeypadCol(2)
    }elsewhen(Key === 0xC || Key === 0xD || Key === 0xE || Key === 0xF) {
        io.KeyOut := io.KeypadCol(3)
    }otherwise(io.KeyOut := False)
}

class KeypadScanner(val ColDepth: Int = 4, val RowDepth: Int = 4, val CycleSpeed: BigInt = 10) extends Component {
    val io = new Bundle {
        val KeypadCol = in Bits (ColDepth bits)
        val KeypadRow = out Bits (RowDepth bits)
        val KeysOut = out Bits(ColDepth * RowDepth bits)
    }

    val timer = Timeout(CycleSpeed)
    val rowCounter = Counter(0, RowDepth-1)

    val columns = Vec(Reg(Bits(ColDepth bit)) init(0), RowDepth.toInt)
    val col = RegNext(io.KeypadCol) init(0)

    io.KeypadRow := (B"1" << rowCounter).resized

    when(timer) {
        timer.clear()
        rowCounter.increment()
        columns(rowCounter.resized) := col
    }

    io.KeysOut := Cat(columns)
}

class KeypadHexDecoder() extends Component{
    val io = new Bundle {
        val KeysIn = in Bits(16 bits)
        val HexOut = out Bits(4 bits)
        val HexOutLast = out Bits(4 bits)
        val Valid = out Bool()
    }

    val bitCounter = Counter(16)
    bitCounter.increment()
    io.Valid := False

    val hexOut = RegNextWhen(bitCounter.value.asBits, io.Valid) init(0)
    val hexOutLast = RegNextWhen(hexOut, io.Valid) init(0)
    val currentKeys = RegNextWhen(io.KeysIn, bitCounter.willOverflow)
    val scan = currentKeys(bitCounter)
    val found = Reg(Bool()) init(False)
    val keyHeld = Reg(Bool()) init(False)

    io.HexOut := hexOut
    io.HexOutLast := hexOutLast

    when(bitCounter.willOverflow){
        when(keyHeld && io.KeysIn === 0){
            keyHeld := False
        }
    }

    when(scan){
        when(!keyHeld){
            io.Valid := True
            keyHeld := True
        }
    }
}

object Keypad_Test {
    def main(args: Array[String]) {
        SimConfig.withWave.compile{
            val dut = new Keypad()
            dut
        }.doSim { dut =>
            //Fork a process to generate the reset and the clock on the dut
            dut.clockDomain.forkStimulus(period = 10)

            dut.clockDomain.waitRisingEdge()

            var c = 0;
            dut.io.Key #= 0xc
            dut.io.KeypadCol #= 0x0

            val loop = new Breaks;
            loop.breakable {
                while (true) {

                    if(c == 10){
                        dut.io.LatchKey #= true
                    } else dut.io.LatchKey #= false

                    if(c == 20){
                        dut.io.KeypadCol #= 0x8
                    }


                    c+=1
                    if(c == 50) loop.break()
                    dut.clockDomain.waitRisingEdge()
                }
            }
        }
    }
}

object KeypadScanner_Test {
    def main(args: Array[String]) {
        SimConfig.withWave.compile{
            val dut = new KeypadScanner()
            dut
        }.doSim { dut =>
            //Fork a process to generate the reset and the clock on the dut
            dut.clockDomain.forkStimulus(period = 10)

            dut.io.KeypadCol #= 0x0
            dut.clockDomain.waitRisingEdge()

            var c = 0;

            val loop = new Breaks;
            loop.breakable {
                while (true) {
                    if(dut.io.KeypadRow.toInt == 0x2){
                        dut.io.KeypadCol #= 0x8
                    }else if(dut.io.KeypadRow.toInt == 0x4){
                        dut.io.KeypadCol #= 0x8
                    }else{
                        dut.io.KeypadCol #= 0x0
                    }

                    c+=1
                    if(c == 500) loop.break()
                    dut.clockDomain.waitRisingEdge()
                }
            }
        }
    }
}

object KeypadDecoder_Test {
    def main(args: Array[String]) {
        SimConfig.withWave.compile{
            val dut = new KeypadHexDecoder()
            dut
        }.doSim { dut =>
            //Fork a process to generate the reset and the clock on the dut
            dut.clockDomain.forkStimulus(period = 10)

            dut.io.KeysIn #= 0x0000
            dut.clockDomain.waitRisingEdge()

            var c = 0;

            val loop = new Breaks;
            loop.breakable {
                while (true) {
                    if(c>=100 && c < 200){
                        dut.io.KeysIn #= 0x0010
                    }else if(c>=200 && c < 300) {
                        dut.io.KeysIn #= 0x0000
                    }else if(c>=300 && c < 500) {
                        dut.io.KeysIn #= 0x8000
                    }
                    c+=1
                    if(c == 500) loop.break()
                    dut.clockDomain.waitRisingEdge()
                }
            }
        }
    }
}