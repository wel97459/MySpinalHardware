package MySpinalHardware

import spinal.core._
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