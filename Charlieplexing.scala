package MySpinalHardware

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

case class Charlieplexing() extends Component {
    val io = new Bundle {
        val LEDs = in  Bits(19 bits)
        val Pins = out Bits(5 bits)
        val PinsN = out Bits(5 bits)
        val Test0 = out Bool()
        val Test1 = out Bool()
        val Test2 = out Bool()
    }

    val LEDs = Reg(Bits(19 bits)) init(0)

    io.Pins := B"00000"
    io.PinsN := B"00000"
    io.Test0 := False
    io.Test1 := False
    io.Test2 := False

    val fsm = new StateMachine 
    {
        val Init: State = new State with EntryPoint
        {
            whenIsActive{
                LEDs := 0;
                io.Test0 := True
                goto(N1)
            }
        }

        val N1: State = new State
        {
            whenIsActive{
                LEDs := io.LEDs;
                io.Test1 := True
                io.Pins := io.LEDs(16) ## io.LEDs(4) ## io.LEDs(2) ## io.LEDs(0) ## B"0"
                io.PinsN := io.LEDs(16) ## io.LEDs(4) ## io.LEDs(2) ## io.LEDs(0) ## B"1"
                 goto(N2)
            }
        }
        
        val N2: State = new State
        {
            whenIsActive{
                io.Test2 := True
                io.Pins := LEDs(17) ## LEDs(15) ## LEDs(7) ## B"0" ## LEDs(1)
                io.PinsN := LEDs(17) ## LEDs(15) ## LEDs(7) ## B"1" ## LEDs(1)
                goto(N3)
            }
        }

        val N3: State = new State
        {
            whenIsActive{
                io.Test0 := True
                io.Test1 := True
                io.Pins := LEDs(18) ## LEDs(14) ## B"0" ## LEDs(8) ## LEDs(3)
                io.PinsN := LEDs(18) ## LEDs(14) ## B"1" ## LEDs(8) ## LEDs(3)
                goto(N4)
            }
        }

        val N4: State = new State
        {
            whenIsActive{
                io.Test0 := True
                io.Test2 := True
                io.Pins := B"0" ## B"0" ## LEDs(9) ## LEDs(10) ## LEDs(5)
                io.PinsN := B"0" ## B"1" ## LEDs(9) ## LEDs(10) ## LEDs(5)
                goto(N5)
            }
        }

        val N5: State = new State
        {
            whenIsActive{
                io.Test1 := True
                io.Test2 := True
                io.Pins := B"0" ## LEDs(13) ## LEDs(12) ## LEDs(11) ## LEDs(6)
                io.PinsN := B"1" ## LEDs(13) ## LEDs(12) ## LEDs(11) ## LEDs(6)
                goto(N1)
            }
        }
    }
}
