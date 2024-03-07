package MySpinalHardware

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

case class Clock() extends Component {
    val io = new Bundle {
        val PPS = in Bool()

        val IncMinutes = in Bool()
        val IncHours = in Bool()

        val Seconds1 = out Bits(4 bits)
        val Seconds10 = out Bits(4 bits)

        val Minutes1 = out Bits(4 bits)
        val Minutes10 = out Bits(4 bits)

        val Hours1 = out Bits(4 bits)
        val Hours10 = out Bits(4 bits)
    }

    val Seconds1 = Reg(UInt(4 bit)) init(0)
    val Seconds10 = Reg(UInt(4 bit)) init(0)

    val Minutes1 = Reg(UInt(4 bit)) init(0)
    val Minutes10 = Reg(UInt(4 bit)) init(0)
    
    val Hours1 = Reg(UInt(4 bit)) init(0)
    val Hours10 = Reg(UInt(4 bit)) init(0)

    val IncSeconds10 = False
    val IncMinutes1 = False
    val IncMinutes10 = False
    val IncHours1 = False

    io.Seconds1 := Seconds1.asBits
    io.Seconds10 := Seconds10.asBits
    io.Minutes1 := Minutes1.asBits
    io.Minutes10 := Minutes10.asBits
    io.Hours1 := Hours1.asBits
    io.Hours10 := Hours10.asBits

    when(io.PPS.rise){
        when(Seconds1 === 9){
            Seconds1 := 0
            IncSeconds10 := True
        }elsewhen( io.IncHours | io.IncMinutes){
            Seconds1 := 0
        }otherwise{
            Seconds1 := Seconds1 + 1
        }
        
        when(Seconds10 === 5 && IncSeconds10){
            IncMinutes1 := True
            Seconds10 := 0 
        }elsewhen( io.IncHours | io.IncMinutes){
            Seconds10 := 0
        }elsewhen(IncSeconds10){
            Seconds10 := Seconds10 + 1
        }

        when((Minutes1 === 9 && IncMinutes1) | (Minutes1 === 9 && io.IncMinutes)){
            IncMinutes10 := True
            Minutes1 := 0 
        }elsewhen(IncMinutes1 | io.IncMinutes){
            Minutes1 := Minutes1 + 1
        }

        when(Minutes10 === 5 && IncMinutes10){
            when(!io.IncMinutes){
                IncHours1 := True
            }
            Minutes10 := 0 
        }elsewhen(IncMinutes10){
            Minutes10 := Minutes10 + 1
        }

        when((Hours1 === 9 && IncHours1) | (Hours1 === 9 && io.IncHours)){
            Hours1 := 0
            Hours10 := Hours10 + 1
        }elsewhen((Hours1 === 4 && Hours10 === 2 && IncHours1) | (Hours1 === 4 && Hours10 === 2 && io.IncHours)){
            Hours1 := 0
            Hours10 := 0
        }elsewhen(IncHours1 | io.IncHours){
            Hours1 := Hours1 + 1
        }
    }

}
