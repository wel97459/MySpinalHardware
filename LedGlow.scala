package MySpinalHardware

import spinal.core._
import spinal.lib._

class LedGlow(val depth: Int = 24) extends Component {
    val io = new Bundle {
        val led = out Bool()
        val dark = out Bool()
    }

    val cnt = Reg(UInt(depth bits)) init(0)
    val pwm = Reg(UInt(5 bits)) init(0)

    cnt := cnt + 1

    val pwmInput = UInt(4 bits)

    when (cnt(depth - 1)) {
        pwmInput := cnt(depth - 2 downto depth - 5)
    } otherwise {
        pwmInput := ~cnt(depth - 2 downto depth - 5)
    }

    pwm := pwm(3 downto 0).resize(5) + pwmInput.resize(5)

    io.led := pwm(4)
    io.dark := cnt(depth - 1).rise()
}

//Define a custom SpinalHDL configuration with synchronous reset instead of the default asynchronous one. This configuration can be reused everywhere
object LedGlow_config extends SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC))

object LedGlow {
    def main(args: Array[String]) {
        LedGlow_config.generateVerilog(new LedGlow(24))
    }
}


// val LedColor = Reg(UInt(3 bits)) init(1)
// val LedColorNext = LedColor |<< 1

// val glow = new LedGlow(23)
// rgb_driver.io.RGB0PWM := LedColor(0) ? glow.io.led | False
// rgb_driver.io.RGB1PWM := LedColor(1) ? glow.io.led | False
// rgb_driver.io.RGB2PWM := LedColor(2) ? glow.io.led | False
// when(glow.io.dark){
//     when(LedColorNext =/= 0 && loader.io.ram_data_out === 0xAB){
//         LedColor := LedColorNext
//     }otherwise{
//         LedColor := 1
//     }
// }

// io.led_green := rgb_driver.io.RGB0
// io.led_blue := rgb_driver.io.RGB1
// io.led_red := rgb_driver.io.RGB2