package MyHardware

import spinal.core._
import spinal.lib._

class LedGlow(val depth: Int = 24) extends Component {
    val io = new Bundle {
        val led = out Bool
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
}

//Define a custom SpinalHDL configuration with synchronous reset instead of the default asynchronous one. This configuration can be resued everywhere
object LedGlow_config extends SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC))

object LedGlow {
    def main(args: Array[String]) {
        LedGlow_config.generateVerilog(new LedGlow(24))
    }
}