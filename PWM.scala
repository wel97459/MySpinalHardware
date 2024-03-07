package MySpinalHardware

import spinal.core._
import spinal.core.sim._
import spinal.lib._

class PWM(val CounterWidth: Int = 8 ) extends ImplicitArea[Bool] {
    val counter = Reg(UInt(CounterWidth bits)) init(0)
    val PWMValue = Reg(UInt(CounterWidth bits)) init(0)

    def setValue(value: UInt): Unit = {
        PWMValue := value
    }

    counter := counter + 1;

    override def implicitValue: Bool = (counter <= PWMValue)
}

case class PWMtest() extends Component {
    val io = new Bundle {
        val led = out Bool()
    }

    val pwm = new PWM()
    pwm.setValue(128)

    io.led := pwm
}

object ConfigPWM {
  def spinal = SpinalConfig(
    targetDirectory = "hw/gen",
    defaultConfigForClockDomains = ClockDomainConfig(
      resetActiveLevel = HIGH
    ),
    //onlyStdLogicVectorAtTopLevelIo = true
  )

  def sim = SimConfig.withConfig(spinal).withFstWave
}

object MyTopLevelSim extends App {
  ConfigPWM.sim.compile(PWMtest()).doSim { dut =>
    // Fork a process to generate the reset and the clock on the dut
    dut.clockDomain.forkStimulus(period = 10)

    var modelState = 0
    for (idx <- 0 to 1024) {
      // Drive the dut inputs with random values

      // Wait a rising edge on the clock
      dut.clockDomain.waitRisingEdge()
    }
  }
}
