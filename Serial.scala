package MyHardware

import spinal.core._
import spinal.lib._
import spinal.lib.com.uart._

class uart_rx extends Component {
    val io = new Bundle {
        val buffer_reset = in Bool
        val serial_in = in Bool
        val en_16_x_baud = in Bool
        val buffer_read = in Bool

        val buffer_data_present = out Bool
        val buffer_half_full = out Bool
        val buffer_full = out Bool
        val data_out = out Bits(8 bits)
    }

    val g = UartCtrlGenerics()

    val clockDivider = new Area {
      val counter = Reg(UInt(g.clockDividerWidth bits)) init(0)
      val tick = counter === 0

      counter := counter - 1
      when(tick) {
        counter := (ClockDomain.current.frequency.getValue / 115200 / g.rxSamplePerBit).toInt
      }
    }

    val uartCtrlRx = new UartCtrlRx(g)
    uartCtrlRx.io.configFrame.dataLength := 7  //8 bits
    uartCtrlRx.io.configFrame.parity := UartParityType.NONE
    uartCtrlRx.io.configFrame.stop := UartStopType.ONE
    uartCtrlRx.io.samplingTick := clockDivider.tick
    uartCtrlRx.io.rxd <> io.serial_in

    io.buffer_data_present := uartCtrlRx.io.read.valid
    io.buffer_half_full := False
    io.buffer_full := False
    io.data_out := uartCtrlRx.io.read.payload
}

class uart_tx extends Component {
    val io = new Bundle {
        val buffer_reset = in Bool
        val data_in = in Bits(8 bits)
        val en_16_x_baud = in Bool
        val buffer_write = in Bool

        val serial_out= out Bool
        val buffer_data_present = out Bool
        val buffer_half_full = out Bool
        val buffer_full = out Bool
    }

    val g = UartCtrlGenerics()

    val clockDivider = new Area {
      val counter = Reg(UInt(g.clockDividerWidth bits)) init(0)
      val tick = counter === 0

      counter := counter - 1
      when(tick) {
        counter := (ClockDomain.current.frequency.getValue / 115200 / g.rxSamplePerBit).toInt
      }
    }

    val uartCtrlTx = new UartCtrlTx(g)
    uartCtrlTx.io.configFrame.dataLength := 7  //8 bits
    uartCtrlTx.io.configFrame.parity := UartParityType.NONE
    uartCtrlTx.io.configFrame.stop := UartStopType.ONE
    uartCtrlTx.io.samplingTick := clockDivider.tick
    uartCtrlTx.io.txd <> io.serial_out

    val write = Stream(Bits(8 bits))
    write.valid := io.buffer_write
    write.payload := io.data_in
    write >-> uartCtrlTx.io.write

    io.buffer_data_present := False
    io.buffer_half_full := False
    io.buffer_full := False
}