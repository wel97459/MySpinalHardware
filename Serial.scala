package MySpinalHardware

import spinal.core._
import spinal.lib._
import spinal.lib.com.uart._

class uart_rx extends Component {
    val io = new Bundle {
        val buffer_reset = in Bool()
        val serial_in = in Bool()
        val en_16_x_baud = in Bool()
        val buffer_read = in Bool()

        val buffer_data_present = out Bool()
        val buffer_half_full = out Bool()
        val buffer_full = out Bool()
        val data_out = out Bits(8 bits)
    }

    val g = UartCtrlGenerics()

    val uartCtrlRx = new UartCtrlRx(g)
    uartCtrlRx.io.configFrame.dataLength := 7  //8 bits
    uartCtrlRx.io.configFrame.parity := UartParityType.NONE
    uartCtrlRx.io.configFrame.stop := UartStopType.ONE
    uartCtrlRx.io.samplingTick := io.en_16_x_baud
    uartCtrlRx.io.rxd <> io.serial_in

    val streamOut = Stream(Bits(8 bits))
    val rxFifo = StreamFifo(
      dataType = Bits(8 bits),
      depth    = 16
    )
    rxFifo.io.push << uartCtrlRx.io.read
    rxFifo.io.pop  >> streamOut

    streamOut.ready := io.buffer_read
    io.buffer_data_present := streamOut.valid
    io.data_out := streamOut.payload
    io.buffer_half_full :=rxFifo.io.availability < 8
    io.buffer_full := rxFifo.io.availability < 2
}

class uart_tx extends Component {
    val io = new Bundle {
        val buffer_reset = in Bool()
        val data_in = in Bits(8 bits)
        val en_16_x_baud = in Bool()
        val buffer_write = in Bool()

        val serial_out= out Bool()
        val buffer_half_full = out Bool()
        val buffer_full = out Bool()
    }

    val g = UartCtrlGenerics()

    val uartCtrlTx = new UartCtrlTx(g)
    uartCtrlTx.io.configFrame.dataLength := 7  //8 bits
    uartCtrlTx.io.configFrame.parity := UartParityType.NONE
    uartCtrlTx.io.configFrame.stop := UartStopType.ONE
    uartCtrlTx.io.samplingTick := io.en_16_x_baud
    uartCtrlTx.io.txd <> io.serial_out
    uartCtrlTx.io.break := False
    uartCtrlTx.io.cts := False

    val streamIn= Stream(Bits(8 bits))
    val txFifo = StreamFifo(
      dataType = Bits(8 bits),
      depth    = 16
    )
    txFifo.io.push << streamIn
    txFifo.io.pop  >> uartCtrlTx.io.write

    streamIn.valid := io.buffer_write
    streamIn.payload := io.data_in

    io.buffer_half_full :=txFifo.io.availability < 8
    io.buffer_full := txFifo.io.availability < 2
}