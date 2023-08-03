package MySpinalHardware


import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.core.sim._

import java.io._
import scala.util.control.Breaks

// CRC polynomial coefficients: x^16 + x^12 + x^5 + 1
//                              0x1021 (hex)
// CRC width:                   16 bits
// CRC shift direction:         left (big endian)
// Input word width:            8 bits

class CRC16 extends Component
{
    val io = new Bundle {
        val pulse_in = in Bool() 
        val start_in = in Bool() 
        val data_in = in Bits(8 bits)
        val data_out = out Bits(16 bits)
    }

    val crc16 = Reg(Bits(16 bits))
    val bitCounter = Counter(16)
    val state = Reg(Bool()) init(False)
    

    when(io.start_in.rise())
    {
        crc16 := 0xFFFF
    }elsewhen(io.pulse_in.rise()){
        crc16(0) := crc16(8) ^ crc16(12) ^ io.data_in(0) ^ io.data_in(4)
        crc16(1) := crc16(9) ^ crc16(13) ^ io.data_in(1) ^ io.data_in(5)
        crc16(2) := crc16(10) ^ crc16(14) ^ io.data_in(2) ^ io.data_in(6)
        crc16(3) := crc16(11) ^ crc16(15) ^ io.data_in(3) ^ io.data_in(7)
        crc16(4) := crc16(12) ^ io.data_in(4)
        crc16(5) := crc16(8) ^ crc16(12) ^ crc16(13) ^ io.data_in(0) ^ io.data_in(4) ^ io.data_in(5)
        crc16(6) := crc16(9) ^ crc16(13) ^ crc16(14) ^ io.data_in(1) ^ io.data_in(5) ^ io.data_in(6)
        crc16(7) := crc16(10) ^ crc16(14) ^ crc16(15) ^ io.data_in(2) ^ io.data_in(6) ^ io.data_in(7)
        crc16(8) := crc16(0) ^ crc16(11) ^ crc16(15) ^ io.data_in(3) ^ io.data_in(7)
        crc16(9) := crc16(1) ^ crc16(12) ^ io.data_in(4)
        crc16(10) := crc16(2) ^ crc16(13) ^ io.data_in(5)
        crc16(11) := crc16(3) ^ crc16(14) ^ io.data_in(6)
        crc16(12) := crc16(4) ^ crc16(8) ^ crc16(12) ^ crc16(15) ^ io.data_in(0) ^ io.data_in(4) ^ io.data_in(7)
        crc16(13) := crc16(5) ^ crc16(9) ^ crc16(13) ^ io.data_in(1) ^ io.data_in(5)
        crc16(14) := crc16(6) ^ crc16(10) ^ crc16(14) ^ io.data_in(2) ^ io.data_in(6)
        crc16(15) := crc16(7) ^ crc16(11) ^ crc16(15) ^ io.data_in(3) ^ io.data_in(7)
    }

    io.data_out := crc16
}
