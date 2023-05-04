package MySpinalHardware

import spinal.core._
import spinal.core.sim._
import scala.util.control.Breaks
import scala.collection.mutable.ArrayBuffer
import spinal.lib.Counter
import spinal.lib.BufferCC

class I2S_In(audio_width:Int = 32) extends Component 
{
    val io = new Bundle
    {
        val i_sclk = in Bool()
        val i_lrclk = in Bool()
        val i_sdin = in Bool()

        val i_ready = in Bool()
        val o_valid = out Bool()
        val o_audio_l = out UInt(audio_width bits)
        val o_audio_r = out UInt(audio_width bits)
        val bitLength = out Bits(3 bits)
    }

//Components
    val bit_count = Counter(audio_width+1)
    val bitLength = Reg(Bits(3 bits)) init(0)
    val i_sclk_db = BufferCC(io.i_sclk, False)
    val i_sdin_db = BufferCC(io.i_sdin, False)
    val i_lrclk_db = BufferCC(io.i_lrclk, False)
//Registers
    val shift_reg = Reg(UInt(audio_width bits)) init(0)
    val audio_l = Reg(UInt(audio_width bits)) init(0)
    val audio_r = Reg(UInt(audio_width bits)) init(0)
    val valid = Reg(Bool()) init(False)
    val LR = Reg(Bool()) init(False)
//Wires
    val shift_data = shift_reg(audio_width-2 downto 0) @@ i_sdin_db.asUInt
    //val data24 = ((shift_reg(23) ? U"8'hFF" | U"8'h00").asBits ## shift_reg(23 downto 0).asBits).asUInt
    //val data16 = ((shift_reg(15) ? U"16'hFFFF" | U"16'h0000").asBits ## shift_reg(15 downto 0).asBits).asUInt
    val data24 = (shift_data(23 downto 0).asBits ## U"8'h00").asUInt 
    val data16 = (shift_data(15 downto 0).asBits ## U"16'h0000").asUInt
    val data_out = (bit_count.value === 31) ? shift_data | ((bit_count.value === 23) ? data24 | ((bit_count.value === 15) ? data16 | 0))
//IO
    io.o_audio_l := audio_l
    io.o_audio_r := audio_r
    io.o_valid := valid
    io.bitLength := bitLength
//Latches
    when(i_sclk_db.rise()){
        bit_count.increment()

        when(LR =/= i_lrclk_db){
            LR := i_lrclk_db
            bit_count.clear()
            shift_reg := 0//i_sdin_db.asUInt @@ U"31'h00000000"
            when(LR){
                audio_r := data_out
                valid := True
                bitLength := (bit_count.value === 31) ? B"100" | ((bit_count.value === 23) ? B"010"  | ((bit_count.value === 15) ? B"001"  | 0))
            }otherwise{
                audio_l := data_out
            }
        }otherwise{
            shift_reg := shift_data
        }
    }

    when(io.i_ready && valid){
        valid := False
    }
}

object I2S_sim {
    def main(args: Array[String]) {
        SimConfig.withFstWave.compile{
            val dut = new I2S_In()
            dut
        }.doSim { dut =>
            //Fork a process to generate the reset and the clock on the dut
            dut.clockDomain.forkStimulus(period = 10)
            dut.io.i_sclk #= false
            dut.io.i_lrclk #= true
            dut.io.i_sdin #= false
            var c = 0;
            var cc = 0;
            val loop = new Breaks;
            loop.breakable {
                while (true) {
                    dut.clockDomain.waitRisingEdge()
                    if(c % 2 == 0){
                        dut.io.i_sclk #= !dut.io.i_sclk.toBoolean
                        if(dut.io.i_sclk.toBoolean){
                            if(cc % 25 == 0){
                                dut.io.i_lrclk #= !dut.io.i_lrclk.toBoolean
                            }
                            cc+=1
                        }
                    }


                    dut.io.i_sdin #= (-14000 >> (cc%25) & 0x1).toBoolean

                    c += 1
                    if(c > 999){
                        loop.break;
                    }
                }
            }
        }
    }
}