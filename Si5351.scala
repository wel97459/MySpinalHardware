package MySpinalHardware

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.core.sim._

class Si5351(File: String) extends Component{
      val io = new Bundle {
		val o_scl_write = out Bool()
		val o_sda_write = out Bool()

		val i_scl = in Bool()
		val i_sda = in Bool()
        val i_skip = in Bool()
        val i_prog = in Bool()

        val o_done = out Bool()
        val o_error = out Bool()
    }

    val romAddr = Reg(UInt(8 bits)) init(0)
    val configRom = new RamInit(File, log2Up(0xFF))
    configRom.io.dina := 0
    configRom.io.ena := True
    configRom.io.wea := 0
    configRom.io.addra := romAddr.asBits

    val deviceAddr = B"7'h60"
    val dataIn = Reg(Bits(8 bits)) init(0)

    val i2c = new i2cMaster();

    i2c.io.i_addr := deviceAddr
	i2c.io.i_data := dataIn

	i2c.io.i_scl := io.i_scl
	i2c.io.i_sda := io.i_sda
	io.o_scl_write := i2c.io.o_scl_write	
	io.o_sda_write := i2c.io.o_sda_write	

	io.o_error := i2c.io.o_error

	i2c.io.i_read := False
	i2c.io.i_send := False
	i2c.io.i_end := False
	i2c.io.i_stop := False

    io.o_done := False;

    val fsm = new StateMachine 
    {
        val Init: State = new State with EntryPoint
        {
            whenIsActive{
                romAddr := 0
                dataIn := romAddr.asBits
                when(io.i_skip){
                    goto(Done)
                }otherwise{
                    i2c.io.i_send := True
                    goto(LoadConfiguration)
                }
            }
        }

        val LoadConfiguration: State = new State
        {
            whenIsActive{
                when(i2c.io.o_sent){
                    dataIn := configRom.io.douta
                    when(romAddr === 0xE8){
                        i2c.io.i_end := True
                        goto(Done)
                    }otherwise{
                        goto(SendNext)
                    }
                }
            }
        }

        val SendNext: State = new State
        {
            whenIsActive{
                when(!i2c.io.o_sent){
                    when(romAddr === 0xE8){
                        goto(Done)
                    }otherwise{
                        romAddr := romAddr + 1
                        goto(LoadConfiguration)
                    }
                }
            }
        }

        val Done: State = new State
        {
            whenIsActive{
                i2c.io.i_end := True
                io.o_done := True
                when(io.i_prog){
                    goto(Init)
                }
            }
        }
    }
}

object Si5351_Sim {
	def main(args: Array[String]) {
		SimConfig.withFstWave.compile{
			val dut = new Si5351("./data/si5351.bin")
			dut
		}.doSim{dut =>
			//Fork a process to generate the reset and the clock on the dut
			var modelState = 0
			var addr = 0x00
			var data = 0x00
			var bitcounter = 0

			var scl = true
			var sda = true
			var last_scl = true
			var last_sda = true
            dut.io.i_sda #= true
            dut.io.i_scl #= true
            dut.io.i_skip #= false 
			dut.clockDomain.forkStimulus(period = 10)

			for(idx <- 0 to 30000){
 				scl = dut.io.o_scl_write.toBoolean
 				sda = dut.io.o_sda_write.toBoolean

                if(!scl && last_scl && idx > 3){
                    if(bitcounter<9){
                        bitcounter=bitcounter+1
                    }else{
                        bitcounter=1
                    }
                }

                if(idx > 21175 && idx <= 21275){
                    dut.clockDomain.assertReset()
                }else{
                    dut.clockDomain.deassertReset()
                }

                if(bitcounter==9){
                    dut.io.i_sda #= false
                }else{
                    dut.io.i_sda #= true
                }

 				last_scl = scl
 				last_sda = sda
				//Wait a rising edge on the clock
				dut.clockDomain.waitRisingEdge()
			}
		}
	}
}