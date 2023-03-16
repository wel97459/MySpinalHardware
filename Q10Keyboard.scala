package MySpinalHardware

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.io._
import spinal.core.sim._

class Q10Keyboard extends Component{
    val io = new Bundle {
		val o_scl_write = out Bool()
		val o_sda_write = out Bool()

		val i_scl = in Bool()
		val i_sda = in Bool()
        val i_hold = in Bool()

        val key_code_stream = master Stream (Bits(8 bits))
    }

    val readAmount = Reg(Bits(3 bits)) init(0)
    val writeAmount = Reg(Bits(3 bits)) init(0)

    val deviceAddr = Reg(Bits(7 bits)) init(0)
    val dataIn = Reg(Bits(8 bits)) init(0)

    val i2c_m = new i2cMasterControl();
    i2c_m.io.i_addr := deviceAddr 
    i2c_m.io.i_start := False
    i2c_m.io.i_writeAmount := writeAmount
    i2c_m.io.i_readAmount := readAmount

    i2c_m.io.i_stream.payload := dataIn
    i2c_m.io.i_stream.valid := False

	i2c_m.io.i_scl := io.i_scl
	i2c_m.io.i_sda := io.i_sda
	io.o_scl_write := i2c_m.io.o_scl_write	
	io.o_sda_write := i2c_m.io.o_sda_write	

    i2c_m.io.o_stream.ready := False

    val keyState = Reg(Bits(2 bits))
    val keyCode = Reg(Bits(8 bits))

    val source = Stream(Bits(8 bits))

	val OutputFiFo = StreamFifo(
			dataType = Bits(8 bits),
			depth    = 16
	)

	OutputFiFo.io.push << source
	OutputFiFo.io.pop  >> io.key_code_stream

    source.payload := keyCode
    source.valid := False

    val fsm = new StateMachine 
    {
        val Init: State = new State with EntryPoint
        {
            whenIsActive{
                dataIn := 0x02 //Configuration register
                deviceAddr := 0x1f
                writeAmount := 2
                readAmount := 0
                when(!io.i_hold){
                    goto(LoadConfiguration)
                }
            }
        }
        val LoadConfiguration: State = new State
        {
            whenIsActive{
                i2c_m.io.i_stream.valid := True
                dataIn := 0xD2 //CFG_OVERFLOW_INT | CFG_KEY_INT | CFG_USE_MODS | CFG_REPORT_MODS
                goto(SendConfiguration)
            }
        }

        val SendConfiguration: State = new State
        {
            whenIsActive{
                i2c_m.io.i_stream.valid := True
                goto(StartConf)
            }
        }
        val StartConf: State = new State
        {
            whenIsActive{
                i2c_m.io.i_start := True
                goto(WaitConf)
            }
        }

        val WaitConf: State = new State
        {
            whenIsActive{
                when(!i2c_m.io.o_busy){
                    goto(Timeout)
                } 
            }
        }
        val GetFifo: State = new State
        {
            whenIsActive{
                dataIn := 0x09 //FIFO access register
                deviceAddr := 0x1f
                writeAmount := 1
                readAmount := 2
                goto(LoadPayload)
            }
        }

        val LoadPayload: State = new State
        {
            whenIsActive{
                i2c_m.io.i_stream.valid := True
                goto(Start)
            }
        }


        val Start: State = new State
        {
            whenIsActive{
                i2c_m.io.i_start := True
                goto(Wait)
            }
        }

        val Wait: State = new State
        {
            whenIsActive{
                when(!i2c_m.io.o_busy){
                    goto(GetKeyState)
                } 
            }
        }

        val GetKeyState: State = new State
        {
            whenIsActive{
                writeAmount := 0
                readAmount := 0
                keyState := i2c_m.io.o_stream.payload(1 downto 0)
                i2c_m.io.o_stream.ready := True
                goto(GetKeyCode)
            }
        }

        val GetKeyCode: State = new State
        {
            whenIsActive{
                keyCode := i2c_m.io.o_stream.payload
                i2c_m.io.o_stream.ready := True
                goto(ShowCode)
            }
        }

        val ShowCode: State = new State
        {
            whenIsActive{
                when(keyState === 3){
                    source.valid := True
                }
                goto(Timeout)
            }
        } 

        val Timeout: State = new StateDelay(100 us){
            whenCompleted{
                goto(GetFifo)
            }
        }
    }
}