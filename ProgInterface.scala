package MySpinalHardware

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import java.rmi.registry.Registry
import scala.util.control.Breaks
class BaudRateGen(Baud: BigInt) extends Component {
    val io = new Bundle
    {
        val tick = out Bool()
    }
      val counter = CounterFreeRun((ClockDomain.current.frequency.getValue / Baud.toInt / 8).toInt)

      val tick = Reg(Bool()) init(False)

      when(counter.willOverflowIfInc) {
        tick := True
      }otherwise{
        tick := False
      }
      io.tick := tick


}
class ProgrammingInterface(Baud: BigInt) extends Component {
    val io = new Bundle
    {
        val keys = master Stream (Bits(8 bit))
        val RamInterface = new Bundle
        {
            val Address = out Bits(16 bit)
            val DataIn  = in Bits(8 bit)
            val DataOut = out Bits(8 bit)
            val Write   = out Bool()
        }

        val FlagIn = in Bits(8 bit)
        val FlagOut = out Bits(8 bit)
        val UartTX = out Bool()
        val UartRX = in Bool()
    }

/***-Defines-***/    

    val StartMSGList = "ProgrammingInterface 1.0\r\n\r\n\0Ok.\r\n\0".toList
    val StartMSG_Rom = Mem(UInt(8 bits), StartMSGList.map(U(_, 8 bits)))

/***-Registers-***/

    val flag = Reg(Bits(8 bits)) init(0)
    val address = Reg(UInt(16 bits)) init(0)
    val workingReg = Reg(Bits(16 bits))
    val amount = CounterUpDownSet(256)

    val byte2HexSelect = Reg(Bool) init(False)

/***-Wires-***/

    val uartReset = False;
    val hexValue = UInt(4 bits)
    val dataInt = UInt(8 bits)
    val shiftWorkingReg = False
    val latchWorkingReg = False
    val addressAdd = False

    val byte2HexInput = UInt(8 bits)
    val byteUpperHex = UInt(8 bits)
    val byteLowerHex = UInt(8 bits)

    val ramWrite = False
    val ramDataSel = False
    val ramDataSelLast = RegNext(ramDataSel)
    val ramDataOut = Bits(8 bits)

/***-IO stuff-***/

    io.RamInterface.Address := address.asBits
    io.RamInterface.DataOut := ramDataOut
    io.RamInterface.Write := ramWrite
    io.FlagOut := flag

/***-Streams-***/
    val keyIn = Stream(Bits(8 bits))
    val keyFifo = StreamFifo(
      dataType = Bits(8 bits),
      depth    = 16
    )
    keyFifo.io.push << keyIn
    keyFifo.io.pop  >> io.keys

    keyIn.payload := workingReg(7 downto 0)
    keyIn.valid := False

/***-Blocks-***/

    val baudGen = new BaudRateGen(Baud);
    val uart_tx = new uart_tx()
    val uart_rx = new uart_rx()

    val hex2value = new Hex2Value()
    val byte2hex = new Byte2Hex()

/***-Routing-***/

    //UartTX
    uart_rx.io.en_16_x_baud := baudGen.io.tick
    uart_rx.io.buffer_read := False
    uart_rx.io.buffer_reset := uartReset

    //UartTX
    uart_tx.io.en_16_x_baud := baudGen.io.tick
    uart_tx.io.data_in := 0x00
    uart_tx.io.buffer_write := False
    uart_tx.io.buffer_reset := uartReset

    //Ram
    when(ramDataSel || ramDataSelLast){
        ramDataOut := workingReg(7 downto 0)
    }otherwise{
        ramDataOut := uart_rx.io.data_out
    }

    //Byte2Hex
    when(byte2HexSelect){
        byte2HexInput := io.FlagIn.asUInt
    }otherwise{
        byte2HexInput := io.RamInterface.DataIn.asUInt
    }
    
/***-LutChains-***/
    //Hex conversion
    dataInt := uart_rx.io.data_out.asUInt 
    hex2value.io.inHex := dataInt
    hexValue := hex2value.io.outNum

    byte2hex.io.inValue := byte2HexInput
    byteUpperHex := byte2hex.io.outHexUpper
    byteLowerHex := byte2hex.io.outHexLower

    when(shiftWorkingReg){
        workingReg := (workingReg |<< 4) | Cat(B"000000000000", hexValue.asBits)
    }elsewhen(latchWorkingReg){
        workingReg := Cat(B"00000000", uart_rx.io.data_out.asBits)
    }

    when(addressAdd){
        address := address + 1;
    }


/***-Logic-***/   

    val InterfaceFMS = new StateMachine {
        /***-Registers-***/
        val StartMSGPointer = CounterSet(StartMSGList.length)
        val StartMSGChar = StartMSG_Rom(StartMSGPointer)
        val lastState = Reg(this.enumDef())

        /***-FMS-***/
        val Reset: State = new StateDelay(10) with EntryPoint {
            whenIsActive {
                StartMSGPointer.clear()
                uartReset := False
                lastState := enumOf(Reset)
            }

            whenCompleted {
                goto(SendStartMsg)
            }
        }

        val SendStartMsg: State = new  State {
            whenIsActive{
                lastState := enumOf(SendStartMsg)
                uart_tx.io.data_in := StartMSGChar.asBits
                when(StartMSGChar =/= 0) {
                    when(!uart_tx.io.buffer_full){
                        StartMSGPointer.increment()
                        uart_tx.io.buffer_write := True
                        goto(CheckTX_FIFO)
                    }
                }otherwise{
                    StartMSGPointer.clear()
                    goto(Waiting)
                }
            }
        }

        val CheckTX_FIFO: State = new  State {
            whenIsActive{
                when(!uart_tx.io.buffer_full) {
                    stateNext := lastState
                }
            }
        }

        val Waiting: State = new State {
            whenIsActive {
                lastState := enumOf(Waiting)
                when(uart_rx.io.buffer_data_present){
                    uart_rx.io.buffer_read := True
                    workingReg := 0;
                    when(dataInt === '?'){
                        goto(SendStartMsg)
                    }elsewhen(dataInt === 'a'){
                        amount.setValue(4)
                        goto(SetAddress)   
                    }elsewhen(dataInt === 'f'){
                        amount.setValue(2)
                        goto(SetFlag)   
                    }elsewhen(dataInt === 'g'){
                        byte2HexSelect := True
                        amount.setValue(2)
                        goto(GetFlag)   
                    }elsewhen(dataInt === 'w'){
                        amount.setValue(2)
                        goto(WriteByte)   
                    }elsewhen(dataInt === 'r'){
                        byte2HexSelect := False
                        amount.setValue(2)
                        goto(ReadByte)   
                    }elsewhen(dataInt === '`'){
                        amount.setValue(1)
                        goto(WriteBytes)   
                    }elsewhen(dataInt === 't'){
                        amount.setValue(1)
                        goto(TypeByte)   
                    }
                }
            }
        }

        val SetAddress: State = new State {
            whenIsActive {
                when(uart_rx.io.buffer_data_present || amount === 0){                    
                    when(amount >= 1){
                        uart_rx.io.buffer_read := True
                        shiftWorkingReg := True
                        amount.decrement()
                    }otherwise{
                        address := workingReg.asUInt
                        StartMSGPointer.setValue(29) //Ok.
                        goto(SendStartMsg)
                    }
                }
            }
        }

        val SetFlag: State = new State {
            whenIsActive {
                when(uart_rx.io.buffer_data_present || amount === 0){                    
                    when(amount >= 1){
                        uart_rx.io.buffer_read := True
                        shiftWorkingReg := True
                        amount.decrement()
                    }otherwise{
                        flag := workingReg.resize(8)
                        StartMSGPointer.setValue(29) //Ok.
                        goto(SendStartMsg)
                    }
                }
            }
        }
 
        val GetFlag: State = new State {
            whenIsActive {
                lastState := enumOf(GetFlag)
                when(!uart_tx.io.buffer_half_full || amount === 0){                    
                    when(amount >= 1){
                        uart_tx.io.buffer_write := True
                        when(amount === 2){
                            uart_tx.io.data_in := byteUpperHex.asBits
                        }otherwise{
                            uart_tx.io.data_in := byteLowerHex.asBits
                        }
                        amount.decrement()
                        goto(CheckTX_FIFO)
                    }otherwise{
                        StartMSGPointer.setValue(32) //NewLine
                        goto(SendStartMsg)
                    }
                }
            }
        }

        val WriteByte: State = new State {
            whenIsActive {
                when(uart_rx.io.buffer_data_present || amount === 0){                    
                    when(amount >= 1){
                        uart_rx.io.buffer_read := True
                        shiftWorkingReg := True
                        amount.decrement()
                    }otherwise{
                        ramDataSel := True
                        ramWrite := True
                        addressAdd := True
                        StartMSGPointer.setValue(29) //Ok.
                        goto(SendStartMsg)
                    }
                }
            }
        }

        val ReadByte: State = new State {
            whenIsActive {
                lastState := enumOf(ReadByte)
                when(!uart_tx.io.buffer_half_full || amount === 0){                    
                    when(amount >= 1){
                        uart_tx.io.buffer_write := True
                        when(amount === 2){
                            uart_tx.io.data_in := byteUpperHex.asBits
                        }otherwise{
                            uart_tx.io.data_in := byteLowerHex.asBits
                        }
                        amount.decrement()
                        goto(CheckTX_FIFO)
                    }otherwise{
                        addressAdd := True
                        StartMSGPointer.setValue(32) //NewLine
                        goto(SendStartMsg)
                    }
                }
            }
        }

        val WriteBytes: State = new State {
            whenIsActive {
                when(uart_rx.io.buffer_data_present){
                    when(amount === 0){
                        amount.setValue(workingReg.resize(8).asUInt)
                        goto(WriteBytesExec)
                    }otherwise{
                        latchWorkingReg := True
                        uart_rx.io.buffer_read := True
                        amount.decrement()
                    }
                }
            }
        }

        val WriteBytesExec: State = new State {
            whenIsActive {
                when(uart_rx.io.buffer_data_present || amount === 0){                 
                    when(amount === 0){
                        StartMSGPointer.setValue(29) //Ok.
                        goto(SendStartMsg)
                    }otherwise{
                        ramWrite := True
                        uart_rx.io.buffer_read := True
                        addressAdd := True
                        amount.decrement()
                    }
                }
            }
        }
        val TypeByte: State = new State {
            whenIsActive {
                when(uart_rx.io.buffer_data_present || amount === 0){                    
                    when(amount === 0){
                        when(keyIn.ready){
                            keyIn.valid := True
                            StartMSGPointer.setValue(29) //Ok.
                            goto(SendStartMsg)
                        }
                    }otherwise{
                        latchWorkingReg := True
                        uart_rx.io.buffer_read := True
                        amount.decrement()
                    }
                }
            }
        }
    }

/***-Post IO-***/
    io.UartTX := uart_tx.io.serial_out 
    uart_rx.io.serial_in := io.UartRX
}



//Define a custom SpinalHDL configuration with synchronous reset instead of the default asynchronous one. This configuration can be reused everywhere
object ProgrammingInterfaceConfig extends SpinalConfig(
    targetDirectory = ".",
    oneFilePerComponent = true,
    defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC),
    defaultClockDomainFrequency = FixedFrequency(50 MHz)
)

//Generate the MyTopLevel's Verilog using the above custom configuration.
object ProgrammingInterfaceGen {
    def main(args: Array[String]) {
        ProgrammingInterfaceConfig.generateVerilog(new ProgrammingInterface(9600)).printPruned
    }
}

//Define a custom SpinalHDL configuration with synchronous reset instead of the default asynchronous one. This configuration can be reused everywhere
object ProgrammingInterface_Test {
    def main(args: Array[String]) {
        SimConfig.withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(1 MHz), defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC))).withFstWave.compile{
            val dut = new ProgrammingInterface(9600)
            dut.uart_tx.io.buffer_full.simPublic()
            dut.uart_rx.rxFifo.io.push.payload.simPublic()
            dut.uart_rx.rxFifo.io.push.ready.simPublic()
            dut.uart_rx.rxFifo.io.push.valid.simPublic()
            dut.uart_rx.io.buffer_full.simPublic()
            dut
        }.doSim { dut =>
            //Fork a process to generate the reset and the clock on the dut
            dut.clockDomain.forkStimulus(period = 10)
            dut.clockDomain.waitRisingEdge()
            dut.io.FlagIn #= 0xf4
            dut.io.RamInterface.DataIn #= 0x5a
            var str = "a0400w42tc"
            var t = 0
            var d = 0
            val loop = new Breaks;
            loop.breakable {
                while (true) {
                    if(t>80){
                        if(dut.uart_rx.io.buffer_full.toBoolean == false && d < str.length())
                        {
                            dut.uart_rx.rxFifo.io.push.payload #= str.charAt(d).toByte
                            dut.uart_rx.rxFifo.io.push.valid #= true
                            d+=1
                        }else {
                            dut.uart_rx.rxFifo.io.push.payload #= 0
                            dut.uart_rx.rxFifo.io.push.valid #= false
                        }
                    }

                    if(t >= 99999) loop.break;
                    t+=1
                    dut.clockDomain.waitRisingEdge()
                }
            }
        }
    }
}