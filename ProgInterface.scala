package MySpinalHardware

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import java.rmi.registry.Registry
import scala.util.control.Breaks

class ProgrammingInterface() extends Component {
    val io = new Bundle
    {
        val RamInterface = new Bundle
        {
            val Address = out Bits(16 bit)
            val DataIn  = in Bits(8 bit)
            val DataOut = out Bits(8 bit)
            val Write   = out Bool()
        }
//        val RX_Data = in Bits(8 bit)
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
    val ramDataOut = Bits(8 bits)

/***-IO stuff-***/

    io.RamInterface.Address := address.asBits
    io.RamInterface.DataOut := ramDataOut
    io.RamInterface.Write := ramWrite
    io.FlagOut := flag

/***-Blocks-***/

    val baudgen = True//BaudRateGen(57600);
    val uart_tx = new uart_tx()
    val uart_rx = new uart_rx()

    val hex2value = new Hex2Value()

    val byte2hex = new Byte2Hex()

/***-Routing-***/

    //UartTX
    uart_rx.io.en_16_x_baud := baudgen
    uart_rx.io.buffer_read := False
    uart_rx.io.buffer_reset := uartReset

    //UartTX
    uart_tx.io.en_16_x_baud := baudgen
    uart_tx.io.data_in := 0x00
    uart_tx.io.buffer_write := False
    uart_tx.io.buffer_reset := uartReset

    //Ram
    ramDataOut := uart_rx.io.data_out

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
    }

/***-Post IO-***/
    io.UartTX := uart_tx.io.serial_out
    uart_rx.io.serial_in := io.UartRX
}

//Define a custom SpinalHDL configuration with synchronous reset instead of the default asynchronous one. This configuration can be resued everywhere
object ProgrammingInterfaceConfig extends SpinalConfig(
    targetDirectory = ".",
    oneFilePerComponent = true,
    defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC),
    defaultClockDomainFrequency = FixedFrequency(50 MHz)
)

//Generate the MyTopLevel's Verilog using the above custom configuration.
object ProgrammingInterfaceGen {
    def main(args: Array[String]) {
        ProgrammingInterfaceConfig.generateVerilog(new ProgrammingInterface).printPruned
    }
}

//Define a custom SpinalHDL configuration with synchronous reset instead of the default asynchronous one. This configuration can be resued everywhere
object ProgrammingInterface_Test {
    def main(args: Array[String]) {
        SimConfig.withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(8 MHz), defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC))).withWave.compile{
            val dut = new ProgrammingInterface()
            dut.uart_tx.io.buffer_full.simPublic()
            dut.uart_rx.io.buffer_read.simPublic()
            dut.uart_rx.io.buffer_data_present.simPublic()
            dut.uart_rx.io.data_out.simPublic()
            dut
        }.doSim { dut =>
            //Fork a process to generate the reset and the clock on the dut
            dut.clockDomain.forkStimulus(period = 10)
            dut.clockDomain.waitRisingEdge()
            dut.io.FlagIn #= 0xf4
            dut.io.RamInterface.DataIn #= 0x5a
            var t = 0
            var d = 0
            val loop = new Breaks;
            loop.breakable {
                while (true) {
                    if(t >= 35 && t <= 40){
                        dut.uart_tx.io.buffer_full #= true 
                    }else{
                        dut.uart_tx.io.buffer_full #= false 
                    }

                    if(t>80){
                        if(dut.uart_rx.io.buffer_read.toBoolean == true) d+=1
                        if(d == 0){
                            dut.uart_rx.io.data_out #= 'a'
                            dut.uart_rx.io.buffer_data_present #= true
                        }else if(d == 1){
                            dut.uart_rx.io.data_out #= '0'
                            dut.uart_rx.io.buffer_data_present #= true
                        }else if(d == 2){
                            dut.uart_rx.io.data_out #= 'f'
                            dut.uart_rx.io.buffer_data_present #= true
                        }else if(d == 3){
                            dut.uart_rx.io.data_out #= '0'
                            dut.uart_rx.io.buffer_data_present #= true
                        }else if(d == 4){
                            dut.uart_rx.io.data_out #= '0'
                            dut.uart_rx.io.buffer_data_present #= true
                        }else if(d == 5){
                            dut.uart_rx.io.data_out #= '`'
                            dut.uart_rx.io.buffer_data_present #= true
                        }else if(d == 6){
                            dut.uart_rx.io.data_out #= 16
                            dut.uart_rx.io.buffer_data_present #= true
                        }else if(d == 7){
                            dut.uart_rx.io.data_out #= 0x00
                            dut.uart_rx.io.buffer_data_present #= true
                        }else if(d == 8){
                            dut.uart_rx.io.data_out #= 0x01
                            dut.uart_rx.io.buffer_data_present #= true
                        }else if(d == 9){
                            dut.uart_rx.io.data_out #= 0x02
                            dut.uart_rx.io.buffer_data_present #= true
                        }else if(d == 10){
                            dut.uart_rx.io.data_out #= 0x03
                            dut.uart_rx.io.buffer_data_present #= true
                        }else if(d == 11){
                            dut.uart_rx.io.data_out #= 0x04
                            dut.uart_rx.io.buffer_data_present #= true
                        }else if(d == 12){
                            dut.uart_rx.io.data_out #= 0x05
                            dut.uart_rx.io.buffer_data_present #= true
                        }else if(d == 13){
                            dut.uart_rx.io.data_out #= 0x06
                            dut.uart_rx.io.buffer_data_present #= true
                        }else if(d == 14){
                            dut.uart_rx.io.data_out #= 0x07
                            dut.uart_rx.io.buffer_data_present #= true
                        }else if(d == 15){
                            dut.uart_rx.io.data_out #= 0x08
                            dut.uart_rx.io.buffer_data_present #= true
                        }else if(d == 16){
                            dut.uart_rx.io.data_out #= 0x09
                            dut.uart_rx.io.buffer_data_present #= true
                        }else if(d == 17){
                            dut.uart_rx.io.data_out #= 0x0A
                            dut.uart_rx.io.buffer_data_present #= true  
                        }else if(d == 18){
                            dut.uart_rx.io.data_out #= 0x0B
                            dut.uart_rx.io.buffer_data_present #= true  
                        }else if(d == 19){
                            dut.uart_rx.io.data_out #= 0x0C
                            dut.uart_rx.io.buffer_data_present #= true  
                        }else if(d == 20){
                            dut.uart_rx.io.data_out #= 0x0D
                            dut.uart_rx.io.buffer_data_present #= true
                        }else if(d == 21){
                            dut.uart_rx.io.data_out #= 0x0E
                            dut.uart_rx.io.buffer_data_present #= true  
                        }else if(d == 21){
                            dut.uart_rx.io.data_out #= 0x0F
                            dut.uart_rx.io.buffer_data_present #= true          
                        }else if(d == 22){
                            dut.uart_rx.io.data_out #= 0x10
                            dut.uart_rx.io.buffer_data_present #= true                   
                        }else if(d == 23){
                            dut.uart_rx.io.data_out #= 0x11
                            dut.uart_rx.io.buffer_data_present #= true
                        }else if(d == 24){
                            dut.uart_rx.io.data_out #= 0x12
                            dut.uart_rx.io.buffer_data_present #= true    
                        }else {
                            dut.uart_rx.io.data_out #= 0
                            dut.uart_rx.io.buffer_data_present #= false
                        }
                    }

                    if(t >= 9000) loop.break;
                    t+=1
                    dut.clockDomain.waitRisingEdge()
                }
            }
        }
    }
}