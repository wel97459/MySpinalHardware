package MySpinalHardware

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.io._
import spinal.core.sim._

class i2cMaster() extends Component {
	val io = new Bundle {

		val i_addr = in Bits(7 bits)
		val i_read = in Bool()

		val i_send = in Bool()
		val i_end = in Bool()
		val i_stop = in Bool()

		val i_data = in Bits(8 bits)

		val o_data = out Bits(8 bits)
		val o_vaild = out Bool()
		val o_sent = out Bool()
		val o_error = out Bool()
		val o_busy = out Bool()

		val o_scl_write = out Bool()
		val o_sda_write = out Bool()

		val i_scl = in Bool()
		val i_sda = in Bool()
	}

	val start = Reg(Bool()) init(False)
	val sent1 = Reg(Bool()) init(False)
	val error = Reg(Bool()) init(False)
	val busy = Reg(Bool()) init(False)
	io.o_sent := False
	io.o_vaild := False
	io.o_error := error
	io.o_busy := busy

	val addr = Cat(io.i_addr, io.i_read)
	val scl_write = Reg(Bool()) init(True)
	val sda_write = Reg(Bool()) init(True)

	val scl_read = io.i_scl
	val sda_read = io.i_sda
	io.o_scl_write := scl_write
	io.o_sda_write := sda_write

	val dataIn = Reg(Bits(8 bits)) init(0)
	io.o_data := dataIn

	val clkCounter = Counter(4)
	val bitCounter = Counter(8)
    val revBitCount = 7 - bitCounter.value

	val fsm = new StateMachine
	{
		val Wait: State = new State with EntryPoint
		{
			whenIsActive{
				start := False
				sent1 := False
				busy := False
				sda_write := True
				scl_write := True
				when(io.i_send){
					error := False
					busy := True
					goto(Start)
				}
			}
		}

		val Start: State = new StateDelay(2)
		{
			whenIsActive{
				sda_write := False
			}

			whenCompleted{
				scl_write := False
				bitCounter.clear()
				clkCounter.clear()
				goto(SendAddr)
			}
		}

		val StopStart: State = new State
		{
			whenIsActive{
				busy := False
				sent1 := False
				sda_write := True
				scl_write := !clkCounter(1)
				clkCounter.increment()
				when(clkCounter === 1)
				{
					sda_write := False
				}
				when(clkCounter === 2)
				{
					bitCounter.clear()
					clkCounter.clear()
					sda_write := False
					busy := True
					goto(SendAddr)
				}
			}
		}

		val SendAddr: State = new State
		{
			whenIsActive{
				sda_write := addr(revBitCount)
				start := True
				when(start){
					scl_write := !clkCounter(1)
					clkCounter.increment()
					when(clkCounter === 2)
					{
						bitCounter.increment()
					}
					when(bitCounter.willOverflow)
					{
						goto(GetSlaveACK)
						start := False
					}
				}
			}
		}

		val GetSlaveACK: State = new State
		{
			whenIsActive{
				sda_write := True
				when(sent1){
					io.o_sent := True
				}
				clkCounter.increment()
				scl_write := !clkCounter(1)
				when(clkCounter === 1){
                    error := sda_read
				}

				when(clkCounter === 2){
					when(error){
						goto(Error)
					}elsewhen(io.i_end && sent1){
						goto(End)
					}elsewhen(io.i_stop){
						goto(StopStart)
					}otherwise{
						bitCounter.clear()
						when(io.i_read)
						{
							goto(ReadByte)
						}otherwise{
							goto(WriteByte)
						}
					}
				}
			}
		}

		val MasterACK: State = new State
		{
			whenIsActive{
				when(io.i_end){
					sda_write := True
				}otherwise(sda_write := False)

				clkCounter.increment()
				scl_write := !clkCounter(1)

				when(clkCounter === 2){
					when(io.i_end){
						goto(End)
					}otherwise{
						goto(ReadByte)
					}
				}
			}
		}

		val ReadByte: State = new State
		{
			whenIsActive{
				sda_write := True
				scl_write := !clkCounter(1)
				when(clkCounter === 1){
					dataIn := dataIn(6 downto 0) ## sda_read
					when(scl_read){
						clkCounter.increment()
					}
				}otherwise{ 
					clkCounter.increment()
				}
				when(clkCounter === 2)
				{
					bitCounter.increment()
				}
				when(bitCounter.willOverflow)
				{
					io.o_vaild := True
					start := False
					goto(MasterACK)
				}
			}
		}

		val WriteByte: State = new State
		{
			whenIsActive{
				sda_write := io.i_data(revBitCount)
				scl_write := !clkCounter(1)
				when(clkCounter === 1){
					when(scl_read){
						clkCounter.increment()
					}
				}otherwise{ 
					clkCounter.increment()
				}
				when(clkCounter === 2)
				{
					bitCounter.increment()
				}
				when(bitCounter.willOverflow)
				{
					sent1 := True
					start := False
					goto(GetSlaveACK)
				}
			}
		}

		val Error: State = new State{
			whenIsActive{
				goto(End)
			}
		}

		val End: State = new State
		{
			whenIsActive{
				clkCounter.increment()
				when(clkCounter === 3){
					scl_write := False
					sda_write := False
				}
				when(clkCounter === 0){
					scl_write := True
				}
				when(clkCounter === 1){
					sda_write := True
					goto(Wait)
				}
			}
		}
	}
}

class i2cMasterControl() extends Component {
	val io = new Bundle {
		val i_addr = in Bits(7 bits)

		val i_start = in Bool()

		val i_readAmount = in Bits(3 bits)
		val i_writeAmount = in Bits(3 bits)

		val i_stream = slave Stream (Bits(8 bits))
		val o_stream = master Stream (Bits(8 bits))

		val o_error = out Bool()
		val o_busy = out Bool()

		val o_scl_write = out Bool()
		val o_sda_write = out Bool()

		val i_scl = in Bool()
		val i_sda = in Bool()
	}

	val source,sink = Stream(Bits(8 bits))

	val InputFiFo = StreamFifo(
			dataType = Bits(8 bits),
			depth    = 16
	)

	val OutputFiFo = StreamFifo(
			dataType = Bits(8 bits),
			depth    = 16
	)

	InputFiFo.io.push << io.i_stream
	InputFiFo.io.pop  >> sink

	OutputFiFo.io.push << source
	OutputFiFo.io.pop  >> io.o_stream

	sink.ready := False


	val i2c = new i2cMaster()
	i2c.io.i_addr := io.i_addr
	i2c.io.i_data := sink.payload

	i2c.io.i_scl := io.i_scl
	i2c.io.i_sda := io.i_sda
	io.o_scl_write := i2c.io.o_scl_write	
	io.o_sda_write := i2c.io.o_sda_write	

	io.o_error := i2c.io.o_error

	i2c.io.i_send := False
	i2c.io.i_end := False
	i2c.io.i_stop := False
	val read = Reg(Bool) init(False)
	i2c.io.i_read := read

	source.payload := i2c.io.o_data
	source.valid := i2c.io.o_vaild

	val busy = Reg(Bool) init(False)
	io.o_busy := busy

	val counter = Counter(16)

	val fsm = new StateMachine
	{
		val Wait: State = new State with EntryPoint
		{
			whenIsActive{
				counter.clear()
				busy := False
				when(io.i_start)
				{
					busy := True
					when(io.i_writeAmount =/= 0){
						goto(Send)
						read := False
						i2c.io.i_send := True
					}otherwise{
						goto(Read)
						read := True
						i2c.io.i_send := True
					}
				}
			}

			whenIsInactive{
				when(i2c.io.o_error){

				}
			}
		}

		val Send: State = new State
		{
			whenIsActive{
				when(i2c.io.o_sent.rise()){
					counter.increment()
					sink.ready := True

				}
				when(io.i_writeAmount.asUInt === counter){
					when(io.i_readAmount === 0){
						i2c.io.i_end := True
						when(!i2c.io.o_busy){
							goto(Wait)
						}
					}otherwise{
						i2c.io.i_stop := True
						read := True
						when(!i2c.io.o_busy){
							goto(Read)
							counter.clear()
						}
					}
				}
			}
		}

		val Read: State = new State
		{
			whenIsActive{
				when(i2c.io.o_vaild && io.i_readAmount.asUInt =/= counter){
					counter.increment()
				}
				when(io.i_readAmount.asUInt === counter){
					i2c.io.i_end := True
					when(!i2c.io.o_busy){
						goto(Wait)
					}
				}
			}
		}
	}
}

// object I2CSim {
// 	def main(args: Array[String]) {
// 		SimConfig.withWave.compile{
// 			val dut = new i2cMasterControl()
// 			dut
// 		}.doSim{dut =>
// 			//Fork a process to generate the reset and the clock on the dut

// 			dut.clockDomain.forkStimulus(period = 10)


// 			var modelState = 0
// 			var addr = 0x00
// 			var data = 0x00
// 			var bitcounter = 0

// 			var last_scl = true
// 			var last_sda = true

// 			// dut.io.i_data #= 0xA5;
// 			dut.io.i_addr #= 0x1F;
// 			dut.io.i_stream.payload #= 0x01;
// 			dut.io.i_writeAmount #= 0x01;
// 			dut.io.i_readAmount #= 0x02;
// 			// dut.io.i_read #= false;
// 			// dut.io.i_end  #= false;
// 			// dut.io.i_stop #= false;
// 			for(idx <- 0 to 300){
// 				dut.io.i_start #= false;
// 				dut.io.i_stream.valid #= false;
// 				dut.io.io_scl #= true;
// 				if(idx == 10) dut.io.i_stream.valid #= true;
// 				if(idx == 20) dut.io.i_start #= true;
// 				//if(idx >= 149 && idx <= 200) dut.io.io_scl #= false;
// 				// if(idx == 10) dut.io.i_send #= true; else dut.io.i_send #= false;

// 				// if(idx > 10 && dut.io.io_scl.toBoolean && !dut.io.io_sda.toBoolean && last_sda && modelState == 0){
// 				//   println("Start!");
// 				//   modelState = 1
// 				// }else if(idx > 10 && dut.io.io_scl.toBoolean && !last_scl && bitcounter < 8 && modelState == 1){
// 				//   println("Got Addr Bit: ", bitcounter, dut.io.io_sda.toBoolean);
// 				// }else if(idx > 10 && !dut.io.io_scl.toBoolean && last_scl && bitcounter < 8 && modelState == 1){
// 				//   bitcounter+=1
// 				// }else if(idx > 10 && bitcounter == 8 && modelState >= 1 && modelState <= 3){
// 				//   if(modelState == 1) modelState = 2;
// 				//   if(!dut.io.io_scl.toBoolean && last_scl && modelState == 3){bitcounter = 0; modelState = 4;}
// 				//   if(dut.io.io_scl.toBoolean && !last_scl && modelState == 2) modelState = 3;
// 				//    dut.io.io_sda #= false
// 				// }else if(idx > 10 && dut.io.io_scl.toBoolean && !last_scl && bitcounter < 8 && modelState == 4){
// 				//   println("Got Data Bit: ", bitcounter, dut.io.io_sda.toBoolean);
// 				// }else if(idx > 10 && !dut.io.io_scl.toBoolean && last_scl && bitcounter < 8 && modelState == 4){
// 				//   bitcounter+=1
// 				// }else if(idx > 10 && bitcounter == 8 && modelState >= 4 && modelState <= 5){
// 				//   if(modelState == 1) modelState = 5;
// 				//   if(!dut.io.io_scl.toBoolean && last_scl && modelState == 6) bitcounter = 0;
// 				//   if(dut.io.io_scl.toBoolean && !last_scl && modelState == 5) modelState = 6;
// 				//    dut.io.io_sda #= false
// 				//    dut.io.i_end #= true
// 				// }else if(idx > 10 && dut.io.io_scl.toBoolean && dut.io.io_sda.toBoolean && !last_sda){
// 				//   modelState = 0
// 				//   println("End!");
// 				//}

// 				last_scl = dut.io.io_scl.toBoolean
// 				last_sda = dut.io.io_sda.toBoolean
// 				//Wait a rising edge on the clock
// 				dut.clockDomain.waitRisingEdge()
// 			}
// 		}
// 	}
// }