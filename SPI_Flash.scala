package MyHardware

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import spinal.core.sim._
import scala.util.control.Breaks


class SPI_Flash extends Component{
    val io = new Bundle
    {
        val c = in Bits(16 bits)

        val startRead = in Bool
        val address = in Bits(24 bits)

        val output = master Stream(Bits(8 bits))

        val wake = in Bool

        val SPI = new Bundle
        {
            val SCLK = out Bool
            val CS = out Bool
            val MOSI = out Bool
            val MISO = in Bool
        }
    }
    
    val spi = new mySPI()
    spi.io.SPI <> io.SPI

    io.output <> spi.io.input

    spi.io.output.payload := 0x00
    spi.io.output.valid := False

    val holdOpen = Reg(Bool) init(False)
    spi.io.holdOpen := holdOpen

    val timer = new Timeout(4)
    val fsm = new StateMachine {
        val Init: State = new State with EntryPoint {
            whenIsActive {
                when(io.wake){
                    goto(WakeUp)
                }  
            }
        }

        val WakeUp: State = new State {
            whenIsActive {
                spi.io.output.payload := 0x0AB
                spi.io.output.valid := True
                goto(WakeUpDelay)
            }
        }

        val WakeUpDelay: State = new StateDelay(40) { //4 us
            whenCompleted {
                goto(Wait)
            }
        }

        val Wait: State = new State {
            whenIsActive {
                when(!io.wake){
                    spi.io.output.payload := 0x0B9
                    spi.io.output.valid := True
                    goto(Init)
                } elsewhen(io.startRead && !holdOpen) {
                    spi.io.output.payload := 0x00B
                    spi.io.output.valid := True
                    holdOpen := True
                    goto(Address23_16)
                }
            }
        }

        val Address23_16: State = new State {
           whenIsActive {
                spi.io.output.payload := B"0" ## io.address(23 downto 16)
                spi.io.output.valid := True
                goto(Address15_8)
           }
        }

        val Address15_8: State = new State {
           whenIsActive {
                spi.io.output.payload := B"0" ## io.address(15 downto 8)
                spi.io.output.valid := True
                goto(Address7_0)
           }
        }

        val Address7_0: State = new State {
           whenIsActive {
                spi.io.output.payload := B"0" ## io.address(7 downto 0)
                spi.io.output.valid := True
                goto(SendBlank)
           }
        }

        val SendBlank: State = new State {
           whenIsActive {
                spi.io.output.payload := 0x000
                spi.io.output.valid := True
                goto(Wait4DoneSending)
           }
        }

        val Wait4DoneSending: State = new State {
            onEntry{
                timer.clear()
            }
            whenIsActive {
                when(!spi.io.sending && timer){
                    goto(GetData)
                }elsewhen(spi.io.sending){
                    timer.clear()
                }
            }
        } 

        val GetData: State = new State {
            whenIsActive {
                when(!io.startRead){
                    holdOpen := False
                    goto(Wait)
                }elsewhen(!spi.io.input_full){
                    spi.io.output.payload := 0x100
                    spi.io.output.valid := True
                    goto(Wait4DoneSending)
                }
            }
        }
    }
}


object SPI_Flash_Test {
    def main(args: Array[String]) {
        SimConfig.withWave.compile{
            val dut = new SPI_Flash()
            dut
        }.doSim { dut =>
            //Fork a process to generate the reset and the clock on the dut
            dut.clockDomain.forkStimulus(period = 10)

            dut.clockDomain.waitRisingEdge()
            dut.io.output.ready #= false
            var c = 0;
            val loop = new Breaks;
            loop.breakable {
                while (true) {
                    dut.io.c #= c
                    if(c == 1){
                        dut.io.wake #= true
                    }
                    if(c == 50){
                        dut.io.address #= 0x01000
                        dut.io.startRead #= true
                    }
                    if(c == 532){
                        dut.io.startRead #= false
                    }
                    if(c == 1000) loop.break()
                    c+=1
                    dut.clockDomain.waitRisingEdge()
                }
            }
        }
    }
}