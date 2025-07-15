package MySpinalHardware

import spinal.core._
import spinal.lib._

case class BasicBall(val SX: BigInt, val SY: BigInt) extends Component
{
    val io = new Bundle {
        val update = in Bool()
        val reset = in Bool()
        val X = out UInt(8 bits)
        val Y = out UInt(8 bits)
    }

    val ballX = Reg(UInt(8 bits)) init(0)
    val ballY = Reg(UInt(8 bits)) init(10)
    val ballXDir = Reg(Bool) init(True)
    val ballYDir = Reg(Bool) init(True)

    io.X := ballX
    io.Y := ballY
    when(io.reset){
        ballX := 0
        ballY := 10
        ballXDir := True
        ballYDir := True
    }elsewhen(io.update.fall()){
        when(ballX < 1){
            ballXDir := True
        } elsewhen(ballX >= SX){
            ballXDir := False
        }

        when(ballY < 1) {
            ballYDir := True
        } elsewhen(ballY >= SY){
            ballYDir := False
        }
    }elsewhen(io.update.rise()){
        when(ballXDir) {
            ballX := ballX + 1
        } otherwise (ballX := ballX - 1)

        when(ballYDir) {
            ballY := ballY + 1
        } otherwise (ballY := ballY - 1)
    }
}