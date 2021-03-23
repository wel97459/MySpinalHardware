package MyHardware

import spinal.core._
import spinal.lib._

object Pow2 {
  def apply(value: BigInt): Int = {
    if (value < 0) SpinalError(s"No negative value ($value) on ${this.getClass.getSimpleName}")
    scala.math.pow(2, value.toDouble).toInt;
  }
}


object CounterUpDownSet {
    def apply(stateCount: BigInt): CounterUpDownSet = new CounterUpDownSet(stateCount)
    def apply(stateCount: BigInt, incWhen: Bool, decWhen : Bool): CounterUpDownSet = {
    val counter = CounterUpDownSet(stateCount)
    when(incWhen) {
        counter.increment()
        }
        when(decWhen) {
        counter.decrement()
        }
        counter
    }
//  implicit def implicitValue(c: Counter) = c.value
}

class CounterUpDownSet(val stateCount: BigInt) extends ImplicitArea[UInt]
{
    val incrementIt = False
    val decrementIt = False
    val setIt = False
    val newValue = UInt(log2Up(stateCount) bit)

    newValue := 0
    def increment(): Unit = incrementIt := True
    def decrement(): Unit = decrementIt := True

    def setValue(value: UInt): Unit = {
        newValue := value
        setIt := True
    }

    def ===(that: UInt): Bool = this.value === that
    def !==(that: UInt): Bool = this.value =/= that
    def =/=(that: UInt): Bool = this.value =/= that

    val valueNext = UInt(log2Up(stateCount) bit)
    val value = RegNext(valueNext) init(0)
    val willOverflowIfInc = value === stateCount - 1 && !decrementIt
    val willOverflow = willOverflowIfInc && incrementIt

    val finalIncrement = UInt(log2Up(stateCount) bit)
    when(incrementIt && !decrementIt){
        finalIncrement := 1
    }elsewhen(!incrementIt && decrementIt){
        finalIncrement := finalIncrement.maxValue
    }otherwise{
        finalIncrement := 0
    }

    when(setIt){
        valueNext := newValue
    }otherwise{
        if (isPow2(stateCount)) {
            valueNext := (value + finalIncrement).resized
        } else {
            assert(false,"CounterUpDownSet Need to be power of 2 and is: "+ stateCount)
        }
    }

    override def implicitValue: UInt = value
}

object CounterSet {
  def apply(start: BigInt,end: BigInt) : CounterSet  = new CounterSet(start = start, end = end)
  def apply(range : Range) : CounterSet = {
    require(range.step == 1)
    CounterSet(start = range.low, end = range.high)
  }
  def apply(stateCount: BigInt): CounterSet = new CounterSet(start = 0, end = stateCount-1)
  def apply(bitCount: BitCount): CounterSet = new CounterSet(start = 0, end = (BigInt(1)<<bitCount.value)-1)

  def apply(start: BigInt,end: BigInt, inc: Bool) : CounterSet  = {
    val counter = CounterSet(start,end)
    when(inc) {
      counter.increment()
    }
    counter
  }
  def apply(range : Range, inc: Bool) : CounterSet  = {
    require(range.step == 1)
    CounterSet(start = range.low, end = range.high,inc = inc)
  }

  def apply(stateCount: BigInt, inc: Bool): CounterSet = CounterSet(start = 0, end = stateCount-1,inc = inc)
  def apply(bitCount: BitCount, inc: Bool): CounterSet = CounterSet(start = 0, end = (BigInt(1)<<bitCount.value)-1,inc = inc)
}

// start and end inclusive, up counter
class CounterSet(val start: BigInt,val end: BigInt) extends ImplicitArea[UInt] {
  require(start <= end)
  val willIncrement = False.allowOverride
  val willClear = False.allowOverride
  
  val setValue = False
  val newValue = UInt(log2Up(end + 1) bit)

  def clear(): Unit = willClear := True
  def increment(): Unit = willIncrement := True
    
  newValue := 0
  def setValue(value: UInt): Unit = {
      newValue := value
      setValue := True
  }

  val valueNext = UInt(log2Up(end + 1) bit)
  val value = RegNext(valueNext) init(start)
  val willOverflowIfInc = value === end
  val willOverflow = willOverflowIfInc && willIncrement

  if (isPow2(end + 1) && start == 0) {   //Check if using overflow follow the spec
    valueNext := (value + U(willIncrement)).resized
  } else {
    when(willOverflow){
      valueNext := U(start)
    } otherwise {
      valueNext := (value + U(willIncrement)).resized
    }
  }

  when(setValue) {
    valueNext := newValue
  }elsewhen(willClear) {
    valueNext := start
  }

  willOverflowIfInc.allowPruning
  willOverflow.allowPruning

  override def implicitValue: UInt = this.value
}
