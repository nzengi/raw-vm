import scala.collection.mutable
import scala.util.{Try, Success, Failure}
import java.math.BigInteger

// Constants
val STACK_SIZE_LIMIT: Int = 1024
val MEMORY_SIZE_LIMIT: Int = 1024 * 1024 // 1MB

// Errors
sealed trait ExecutionError extends Exception
case object StackOverflow extends ExecutionError
case object StackUnderflow extends ExecutionError
case class InvalidOpcode(opcode: Int) extends ExecutionError
case object OutOfGas extends ExecutionError
case object MemoryAccessViolation extends ExecutionError
case object InvalidJumpDestination extends ExecutionError
case class Reverted(reason: String) extends ExecutionError

// Execution Context
case class ExecutionContext(
    var pc: Int = 0,
    var gas: Long,
    stack: mutable.Stack[BigInteger] = mutable.Stack.empty,
    memory: mutable.ArrayBuffer[Byte] = mutable.ArrayBuffer.fill(1024)(0.toByte),
    storage: mutable.Map[BigInteger, BigInteger] = mutable.Map.empty,
    var depth: Int = 0,
    var returnData: Array[Byte] = Array.emptyByteArray
) {
  def stackPush(value: BigInteger): Either[ExecutionError, Unit] = {
    if (stack.size >= STACK_SIZE_LIMIT) Left(StackOverflow)
    else {
      stack.push(value)
      Right(())
    }
  }

  def stackPop(): Either[ExecutionError, BigInteger] =
    if (stack.isEmpty) Left(StackUnderflow) else Right(stack.pop())

  def memoryRead(offset: Int, size: Int): Either[ExecutionError, Array[Byte]] = {
    if (offset + size > MEMORY_SIZE_LIMIT) Left(MemoryAccessViolation)
    else {
      val result = Array.fill(size)(0.toByte)
      for (i <- 0 until size) {
        if (offset + i < memory.size) result(i) = memory(offset + i)
      }
      Right(result)
    }
  }

  def memoryWrite(offset: Int, data: Array[Byte]): Either[ExecutionError, Unit] = {
    if (offset + data.length > MEMORY_SIZE_LIMIT) Left(MemoryAccessViolation)
    else {
      if (offset + data.length > memory.size) {
        memory.appendAll(Array.fill(offset + data.length - memory.size)(0.toByte))
      }
      for (i <- data.indices) memory(offset + i) = data(i)
      Right(())
    }
  }

  def storageRead(key: BigInteger): BigInteger =
    storage.getOrElse(key, BigInteger.ZERO)

  def storageWrite(key: BigInteger, value: BigInteger): Unit = {
    storage(key) = value
  }
}

// EVMExecutor
class EVMExecutor(code: Array[Byte], gasLimit: Long) {
  val context: ExecutionContext = ExecutionContext(gas = gasLimit)
  val jumpDestinations: Set[Int] = analyzeJumpDestinations(code)

  def analyzeJumpDestinations(code: Array[Byte]): Set[Int] = {
    val destinations = mutable.Set[Int]()
    var i = 0
    while (i < code.length) {
      if (code(i) == 0x5b) { // JUMPDEST opcode
        destinations.add(i)
      }
      if (0x60 to 0x7f contains code(i)) {
        i += code(i) - 0x5f
      }
      i += 1
    }
    destinations.toSet
  }

  def executeOp(op: EVMOp): Either[ExecutionError, Unit] = {
    if (context.gas < op.gasCost) Left(OutOfGas)
    else {
      context.gas -= op.gasCost
      op.opType match {
        case EVMOpType.Arithmetic => executeArithmetic(op)
        case EVMOpType.Stack => executeStack(op)
        case EVMOpType.Memory => executeMemory(op)
        case EVMOpType.Storage => executeStorage(op)
        case EVMOpType.Control => executeControl(op)
        case _ => Left(InvalidOpcode(op.opcode))
      }
    }
  }

  private def executeArithmetic(op: EVMOp): Either[ExecutionError, Unit] = {
    op.opcode match {
      case 0x01 => // ADD
        for {
          a <- context.stackPop()
          b <- context.stackPop()
          _ <- context.stackPush(a.add(b))
        } yield ()
      case 0x02 => // MUL
        for {
          a <- context.stackPop()
          b <- context.stackPop()
          _ <- context.stackPush(a.multiply(b))
        } yield ()
      case _ => Left(InvalidOpcode(op.opcode))
    }
  }

  private def executeStack(op: EVMOp): Either[ExecutionError, Unit] = {
    op.opcode match {
      case opcode if 0x60 to 0x7f contains opcode => // PUSH1-PUSH32
        op.immediateValue match {
          case Some(value) =>
            context.stackPush(new BigInteger(1, value.toArray))
          case None => Left(InvalidOpcode(op.opcode))
        }
      case 0x50 => // POP
        context.stackPop().map(_ => ())
      case _ => Left(InvalidOpcode(op.opcode))
    }
  }

  private def executeMemory(op: EVMOp): Either[ExecutionError, Unit] = {
    op.opcode match {
      case 0x51 => // MLOAD
        for {
          offset <- context.stackPop().map(_.intValue())
          value <- context.memoryRead(offset, 32)
          _ <- context.stackPush(new BigInteger(1, value))
        } yield ()
      case 0x52 => // MSTORE
        for {
          offset <- context.stackPop().map(_.intValue())
          value <- context.stackPop()
          _ <- context.memoryWrite(offset, value.toByteArray)
        } yield ()
      case _ => Left(InvalidOpcode(op.opcode))
    }
  }

  private def executeStorage(op: EVMOp): Either[ExecutionError, Unit] = {
    op.opcode match {
      case 0x54 => // SLOAD
        for {
          key <- context.stackPop()
          value = context.storageRead(key)
          _ <- context.stackPush(value)
        } yield ()
      case 0x55 => // SSTORE
        for {
          key <- context.stackPop()
          value <- context.stackPop()
        } yield context.storageWrite(key, value)
      case _ => Left(InvalidOpcode(op.opcode))
    }
  }

  private def executeControl(op: EVMOp): Either[ExecutionError, Unit] = {
    op.opcode match {
      case 0x56 => // JUMP
        for {
          dest <- context.stackPop().map(_.intValue())
          _ <- if (jumpDestinations.contains(dest)) {
            context.pc = dest
            Right(())
          } else Left(InvalidJumpDestination)
        } yield ()
      case _ => Left(InvalidOpcode(op.opcode))
    }
  }
}

// Test
object EVMExecutorTest extends App {
  val code = Array[Byte](0x60, 0x03, 0x60, 0x02, 0x01) // PUSH1 3, PUSH1 2, ADD
  val executor = new EVMExecutor(code, 100000)

  val push1 = EVMOp(
    "test", 0x60, "PUSH1", EVMOpType.Stack, 3, 0, 1, Some(Seq(3.toByte)), false, 0
  )
  val push2 = EVMOp(
    "test", 0x60, "PUSH1", EVMOpType.Stack, 3, 0, 1, Some(Seq(2.toByte)), false, 0
  )
  val add = EVMOp(
    "test", 0x01, "ADD", EVMOpType.Arithmetic, 3, 2, 1, None, false, 0
  )

  assert(executor.executeOp(push1).isRight)
  assert(executor.executeOp(push2).isRight)
  assert(executor.executeOp(add).isRight)

  assert(executor.context.stack.pop() == BigInteger.valueOf(5))
  println("All tests passed!")
}
