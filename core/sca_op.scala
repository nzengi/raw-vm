package core

package object gas {
  // Gas cost type for EVM operations
  type GasCost = Long
}

import core.gas._
import scala.collection.mutable

// Represents the type of EVM operation
sealed trait EVMOpType
object EVMOpType {
  case object Arithmetic extends EVMOpType // ADD, SUB, MUL, etc.
  case object Stack extends EVMOpType      // PUSH, POP, etc.
  case object Memory extends EVMOpType     // MLOAD, MSTORE, etc.
  case object Storage extends EVMOpType    // SLOAD, SSTORE
  case object Control extends EVMOpType    // JUMP, JUMPI
  case object Environment extends EVMOpType // ADDRESS, BALANCE, etc.
  case object Block extends EVMOpType      // BLOCKHASH, COINBASE, etc.
  case object System extends EVMOpType     // CREATE, CALL, etc.
  case object Log extends EVMOpType        // LOG0, LOG1, etc.
  case object Special extends EVMOpType    // STOP, REVERT, etc.
}

// Contains detailed information about an EVM operation
case class EVMOp(
    contextName: String,
    opcode: Int,
    opName: String,
    opType: EVMOpType,
    gasCost: GasCost,
    stackInput: Int,
    stackOutput: Int,
    immediateValue: Option[Seq[Byte]],
    shouldBreak: Boolean,
    memoryChange: Int
) {
  override def toString: String = {
    val immediate = immediateValue
      .map(value => s", Immediate=0x${value.map("%02x".format(_)).mkString}")
      .getOrElse("")
    s"Context=$contextName, Opcode=0x${opcode.toHexString}, Operation=$opName, " +
      s"Type=$opType, Gas=$gasCost, Stack=$stackInput->$stackOutput, MemoryChange=$memoryChange$immediate"
  }
}

// Factory for creating common EVM operations
class EVMOpFactory {
  // Cached standard gas costs
  private val gasCosts: mutable.Map[Int, GasCost] = mutable.Map(
    0x00 -> 0, // STOP
    0x01 -> 3, // ADD
    0x02 -> 5, // MUL
    0x03 -> 3, // SUB
    0x04 -> 5  // DIV
    // Add more standard gas costs...
  )

  // Creates common arithmetic operations
  def createArithmetic(context: String, opcode: Int): EVMOp = {
    val (opName, gasCost) = opcode match {
      case 0x01 => ("ADD", 3)
      case 0x02 => ("MUL", 5)
      case 0x03 => ("SUB", 3)
      case 0x04 => ("DIV", 5)
      case _    => throw new IllegalArgumentException(s"Invalid arithmetic opcode: $opcode")
    }

    EVMOp(
      context,
      opcode,
      opName,
      EVMOpType.Arithmetic,
      gasCost,
      stackInput = 2, // Most arithmetic ops take 2 inputs
      stackOutput = 1, // and produce 1 output
      immediateValue = None,
      shouldBreak = false,
      memoryChange = 0
    )
  }

  // Creates PUSH operations (PUSH1-PUSH32)
  def createPush(context: String, opcode: Int, value: Seq[Byte]): EVMOp = {
    if (!(0x60 to 0x7f).contains(opcode))
      throw new IllegalArgumentException(s"Invalid PUSH opcode: $opcode")

    val size = opcode - 0x5f
    val opName = s"PUSH$size"

    EVMOp(
      context,
      opcode,
      opName,
      EVMOpType.Stack,
      gasCost = 3, // Standard gas cost for PUSH
      stackInput = 0, // Takes no stack inputs
      stackOutput = 1, // Produces one stack output
      immediateValue = Some(value),
      shouldBreak = false,
      memoryChange = 0
    )
  }

  // Creates memory operations
  def createMemory(context: String, opcode: Int): EVMOp = {
    val (opName, gasCost, stackIn, stackOut, memChange) = opcode match {
      case 0x51 => ("MLOAD", 3, 1, 1, 32)
      case 0x52 => ("MSTORE", 3, 2, 0, 32)
      case 0x53 => ("MSTORE8", 3, 2, 0, 1)
      case _    => throw new IllegalArgumentException(s"Invalid memory opcode: $opcode")
    }

    EVMOp(
      context,
      opcode,
      opName,
      EVMOpType.Memory,
      gasCost,
      stackInput = stackIn,
      stackOutput = stackOut,
      immediateValue = None,
      shouldBreak = false,
      memoryChange = memChange
    )
  }
}

// Test cases
object EVMOpTest {
  def main(args: Array[String]): Unit = {
    val factory = new EVMOpFactory()

    // Test Arithmetic Operation
    val addOp = factory.createArithmetic("main", 0x01)
    assert(addOp.opName == "ADD")
    assert(addOp.gasCost == 3)

    println(addOp)

    // Test PUSH Operation
    val pushValue = Seq[Byte](0x12, 0x34)
    val pushOp = factory.createPush("main", 0x61, pushValue)
    assert(pushOp.opName == "PUSH2")
    assert(pushOp.immediateValue.contains(pushValue))

    println(pushOp)
  }
}
