use core::fmt;
use num_bigint::BigUint;
use std::collections::HashMap;

/// Gas cost type for EVM operations
pub type GasCost = u64;

/// Represents the type of EVM operation
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EVMOpType {
    Arithmetic,    // ADD, SUB, MUL, etc.
    Stack,        // PUSH, POP, etc.
    Memory,       // MLOAD, MSTORE, etc.
    Storage,      // SLOAD, SSTORE
    Control,      // JUMP, JUMPI
    Environment,  // ADDRESS, BALANCE, etc.
    Block,        // BLOCKHASH, COINBASE, etc.
    System,       // CREATE, CALL, etc.
    Log,          // LOG0, LOG1, etc.
    Special,      // STOP, REVERT, etc.
}

/// Contains detailed information about an EVM operation
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EVMOp {
    /// Context or scope where operation is executed
    context_name: String,
    /// Opcode in hex
    opcode: u8,
    /// Human readable operation name
    op_name: String,
    /// Type of operation
    op_type: EVMOpType,
    /// Gas cost for the operation
    gas_cost: GasCost,
    /// Stack items consumed
    stack_input: u8,
    /// Stack items produced
    stack_output: u8,
    /// Immediate value if any (for PUSH operations)
    immediate_value: Option<Vec<u8>>,
    /// Should break in debugger
    should_break: bool,
    /// Memory size change
    memory_change: i32,
}

impl EVMOp {
    /// Creates a new EVM operation instance
    pub fn new(
        context_name: String,
        opcode: u8,
        op_name: String,
        op_type: EVMOpType,
        gas_cost: GasCost,
        stack_input: u8,
        stack_output: u8,
        immediate_value: Option<Vec<u8>>,
        should_break: bool,
        memory_change: i32,
    ) -> Self {
        Self {
            context_name,
            opcode,
            op_name,
            op_type,
            gas_cost,
            stack_input,
            stack_output,
            immediate_value,
            should_break,
            memory_change,
        }
    }

    // Getters
    pub fn context_name(&self) -> &str {
        &self.context_name
    }

    pub fn opcode(&self) -> u8 {
        self.opcode
    }

    pub fn op_name(&self) -> &str {
        &self.op_name
    }

    pub fn op_type(&self) -> &EVMOpType {
        &self.op_type
    }

    pub fn gas_cost(&self) -> GasCost {
        self.gas_cost
    }

    pub fn stack_input(&self) -> u8 {
        self.stack_input
    }

    pub fn stack_output(&self) -> u8 {
        self.stack_output
    }

    pub fn immediate_value(&self) -> Option<&Vec<u8>> {
        self.immediate_value.as_ref()
    }

    pub fn should_break(&self) -> bool {
        self.should_break
    }

    pub fn memory_change(&self) -> i32 {
        self.memory_change
    }

    // Setters
    pub fn set_gas_cost(&mut self, gas_cost: GasCost) {
        self.gas_cost = gas_cost;
    }

    pub fn set_should_break(&mut self, should_break: bool) {
        self.should_break = should_break;
    }

    pub fn set_immediate_value(&mut self, value: Option<Vec<u8>>) {
        self.immediate_value = value;
    }
}

impl fmt::Display for EVMOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Context={}, Opcode=0x{:02x}, Operation={}, Type={:?}, Gas={}, Stack={}->{}, MemoryChange={}",
            self.context_name,
            self.opcode,
            self.op_name,
            self.op_type,
            self.gas_cost,
            self.stack_input,
            self.stack_output,
            self.memory_change
        )?;
        if let Some(immediate) = &self.immediate_value {
            write!(f, ", Immediate=0x{}", hex::encode(immediate))?;
        }
        Ok(())
    }
}

/// Factory for creating common EVM operations
pub struct EVMOpFactory {
    /// Cached standard gas costs
    gas_costs: HashMap<u8, GasCost>,
}

impl EVMOpFactory {
    /// Creates a new factory with standard gas costs
    pub fn new() -> Self {
        let mut gas_costs = HashMap::new();
        // Basic operations
        gas_costs.insert(0x00, 0);     // STOP
        gas_costs.insert(0x01, 3);     // ADD
        gas_costs.insert(0x02, 5);     // MUL
        gas_costs.insert(0x03, 3);     // SUB
        gas_costs.insert(0x04, 5);     // DIV
        // Add more standard gas costs...
        
        Self { gas_costs }
    }

    /// Creates common arithmetic operations
    pub fn create_arithmetic(&self, context: String, opcode: u8) -> EVMOp {
        let (op_name, gas_cost) = match opcode {
            0x01 => ("ADD".to_string(), 3),
            0x02 => ("MUL".to_string(), 5),
            0x03 => ("SUB".to_string(), 3),
            0x04 => ("DIV".to_string(), 5),
            // Add more arithmetic operations...
            _ => panic!("Invalid arithmetic opcode: {}", opcode),
        };

        EVMOp::new(
            context,
            opcode,
            op_name,
            EVMOpType::Arithmetic,
            gas_cost,
            2,  // Most arithmetic ops take 2 inputs
            1,  // and produce 1 output
            None,
            false,
            0,
        )
    }

    /// Creates PUSH operations (PUSH1-PUSH32)
    pub fn create_push(&self, context: String, opcode: u8, value: Vec<u8>) -> EVMOp {
        if !(0x60..=0x7f).contains(&opcode) {
            panic!("Invalid PUSH opcode: {}", opcode);
        }

        let size = (opcode - 0x5f) as usize;
        let op_name = format!("PUSH{}", size);

        EVMOp::new(
            context,
            opcode,
            op_name,
            EVMOpType::Stack,
            3, // Standard gas cost for PUSH
            0, // Takes no stack inputs
            1, // Produces one stack output
            Some(value),
            false,
            0,
        )
    }

    /// Creates memory operations
    pub fn create_memory(&self, context: String, opcode: u8) -> EVMOp {
        let (op_name, gas_cost, stack_in, stack_out, mem_change) = match opcode {
            0x51 => ("MLOAD".to_string(), 3, 1, 1, 32),
            0x52 => ("MSTORE".to_string(), 3, 2, 0, 32),
            0x53 => ("MSTORE8".to_string(), 3, 2, 0, 1),
            // Add more memory operations...
            _ => panic!("Invalid memory opcode: {}", opcode),
        };

        EVMOp::new(
            context,
            opcode,
            op_name,
            EVMOpType::Memory,
            gas_cost,
            stack_in,
            stack_out,
            None,
            false,
            mem_change,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_arithmetic_op() {
        let factory = EVMOpFactory::new();
        let add_op = factory.create_arithmetic("main".to_string(), 0x01);
        
        assert_eq!(add_op.opcode(), 0x01);
        assert_eq!(add_op.op_name(), "ADD");
        assert_eq!(add_op.gas_cost(), 3);
        assert_eq!(add_op.stack_input(), 2);
        assert_eq!(add_op.stack_output(), 1);
    }

    #[test]
    fn test_push_op() {
        let factory = EVMOpFactory::new();
        let value = vec![0x12, 0x34];
        let push_op = factory.create_push("main".to_string(), 0x61, value.clone());
        
        assert_eq!(push_op.opcode(), 0x61);
        assert_eq!(push_op.op_name(), "PUSH2");
        assert_eq!(push_op.immediate_value(), Some(&value));
    }

    #[test]
    fn test_display() {
        let factory = EVMOpFactory::new();
        let add_op = factory.create_arithmetic("main".to_string(), 0x01);
        
        let display = format!("{}", add_op);
        assert!(display.contains("ADD"));
        assert!(display.contains("Gas=3"));
    }
}
