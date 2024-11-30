use std::collections::HashMap;
use num_bigint::BigUint;
use thiserror::Error;

use super::assembly::{EVMOp, EVMOpType};

const STACK_SIZE_LIMIT: usize = 1024;
const MEMORY_SIZE_LIMIT: usize = 1024 * 1024; // 1MB

#[derive(Debug, Error)]
pub enum ExecutionError {
    #[error("Stack overflow")]
    StackOverflow,
    #[error("Stack underflow")]
    StackUnderflow,
    #[error("Invalid opcode: {0}")]
    InvalidOpcode(u8),
    #[error("Out of gas")]
    OutOfGas,
    #[error("Memory access violation")]
    MemoryAccessViolation,
    #[error("Invalid jump destination")]
    InvalidJumpDestination,
    #[error("Execution reverted: {0}")]
    Reverted(String),
}

#[derive(Debug, Clone)]
pub struct ExecutionContext {
    /// Current program counter
    pc: usize,
    /// Remaining gas
    gas: u64,
    /// EVM stack
    stack: Vec<BigUint>,
    /// EVM memory
    memory: Vec<u8>,
    /// Storage state
    storage: HashMap<BigUint, BigUint>,
    /// Execution depth for calls
    depth: u32,
    /// Return data from last call
    return_data: Vec<u8>,
}

impl ExecutionContext {
    pub fn new(gas_limit: u64) -> Self {
        Self {
            pc: 0,
            gas: gas_limit,
            stack: Vec::with_capacity(STACK_SIZE_LIMIT),
            memory: Vec::with_capacity(1024), // Initial memory size
            storage: HashMap::new(),
            depth: 0,
            return_data: Vec::new(),
        }
    }

    /// Push a value onto the stack
    pub fn stack_push(&mut self, value: BigUint) -> Result<(), ExecutionError> {
        if self.stack.len() >= STACK_SIZE_LIMIT {
            return Err(ExecutionError::StackOverflow);
        }
        self.stack.push(value);
        Ok(())
    }

    /// Pop a value from the stack
    pub fn stack_pop(&mut self) -> Result<BigUint, ExecutionError> {
        self.stack.pop().ok_or(ExecutionError::StackUnderflow)
    }

    /// Read from memory
    pub fn memory_read(&self, offset: usize, size: usize) -> Result<Vec<u8>, ExecutionError> {
        if offset + size > MEMORY_SIZE_LIMIT {
            return Err(ExecutionError::MemoryAccessViolation);
        }
        
        let mut result = vec![0; size];
        for i in 0..size {
            result[i] = self.memory.get(offset + i).copied().unwrap_or(0);
        }
        Ok(result)
    }

    /// Write to memory
    pub fn memory_write(&mut self, offset: usize, data: &[u8]) -> Result<(), ExecutionError> {
        if offset + data.len() > MEMORY_SIZE_LIMIT {
            return Err(ExecutionError::MemoryAccessViolation);
        }

        // Expand memory if needed
        if offset + data.len() > self.memory.len() {
            self.memory.resize(offset + data.len(), 0);
        }

        self.memory[offset..offset + data.len()].copy_from_slice(data);
        Ok(())
    }

    /// Read from storage
    pub fn storage_read(&self, key: &BigUint) -> BigUint {
        self.storage.get(key).cloned().unwrap_or_else(BigUint::zero)
    }

    /// Write to storage
    pub fn storage_write(&mut self, key: BigUint, value: BigUint) {
        self.storage.insert(key, value);
    }
}

pub struct EVMExecutor {
    /// Current execution context
    context: ExecutionContext,
    /// Program bytecode
    code: Vec<u8>,
    /// Valid jump destinations
    jump_destinations: Vec<usize>,
}

impl EVMExecutor {
    pub fn new(code: Vec<u8>, gas_limit: u64) -> Self {
        let jump_destinations = Self::analyze_jump_destinations(&code);
        Self {
            context: ExecutionContext::new(gas_limit),
            code,
            jump_destinations,
        }
    }

    /// Find all valid JUMPDEST instructions
    fn analyze_jump_destinations(code: &[u8]) -> Vec<usize> {
        let mut destinations = Vec::new();
        let mut i = 0;
        while i < code.len() {
            if code[i] == 0x5b {  // JUMPDEST opcode
                destinations.push(i);
            }
            // Skip push data
            if (0x60..=0x7f).contains(&code[i]) {
                i += (code[i] - 0x5f) as usize;
            }
            i += 1;
        }
        destinations
    }

    /// Execute a single instruction
    pub fn execute_op(&mut self, op: &EVMOp) -> Result<(), ExecutionError> {
        // Check gas
        if self.context.gas < op.gas_cost() {
            return Err(ExecutionError::OutOfGas);
        }
        self.context.gas -= op.gas_cost();

        match op.op_type() {
            EVMOpType::Arithmetic => self.execute_arithmetic(op),
            EVMOpType::Stack => self.execute_stack(op),
            EVMOpType::Memory => self.execute_memory(op),
            EVMOpType::Storage => self.execute_storage(op),
            EVMOpType::Control => self.execute_control(op),
            _ => Err(ExecutionError::InvalidOpcode(op.opcode())),
        }
    }

    /// Execute arithmetic operations
    fn execute_arithmetic(&mut self, op: &EVMOp) -> Result<(), ExecutionError> {
        match op.opcode() {
            0x01 => { // ADD
                let a = self.context.stack_pop()?;
                let b = self.context.stack_pop()?;
                self.context.stack_push(a + b)?;
            }
            0x02 => { // MUL
                let a = self.context.stack_pop()?;
                let b = self.context.stack_pop()?;
                self.context.stack_push(a * b)?;
            }
            0x03 => { // SUB
                let a = self.context.stack_pop()?;
                let b = self.context.stack_pop()?;
                self.context.stack_push(a - b)?;
            }
            // Add more arithmetic operations...
            _ => return Err(ExecutionError::InvalidOpcode(op.opcode())),
        }
        Ok(())
    }

    /// Execute stack operations
    fn execute_stack(&mut self, op: &EVMOp) -> Result<(), ExecutionError> {
        match op.opcode() {
            0x60..=0x7f => { // PUSH1-PUSH32
                if let Some(value) = op.immediate_value() {
                    let num = BigUint::from_bytes_be(value);
                    self.context.stack_push(num)?;
                }
            }
            0x50 => { // POP
                self.context.stack_pop()?;
            }
            // Add more stack operations...
            _ => return Err(ExecutionError::InvalidOpcode(op.opcode())),
        }
        Ok(())
    }

    /// Execute memory operations
    fn execute_memory(&mut self, op: &EVMOp) -> Result<(), ExecutionError> {
        match op.opcode() {
            0x51 => { // MLOAD
                let offset = self.context.stack_pop()?.to_usize().unwrap();
                let value = self.context.memory_read(offset, 32)?;
                let num = BigUint::from_bytes_be(&value);
                self.context.stack_push(num)?;
            }
            0x52 => { // MSTORE
                let offset = self.context.stack_pop()?.to_usize().unwrap();
                let value = self.context.stack_pop()?;
                let bytes = value.to_bytes_be();
                self.context.memory_write(offset, &bytes)?;
            }
            // Add more memory operations...
            _ => return Err(ExecutionError::InvalidOpcode(op.opcode())),
        }
        Ok(())
    }

    /// Execute storage operations
    fn execute_storage(&mut self, op: &EVMOp) -> Result<(), ExecutionError> {
        match op.opcode() {
            0x54 => { // SLOAD
                let key = self.context.stack_pop()?;
                let value = self.context.storage_read(&key);
                self.context.stack_push(value)?;
            }
            0x55 => { // SSTORE
                let key = self.context.stack_pop()?;
                let value = self.context.stack_pop()?;
                self.context.storage_write(key, value);
            }
            // Add more storage operations...
            _ => return Err(ExecutionError::InvalidOpcode(op.opcode())),
        }
        Ok(())
    }

    /// Execute control flow operations
    fn execute_control(&mut self, op: &EVMOp) -> Result<(), ExecutionError> {
        match op.opcode() {
            0x56 => { // JUMP
                let dest = self.context.stack_pop()?.to_usize().unwrap();
                if !self.jump_destinations.contains(&dest) {
                    return Err(ExecutionError::InvalidJumpDestination);
                }
                self.context.pc = dest;
            }
            0x57 => { // JUMPI
                let dest = self.context.stack_pop()?.to_usize().unwrap();
                let condition = self.context.stack_pop()?;
                if condition != BigUint::zero() {
                    if !self.jump_destinations.contains(&dest) {
                        return Err(ExecutionError::InvalidJumpDestination);
                    }
                    self.context.pc = dest;
                }
            }
            // Add more control operations...
            _ => return Err(ExecutionError::InvalidOpcode(op.opcode())),
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_arithmetic_execution() {
        let code = vec![0x60, 0x03, 0x60, 0x02, 0x01]; // PUSH1 3, PUSH1 2, ADD
        let mut executor = EVMExecutor::new(code, 100000);
        
        // Create and execute PUSH 3
        let push1 = EVMOp::new(
            "test".to_string(),
            0x60,
            "PUSH1".to_string(),
            EVMOpType::Stack,
            3,
            0,
            1,
            Some(vec![3]),
            false,
            0,
        );
        assert!(executor.execute_op(&push1).is_ok());
        
        // Create and execute PUSH 2
        let push2 = EVMOp::new(
            "test".to_string(),
            0x60,
            "PUSH1".to_string(),
            EVMOpType::Stack,
            3,
            0,
            1,
            Some(vec![2]),
            false,
            0,
        );
        assert!(executor.execute_op(&push2).is_ok());
        
        // Create and execute ADD
        let add = EVMOp::new(
            "test".to_string(),
            0x01,
            "ADD".to_string(),
            EVMOpType::Arithmetic,
            3,
            2,
            1,
            None,
            false,
            0,
        );
        assert!(executor.execute_op(&add).is_ok());
        
        // Check result
        assert_eq!(executor.context.stack_pop().unwrap(), BigUint::from(5u32));
    }
}
