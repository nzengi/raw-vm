# ZK-DAG-PoS Blockchain Project

This project is an innovative blockchain platform built on an object-oriented, DAG (Directed Acyclic Graph) structure, using Proof of Stake consensus mechanism and supporting Zero-Knowledge proofs.

## Features

- **DAG-Based**: High efficiency with parallel processing
- **Object-Oriented**: Object-centered programming model inspired by Sui and Move
- **Proof of Stake**: Energy-efficient consensus mechanism
- **Zero-Knowledge Integration**: Private transactions and verifiable computations
- **Scalable Architecture**: Designed for high performance

## Technology Stack

- **Language**: Scala 2.13
- **Network Communication**: Akka Actor, Akka HTTP
- **Cryptography**: Bouncy Castle
- **Data Processing**: Circe (JSON)

## Algorithms Used

- **Consensus**: BullShark / HotStuff variant
- **Cryptography**: Ed25519, BLS signatures
- **Zero-Knowledge**: PlonK, zk-SNARKs
- **Concurrency Control**: MVCC (Multi-Version Concurrency Control)

## Project Structure

Main project components:

- **core/**: Core modules and subsystems

  - `DAGConsensus.scala`: DAG-based consensus mechanism
  - `ObjectModel.scala`: Object model and storage
  - `ZKSystem.scala`: Zero-Knowledge system integration
  - `PoSStaking.scala`: Proof of Stake and validator management
  - `NetworkNode.scala`: P2P network communication
  - `Blockchain.scala`: Main blockchain logic
  - `Main.scala`: Application entry point

- **provi/**: VM and execution modules
  - `sca_executor.scala`: VM operation executor

## Getting Started

### Requirements

- JDK 11+
- SBT (Scala Build Tool)

### Installation

```bash
# Clone the project
git clone https://github.com/username/zk-dag-pos-blockchain.git
cd zk-dag-pos-blockchain

# Compile
sbt compile

# Run (Genesis node)
sbt "run --node-id node-1 --endpoint 127.0.0.1:9000"

# Second node (in another terminal)
sbt "run --node-id node-2 --endpoint 127.0.0.1:9001 --bootstrap 127.0.0.1:9000"
```

### Command Line Interface

When the node is started, you can use the following commands:

- `info` - Displays blockchain information
- `validator register <name> <amount> <rate>` - Registers as a validator
- `stake <validator-id> <amount>` - Adds stake to a validator
- `object create <type> <data>` - Creates a new object
- `help` - Lists all commands
- `exit` - Shuts down the node

## Architecture

This blockchain platform consists of the following main components:

1. **Object Store**: Stores the blockchain's world state as objects
2. **DAG Consensus**: System that processes and orders transactions in parallel
3. **Staking System**: PoS mechanism that manages validators and stakes
4. **ZK System**: Module that validates and manages Zero-Knowledge proofs
5. **Network Communication**: Component that manages the P2P protocol

## Development Plan

- [ ] Completion of basic consensus and object model
- [ ] Improvement of network communication (Akka HTTP/WebSocket)
- [ ] Development of ZK integration with real cryptography libraries
- [ ] Smart Contract support (WASM or custom VM)
- [ ] Comprehensive testing and performance improvements

## Contributing

To contribute:

1. Fork this project
2. Make your changes and test them
3. Submit a pull request

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.
