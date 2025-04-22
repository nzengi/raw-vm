#!/bin/bash

# Setup Java environment for ZK-DAG-PoS Blockchain
echo "Setting up environment for ZK-DAG-PoS Blockchain..."

# Set JAVA_HOME to Java 11
export JAVA_HOME=$(/usr/libexec/java_home -v 11)

# Verify the Java version
echo "Using Java version:"
java -version

# Clean any previous build
echo "Cleaning previous build..."
sbt clean

# Compile the project
echo "Compiling project..."
sbt compile

# Run instructions
echo ""
echo "Setup complete. To run the project, use one of the following commands:"
echo "  - Run bootstrap node: make run-node1"
echo "  - Run secondary node: make run-node2"
echo "  - Run test network:   make network-up"
echo ""
echo "For more options, run: make help" 