#!/bin/bash

##
## EPITECH PROJECT, 2025
## glados
## File description:
## setup.sh - Initial setup script
##

set -e

echo "🚀 GLaDOS Project Setup"
echo "======================="
echo ""

# Check if stack is installed
if ! command -v stack &> /dev/null; then
    echo "❌ Error: Stack is not installed"
    echo "Please install Stack: https://docs.haskellstack.org/en/stable/install_and_upgrade/"
    exit 84
fi

echo "✓ Stack found: $(stack --version)"
echo ""

# Initialize stack
echo "📦 Installing dependencies..."
stack setup
stack build --only-dependencies

echo ""
echo "🔨 Building project..."
make

echo ""
echo "✅ Setup complete!"
echo ""
echo "Available commands:"
echo "  make          - Build the project"
echo "  make test     - Run tests"
echo "  make clean    - Clean build artifacts"
echo "  make re       - Rebuild from scratch"
echo "  ./glados      - Run the interpreter"
echo ""
