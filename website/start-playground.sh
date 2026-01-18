#!/bin/bash

echo "=================================="
echo "GLaDOS Playground Setup"
echo "=================================="
echo ""

# Check if we're in the website directory
if [ ! -f "backend.py" ]; then
    echo "❌ Error: Please run this script from the website directory"
    exit 1
fi

# Go to parent directory (glados root)
cd ..

# Build GLaDOS if not already built
if [ ! -f "glados" ]; then
    echo "📦 Building GLaDOS..."
    make
    if [ $? -ne 0 ]; then
        echo "❌ Build failed!"
        exit 1
    fi
    echo "✅ GLaDOS built successfully!"
else
    echo "✅ GLaDOS executable found"
fi

# Go back to website directory
cd website

# Check Python
if ! command -v python3 &> /dev/null; then
    echo "❌ Python 3 not found. Please install Python 3."
    exit 1
fi

# Prefer .venv (matches repo tooling), but keep compatibility with old ./venv
VENV_DIR=".venv"
if [ -d "venv" ] && [ ! -d ".venv" ]; then
    VENV_DIR="venv"
fi

# Create virtual environment if it doesn't exist
if [ ! -d "$VENV_DIR" ]; then
    echo ""
    echo "📦 Creating Python virtual environment..."
    python3 -m venv "$VENV_DIR"
    if [ $? -ne 0 ]; then
        echo "❌ Failed to create virtual environment!"
        exit 1
    fi
    echo "✅ Virtual environment created"
fi

# Activate virtual environment
source "$VENV_DIR"/bin/activate

# Install Python dependencies
echo ""
echo "📦 Installing Python dependencies..."
pip install -r requirements.txt --quiet
if [ $? -ne 0 ]; then
    echo "❌ Failed to install dependencies!"
    deactivate
    exit 1
fi
echo "✅ Dependencies installed"

echo ""
echo "=================================="
echo "🚀 Starting GLaDOS Playground"
echo "=================================="
echo ""
echo "Backend will run on: http://localhost:5000"
echo "Frontend should run on: http://localhost:8080"
echo ""
echo "To start the frontend (in another terminal):"
echo "  cd website && python3 -m http.server 8080"
echo ""
echo "Press Ctrl+C to stop the backend"
echo ""

# Start the backend (virtual environment is already activated)
python backend.py
