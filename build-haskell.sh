#!/bin/bash

# Build script for Haskell server compilation
# This script compiles the Haskell server for deployment

echo "🏗️  Building Haskell L Language Server..."
echo "=========================================="

# Check if stack is installed
if ! command -v stack &> /dev/null; then
    echo "❌ Stack not found. Please install Haskell Stack first."
    echo "   Visit: https://docs.haskellstack.org/en/stable/install_and_upgrade/"
    exit 1
fi

# Clean previous builds
echo "🧹 Cleaning previous builds..."
stack clean

# Build the project
echo "🔨 Building Haskell project..."
stack build --flag l-lang:production

# Check if build was successful
if [ $? -eq 0 ]; then
    echo "✅ Haskell build completed successfully!"
    echo ""
    echo "📁 Executable location:"
    stack path --local-install-root
    echo ""
    echo "🚀 To run the server:"
    echo "   stack exec l-lang-exe -- -w"
    echo ""
    echo "💡 The server will run on port 3000"
else
    echo "❌ Haskell build failed!"
    exit 1
fi