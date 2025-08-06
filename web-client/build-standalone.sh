#!/bin/bash

# Build script for L Language Standalone App
# This creates a production build that can run without the Haskell API server

echo "🏗️  Building L Language Standalone App..."
echo "==========================================="

# Change to web-client directory
cd "$(dirname "$0")"

# Install dependencies if node_modules doesn't exist
if [ ! -d "node_modules" ]; then
  echo "📦 Installing dependencies..."
  npm install
fi

# Clean previous build
echo "🧹 Cleaning previous build..."
rm -rf dist

# Build with standalone mode
echo "🔨 Building for standalone mode..."
npm run build:standalone

# Check if build was successful
if [ $? -eq 0 ]; then
  echo "✅ Build completed successfully!"
  echo ""
  echo "📁 Built files are in: ./dist"
  echo "🚀 To serve the standalone app:"
  echo "   npm run serve:standalone"
  echo ""
  echo "   Or using the executable script:"
  echo "   ./standalone-server.js"
  echo ""
  echo "💡 The standalone app includes:"
  echo "   - Offline L language interpreter"
  echo "   - Sample code snippets"
  echo "   - Full UI functionality"
else
  echo "❌ Build failed!"
  exit 1
fi