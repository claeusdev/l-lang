# L Language Standalone App

This directory contains a standalone version of the L Language interpreter that can run completely independently of the Haskell backend server.

## 🚀 Quick Start

```bash
# Build the standalone app
npm run build:standalone

# Serve it
npm run serve:standalone
# Or
./standalone-server.js
# Or use any static file server
npx serve dist
```

## ✨ Features

### 🔄 Offline L Language Interpreter
- **Mock API Service**: `src/services/mock-api.ts` provides a JavaScript-based L language interpreter
- **Local Evaluation**: No network requests needed - everything runs in the browser
- **Compatible Syntax**: Supports the same L language constructs as the Haskell server

### 📝 Sample Code Snippets
- **Pre-loaded Examples**: `src/data/sample-snippets.ts` contains 10 sample programs
- **Valid Syntax**: All examples use proper L language syntax
- **Progressive Complexity**: From basic arithmetic to advanced function composition

### 🎨 Full UI Experience
- **Monaco Editor**: Syntax highlighting and code editing
- **Real-time Evaluation**: Instant feedback on code execution
- **Error Handling**: Comprehensive error messages and debugging
- **Snippet Management**: Save, load, and organize code snippets

## 🏗 Build Process

### Development
```bash
npm run dev  # Vite dev server with hot reload
```

### Production Build
```bash
npm run build:standalone  # Creates optimized dist/ folder
```

### Build Script
```bash
./build-standalone.sh  # Automated build with status messages
```

## 📁 File Structure

```
web-client/
├── src/
│   ├── services/
│   │   └── mock-api.ts           # Offline L language interpreter
│   ├── data/
│   │   └── sample-snippets.ts    # Pre-defined code examples
│   └── App.tsx                   # Main app with standalone detection
├── standalone-server.js          # Node.js server for serving built app
├── build-standalone.sh           # Build automation script
├── .env.standalone               # Environment config for standalone mode
└── dist/                         # Built application (after build)
```

## 🔧 Technical Details

### Environment Detection
The app automatically detects if it should run in standalone mode:

```typescript
// From src/services/mock-api.ts
export const isStandaloneMode = (): boolean => {
  return import.meta.env.MODE === 'standalone' || 
         import.meta.env.VITE_STANDALONE === 'true' ||
         !navigator.onLine;
};
```

### Mock Interpreter Features
- **Tokenizer**: Parses L language syntax
- **Expression Evaluator**: Handles arithmetic and function calls
- **Lambda Support**: Basic lambda expression parsing
- **Let Expressions**: Local variable binding
- **Environment Management**: Variable scope and bindings

### Sample Code Examples
1. **Basic Arithmetic** - Simple math operations
2. **Lambda Functions** - Function definitions and calls
3. **Function Composition** - Higher-order functions
4. **Let Expressions** - Variable scoping
5. **Recursive Functions** - Factorial and Fibonacci
6. **Higher-Order Functions** - Map, filter, reduce patterns
7. **Complex Composition** - Advanced function combinations
8. **Conditional Logic** - If-then-else patterns
9. **Curried Functions** - Partial application
10. **Mathematical Functions** - Power, square root, etc.

## 🚢 Deployment Options

### Static Hosting
Deploy the `dist/` folder to any static hosting service:
- Netlify: Drag and drop the `dist` folder
- Vercel: Connect your repository and set build command to `npm run build:standalone`
- GitHub Pages: Use GitHub Actions to build and deploy
- AWS S3: Upload `dist` contents to S3 bucket
- Traditional web servers: Copy to Apache/Nginx document root

### Self-Hosted
Use the included Node.js server:
```bash
# On your server
npm install --production
npm run build:standalone
PORT=3000 node standalone-server.js
```

### Docker
Build a lightweight Docker image:
```dockerfile
FROM node:18-alpine
WORKDIR /app
COPY package*.json ./
RUN npm install --production
COPY dist ./dist
COPY standalone-server.js ./
EXPOSE 8080
CMD ["node", "standalone-server.js"]
```

## 🎯 Use Cases

1. **Educational Environments**: Perfect for teaching lambda calculus without server setup
2. **Offline Development**: Work with L language without internet connection
3. **Demo Purposes**: Showcase L language features at conferences or presentations
4. **Static Deployments**: Deploy to CDNs for fast global access
5. **Mobile Learning**: Responsive design works on tablets and phones

## 🔍 Troubleshooting

### Build Issues
- Ensure Node.js v18+ is installed
- Run `npm install` before building
- Check that TypeScript compiles without errors

### Server Issues
- Verify port 8080 is available
- Check `server.log` for error messages
- Ensure `dist/` directory exists and contains built files

### Runtime Issues
- Open browser developer tools to check console errors
- Verify JavaScript is enabled
- Check network tab to ensure static assets load correctly