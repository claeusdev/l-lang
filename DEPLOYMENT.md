# Vercel Deployment Guide for L Language

This guide explains how to deploy the L Language interpreter to Vercel with both the compiled Haskell server and the React frontend.

## 🚀 Deployment Options

### Option 1: Frontend-Only Deployment (Recommended)

This is the easiest and most cost-effective option. The frontend includes a JavaScript implementation of the L language interpreter that works without the Haskell backend.

#### Steps:

1. **Install Vercel CLI** (if not already installed):
   ```bash
   npm install -g vercel
   ```

2. **Deploy the frontend**:
   ```bash
   cd web-client
   vercel
   ```

3. **Follow the prompts**:
   - Link to existing project or create new one
   - Set build command: `npm run build:standalone`
   - Set output directory: `dist`

#### Benefits:
- ✅ No server costs
- ✅ Fast deployment
- ✅ Works offline
- ✅ Includes full L language interpreter
- ✅ All UI features work

### Option 2: Full Stack with Serverless Functions

This option uses Vercel's serverless functions to provide the API backend.

#### Steps:

1. **Deploy the full project**:
   ```bash
   vercel
   ```

2. **The deployment includes**:
   - React frontend (static)
   - Serverless API functions (`/api/evaluate`)
   - Automatic routing configuration

#### Benefits:
- ✅ Full API backend
- ✅ Scalable serverless functions
- ✅ Automatic HTTPS
- ✅ Global CDN

### Option 3: Hybrid Deployment (Haskell + Frontend)

For production use with the actual Haskell server:

#### Steps:

1. **Build the Haskell server**:
   ```bash
   ./build-haskell.sh
   ```

2. **Deploy frontend to Vercel**:
   ```bash
   cd web-client
   vercel
   ```

3. **Deploy Haskell server separately**:
   - Use Railway, Render, or DigitalOcean
   - Update frontend API URL to point to Haskell server

## 📁 Project Structure

```
├── vercel.json              # Vercel configuration
├── api/
│   └── evaluate.js          # Serverless function (JavaScript L interpreter)
├── web-client/
│   ├── vercel.json          # Frontend-specific Vercel config
│   ├── package.json         # Frontend dependencies
│   └── dist/                # Built frontend (generated)
├── build-haskell.sh         # Haskell build script
└── package.json             # Root package.json
```

## 🔧 Configuration Files

### `vercel.json` (Root)
- Configures builds for both frontend and API
- Sets up routing rules
- Defines serverless function timeouts

### `api/evaluate.js`
- JavaScript implementation of L language interpreter
- Provides same API as Haskell server
- Handles CORS and error cases

### `web-client/vercel.json`
- Frontend-specific configuration
- Build command and output directory
- API routing rules

## 🌐 Environment Variables

No environment variables are required for basic deployment. The app works out of the box.

## 🚀 Quick Start

1. **Clone and setup**:
   ```bash
   git clone <your-repo>
   cd l-lang
   ```

2. **Deploy to Vercel**:
   ```bash
   vercel
   ```

3. **Access your app**:
   - Vercel will provide a URL like `https://your-app.vercel.app`
   - The app will be fully functional

## 🔄 Development Workflow

1. **Local development**:
   ```bash
   cd web-client
   npm run dev
   ```

2. **Test production build**:
   ```bash
   cd web-client
   npm run build:standalone
   npm run serve:standalone
   ```

3. **Deploy changes**:
   ```bash
   vercel --prod
   ```

## 📊 Performance

- **Frontend**: Served from Vercel's global CDN
- **API**: Serverless functions with automatic scaling
- **Cold start**: ~100-200ms for API functions
- **Bundle size**: ~2MB for frontend (includes Monaco editor)

## 🛠 Troubleshooting

### Build Issues
- Ensure Node.js 18+ is installed
- Run `npm install` in web-client directory
- Check Vercel build logs for specific errors

### API Issues
- Check function logs in Vercel dashboard
- Verify CORS headers are set correctly
- Test API endpoints directly

### Frontend Issues
- Clear browser cache
- Check console for JavaScript errors
- Verify all static assets are loading

## 📈 Monitoring

- Use Vercel Analytics for performance monitoring
- Check function logs for API errors
- Monitor build times and deployment status

## 🔒 Security

- CORS is configured for all origins (adjust for production)
- No sensitive data is stored
- All evaluation happens in isolated functions

## 💡 Tips

1. **Use Vercel's preview deployments** for testing changes
2. **Monitor function usage** to stay within limits
3. **Enable Vercel Analytics** for performance insights
4. **Use environment variables** for different API endpoints per environment