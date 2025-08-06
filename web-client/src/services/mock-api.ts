// Mock L Language Interpreter for Standalone Mode
// This provides basic lambda calculus evaluation without the Haskell server

interface LangResult {
  ast: string;
  output: string;
}

interface Environment {
  [key: string]: any;
}

// Basic tokenizer for L language
function tokenize(code: string): string[] {
  const tokens: string[] = [];
  let current = '';
  let inString = false;
  
  for (let i = 0; i < code.length; i++) {
    const char = code[i];
    
    if (char === '"') {
      inString = !inString;
      current += char;
    } else if (inString) {
      current += char;
    } else if (char.match(/\s/)) {
      if (current) {
        tokens.push(current);
        current = '';
      }
    } else if (['(', ')', '\\', '=', '+', '-', '*', '/'].includes(char)) {
      if (current) {
        tokens.push(current);
        current = '';
      }
      tokens.push(char);
    } else {
      current += char;
    }
  }
  
  if (current) {
    tokens.push(current);
  }
  
  return tokens.filter(token => token.trim());
}

// Simple expression evaluator for basic arithmetic and lambda calculus
function evaluateExpression(tokens: string[], env: Environment): any {
  if (tokens.length === 0) return null;
  
  // Handle numbers
  if (tokens.length === 1) {
    const token = tokens[0];
    if (!isNaN(Number(token))) {
      return Number(token);
    }
    if (env[token] !== undefined) {
      return env[token];
    }
    return token;
  }
  
  // Handle basic arithmetic
  if (tokens.length === 3) {
    const [left, op, right] = tokens;
    const leftVal = !isNaN(Number(left)) ? Number(left) : (env[left] || 0);
    const rightVal = !isNaN(Number(right)) ? Number(right) : (env[right] || 0);
    
    switch (op) {
      case '+': return leftVal + rightVal;
      case '-': return leftVal - rightVal;
      case '*': return leftVal * rightVal;
      case '/': return rightVal !== 0 ? leftVal / rightVal : 'Division by zero';
      default: return `${leftVal} ${op} ${rightVal}`;
    }
  }
  
  return tokens.join(' ');
}

// Parse and evaluate L language code
function parseAndEvaluate(code: string): LangResult {
  try {
    const lines = code.split('\n').map(line => line.trim()).filter(line => line);
    const env: Environment = {};
    const results: string[] = [];
    let lastResult: any = null;
    
    for (const line of lines) {
      // Handle variable assignments
      if (line.includes('=') && !line.includes('\\')) {
        const [varName, expression] = line.split('=').map(s => s.trim());
        const tokens = tokenize(expression);
        const value = evaluateExpression(tokens, env);
        env[varName] = value;
        results.push(`${varName} = ${value}`);
        lastResult = value;
      }
      // Handle lambda expressions (simplified)
      else if (line.includes('\\')) {
        const parts = line.split('->');
        if (parts.length === 2) {
          const param = parts[0].replace('\\', '').trim();
          const body = parts[1].trim();
          const lambdaFunc = `λ${param}.${body}`;
          results.push(lambdaFunc);
          lastResult = lambdaFunc;
        }
      }
      // Handle let expressions
      else if (line.startsWith('let')) {
        const letMatch = line.match(/let\s+(\w+)\s*=\s*(.+?)\s+in\s+(.+)/);
        if (letMatch) {
          const [, varName, value, expression] = letMatch;
          const tempEnv = { ...env };
          const tokens = tokenize(value);
          tempEnv[varName] = evaluateExpression(tokens, env);
          const exprTokens = tokenize(expression);
          const result = evaluateExpression(exprTokens, tempEnv);
          results.push(`let ${varName} = ${tempEnv[varName]} in ${expression} = ${result}`);
          lastResult = result;
        }
      }
      // Handle function calls and expressions
      else {
        const tokens = tokenize(line);
        const result = evaluateExpression(tokens, env);
        results.push(`${line} = ${result}`);
        lastResult = result;
      }
    }
    
    return {
      ast: JSON.stringify({ 
        parsed: lines,
        environment: env,
        steps: results
      }, null, 2),
      output: String(lastResult || results[results.length - 1] || '')
    };
  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : String(error);
    return {
      ast: JSON.stringify({ error: errorMessage }, null, 2),
      output: `Error: ${errorMessage}`
    };
  }
}

// Mock API function that mimics the Haskell server
export async function mockEvaluate(code: string): Promise<LangResult> {
  // Simulate network delay
  await new Promise(resolve => setTimeout(resolve, 100 + Math.random() * 400));
  
  if (!code.trim()) {
    throw new Error('Empty code provided');
  }
  
  return parseAndEvaluate(code);
}

// Check if we're in standalone mode
export const isStandaloneMode = (): boolean => {
  return import.meta.env.MODE === 'standalone' || 
         import.meta.env.VITE_STANDALONE === 'true' ||
         !navigator.onLine;
};