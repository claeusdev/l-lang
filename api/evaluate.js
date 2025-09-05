// Vercel serverless function that emulates the Haskell API
// This provides the same interface as the Haskell server for the frontend

const corsHeaders = {
  'Access-Control-Allow-Origin': '*',
  'Access-Control-Allow-Methods': 'GET, POST, PUT, DELETE, OPTIONS',
  'Access-Control-Allow-Headers': 'Content-Type, Authorization',
};

// Simple L language interpreter implementation
class LInterpreter {
  constructor() {
    this.env = new Map();
    this.initializeBuiltins();
  }

  initializeBuiltins() {
    // Add basic arithmetic operations
    this.env.set('+', (a, b) => a + b);
    this.env.set('-', (a, b) => a - b);
    this.env.set('*', (a, b) => a * b);
    this.env.set('/', (a, b) => Math.floor(a / b));
    
    // Add comparison operations
    this.env.set('==', (a, b) => a === b);
    this.env.set('!=', (a, b) => a !== b);
    this.env.set('<', (a, b) => a < b);
    this.env.set('>', (a, b) => a > b);
    this.env.set('<=', (a, b) => a <= b);
    this.env.set('>=', (a, b) => a >= b);
  }

  parseDefinition(line) {
    const defMatch = line.match(/^(\w+)\s*=\s*(.+)$/);
    if (defMatch) {
      const [, name, expr] = defMatch;
      return { type: 'definition', name: name.trim(), expression: expr.trim() };
    }
    return null;
  }

  evaluate(expr) {
    try {
      // Handle numbers
      if (/^\d+$/.test(expr)) {
        return { value: parseInt(expr), trace: [`Evaluated number: ${expr}`] };
      }

      // Handle variable references
      if (/^\w+$/.test(expr)) {
        if (this.env.has(expr)) {
          const value = this.env.get(expr);
          return { 
            value: typeof value === 'function' ? `[Function: ${expr}]` : value, 
            trace: [`Variable reference: ${expr} = ${value}`] 
          };
        }
        throw new Error(`Undefined variable: ${expr}`);
      }

      // Handle function calls
      const callMatch = expr.match(/^(\w+)\s+(.+)$/);
      if (callMatch) {
        const [, funcName, args] = callMatch;
        if (this.env.has(funcName)) {
          const func = this.env.get(funcName);
          if (typeof func === 'function') {
            const argValues = this.parseArguments(args);
            const result = func(...argValues);
            return { 
              value: result, 
              trace: [`Function call: ${funcName}(${argValues.join(', ')}) = ${result}`] 
            };
          }
        }
        throw new Error(`Undefined function: ${funcName}`);
      }

      // Handle arithmetic expressions
      const arithMatch = expr.match(/^(.+)\s*([+\-*/])\s*(.+)$/);
      if (arithMatch) {
        const [, left, op, right] = arithMatch;
        const leftResult = this.evaluate(left.trim());
        const rightResult = this.evaluate(right.trim());
        
        if (leftResult.error || rightResult.error) {
          throw new Error(leftResult.error || rightResult.error);
        }

        const leftVal = leftResult.value;
        const rightVal = rightResult.value;
        
        let result;
        switch (op) {
          case '+': result = leftVal + rightVal; break;
          case '-': result = leftVal - rightVal; break;
          case '*': result = leftVal * rightVal; break;
          case '/': result = Math.floor(leftVal / rightVal); break;
          default: throw new Error(`Unknown operator: ${op}`);
        }

        return {
          value: result,
          trace: [
            ...leftResult.trace,
            ...rightResult.trace,
            `Arithmetic: ${leftVal} ${op} ${rightVal} = ${result}`
          ]
        };
      }

      // Handle lambda expressions (simplified)
      const lambdaMatch = expr.match(/^\\(\w+)\s*->\s*(.+)$/);
      if (lambdaMatch) {
        const [, param, body] = lambdaMatch;
        return {
          value: `[Lambda: ${param} -> ${body}]`,
          trace: [`Lambda expression: \\${param} -> ${body}`]
        };
      }

      throw new Error(`Cannot evaluate expression: ${expr}`);
    } catch (error) {
      return { error: error.message, trace: [`Error: ${error.message}`] };
    }
  }

  parseArguments(args) {
    // Simple argument parsing - split by spaces and evaluate each
    return args.split(/\s+/).map(arg => {
      const result = this.evaluate(arg);
      return result.error ? 0 : result.value;
    });
  }

  processLine(line) {
    const trimmed = line.trim();
    if (!trimmed) {
      return { output: '', ast: null, trace: [] };
    }

    const definition = this.parseDefinition(trimmed);
    if (definition) {
      const result = this.evaluate(definition.expression);
      if (result.error) {
        return { 
          output: `Error in definition '${definition.name}': ${result.error}`, 
          ast: null, 
          trace: result.trace 
        };
      }
      
      this.env.set(definition.name, result.value);
      return { 
        output: `Defined (rec): ${definition.name}`, 
        ast: `Definition: ${definition.name} = ${definition.expression}`, 
        trace: result.trace 
      };
    }

    const result = this.evaluate(trimmed);
    if (result.error) {
      return { 
        output: `Error: ${result.error}`, 
        ast: null, 
        trace: result.trace 
      };
    }

    return { 
      output: result.value.toString(), 
      ast: `Expression: ${trimmed}`, 
      trace: result.trace 
    };
  }
}

export default async function handler(req, res) {
  // Handle CORS preflight
  if (req.method === 'OPTIONS') {
    res.status(200).set(corsHeaders).end();
    return;
  }

  if (req.method !== 'POST') {
    res.status(405).set(corsHeaders).json({ error: 'Method not allowed' });
    return;
  }

  try {
    const { code } = req.body;
    if (!code) {
      res.status(400).set(corsHeaders).json({ error: 'No code provided' });
      return;
    }

    const interpreter = new LInterpreter();
    const lines = code.split('\n');
    const steps = [];
    const allTraces = [];

    for (const line of lines) {
      const result = interpreter.processLine(line);
      if (result.output || result.ast) {
        steps.push({
          output: result.output,
          ast: result.ast
        });
      }
      allTraces.push(...result.trace);
    }

    const response = {
      steps,
      finalError: null,
      finalEnvironment: Object.fromEntries(interpreter.env),
      traceLog: allTraces
    };

    res.status(200).set(corsHeaders).json(response);
  } catch (error) {
    console.error('Evaluation error:', error);
    res.status(500).set(corsHeaders).json({ 
      error: 'Internal server error',
      details: error.message 
    });
  }
}