// Sample L Language Code Snippets
// These demonstrate valid L language syntax and common patterns

export interface SampleSnippet {
  title: string;
  code: string;
  description: string;
}

export const sampleSnippets: SampleSnippet[] = [
  {
    title: "Basic Arithmetic",
    description: "Simple arithmetic operations",
    code: `x = 10
y = 20
x + y
x * y
x - y
y / x`
  },
  {
    title: "Lambda Functions",
    description: "Basic lambda function definitions",
    code: `double = \\x -> x * 2
triple = \\x -> x * 3
square = \\x -> x * x

double 5
triple 4
square 6`
  },
  {
    title: "Function Composition", 
    description: "Composing functions together",
    code: `double = \\x -> x * 2
triple = \\x -> x * 3
compose = \\f -> \\g -> \\x -> f (g x)

doubleTriple = compose double triple
doubleTriple 5`
  },
  {
    title: "Let Expressions",
    description: "Local variable binding with let expressions",
    code: `double = \\x -> x * 2
let x = 10 in double x
let y = 5 in let z = y + 3 in z * 2`
  },
  {
    title: "Recursive Functions",
    description: "Examples of recursive function patterns",
    code: `factorial = \\n -> n == 0 ? 1 : n * factorial (n - 1)
fibonacci = \\n -> n <= 1 ? n : fibonacci (n - 1) + fibonacci (n - 2)

factorial 5
fibonacci 8`
  },
  {
    title: "Higher-Order Functions",
    description: "Functions that operate on other functions",
    code: `map = \\f -> \\xs -> [f x | x <- xs]
filter = \\p -> \\xs -> [x | x <- xs, p x]
reduce = \\f -> \\acc -> \\xs -> fold f acc xs

double = \\x -> x * 2
isEven = \\x -> x % 2 == 0

map double [1, 2, 3, 4, 5]
filter isEven [1, 2, 3, 4, 5, 6]`
  },
  {
    title: "Complex Composition",
    description: "Advanced function composition example",
    code: `double = \\x -> x * 2
triple = \\x -> x * x * x
compose = \\f -> \\g -> \\x -> f (g x)

composedDoubleTriple = compose double triple
tripledDoubledComposed = composedDoubleTriple triple

tripledDoubledComposed 5

let x = 100 in tripledDoubledComposed (double 10)`
  },
  {
    title: "Conditional Logic",
    description: "Using conditional expressions",
    code: `abs = \\x -> x < 0 ? -x : x
max = \\x -> \\y -> x > y ? x : y
min = \\x -> \\y -> x < y ? x : y

abs (-10)
max 15 20
min 15 20`
  },
  {
    title: "Curried Functions",
    description: "Examples of curried function applications",
    code: `add = \\x -> \\y -> x + y
multiply = \\x -> \\y -> x * y

addFive = add 5
multiplyByTwo = multiply 2

addFive 10
multiplyByTwo 8

let partial = add 3 in partial 7`
  },
  {
    title: "Mathematical Functions",
    description: "Common mathematical operations",
    code: `power = \\base -> \\exp -> base ^ exp
sqrt = \\x -> x ^ 0.5
cube = \\x -> x * x * x

power 2 8
sqrt 16
cube 4

let base = 3 in let exp = 4 in power base exp`
  }
];

// Get a random sample snippet
export const getRandomSnippet = (): SampleSnippet => {
  const randomIndex = Math.floor(Math.random() * sampleSnippets.length);
  return sampleSnippets[randomIndex];
};

// Get snippets by category/complexity
export const getBasicSnippets = (): SampleSnippet[] => {
  return sampleSnippets.slice(0, 4);
};

export const getAdvancedSnippets = (): SampleSnippet[] => {
  return sampleSnippets.slice(4);
};