# Factorial implementation
# Note: This requires if-then-else which is commented out in your AST
# This example won't work until you uncomment that part
func factorial n =
  # if n = 0 then 1 else n * factorial (n - 1)
  # Since conditional is commented out, we'll just use a direct calculation
  n * n # placeholder

factorial 5
# Expected result (with proper factorial): 120
# Expected with placeholder: 25
