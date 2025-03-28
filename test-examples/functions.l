# Function definitions
func square x = x * x
func twice f x = f (f x)

twice square 3
# Expected result: 81
