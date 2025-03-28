# Higher-order functions
func apply f x = f x
func add1 x = x + 1
func mul2 x = x * 2

apply add1 (apply mul2 10)
# Expected result: 21
