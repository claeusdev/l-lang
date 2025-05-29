double = \x -> x * 2

triple = \x -> x * x * x

compose = \f -> \g -> \x -> f (g x)

composedDoubleTripple = compose double

trippledDoubledComposed = composedDoubleTripple triple

trippledDoubledComposed 5

let x = 100 in trippledDoubledComposed (double 10)
