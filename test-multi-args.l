# Multi-argument function

let compose = lambda f. lambda g. lambda x. f (g x) in
let double = lambda x. x * 2 in
let increment = lambda x. x + 1 in
compose double increment 5
