-- Define a module (name must be different from file name)
module AgdaFromNothing2017 where

-- Define a type with constructors
data Bool : Set where
  true : Bool
  false : Bool

-- Define a variable (constant)
mybool : Bool
mybool = true

-- Define a function using pattern matching
not : Bool -> Bool
not true = false
not false = true

-- Define a function with infix notation and named arguments
_∧_ : (x : Bool) -> (y : Bool) -> Bool
true ∧ y = y
false ∧ y = false

-- Define a recursive type
data Nat : Set where
  zero : Nat
  suc : Nat -> Nat

-- Import a builtin type into the namespace
{-# BUILTIN NATURAL Nat #-}

myNat : Nat
myNat = 2

-- Define a function using pattern matching and recursion
_<=_ : (x y : Nat) -> Bool
zero <= y = true
suc x <= zero = false
suc x <= suc y = x <= y

-- Define a recursive type with right-associative infix notation.
data List : Set where
  nil : List
  _::_ : Nat -> List -> List
infixr 5 _::_

myList : List
myList = 6 :: 3 :: nil

-- Define a recursive type with named constructor parameters
data Tree : Set where
  leaf : Tree
  node : (p : Nat) (lt tr : Tree) -> Tree

myTree : Tree
myTree = node 3 (node 1 leaf leaf) (node 5 leaf leaf)

-- append implementation
append : List -> List -> List
append nil ys = ys
append (x :: xs) ys = x :: append xs ys

-- type conversion implementation
toList : Tree -> List
toList leaf = nil
toList (node p lt rt) = append (toList lt) (p :: toList rt)

-- Define a function using recursion and with clause
insert : Nat -> Tree -> Tree
insert p leaf = node p leaf leaf
insert p (node x lt rt) with p <= x
insert p (node x lt rt) | true = node x (insert p lt) rt
insert p (node x lt rt) | false = node x lt (insert p rt)

fromList : List -> Tree
fromList nil = leaf
fromList (x :: xs) = insert x (fromList xs)

-- fold right implementation showing parametric types and type inference
foldr : {X : Set} -> (Nat -> X -> X) -> X -> List -> X
foldr f d nil = d
foldr f d (x :: xs) = f x (foldr f d xs)

-- Sort implementation
sort : List -> List
sort xs = toList (foldr insert leaf xs)

