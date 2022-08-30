module tut1 where


data Boolean : Set where
  true : Boolean
  false : Boolean

data Empty : Set where

mybool : Boolean
mybool = true

identity : Boolean -> Boolean
identity true = true
identity false = false

identity2 : Boolean -> Boolean
identity2 x = x

not : Boolean -> Boolean
not true = false
not false = true
