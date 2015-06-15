module Semantics where

data Reduction = Reduction Name Name Process
               | ToToTodo

piDerivations :: [Def] -> Process -> a
piDerivations = error "piDerivations TODO"

piReductions :: [Def] -> Process -> [Reduction]
piReductions = error "piReductions TODO"
