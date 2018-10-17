module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Data.List as List
import Data.Maybe
import Data.Array 
import Data.Array.Partial (head, tail)
import Partial.Unsafe (unsafePartial)
import Control.Plus(empty)
 

-- "Normal" recursive function. No tail recursion optimisation
-- Counts how many values in array evaluate to true
count :: forall a. (a -> Boolean) -> Array a -> Int
count _ [] = 0
count p xs = case p (unsafePartial head xs) of
              True  -> count p (unsafePartial tail xs) + 1
              False -> count p (unsafePartial tail xs)

-- Tail Recursive Optimised implementation
countTR :: forall a. (a -> Boolean) -> Array a -> Int
countTR = countTR' 0
  where
    countTR' acc _ []  = acc
    countTR' acc p xs = case p (unsafePartial head xs) of
                          True  -> countTR' (acc + 1) p (unsafePartial tail xs)
                          False -> countTR' acc p (unsafePartial tail xs)

-- How do notation relates to binding
-- First, the do notation
doFindPairsEqualTo :: Int -> Array (Array Int)
doFindPairsEqualTo x = do
    i <- 1 .. 6
    j <- 1 .. 6
    if i + j == x
        then pure [i,j]
        else empty

-- Using bind notation
bindFindPairsEqualTo :: Int -> Array (Array Int)
bindFindPairsEqualTo x =  1 .. 6 >>=
                   (\i -> 1 .. 6 >>=
                   (\j -> if i + j == x
                           then pure [i,j]
                           else empty))

-- Combining effects!!
-- Combine List implementation
combineList :: forall f a. Applicative f => List.List (f a) -> f (List.List a)
combineList List.Nil = pure List.Nil
combineList (List.Cons head tail) = List.Cons <$> head <*> combineList tail

-- Combine Maybe implementation
combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe (Just x) = Just <$> x 
combineMaybe Nothing = pure Nothing


-- Stuff I still don't feel I know very well...
-- Effects
-- Monad Traversal
-- State, Reader, Writer monads
-- The Free datatype
-- FreeMonads
-- Lenses, Traversals, etc.
data Free f a = Pure f a
              | Impure (f (Free f a))


main :: Effect Unit
main = do
  log "Hello sailor!"

