module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Data.List
import Data.Maybe
import Data.Array
import Data.Array.Partial (head, tail)
import Partial.Unsafe (unsafePartial)

-- "Normal" recursive function. No tail recursion optimisation
count :: forall a. (a -> Boolean) -> Array a -> Int
count _ [] = 0
count p xs = if p (unsafePartial head xs)
              then count p (unsafePartial tail xs) + 1
              else count p (unsafePartial tail xs)

-- Tail Recursive Optimised implementation
countTR :: forall a. (a -> Boolean) -> Array a -> Int
countTR = countTR' 0
  where
    countTR' acc _ []  = acc
    countTR' acc p xs = if p (unsafePartial head xs) 
                          then countTR' (acc + 1) p (unsafePartial tail xs)
                          else countTR' acc p (unsafePartial tail xs)

-- Combine List
combineList :: forall f a. Applicative f => List (f a) -> f (List a)
combineList Nil = pure Nil
combineList (Cons head tail) = Cons <$> head <*> combineList tail

-- Combine Maybe
combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Just x = Just <$> x 
combineMaybe Nothing = pure Nothing


-- Try this
conCon :: forall a. List a -> List a
conCon Nil = Nil
conCon (Cons a ax) = ax


main :: Effect Unit
main = do
  log "Hello sailor!"

