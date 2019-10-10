module Data.Deque
  ( class Deque
  , empty
  , isEmpty
  , cons
  , head
  , tail
  , snoc
  , last
  , init
  , fromFoldable
  , toList
  ) where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))

class Deque d where
  empty ∷ ∀ a. d a
  isEmpty ∷ ∀ a. d a → Boolean
  cons ∷ ∀ a. a → d a → d a
  head ∷ ∀ a. d a → Maybe a
  tail ∷ ∀ a. d a → Maybe (d a)
  snoc ∷ ∀ a. a → d a → d a
  last ∷ ∀ a. d a → Maybe a
  init ∷ ∀ a. d a → Maybe (d a)

fromFoldable ∷ ∀ f d a. Deque d ⇒ Foldable f ⇒ f a → d a
fromFoldable = foldl (flip snoc) empty

toList ∷ ∀ d a. Deque d ⇒ d a → List a
toList deque =
  case head deque, tail deque of
    Nothing, _ → Nil
    Just a, Nothing → List.singleton a
    Just a, Just as → Cons a (toList as)
