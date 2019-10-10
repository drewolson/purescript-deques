module Data.Deque.BankersDeque
  ( BankersDeque(..)
  , cons
  , length
  , empty
  , empty'
  , head
  , init
  , last
  , snoc
  , tail
  ) where

import Prelude

import Data.Deque (class Deque, fromFoldable, toList)
import Data.List as StrictList
import Data.List.Lazy (List, Step(..), (:))
import Data.List.Lazy as List
import Data.Maybe (Maybe(..))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)

data BankersDeque a = BankersDeque Int Int (List a) Int (List a)

instance eqBankersDeque ∷ Eq a ⇒ Eq (BankersDeque a) where
  eq ∷ BankersDeque a → BankersDeque a → Boolean
  eq as bs = eq (toList as) (toList bs)

instance showBankersDeque ∷ Show a ⇒ Show (BankersDeque a) where
  show ∷ BankersDeque a → String
  show d = "(fromFoldable " <> show (toList d) <> ")"

instance arbitraryBankersDeque ∷ Arbitrary a ⇒ Arbitrary (BankersDeque a) where
  arbitrary ∷ Gen (BankersDeque a)
  arbitrary = fromFoldable <$> (arbitrary ∷ Gen (StrictList.List a))

instance dequeBankersDeque ∷ Deque BankersDeque where
  empty = empty
  isEmpty = isEmpty
  cons = cons
  head = head
  tail = tail
  snoc = snoc
  last = last
  init = init

empty ∷ ∀ a. BankersDeque a
empty = empty' 2

empty' ∷ ∀ a. Int → BankersDeque a
empty' c = BankersDeque c 0 List.nil 0 List.nil

isEmpty ∷ ∀ a. BankersDeque a → Boolean
isEmpty (BankersDeque _ lenf _ lenr _) = lenf + lenr == 0

length ∷ ∀ a. BankersDeque a → Int
length (BankersDeque _ lenf _ lenr _) = lenf + lenr

cons ∷ ∀ a. a → BankersDeque a → BankersDeque a
cons a (BankersDeque c lenf f lenr r) =
  check $ BankersDeque c (lenf + 1) (a : f) lenr r

head ∷ ∀ a. BankersDeque a → Maybe a
head (BankersDeque _ _ f _ r) =
  case List.step f, List.step r of
    Nil, Nil -> Nothing
    Nil, (Cons a _) -> Just a
    (Cons a _), _ -> Just a

tail ∷ ∀ a. BankersDeque a → Maybe (BankersDeque a)
tail (BankersDeque c lenf f lenr r) =
  case List.step f, List.step r of
    Nil, Nil → Nothing
    Nil, (Cons _ _) → Just $ empty' c
    (Cons _ f'), _ → Just $ check $ BankersDeque c (lenf - 1) f' lenr r

snoc ∷ ∀ a. a → BankersDeque a → BankersDeque a
snoc a (BankersDeque c lenf f lenr r) =
  check $ BankersDeque c lenf f (lenr + 1) (a : r)

last ∷ ∀ a. BankersDeque a → Maybe a
last (BankersDeque _ _ f _ r) =
  case List.step f, List.step r of
    Nil, Nil → Nothing
    (Cons a _), Nil → Just a
    _, (Cons a _) → Just a

init ∷ ∀ a. BankersDeque a → Maybe (BankersDeque a)
init (BankersDeque c lenf f lenr r) =
  case List.step f, List.step r of
    Nil, Nil → Nothing
    (Cons _ _), Nil → Just $ empty' c
    _, (Cons _ r') → Just $ check $ BankersDeque c lenf f (lenr - 1) r'

check ∷ ∀ a. BankersDeque a → BankersDeque a
check q@(BankersDeque c lenf f lenr r) =
  if lenf > c * lenr + 1
  then
    let i = (lenf + lenr) `div` 2
        j = lenf + lenr - i
        f' = List.take i f
        r' = r <> List.reverse (List.drop i f)
     in BankersDeque c i f' j r'
  else if lenr > c * lenf + 1
  then
    let j = (lenf + lenr) `div` 2
        i = lenf + lenr - j
        r' = List.take j r
        f' = f <> List.reverse (List.drop j r)
     in BankersDeque c i f' j r'
  else q
