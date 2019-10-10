module Data.Deque.RealTimeDeque
  ( RealTimeDeque(..)
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
import Data.Lazy (defer)
import Data.List as StrictList
import Data.List.Lazy (List(..), Step(..), (:))
import Data.List.Lazy as List
import Data.Maybe (Maybe(..))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)

data RealTimeDeque a = RealTimeDeque Int Int (List a) (List a) Int (List a) (List a)

instance eqRealTimeDeque ∷ Eq a ⇒ Eq (RealTimeDeque a) where
  eq ∷ RealTimeDeque a → RealTimeDeque a → Boolean
  eq as bs = eq (toList as) (toList bs)

instance showRealTimeDeque ∷ Show a ⇒ Show (RealTimeDeque a) where
  show ∷ RealTimeDeque a → String
  show d = "(fromFoldable " <> show (toList d) <> ")"

instance arbitraryRealTimeDeque ∷ Arbitrary a ⇒ Arbitrary (RealTimeDeque a) where
  arbitrary ∷ Gen (RealTimeDeque a)
  arbitrary = fromFoldable <$> (arbitrary ∷ Gen (StrictList.List a))

instance dequeRealTimeDeque ∷ Deque RealTimeDeque where
  empty = empty
  isEmpty = isEmpty
  cons = cons
  head = head
  tail = tail
  snoc = snoc
  last = last
  init = init

empty ∷ ∀ a. RealTimeDeque a
empty = empty' 2

empty' ∷ ∀ a. Int → RealTimeDeque a
empty' c = RealTimeDeque c 0 List.nil List.nil 0 List.nil List.nil

isEmpty ∷ ∀ a. RealTimeDeque a → Boolean
isEmpty = (_ == 0) <<< length

length ∷ ∀ a. RealTimeDeque a → Int
length (RealTimeDeque _ lenf _ _ lenr _ _) = lenf + lenr

cons ∷ ∀ a. a → RealTimeDeque a → RealTimeDeque a
cons a (RealTimeDeque c lenf f sf lenr r sr) =
  check $ RealTimeDeque c (lenf + 1) (a : f) (exec1 sf) lenr r (exec1 sr)

head ∷ ∀ a. RealTimeDeque a → Maybe a
head (RealTimeDeque _ _ f _ _ r _) =
  case List.step f, List.step r of
    Nil, Nil → Nothing
    Nil, Cons x _ → Just x
    Cons x _, _ → Just x

tail ∷ ∀ a. RealTimeDeque a → Maybe (RealTimeDeque a)
tail (RealTimeDeque c lenf f sf lenr r sr) =
  case List.step f, List.step r of
    Nil, Nil → Nothing
    Nil, Cons x _ → Just empty
    Cons _ f', _ →
      Just $ check $ RealTimeDeque c (lenf - 1) f' (exec2 sf) lenr r (exec2 sr)

snoc ∷ ∀ a. a → RealTimeDeque a → RealTimeDeque a
snoc a (RealTimeDeque c lenf f sf lenr r sr) =
  check $ RealTimeDeque c lenf f (exec1 sf) (lenr + 1) (a : r) (exec1 sr)

last ∷ ∀ a. RealTimeDeque a → Maybe a
last (RealTimeDeque _ _ f _ _ r _) =
  case List.step f, List.step r of
    Nil, Nil → Nothing
    Cons x _, Nil → Just x
    _, Cons x _ → Just x

init ∷ ∀ a. RealTimeDeque a → Maybe (RealTimeDeque a)
init (RealTimeDeque c lenf f sf lenr r sr) =
  case List.step f, List.step r of
    Nil, Nil → Nothing
    Cons s _, Nil → Just empty
    _, Cons _ r' →
      Just $ check $ RealTimeDeque c lenf f (exec2 sf) (lenr - 1) r' (exec2 sr)

rotateRev ∷ ∀ a. Int → List a → List a → List a → List a
rotateRev c f r a =
  case List.step f of
    Nil → List.reverse r <> a
    Cons x f' →
      List $ defer \_ →
        Cons x $ rotateRev c f' (List.drop c r) (List.reverse (List.take c r) <> a)

rotateDrop ∷ ∀ a. Int → List a → Int → List a → List a
rotateDrop c f j r =
  if j < c
  then rotateRev c f (List.drop j r) List.nil
  else
    case List.step f of
      Nil → List.nil
      Cons x f' →
        List $ defer \_ →
          Cons x $ rotateDrop c f' (j - c) (List.drop c r)

check ∷ ∀ a. RealTimeDeque a → RealTimeDeque a
check d@(RealTimeDeque c lenf f sf lenr r sr) =
  if lenf > c * lenr + 1
  then
    let i = lenf + lenr `div` 2
        j = lenf + lenr - i
        f' = List.take i f
        r' = rotateDrop c r i f
     in RealTimeDeque c i f' f' j r' r'
  else if lenr > c * lenf + 1
  then
    let j = lenf + lenr `div` 2
        i = lenf + lenr - j
        r' = List.take j r
        f' = rotateDrop c f j r
     in RealTimeDeque c i f' f' j r' r'
  else d

exec1 ∷ ∀ a. List a → List a
exec1 list =
  case List.step list of
    Cons _ rest → rest
    Nil → List.nil

exec2 ∷ ∀ a. List a → List a
exec2 = exec1 <<< exec1
