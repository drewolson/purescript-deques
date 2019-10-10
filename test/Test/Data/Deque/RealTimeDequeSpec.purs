module Test.Data.Deque.RealTimeDequeSpec
  ( spec
  ) where

import Prelude

import Data.Deque as Deque
import Data.Deque.RealTimeDeque (RealTimeDeque)
import Data.Deque.RealTimeDeque as RealTimeDeque
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Test.QuickCheck ((==?))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

spec ∷ Spec Unit
spec = do
  describe "RealTimeDeque" do
    it "tail <<< cons returns the initial deque" do
      quickCheck \(deque ∷ RealTimeDeque Int) i →
        (Deque.tail $ Deque.cons i $ deque) ==? (Just deque)

    it "init <<< snoc returns the initial deque" do
      quickCheck \(deque ∷ RealTimeDeque Int) i →
        (Deque.init $ Deque.snoc i $ deque) ==? (Just deque)

    it "head <<< cons a returns a" do
      quickCheck \(deque ∷ RealTimeDeque Int) i →
        (Deque.head $ Deque.cons i $ deque) ==? (Just i)

    it "last <<< snoc a returns a" do
      quickCheck \(deque ∷ RealTimeDeque Int) i →
        (Deque.last $ Deque.snoc i $ deque) ==? (Just i)

    it "toList returns a list of the same length as the deque" do
      quickCheck \(deque ∷ RealTimeDeque Int) →
        (RealTimeDeque.length deque) ==? (List.length $ Deque.toList deque)

    it "fromFoldable and toList round trip correctly" do
      quickCheck \(list ∷ List Int) →
        let deque = Deque.fromFoldable list ∷ RealTimeDeque Int
         in Deque.toList deque ==? list
