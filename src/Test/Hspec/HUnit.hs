{-# OPTIONS -XFlexibleInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :  Test.Hspec.HUnit
-- Copyright   :  (c) Trystan Spangler 2011
-- License     :  modified BSD
--
-- Maintainer  : trystan.s@comcast.net
-- Stability   : experimental
-- Portability : portable
--
-----------------------------------------------------------------------------


-- | Importing this module allows you to use an HUnit test case as an example
-- for a requirement. You can use an explicit TestCase data constructor or
-- use an IO() action. For an IO() action, any exception means the example
-- failed; otherwise, it's successfull.
--
-- > describe "cutTheDeck" [
-- >   it "puts the first half of a list after the last half"
-- >      (TestCase $ assertEqual "cut the deck" [3,4,1,2] (cutTheDeck [1,2,3,4])),
-- >
-- >   it "restores an even sized list when cut twice"
-- >      (assertEqual "cut the deck twice" [3,4,1,2] (cutTheDeck (cutTheDeck [1,2,3,4]))),
-- >   ]
-- >
module Test.Hspec.HUnit (
) where

import Test.Hspec.Internal
import qualified Test.HUnit as HU

instance SpecVerifier (IO ()) where
  it n t = it n (HU.TestCase t)

instance SpecVerifier HU.Test where
  it n t = do
    (counts, fails) <- HU.runTestText HU.putTextToShowS t
    let rows = lines (fails "")
    if HU.errors counts + HU.failures counts == 0
      then return (n, Success)
      else return (n, Fail (rows !! 1))

