module Test.FileSpec where

import Test.Prelude

import Test.Spec (before_)
import Webb.Writer (assertProjectRoot, resetDir)


dir :: String
dir =  "test/Test/FileSpecDir"

spec :: Spec Unit
spec = describe "File operations" do
  before_ assertProjectRoot do 
    it "hello" do 
      resetDir dir
