module Test.FileSpec where

import Test.Prelude

import Test.Spec (before_)
import Webb.Writer (assertProjectRoot, getFile, openAndTruncate, resetDir)
import Webb.Writer.File (existsDir, write, read, countDir)

dir :: String
dir =  "testData/FileSpecDir"

spec :: Spec Unit
spec = describe "File operations" do
  before_ assertProjectRoot do 
    it "Allow reset of directory, even twice, without error." do 
      resetDir dir
      resetDir dir
      existsDir dir ?= true
      
    it "reset eliminates files" do
      resetDir dir
      countDir dir ?= 0

      file <- getFile dir "file.txt"
      writeFile file "hello"
      countDir dir ?= 1
      
      resetDir dir
      countDir dir ?= 0
      
    it "add, open, and write file" do
      resetDir dir
      file <- getFile dir "file.txt"
      writeFile file "hello"
      read file ?= "hello"

    it "overwrite file" do
      resetDir dir
      file <- getFile dir "file.txt"
      writeFile file "hello"
      
      file2 <- getFile dir "file.txt"
      writeFile file2 "goodbye"
      read file ?= "goodbye"

  where
  writeFile file str = do
    openAndTruncate file
    write file str