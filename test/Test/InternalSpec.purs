module Test.InternalSpec where

import Test.Prelude

import Data.Map as M
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Webb.Writer.Internal as I
import Webb.Writer.String (trimMargin)


spec :: Spec Unit
spec = describe "Testing internals of writing" do
  run "view output" do
    outputIs ""
    posIs I.defaultPos
    
  run "simple write" do 
    I.write "abc"
    outputIs "abc"
    posIs $ I.defaultPos { column = 3 }
    
  run "write a newline" do
    I.write "abc\n"
    outputIs "abc\n"
    posIs $ I.defaultPos { column = 0, line = 1 }

  run "write multiple newlines" do
    I.write "\na\nb\n"
    outputIs "\na\nb\n"
    posIs $ I.defaultPos { 
      column = 0
    , line = 3
    }
    
  run "ensure single-line indent" do
    I.withIndent 2 do
      I.write "abc"
    outputIs "  abc"
    posIs $ I.defaultPos {
      indentSpaces = 0
    , line = 0
    , column = 5 -- We were indented
    }

  run "indent with newline" do
    I.withIndent 2 do
      I.write "abc\n"
    outputIs "  abc\n"
    posIs $ I.defaultPos {
      indentSpaces = 0
    , line = 1
    , column = 0 -- Newline goes to 0, until attempt to write.
    }
    
  run "ensure multi-line, nested indent" do
    I.withIndent 2 do
      I.write "a\n"
      I.withIndent 2 do
        I.write "b\n"
      I.write "c"
      
    outputIs $ trimMargin """
      |  a
      |    b
      |  c
      """
      
    posIs $ I.defaultPos {
      indentSpaces = 0
    , line = 2
    , column = 3 -- We were indented
    }
    
  run "token adds a space if the previous character was a non-space" do
    I.tokens ["a", "b", "c"]
    outputIs "a b c"

    posIs $ I.defaultPos {
      indentSpaces = 0
    , line = 0
    , column = 5
    }
    
  run "token adds a space after start of line" do
    I.write "a"
    I.token "b"
    outputIs "a b"
    
    posIs $ I.defaultPos {
      indentSpaces = 0
    , line = 0
    , column = 3 
    }
    
  run "token will not add space at start of line" do
    I.withIndent 2 do
      I.token "a\n"
      I.withIndent 2 do
        I.token "b\n"
        I.token "c"
        I.token "d"
        
    outputIs $ trimMargin """
      |  a
      |    b
      |    c d
      """
      
    posIs $ I.defaultPos {
      indentSpaces = 0
    , line = 2
    , column = 7
    }
      
  run "can add a newline safely" do
    I.newline
    I.newline
    outputIs $ "\n\n"
      
  run "can put a map as a record" do
    let map = M.fromFoldable 
          [ "name" /\ "String"
          , "age" /\ "Int"
          ]
    I.putRecord map
    
    outputIs $ trimMargin """
      |{ age : Int
      |, name : String
      |}
      """
      
  run "writing a type" do
    I.putType "hello" ["a", "b"]
    outputIs "hello a b"
    
  run "module declaration" do
    I.moduleWhere "Test.InternalSpec"
    outputIs "module Test.InternalSpec where"

  run "import" do
    I.putImport "Test.InternalSpec as P"
    outputIs "import Test.InternalSpec as P"
    
  run "string literal escapes newline" do
    I.string "hello\nthere"
    outputIs "\"hello\\nthere\""
    
  run "multi-line string literal retains newline" do
    I.multiLine "hello\nthere"
    outputIs $ "\"\"\"hello\nthere\"\"\""
  
  run "various 'equals' output" do
    I.equals *> new
    I.typeEquals "hello" *> new
    I.bodyEquals "hello" ["a", "b"] *> new
    I.newtypeEquals "hello" ["a", "b"] *> new
    I.dataEquals "hello" ["a", "b"]
    
    outputIs $ trimMargin """
      |=
      |type hello =
      |hello a b =
      |newtype hello a b =
      |data hello a b =
      """
    
  where
  run msg prog = 
    let p = (do 
          void $ I.execWriterM I.default do void prog
        ) :: Aff Unit
    in it msg p
    
  outputIs expected = do
    output <- I.getOutput
    output === expected -- actual == expected

  posIs expected = do
    pos <- I.getPosition
    pos === expected
    
  new = I.newline
    
