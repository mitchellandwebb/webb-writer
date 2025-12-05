module Webb.Writer.Internal where

import Prelude

import Control.Monad.State (StateT, evalStateT, execStateT, lift, runStateT)
import Data.Array as A
import Data.Map (Map)
import Data.Map as M
import Data.String (Pattern(..), Replacement(..))
import Data.String as Str
import Data.Tuple (Tuple(..))
import Webb.State.Prelude (mmodify_, mread, mreads, mwrite)
import Webb.Writer.ForEach (after, each, forEach_, inBetween)


{- The writer monad enables us to write to the monoidal source. It has to maintain the indent level in such a way that changes after a newline. Thus, we write to a thing, and utilize the indent level. Newlines ... are special. To prevent newlines from causing a problem, though, perhaps we have to count the newlines and reset the indent level when needed.
-}


type Position = 
  { indentSpaces :: Int
  , line :: Int
  , column :: Int
  , output :: Array String
  }

type WriterM' m a = StateT Position m a
type WriterM m = WriterM' m Unit

default :: Position
default = 
  { indentSpaces: 0
  , line: 0
  , column: 0
  , output: []
  }

execWriterM :: forall m a. Monad m => Position -> WriterM' m a -> m Position
execWriterM pos prog = do execStateT prog pos

evalWriterM :: forall m a. Monad m => Position -> WriterM' m a -> m a
evalWriterM pos prog = do evalStateT prog pos

runWriterM :: forall m a. Monad m => Position -> WriterM' m a -> m (Tuple a Position)
runWriterM pos prog = do runStateT prog pos

getPosition :: forall m. Monad m => WriterM' m Position
getPosition = mread

getOutput :: forall m. Monad m => WriterM' m String
getOutput = do mreads $ _.output >>> Str.joinWith ""

clearOutput :: forall m. Monad m => WriterM m
clearOutput = do mmodify_ $ _ { output = [] }

token :: forall m. Monad m => String -> WriterM m
token t = do 
  whenM needsSpace do write " "
  write t
  where
  needsSpace = do
    s <- mread
    pure $ s.column > s.indentSpaces

-- Add an indent to the current indent, requiring all subsequent writes to
-- be at that indent or later. Restore the current indent once the inner program
-- is done running.
withIndent :: forall m. Monad m => Int -> WriterM m -> WriterM m
withIndent n prog = do 
  old <- mread
  let indented = old { indentSpaces = old.indentSpaces + n }
  new <- execWriterM indented prog # lift
  let final = new { indentSpaces = old.indentSpaces }
  mwrite final 

-- Write a string to the output. Newlines will be converted to changes to the 
-- line and column state, while tabs will be converted to two-space inserts.
write :: forall m. Monad m => String -> WriterM m
write str = do
  let lines = str # Str.split (Pattern "\n") >>> A.filter (_ == "")
      mapped = lines <#> Str.replace (Pattern "\t") (Replacement $ spaces 2)
  forEach_ mapped do 
    each addToLine
    inBetween \_ -> do addNewLine

  where
  -- Adding to the line always bumps to the requested indent if the current
  -- column isn't there yet.
  addToLine :: String -> WriterM m
  addToLine x = do
    prefix <- getIndentPrefix
    let appendData = if prefix == "" then [x] else [prefix, x]
        appendLen = Str.length prefix + Str.length x
    mmodify_ \s -> s { 
      output = s.output <> appendData
    , column = s.column + appendLen
    }
    
  getIndentPrefix :: WriterM' m String
  getIndentPrefix = do
    s <- mread
    let len = max 0 (s.indentSpaces - s.column)
        prefix = Str.joinWith "" (A.replicate len " ")
    pure prefix
    
  addNewLine = do
    mmodify_ \s -> s {
      output = A.snoc s.output "\n"
    , column = 0
    , line = s.line + 1
    }
    
  spaces i = Str.joinWith "" $ A.replicate i " "

newline :: forall m . Monad m => WriterM m 
newline = do write "\n"

equals :: forall m. Monad m => WriterM m
equals = token "="

typeEquals :: forall m . Monad m => String -> WriterM m 
typeEquals name = do 
  token "type" *> token name *> equals

-- { k1 : v1
-- , k2 : v2
-- }
putRecord :: forall m . Monad m => Map String String -> WriterM m 
putRecord = putRecord' ":"

-- { k1 :: v1
-- , k2 :: v2
-- }
putRecordType :: forall m . Monad m => Map String String -> WriterM m 
putRecordType = putRecord' "::"

-- s { k1 = v1
-- , k2 = v2
-- }
putRecordUpdate :: forall m . Monad m => String -> Map String String -> WriterM m 
putRecordUpdate name map = do 
  token name
  putRecord' "=" map

-- Generator for writing the data.
putRecord' :: forall m. Monad m => String -> Map String String -> WriterM m 
putRecord' sep map = do
  token "{"
  newline

  let arr = M.toUnfoldable map :: Array _
  forEach_ arr do
    each \(Tuple key value') -> do
      token key *> token sep *> token value'
    inBetween \_ -> do
      newline *> token ","
    
  newline
  token "}"
  
-- Write the entire signature.
putSignature :: forall m. Monad m => 
  String -> Array String -> Array String -> Array String -> WriterM m
putSignature name types classes args = do
  token name *> token "::" *> token "forall"
  forEach_ types do each token

  token "."
  forEach_ classes do
    each token 
    after \_ -> token "=>"

  forEach_ args do
    each token
    inBetween \_ -> token "->"
  
-- Put the initial part of the function body, but not all the body.
bodyEquals :: forall m. Monad m => 
  String -> Array String -> WriterM m
bodyEquals name args = do
  token name 
  forEach_ args  do each token
  equals

-- Puts the initial part of the newtype.
newtypeEquals :: forall m. Monad m => 
  String -> WriterM m
newtypeEquals name = do
  token "newtype" *> token name *> equals 

dataEquals :: forall m. Monad m => WriterM m
dataEquals = do token "data" *> equals
  
-- Write a type, whether that's "Either a b" or "a" or "Boolean"
putType :: forall m. Monad m => String -> Array String -> WriterM m
putType name args = do
  token name 
  forEach_ args do each token
  
putDo :: forall m. Monad m => WriterM m
putDo = do token "do"

putAdo :: forall m. Monad m => WriterM m
putAdo = do token "ado"

moduleWhere :: forall m. Monad m => String -> WriterM m
moduleWhere m = do
  token "module" *> token m *> token "where"
  
putImport :: forall m. Monad m => String -> WriterM m
putImport s = token "import" *> token s
  
string :: forall m. Monad m => String -> WriterM m
string s = do 
  token "\""
  token s
  token "\""

-- An input multiline string automatically escapes the newlines and tabs so
-- no escaping is needed. That ... should be fine, I think.
multiLine :: forall m. Monad m => String -> WriterM m
multiLine s = do 
  token "\"\"\"" 
  token s 
  token "\"\"\""