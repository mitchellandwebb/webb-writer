module Webb.Writer.Internal where

import Prelude

import Control.Monad.State (StateT, evalStateT, execStateT, lift, runStateT)
import Data.Array as A
import Data.Array as Array
import Data.CodePoint.Unicode as Unicode
import Data.Foldable (for_)
import Data.Identity (Identity)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (unwrap)
import Data.String (CodePoint, Pattern(..), Replacement(..))
import Data.String as Str
import Data.String as String
import Data.Tuple (Tuple(..))
import Webb.Monad.Prelude (timesRepeat_)
import Webb.State.Prelude (mmodify_, mread, mreads, mwrite)
import Webb.Writer.ForEach (after, each, forEach_, inBetween)
import Webb.Writer.String (escape)


{- The writer monad enables us to write to the monoidal source. It has to maintain the indent level in such a way that changes after a newline. Thus, we write to a thing, and utilize the indent level. Newlines ... are special. To prevent newlines from causing a problem, though, perhaps we have to count the newlines and reset the indent level when needed.
-}

type St = 
  { indentSpaces :: Int
  , line :: Int
  , column :: Int
  , output :: Array String
  , lastWrite :: String
  }

type Position = 
  { indentSpaces :: Int
  , line :: Int
  , column :: Int
  }

type WriterM' m a = StateT St m a
type WriterM m = WriterM' m Unit

default :: St
default = 
  { indentSpaces: 0
  , line: 0
  , column: 0
  , output: []
  , lastWrite: ""
  }

defaultPos :: Position
defaultPos = 
  { indentSpaces: 0
  , line: 0
  , column: 0
  }
  
-- Run a writer to a string -- good for building a string with ease.
runToString :: WriterM Identity -> String
runToString prog = let 
  state = unwrap $ execWriterM default prog :: St
  in String.joinWith "" state.output

execWriterM :: forall m a. Monad m => St -> WriterM' m a -> m St
execWriterM pos prog = do execStateT prog pos

evalWriterM :: forall m a. Monad m => St -> WriterM' m a -> m a
evalWriterM pos prog = do evalStateT prog pos

runWriterM :: forall m a. Monad m => St -> WriterM' m a -> m (Tuple a St)
runWriterM pos prog = do runStateT prog pos

getPosition :: forall m. Monad m => WriterM' m Position
getPosition = do 
  st <- mread
  pure $ { indentSpaces: st.indentSpaces, line: st.line, column: st.column }

getOutput :: forall m. Monad m => WriterM' m String
getOutput = do mreads $ _.output >>> Str.joinWith ""

clearOutput :: forall m. Monad m => WriterM m
clearOutput = do mmodify_ $ _ { output = [] }

lastCodePoint :: forall m. Monad m => WriterM' m (Maybe CodePoint)
lastCodePoint = do
  st <- mread
  let points = String.toCodePointArray st.lastWrite
  pure $ Array.last points

-- Write a string. Ensure it is a separate token -- if the previous character
-- was _not_ whitespace, add a space so that it's a separate word.
token :: forall m. Monad m => String -> WriterM m
token t = do 
  whenM needsExtraSpace do write " "
  write t
  where
  -- Do we need to print a space beforehand? We only need it if
  -- the last character written was NOT a space.
  needsExtraSpace :: WriterM' m Boolean
  needsExtraSpace = do
    mpoint <- lastCodePoint
    pure $ fromMaybe false do
      point <- mpoint
      pure $ not (Unicode.isSpace point)

-- Write multiple tokens to the stream as an array.
tokens :: forall m. Monad m => Array String -> WriterM m
tokens arr = do for_ arr token

word :: forall m. Monad m => String -> WriterM m
word = token

words :: forall m. Monad m => Array String -> WriterM m
words arr = do for_ arr word

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

indent :: forall m. Monad m => Int -> WriterM m -> WriterM m
indent = withIndent

-- Write a string. Respect the current indent, even as newlines occur in the string.
write :: forall m. Monad m => String -> WriterM m
write str = do
  when (str /= "") do
    let split = str # Str.split (Pattern "\n")
        lines = A.filter (_ /= "") split
        beforeCount = split # A.takeWhile (_ == "") >>> A.length
        afterCount = split # A.reverse >>> A.takeWhile (_ == "") >>> A.length
        mapped = lines <#> Str.replace (Pattern "\t") (Replacement $ spaces 2)
        
    if A.length lines == 0 then
      timesRepeat_ (max 0 (beforeCount - 1)) do addNewLine
    else do
      timesRepeat_ beforeCount do addNewLine

      forEach_ mapped do 
        each addToLine
        inBetween \_ -> addNewLine
        
      timesRepeat_ afterCount do addNewLine

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
    , lastWrite = String.joinWith "" appendData
    }
    
  getIndentPrefix :: WriterM' m String
  getIndentPrefix = do
    s <- mread
    let len = max 0 (s.indentSpaces - s.column)
        prefix = Str.joinWith "" (A.replicate len " ")
    pure prefix
    
  addNewLine :: WriterM m
  addNewLine = do
    mmodify_ \s -> s {
      output = A.snoc s.output "\n"
    , column = 0
    , line = s.line + 1
    , lastWrite = "\n"
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
  String -> Array String -> WriterM m
newtypeEquals name args = do
  token "newtype" *> token name 
  forEach_ args do each token 
  equals 

dataEquals :: forall m. Monad m => 
  String -> Array String -> WriterM m
dataEquals name args = do 
  token "data" *> token name
  forEach_ args do each token 
  equals
  
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
  write $ escape s
  write "\""

-- An input multiline string automatically escapes the newlines and tabs so
-- no escaping is needed. That ... should be fine, I think.
multiLine :: forall m. Monad m => String -> WriterM m
multiLine s = do 
  token "\"\"\"" 
  write s 
  write "\"\"\""