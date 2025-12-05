module Webb.Writer.Internal where

import Prelude
import Webb.Writer.ForEach

import Control.Monad.State (StateT)
import Data.Map (Map)
import Data.Map as M
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..))


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

-- TODO -- write a token, adding a space if needed. We don't need it
-- if we're at the start of the (indented) line.
token :: forall m. Monad m => String -> WriterM m
token t = pure unit

-- Add an indent to the current indent, requiring all subsequent writes to
-- be at that indent or later. Restore the current indent once the inner program
-- is done running.
withIndent :: forall m. Monad m => Int -> WriterM m -> WriterM m
withIndent n prog = do pure unit

-- TODO -- Put a newline. This should reset things so that we know to
-- be back at the indent, the next time we write.
putNewline :: forall m . Monad m => WriterM m 
putNewline = do pure unit

equals :: forall m. Monad m => WriterM m
equals = token "="

putAlias :: forall m . Monad m => String -> WriterM m 
putAlias name = do 
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
  putNewline

  let arr = M.toUnfoldable map :: Array _
  forEach_ arr do
    each \(Tuple key typ) -> do
      token key *> token sep *> token typ
    inBetween \_ -> do
      putNewline *> token ","
    
  putNewline
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
  String -> String -> WriterM m
newtypeEquals name ctor = do
  token "newtype" *> token name *> equals *> token ctor

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
  
string :: forall m. Monad m => String -> WriterM m
string s = do 
  token "\""
  token s
  token "\""

multiLine :: forall m. Monad m => String -> WriterM m
multiLine s = do 
  token "\"\"\"" 
  token s 
  token "\"\"\""