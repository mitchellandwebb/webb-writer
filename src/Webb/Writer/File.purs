module Webb.Writer.File where

import Prelude

import Control.Monad.Error.Class (catchError)
import Data.Array as A
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Node.Buffer (Buffer, fromString)
import Node.Encoding (Encoding(..))
import Node.FS (FileDescriptor, FileFlags(..))
import Node.FS.Aff (fdAppend, fdClose, readdir, stat)
import Node.FS.Aff as Fs
import Node.FS.Perms (permsReadWrite)
import Node.FS.Stats (isDirectory, isFile)
import Node.Path (basename)
import Node.Path as Path
import Node.Process (cwd)
import Webb.Monad.Prelude (expect, forceMaybe', notM)
import Webb.State.Prelude (ShowRef, aread, areads, newShowRef, (:=))

{- Helper functions for scripts that want to quickly setup and write to particular files. 
-}


-- Assert that the current WD is in fact a Spago root.
assertProjectRoot :: forall m. MonadAff m => m Unit
assertProjectRoot = liftAff do
  path <- cwd # liftEffect
  files <- readdir path
  let mspago = A.find (\s -> basename s == "spago.yaml") files
  spago <- forceMaybe' "No spago.yaml file present" mspago
  stats <- stat spago 
  expect (isFile stats) "No spago.yaml file present"
  
-- Find the specified directory, relative to the current working directory,
-- and remove _all_ contents. We assume we are attempting to rebuild the directory
-- from scratch as part of the compilation process.
resetDir :: forall m. MonadAff m => String -> m Unit
resetDir relPath = liftAff do
  path <- cwd # liftEffect
  let dirPath = Path.concat [path, relPath]
  whenM (existsDir relPath) do
    Fs.rm' dirPath { force: true, recursive: true, maxRetries: 2, retryDelay: 100 }
  Fs.mkdir' dirPath { recursive: true, mode: permsReadWrite }
  
-- Does directory exist, relatively?
existsDir :: forall m. MonadAff m => String -> m Boolean
existsDir relPath = liftAff do 
  path <- cwd # liftEffect
  let dirPath = Path.concat [path, relPath]
  catchError (do 
    stats <- Fs.stat dirPath
    pure $ isDirectory stats
  ) (\_ -> 
    pure false
  )
  
getFile :: forall m. MonadAff m => String -> String -> m File
getFile dirPath filePath = do
  path <- cwd # liftEffect
  let fullPath = Path.concat [path, dirPath, filePath]
  ref <- newShowRef Nothing
  pure $ F { path: fullPath, fd: ref }
  
appendPath :: String -> String -> String
appendPath a b = Path.concat [a, b]

concatPath :: Array String -> String
concatPath a = Path.concat a

newtype File = F 
  { path :: String
  , fd :: ShowRef (Maybe FileDescriptor)
  }
  
openAndTruncate :: forall m. MonadAff m => File -> m Unit
openAndTruncate self@(F s) = liftAff do
  whenM (notM $ isOpen self) do
    fd <- Fs.fdOpen s.path W_PLUS Nothing
    s.fd := Just fd
    
isOpen :: forall m. MonadEffect m => File -> m Boolean
isOpen (F s) = areads isJust s.fd 
  
-- Write to the file, appending to it without closing. Writes the final
-- output to the file, without modifying anything. This can be used to
-- run an entire writer program and write the final output. Alternatively,
-- this can be used to write a corresponding 'write' for the WriterM monad
-- itself, that writes the output while clearing it.
write :: forall m. MonadAff m => 
  File -> String -> m Unit
write (F s) str = do
  mfd <- aread s.fd
  fd <- forceMaybe' "File is not open" mfd # liftAff
  buf <- buffer # liftEffect
  void $ fdAppend fd buf # liftAff
  where
  buffer :: Effect Buffer
  buffer = fromString str UTF16LE

read :: forall m. MonadAff m => 
  File -> m String
read (F s) = liftAff do Fs.readTextFile UTF16LE s.path

countDir :: forall m. MonadAff m => 
  String -> m Int
countDir rel = liftAff do 
  path <- cwd # liftEffect
  let dirPath = concatPath [path, rel]
  catchError (do
    files <- Fs.readdir dirPath
    pure $ A.length files
  ) (\_ -> do
    pure 0
  )
  
-- Close the file.
close :: forall m. MonadAff m => File -> m Unit
close (F s ) = liftAff do
  mfd <- aread s.fd
  for_ mfd \fd -> do
    s.fd := Nothing
    fdClose fd