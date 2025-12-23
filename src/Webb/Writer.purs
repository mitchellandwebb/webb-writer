module Webb.Writer 
( module P
, flushToFile
)
where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Webb.Writer.File (File)
import Webb.Writer.File (File, assertProjectRoot, close, getFile, isOpen, openAndTruncate, resetDir) as P
import Webb.Writer.File as F
import Webb.Writer.Internal (WriterM, clearOutput, getOutput)
import Webb.Writer.Internal (Position, St, WriterM, WriterM', bodyEquals, clearOutput, dataEquals, default, defaultPos, equals, evalWriterM, execWriterM, getOutput, getPosition, moduleWhere, multiLine, newline, newtypeEquals, putAdo, putDo, putImport, putRecord, putRecord', putRecordType, putRecordUpdate, putSignature, putType, runWriterM, string, token, typeEquals, withIndent) as P


flushToFile :: forall m. MonadAff m => File -> WriterM m
flushToFile file = do
  output <- getOutput
  clearOutput
  F.write file output