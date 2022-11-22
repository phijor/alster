module Alster.Document where

import Language.LSP.Types (TextDocumentVersion, NormalizedFilePath)

data Document = Document
    { _filePath :: NormalizedFilePath
    , _version :: TextDocumentVersion
    }
