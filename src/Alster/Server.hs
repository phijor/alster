{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Alster.Server (run) where

-- import Agda.TypeChecking.Monad.Base (TCMT)

import Control.Lens ((^.))

import Language.LSP.Server (
    Handlers,
    LspM,
    ServerDefinition (..),
    defaultOptions,
    notificationHandler,
    runLspT,
    runServer,
    sendNotification,
    type (<~>) (Iso),
 )
import Language.LSP.Types (
    DidSaveTextDocumentParams,
    MessageType (..),
    Method (Initialized, TextDocumentDidSave),
    NotificationMessage,
    SMethod (SInitialized, STextDocumentDidSave, SWindowShowMessage, SWorkspaceDidChangeConfiguration),
    ShowMessageParams (ShowMessageParams),
    uriToFilePath,
 )
import Language.LSP.Types.Lens qualified as J

import Control.Monad (void)

import Agda.Syntax.Common (FileType)
import Agda.Syntax.Concrete (Module)
import Agda.Syntax.Parser (PM, moduleParser, parseFile, readFilePM, runPMIO)
import Agda.Utils.FileName (AbsolutePath (AbsolutePath))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (fromMaybe)
import Data.Text as Text (Text, pack, unpack)
import Data.Text.Lazy as TextLazy (toStrict)

type Config = ()

-- type HandlerM config m = Language.LSP.Server.LspT config (TCMT m)
-- type HandlerM config = TCMT (LspM config)

-- instance (MonadIO io) => MonadUnliftIO (TCMT io) where
--     withRunInIO inner = _

parseContent :: FilePath -> Text -> PM (Module, FileType)
parseContent path content =
    parseFile moduleParser (AbsolutePath $ Text.pack path) $ Text.unpack content

showMessage :: MessageType -> Text -> LspM Config ()
showMessage t msg = void $ sendNotification SWindowShowMessage $ ShowMessageParams t msg

initializedHandler :: NotificationMessage 'Initialized -> LspM Config ()
initializedHandler _ = showMessage MtInfo "Initialized Alster"

textDocumentDidSaveHandler :: NotificationMessage 'TextDocumentDidSave -> LspM Config ()
textDocumentDidSaveHandler msg = do
    showMessage MtInfo "Parsing file..."
    (res, _warnings) <- runPMIO $ withContent (msg ^. J.params) parseContent
    let (msgt, notif) = case res of
            Left _err -> (MtError, "Parse Error")
            Right _ -> (MtInfo, "Tada 🎉")
    showMessage msgt notif

    where
        withContent :: DidSaveTextDocumentParams -> (FilePath -> Text -> PM a) -> PM a
        withContent params f =
            let text = params ^. J.text
                uri = params ^. J.textDocument . J.uri
                path = fromMaybe "." $ uriToFilePath uri
            in
            case text of
                Just content -> f path content
                Nothing -> do
                    content <- readFilePM (AbsolutePath $ Text.pack path)
                    f path $ TextLazy.toStrict content

handlers :: Handlers (LspM Config)
handlers =
    mconcat
        [ notificationHandler SInitialized initializedHandler
        , notificationHandler SWorkspaceDidChangeConfiguration $ \_ -> pure ()
        , notificationHandler STextDocumentDidSave textDocumentDidSaveHandler
        ]

run :: IO Int
run =
    Language.LSP.Server.runServer $
        Language.LSP.Server.ServerDefinition
            { defaultConfig = ()
            , onConfigurationChange = \_ _ -> Right ()
            , doInitialize = \env _req -> pure $ Right env
            , staticHandlers = handlers
            , interpretHandler = \env -> Iso (runLspT env) liftIO
            , options = Language.LSP.Server.defaultOptions
            }
