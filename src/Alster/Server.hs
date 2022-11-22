{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Alster.Server (run) where

import Alster.Config (initialConfig)
import Alster.Diagnostic as Diagnostic (asDiagnostic)
import Alster.Handlers.Base (HandlerM)
import Alster.Handlers.Base qualified as Alster
import Alster.State (State, initialState)

import Control.Concurrent.MVar qualified as MVar
import Control.Lens ((^.))
import Control.Monad (when)

import Language.LSP.Logging (logToShowMessage)
import Language.LSP.Server (
    Handlers,
    Options (textDocumentSync),
    ServerDefinition (..),
    defaultOptions,
    notificationHandler,
    runServer,
 )
import Language.LSP.Server qualified as LSP
import Language.LSP.Types qualified as LSPT
import Language.LSP.Types.Lens qualified as J
import Language.LSP.VFS qualified as VFS

import Colog.Core (Severity (..), WithSeverity (WithSeverity), (<&))

import Agda.Syntax.Parser qualified as Agda (moduleParser, parseFile, runPMIO)
import Agda.Utils.FileName (mkAbsolute)

import Data.Either.Combinators (leftToMaybe)
import Data.Maybe (maybeToList, isNothing)
import Data.Text as Text (pack, unpack)

-- Parse the file and publish any parse errors as diagnostics.
publishParseDiagnostics :: MVar.MVar State -> LSPT.Uri -> HandlerM ()
publishParseDiagnostics _state uri = do
    success <- Alster.withVirtualFile (LSPT.toNormalizedUri uri) $
        \path file -> do
            let text = Text.unpack $ VFS.virtualFileText file
                version = VFS.virtualFileVersion file
            (res, warnings) <- LSP.withIndefiniteProgress "Parsing file" LSP.NotCancellable $ do
                Agda.runPMIO $
                    Agda.parseFile Agda.moduleParser (mkAbsolute path) text
            let errors = maybeToList $ leftToMaybe res
            Alster.publishDiagnostics uri (Just version) $
                fmap asDiagnostic warnings <> fmap asDiagnostic errors
            Alster.showInfo $ "Parsed file 🎉 (" <> (Text.pack $ show $ length warnings) <> ")"
    when (isNothing success) $
        Alster.showError $ "No file found for " <> Text.pack (show uri)

handlers :: MVar.MVar State -> Handlers HandlerM
handlers state =
    mconcat
        [ notificationHandler LSPT.SInitialized $ \_ -> do
            Alster.showInfo "initialized alster"
        , notificationHandler LSPT.SWorkspaceDidChangeConfiguration $ \_ -> pure ()
        , notificationHandler LSPT.STextDocumentDidOpen $ \msg -> do
            let uri = msg ^. J.params . J.textDocument . J.uri
            Alster.showInfo $ "Opened " <> Text.pack (show uri)
            publishParseDiagnostics state uri
        , notificationHandler LSPT.STextDocumentDidSave $ \msg -> do
            let uri = msg ^. J.params . J.textDocument . J.uri
            Alster.showInfo $ "Saved " <> Text.pack (show uri)
            publishParseDiagnostics state uri
        , notificationHandler LSPT.STextDocumentDidChange $ \msg -> do
            let uri = msg ^. J.params . J.textDocument . J.uri
            Alster.showInfo $ "Change " <> Text.pack (show uri)
            publishParseDiagnostics state uri
        , notificationHandler LSPT.STextDocumentDidClose $ \_ -> do
            logToShowMessage <& "Closed file..." `WithSeverity` Info
        ]

run :: IO Int
run = do
    state <- MVar.newMVar initialState

    let syncOptions =
            LSPT.TextDocumentSyncOptions
                { _openClose = Just True
                , _change = Just LSPT.TdSyncFull
                , _willSave = Just False
                , _willSaveWaitUntil = Just False
                , _save = Just $ LSPT.InR $ LSPT.SaveOptions{_includeText = Just True}
                }

    runServer $
        ServerDefinition
            { defaultConfig = initialConfig
            , onConfigurationChange = \config _value -> Right config
            , doInitialize = \env _req -> pure $ Right env
            , staticHandlers = handlers state
            , options =
                defaultOptions
                    { textDocumentSync = Just syncOptions
                    }
            , interpretHandler = Alster.interpretHandler
            }
