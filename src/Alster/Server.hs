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
    Diagnostic (Diagnostic),
    DidSaveTextDocumentParams,
    List (List),
    MessageType (..),
    Method (Initialized, TextDocumentDidSave),
    NotificationMessage,
    Position (Position),
    PublishDiagnosticsParams (PublishDiagnosticsParams),
    Range (Range),
    SMethod (SInitialized, STextDocumentDidSave, STextDocumentPublishDiagnostics, SWindowShowMessage, SWorkspaceDidChangeConfiguration),
    ShowMessageParams (ShowMessageParams),
    UInt,
    uriToFilePath,
 )
import Language.LSP.Types.Lens qualified as J

import Control.Monad (void, (>=>))

import Agda.Syntax.Parser qualified as Agda (PM, ParseError, moduleParser, parseFile, readFilePM, runPMIO)
import Agda.Syntax.Position qualified as AgdaPos
import Agda.Utils.FileName (AbsolutePath (AbsolutePath))

import Agda.Utils.Pretty (pretty)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Text as Text (Text, pack, unpack)
import Data.Text.Lazy qualified as TextLazy (unpack)

type Config = ()

showMessage :: MessageType -> Text -> LspM Config ()
showMessage t msg = void $ sendNotification SWindowShowMessage $ ShowMessageParams t msg

initializedHandler :: NotificationMessage 'Initialized -> LspM Config ()
initializedHandler _ = showMessage MtInfo "Initialized Alster"

agdaPositionToPosition :: AgdaPos.Position' a -> Position
agdaPositionToPosition (AgdaPos.Pn _ _ posLine posCol) = Position (cast posLine - 1) (cast posCol - 1)
  where
    cast :: Int32 -> UInt
    cast n = fromInteger (toInteger n)

agdaRangeToRange :: (AgdaPos.HasRange r) => r -> Range
agdaRangeToRange r =
    let range = AgdaPos.getRange r
        start = agdaPositionToPosition $ fromMaybe undefined $ AgdaPos.rStart range
        end = maybe start agdaPositionToPosition $ AgdaPos.rEnd range
     in Range start end

parseErrorToDiagnostic :: Agda.ParseError -> Diagnostic
parseErrorToDiagnostic err = Diagnostic range Nothing Nothing Nothing message Nothing Nothing
  where
    range = agdaRangeToRange err
    message = Text.pack $ show $ pretty err

-- On save, parse the file and publish any parse errors as diagnostics.
textDocumentDidSaveHandler :: NotificationMessage 'TextDocumentDidSave -> LspM Config ()
textDocumentDidSaveHandler msg = do
    let version :: Maybe UInt
        version = Nothing
        uri = msg ^. J.params . J.textDocument . J.uri

    -- Clear all diagnostics for this file
    void $ sendNotification STextDocumentPublishDiagnostics $ PublishDiagnosticsParams uri version $ List []

    showMessage MtInfo "Parsing file..."
    -- sendNotification $ SProgress $ ProgressParams _ $ Begin $ WorkDoneProgressBeginParams "Parsing file..." Nothing Nothing Nothing
    (res, warnings) <-
        Agda.runPMIO $
            withContent
                (msg ^. J.params)
                (Agda.parseFile Agda.moduleParser)
                (Agda.readFilePM >=> pure . TextLazy.unpack)
    case res of
        Left err -> do
            -- Publish parse errors as diagnostics
            let diagnostics = List [parseErrorToDiagnostic err]
            void $ sendNotification STextDocumentPublishDiagnostics $ PublishDiagnosticsParams uri version diagnostics
        Right _ -> do
            -- The file parsed without errors
            showMessage MtInfo $ "Tada 🎉 (" <> Text.pack (show $ length warnings) <> " warning(s))"
  where
    withContent :: DidSaveTextDocumentParams -> (AbsolutePath -> String -> Agda.PM a) -> (AbsolutePath -> Agda.PM String) -> Agda.PM a
    withContent params parse readf =
        let text = params ^. J.text
            uri = params ^. J.textDocument . J.uri
            path = fromMaybe undefined $ uriToFilePath uri
            abspath = AbsolutePath $ Text.pack path
         in case text of
                Just content -> parse abspath $ Text.unpack content
                Nothing -> readf abspath >>= parse abspath

handlers :: Handlers (LspM Config)
handlers =
    mconcat
        [ notificationHandler SInitialized initializedHandler
        , notificationHandler SWorkspaceDidChangeConfiguration $ \_ -> pure ()
        , notificationHandler STextDocumentDidSave textDocumentDidSaveHandler
        ]

run :: IO Int
run =
    runServer $
        ServerDefinition
            { defaultConfig = ()
            , onConfigurationChange = \_ _ -> Right ()
            , doInitialize = \env _req -> pure $ Right env
            , staticHandlers = handlers
            , interpretHandler = \env -> Iso (runLspT env) liftIO
            , options = defaultOptions
            }
