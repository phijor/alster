module Alster.Handlers.Base (
    HandlerM,
    interpretHandler,
    showWithSeverity,
    Severity (..),
    showInfo,
    showWarning,
    showError,
    lift,
    publishDiagnostics,
    withVirtualFile,
) where

import Alster.Config (Config)
import Alster.Diagnostic (AsDiagnostic, asDiagnostic)

import Language.LSP.Logging (logToShowMessage)
import Language.LSP.Server (LspM, type (<~>) (Iso))
import Language.LSP.Server qualified as LSP
import Language.LSP.Types qualified as LSPT
import Language.LSP.VFS qualified as VFS

import Colog.Core (Severity (..), WithSeverity (WithSeverity), (<&))

import Data.Text (Text)

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans (lift)

type HandlerM = LspM Config

showWithSeverity :: Severity -> Text -> HandlerM ()
showWithSeverity severity msg = logToShowMessage <& WithSeverity msg severity

showInfo :: Text -> HandlerM ()
showInfo = showWithSeverity Info

showWarning :: Text -> HandlerM ()
showWarning = showWithSeverity Warning

showError :: Text -> HandlerM ()
showError = showWithSeverity Error

withVirtualFile :: LSPT.NormalizedUri -> (FilePath -> VFS.VirtualFile -> HandlerM a) -> HandlerM (Maybe a)
withVirtualFile uri f = do
    let path' = LSPT.uriToFilePath $ LSPT.fromNormalizedUri uri
    file' <- LSP.getVirtualFile uri
    case (,) <$> path' <*> file' of
        Just (path, file) -> Just <$> f path file
        Nothing -> pure Nothing

interpretHandler :: LSP.LanguageContextEnv Config -> (HandlerM <~> IO)
interpretHandler env = Iso{..}
  where
    forward :: HandlerM a -> IO a
    forward handler = LSP.runLspT env handler
    backward :: IO a -> HandlerM a
    backward = liftIO

-- flushDiagnostics :: Int -> HandlerM ()
-- flushDiagnostics maxDiags = LSP.flushDiagnosticsBySource maxDiags $ source

publishDiagnostics :: (AsDiagnostic diag) => LSPT.Uri -> Maybe LSPT.Int32 -> [diag] -> HandlerM ()
publishDiagnostics uri version diags =
    LSP.sendNotification LSPT.STextDocumentPublishDiagnostics $
        LSPT.PublishDiagnosticsParams uri (fmap fromIntegral version) $
            LSPT.List (fmap asDiagnostic diags)
