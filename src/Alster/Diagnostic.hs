module Alster.Diagnostic (
    AsDiagnostic,
    asDiagnostic,
    position,
    zeroPostion,
    range,
    zeroRange,
    noFileError,
    prettyDiagnostic,
    source,
) where

import Data.Int (Int32)
import Data.Text as Text

import Language.LSP.Types qualified as LSP

import Agda.Syntax.Parser qualified as Agda
import Agda.Syntax.Position qualified as Agda
import Agda.Utils.Pretty qualified as Agda

import Data.Maybe (fromMaybe)

source :: Maybe LSP.DiagnosticSource
source = Just "alster"

class AsDiagnostic a where
    asDiagnostic :: a -> LSP.Diagnostic

position :: Agda.Position' a -> LSP.Position
position (Agda.Pn _ _ line col) =
    LSP.Position (posCast line) (posCast col)
  where
    posCast :: Int32 -> LSP.UInt
    posCast one_indexed =
        case toInteger one_indexed of
            0 -> 0
            n -> fromInteger (n - 1)

zeroPostion :: LSP.Position
zeroPostion = LSP.Position 0 0

zeroRange :: LSP.Range
zeroRange = LSP.Range zeroPostion zeroPostion

range :: (Agda.HasRange r) => r -> Maybe LSP.Range
range ranged = do
    let r = Agda.getRange ranged
    start <- position <$> Agda.rStart r
    let end = maybe start position $ Agda.rEnd r
    pure $ LSP.Range start end

rangeOrZero :: (Agda.HasRange r) => r -> LSP.Range
rangeOrZero = fromMaybe zeroRange . range

mkDiagnostic :: LSP.Range -> LSP.DiagnosticSeverity -> Text -> LSP.Diagnostic
mkDiagnostic range' severity msg = LSP.Diagnostic{..}
  where
    _range = range'
    _severity = Just severity
    _code = Nothing
    _source = source
    _message = msg
    _tags = Nothing
    _relatedInformation = Nothing

noFileError :: LSP.NormalizedUri -> LSP.Diagnostic
noFileError uri = mkDiagnostic zeroRange LSP.DsError $ Text.pack ("No file found for URI " <> show uri)

prettyDiagnostic :: (Agda.HasRange d, Agda.Pretty d) => LSP.DiagnosticSeverity -> d -> LSP.Diagnostic
prettyDiagnostic severity diag = mkDiagnostic (rangeOrZero diag) severity (Text.pack $ show $ Agda.pretty diag)

instance AsDiagnostic (LSP.Diagnostic) where
    asDiagnostic = id

instance AsDiagnostic Agda.ParseError where
    asDiagnostic = prettyDiagnostic LSP.DsError

instance AsDiagnostic Agda.ParseWarning where
    asDiagnostic = prettyDiagnostic LSP.DsWarning

-- newtype DiagnosticStore = DiagnosticStore
--     { _diagnosticStore :: LSP.Diagnostics.DiagnosticStore
--     }
