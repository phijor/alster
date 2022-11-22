module Alster.State (State(..), initialState) where

import Agda.TypeChecking.Monad.Base qualified as Agda

import Language.LSP.Diagnostics qualified as LSP

data State = State
    { _tcState :: Agda.TCState
    , _tcEnv :: Agda.TCEnv
    , _diagnosticStore :: LSP.DiagnosticStore
    }

initialState :: State
initialState =
    State
        { _tcState = Agda.initState
        , _tcEnv = Agda.initEnv
        , _diagnosticStore = mempty
        }
