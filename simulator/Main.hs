{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Control.Monad (void)
import Control.Monad.Freer (interpret)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (def)
import Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import GHC.Generics (Generic)
import qualified Fracada as Fracada
import Plutus.PAB.Core (EffectHandlers)
import Plutus.PAB.Effects.Contract.Builtin (Builtin)
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import qualified Plutus.PAB.Simulator as Simulator
import qualified Plutus.PAB.Webserver.Server as PAB.Server
import qualified Data.OpenApi.Schema as OpenApi

main :: IO ()
main = void $
  Simulator.runSimulationWith simulatorHandlers $ do
    Simulator.logString @(Builtin FracadaContract) "Starting plutus-starter PAB webserver on port 9080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    void $ liftIO getLine

    shutdown

data FracadaContract = Fracada
  deriving (Eq, Show, Generic, Ord, FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty FracadaContract where
  pretty = viaShow

instance Builtin.HasDefinitions FracadaContract where
  getDefinitions = [Fracada]
  getSchema _ = []
  getContract = \case
    Fracada -> Builtin.SomeBuiltin Fracada.endpoints

simulatorHandlers :: EffectHandlers (Builtin FracadaContract) (Simulator.SimulatorState (Builtin FracadaContract))
simulatorHandlers = Simulator.mkSimulatorHandlers def def handler
  where
    handler :: Simulator.SimulatorContractHandler (Builtin FracadaContract)
    handler = interpret (Builtin.contractHandler Builtin.handleBuiltin)
