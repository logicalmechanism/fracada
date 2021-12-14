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

import Data.Aeson (FromJSON, ToJSON)
import Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import GHC.Generics (Generic)
import qualified Fracada as Fracada
import Language.PureScript.Bridge (argonaut, equal, genericShow, mkSumType)
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import Plutus.PAB.Run (runWith)
import Plutus.PAB.Run.PSGenerator (HasPSTypes (..))
import qualified Data.OpenApi.Schema as OpenApi

main :: IO ()
main = runWith (Builtin.handleBuiltin @FracadaContract)

data FracadaContract = Fracada
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty FracadaContract where
  pretty = viaShow

instance HasPSTypes FracadaContract where
  psTypes = [equal . genericShow . argonaut $ mkSumType @FracadaContract]

instance Builtin.HasDefinitions FracadaContract where
  getDefinitions = [Fracada]
  getSchema _ = []
  getContract = \case
    Fracada -> Builtin.SomeBuiltin Fracada.endpoints
