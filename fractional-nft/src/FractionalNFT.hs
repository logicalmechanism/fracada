{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- Options
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}
module TokenSale
  ( tokenSaleScript
  , tokenSaleScriptShortBs
  , contract
  , Schema
  ) where
import           Codec.Serialise
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Short     as SBS
import           Ledger                    hiding (singleton)
import qualified Ledger.Typed.Scripts      as TypedScripts
import           Cardano.Api.Shelley       (PlutusScript (..), PlutusScriptV1)
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Plutus.V1.Ledger.Ada      as Ada
import qualified Plutus.V1.Ledger.Scripts  as Scripts
import           CheckFuncs
import           DataTypes
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 2

  The token sale contract for the Tokhun marketplace.

  A UTxO can be spent if and only if it satisfies one of the three validators,
  buy, remove, and update. A failed validator will report the failure to the console.

  @see buy    :: Bool -- Action 0
  @see remove :: Bool -- Action 1
  @see update :: Bool -- Action 2

  Versions Used In Project:

  cardano-cli 1.34.1 - linux-x86_64 - ghc-8.10
  git rev 73f9a746362695dc2cb63ba757fbcabb81733d23

  cabal-install version 3.4.0.0
  compiled using version 3.4.0.0 of the Cabal library

  The Glorious Glasgow Haskell Compilation System, version 8.10.4
-}
-------------------------------------------------------------------------------
-- | Create the token sale parameters data object.
-------------------------------------------------------------------------------
data TokenSaleParams = TokenSaleParams {}
PlutusTx.makeLift ''TokenSaleParams
-------------------------------------------------------------------------------
-- | Create the redeemer type.
-------------------------------------------------------------------------------
data CustomRedeemerType = Buy BuyType |
                          Remove      |
                          Update 
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ('Buy,    0)
                                                , ('Remove, 1)
                                                , ('Update, 2)
                                                ]
PlutusTx.makeLift ''CustomRedeemerType
-------------------------------------------------------------------------------
-- | mkValidator :: Data -> Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: TokenSaleParams -> CustomDatumType -> CustomRedeemerType -> ScriptContext -> Bool
mkValidator _ datum redeemer context = 
  case redeemer of
    (Buy bt) -> do
      { let buyerPkh = buyerPKH bt
      ; let a = traceIfFalse "Profit Value Error"  $ isPKHGettingPaid txOutputs profitPKH profitValue
      ; let b = traceIfFalse "Seller Value Error"  $ isPKHGettingPaid txOutputs sellerPKH paymentValue
      ; let c = traceIfFalse "Buyer Value Error"   $ isPKHGettingPaid txOutputs buyerPkh  validatingValue
      ; let d = traceIfFalse "Signing Tx Error"    $ txSignedBy info buyerPkh
      ; let e = traceIfFalse "Single Script Error" $ isSingleScript txInputs
      ;         traceIfFalse "Buy Endpoint Error"  $ all (==True) [a,b,c,d,e]
      }
    Remove -> do 
      { let a = traceIfFalse "Seller Value Error"    $ isPKHGettingPaid txOutputs sellerPKH validatingValue
      ; let b = traceIfFalse "Signing Tx Error"      $ txSignedBy info sellerPKH
      ; let c = traceIfFalse "Single Script Error"   $ isSingleScript txInputs
      ;         traceIfFalse "Remove Endpoint Error" $ all (==True) [a,b,c]
      }
    Update -> do
      { let a = traceIfFalse "Script Value Error"    $ isValueContinuing contOutputs validatingValue
      ; let b = traceIfFalse "Signing Tx Error"      $ txSignedBy info sellerPKH
      ; let c = traceIfFalse "Incoming Datum Error"  $ isEmbeddedDatum datum info contOutputs
      ; let d = traceIfFalse "Single Script Error"   $ isSingleScript txInputs
      ;         traceIfFalse "Update Endpoint Error" $ all (==True) [a,b,c,d]
      }
  where
    info :: TxInfo
    info = scriptContextTxInfo  context 
    
    contOutputs :: [TxOut]
    contOutputs = getContinuingOutputs context
    
    txOutputs :: [TxOut]
    txOutputs = txInfoOutputs info
    
    txInputs :: [TxInInfo]
    txInputs = txInfoInputs  info
    
    profitPKH :: PubKeyHash
    profitPKH = cdtProfitPKH datum
    
    sellerPKH :: PubKeyHash
    sellerPKH = cdtSellerPKH datum
    
    profitValue :: Value
    profitValue = Ada.lovelaceValueOf $ cdtProfit datum
    
    paymentValue :: Value
    paymentValue = Ada.lovelaceValueOf $ cdtPrice datum - cdtProfit datum
    
    validatingValue :: Value
    validatingValue = 
      case findOwnInput context of
        Nothing    -> traceError "No Input to Validate." -- This error should never be hit.
        Just input -> txOutValue $ txInInfoResolved input
-------------------------------------------------------------------------------
-- | This determines the data type for Datum and Redeemer.
-------------------------------------------------------------------------------
data Typed
instance TypedScripts.ValidatorTypes Typed where
  type instance DatumType    Typed = CustomDatumType
  type instance RedeemerType Typed = CustomRedeemerType
-------------------------------------------------------------------------------
-- | Now we need to compile the Typed Validator.
-------------------------------------------------------------------------------
typedValidator :: TokenSaleParams -> TypedScripts.TypedValidator Typed
typedValidator ts = TypedScripts.mkTypedValidator @Typed
  ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode ts)
   $$(PlutusTx.compile [|| wrap        ||])
    where
      wrap = TypedScripts.wrapValidator @CustomDatumType @CustomRedeemerType  -- @Datum @Redeemer

validator :: Scripts.Validator
validator = TypedScripts.validatorScript (typedValidator $ TokenSaleParams {})
-------------------------------------------------------------------------------
-- | The code below is required for the plutus script compile.
-------------------------------------------------------------------------------
script :: Scripts.Script
script = Scripts.unValidatorScript validator

tokenSaleScriptShortBs :: SBS.ShortByteString
tokenSaleScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

tokenSaleScript :: PlutusScript PlutusScriptV1
tokenSaleScript = PlutusScriptSerialised tokenSaleScriptShortBs
-------------------------------------------------------------------------------
-- | Off Chain
-------------------------------------------------------------------------------
type Schema = Endpoint "" ()

contract :: AsContractError e => Contract () Schema e ()
contract = selectList [] >> contract
