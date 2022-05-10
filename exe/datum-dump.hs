
{-# LANGUAGE OverloadedStrings   #-}
import           Prelude
import           System.Environment
import           Cardano.Api
import           Cardano.Api.Shelley
import qualified Plutus.V1.Ledger.Api       as Plutus
import           Fracada
import           Plutus.V1.Ledger.Value
import           Data.Aeson
import           Ledger                     (datumHash)
import           Plutus.V1.Ledger.Api
import           Data.String                (IsString (..))

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  if nargs /= 3 then
    do
      putStrLn $ "Usage:"
      putStrLn $ "datum-dump <NFT currency symbol> <NFT token name> <number of fractions>"
  else 
    do 
      let       
        [nftSymbol, nftTokenName', numberOfFractions'] = args
        nftCurrencySymbol = fromString nftSymbol
        nftTokenName = fromString nftTokenName' 
        numberOfFractions = (read numberOfFractions' )::Integer
  
        nft = AssetClass (nftCurrencySymbol, nftTokenName)
  
        datum =FractionNFTDatum{ tokensClass= nft, totalFractions = numberOfFractions}
        dHash = datumHash $ Datum $ toBuiltinData datum
        datumToEncode = Plutus.builtinDataToData $ toBuiltinData datum
        encoded = Data.Aeson.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData datumToEncode) 

      putStrLn $ "encoded datum: " ++ show encoded
      putStrLn $ "datum hash: " ++ show dHash

