module Main
  ( main
  ) where

import qualified ClientTypes
import qualified ServerAPI
import Servant.JS ( writeJSForAPI, vanillaJS )
import Data.Proxy
import Data.Monoid

--(deriveTypeScript (getJSONOptions (Proxy :: Proxy GearItem)) ''GearItem)

main :: IO ()
main = do
  putStrLn "Writing API to `static/api.js`..."
  writeJSForAPI ServerAPI.limitedApi vanillaJS "static/api.js"

  --putStrLn "Writing TS types to `stdout`..."
  --putStrLn $ formatTSDeclarations (
  --  getTypeScriptDeclaration (Proxy :: Proxy GearItem))
