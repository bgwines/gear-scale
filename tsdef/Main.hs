module Main
  ( main
  ) where

import qualified ServerAPI
import Servant.JS ( writeJSForAPI, vanillaJS )
import Data.Proxy
import Data.Monoid

-- dollar signs before parens below
-- (deriveTypeScript (getJSONOptions (Proxy :: Proxy MyType1)) ''MyType1)
-- (deriveTypeScript (getJSONOptions (Proxy :: Proxy MyType2)) ''MyType2)

main :: IO ()
main = do
  putStrLn "Writing API to `static/api.js`..."
  writeJSForAPI ServerAPI.limitedApi vanillaJS "static/api.js"

  -- putStrLn "Writing types to ..."
  -- putStrLn $ formatTSDeclarations (
  --   (getTypeScriptDeclaration (Proxy :: Proxy MyType1)) <>
  --   (getTypeScriptDeclaration (Proxy :: Proxy MyType2)))
