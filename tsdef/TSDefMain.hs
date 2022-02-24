{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main
  ) where

import qualified ClientTypes
import qualified ServerAPI

import Data.Aeson.TH ( defaultOptions )
import Data.Aeson.TypeScript.TH
    ( TypeScript(getTypeScriptDeclarations),
      formatTSDeclarations,
      deriveTypeScript )
import Data.Proxy ( Proxy(..) )
import Servant.JS ( writeJSForAPI, vanillaJS )

$(deriveTypeScript defaultOptions ''ClientTypes.GearKind)
$(deriveTypeScript defaultOptions ''ClientTypes.GearItem)

main :: IO ()
main = do
  putStrLn "Writing API to `src/backend_api.js`..."
  writeJSForAPI ServerAPI.limitedApi vanillaJS "src/backend_api.js"

  putStrLn "Writing TS types to `src/types.ts`..."
  let decl1 = "export " ++ formatTSDeclarations (getTypeScriptDeclarations (Proxy :: Proxy ClientTypes.GearKind))
  let decl2 = "export " ++ formatTSDeclarations (getTypeScriptDeclarations (Proxy :: Proxy ClientTypes.GearItem))
  let decls = decl1 ++ "\n\n" ++ decl2
  writeFile "src/types.ts" decls
