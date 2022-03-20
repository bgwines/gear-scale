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
import Servant.TypeScript ( writeTypeScriptLibrary )

$(deriveTypeScript defaultOptions ''ClientTypes.GearKind)
$(deriveTypeScript defaultOptions ''ClientTypes.GearItem)
$(deriveTypeScript defaultOptions ''ClientTypes.Trip)

main :: IO ()
main = do
  putStrLn "Writing API to `src/backend_api.js`..."
  writeTypeScriptLibrary ServerAPI.limitedApi "src/"

  putStrLn "Writing TS types to `src/types.ts`..."
  let decl1 = "export " ++ formatTSDeclarations (getTypeScriptDeclarations (Proxy :: Proxy ClientTypes.GearKind))
  let decl2 = "export " ++ formatTSDeclarations (getTypeScriptDeclarations (Proxy :: Proxy ClientTypes.GearItem))
  let decl3 = "export " ++ formatTSDeclarations (getTypeScriptDeclarations (Proxy :: Proxy ClientTypes.Trip))

  -- TODO: currently need to manually add
  --           import {Trip, GearItem, GearKind} from "./client.d";
  --       to src/client.ts
  --
  --       currently need to manually add `export` to all top-level functions in
  --       `src/backend_api.js`


  let decls = decl1 ++ "\n\n" ++ decl2 ++ "\n\n" ++ decl3
  writeFile "src/types.ts" decls
