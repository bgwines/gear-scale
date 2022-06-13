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
  putStrLn "Writing API to `src/client.ts`..."
  putStrLn "Writing TS types to `src/client.d.ts`..."
  writeTypeScriptLibrary ServerAPI.limitedApi "src/"

  -- TODO: currently need to manually add
  --           import {Trip, GearItem, GearKind} from "./client.d";
  --       to src/client.ts
  --
  --       we don't yet use src/client.ts; who writes to it?
