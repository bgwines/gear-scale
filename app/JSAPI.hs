module JSAPI
  ( writeJSFiles
  ) where

import qualified ServerAPI
import Servant.JS ( writeJSForAPI, vanillaJS )

writeJSFiles :: IO ()
writeJSFiles = writeJSForAPI ServerAPI.limitedApi vanillaJS "static/api.js"
