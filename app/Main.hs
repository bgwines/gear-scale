module Main where

import qualified Server

main :: IO ()
main = do
  putStrLn $ "Writing JS files..."
  Server.writeJSFiles

  let port = 8081
  putStrLn $ "Running server on port " ++ show port ++ "..."
  Server.runServer port
