{-# LANGUAGE OverloadedStrings #-}

module ImportExport
  ( importGearItems
  , exportGearItems
  ) where

import Control.Monad (when)
import qualified Data.Text as T
import qualified System.IO

import qualified DB
import qualified Ids
import Data.Aeson (Value(String))

lineToGearItem :: String -> Ids.Id -> DB.GearItem
lineToGearItem line itemId = DB.GearItem
    { DB._gearitemId            = itemId
    , DB._gearitemName          = name
    , DB._gearitemIsPersonal    = isPersonal
    , DB._gearitemOz            = oz
    , DB._gearitemKind          = kind
    , DB._gearitemCreatorUserId = "Brett"
    }
  where
    parts :: [String]
    parts = map T.unpack . T.splitOn "," . T.pack $ line

    rawName :: T.Text
    rawName = T.pack . concat . init . init $ parts

    name :: T.Text
    name = if isPersonal then rawName else T.pack . drop 4 . T.unpack $ rawName

    oz :: Double
    oz = read . last . init $ parts

    kind :: DB.GearKind
    kind = read $ last parts

    isPersonal :: Bool
    isPersonal = "[G]" /= (take 3 . T.unpack $ rawName)

-- line format: "name,oz,kind"
--              (name starts with [G] if it's group gear)
--              (name may contain commas)
importGearItemsOldFormat :: String -> Bool -> IO ()
importGearItemsOldFormat filename shouldClearDBFirst = do
  when shouldClearDBFirst DB.clearAllGearItems
  fileLines <- lines <$> System.IO.readFile filename
  ids <- mapM (const Ids.generate) fileLines
  let items = zipWith lineToGearItem fileLines ids
  mapM_ DB.putGearItem items

importGearItems :: String -> Bool -> IO ()
importGearItems filename shouldClearDBFirst = do
  when shouldClearDBFirst DB.clearAllGearItems
  fileLines <- lines <$> System.IO.readFile filename
  let items = map readGearItem fileLines
  mapM_ DB.putGearItem items
  where
    readGearItem :: String -> DB.GearItem
    readGearItem = read

-- TODO: periodic backup
exportGearItems :: String -> IO ()
exportGearItems filename = do
  items <- DB.getAllGearItems
  -- tail to remove first \n
  let contents = tail $ concatMap ((++) "\n" . show) items
  System.IO.writeFile filename contents
