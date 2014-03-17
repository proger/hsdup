{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import System.FilePath (FilePath, (</>))

import Data.Char (ord)
import Numeric (showHex)
import Data.Function (on)

import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as C

import qualified Data.Map.Strict as Map
import Data.List (sortBy)

import System.Directory.Tree
import qualified Crypto.Hash.SHA256 as S

type FileMap = Map.Map C.ByteString [FilePath]

hash :: FilePath -> IO (FilePath, C.ByteString)
hash path = LBS.readFile path >>= \a -> return (path, let h = S.hashlazy a in h `seq` h)

-- uncomment these if you want to see human-readable hashes
--
-- hash path = LBS.readFile path >>= \a -> return (path, (hexdigest . C.unpack . S.hashlazy) $ a)

-- hexdigest :: String -> C.ByteString
-- hexdigest s = C.concat $ map (hex . ord) s
--   where
--     hex i = C.pack $ case showHex i "" of
--               c:[] -> ['0', c]
--               cs -> cs

hashFiles :: FilePath -> IO FileMap
hashFiles path = readDirectoryWithL hash path >>= return . go . dirTree
  where
    f :: (FilePath, C.ByteString) -> FileMap -> FileMap
    f (path, hash) map = Map.insertWith (++) hash [path] map

    go :: DirTree (FilePath, C.ByteString) -> FileMap
    go tree = F.foldr f Map.empty tree

prep :: FileMap -> FileMap
prep = fmap (drop 1 . sortBy (compare `on` length)) . Map.filter ((>1) . length) 

redundant :: FilePath -> IO [FilePath]
redundant path = hashFiles path >>= return . concat . Map.elems . prep

main = do
    path <- fmap head getArgs
    files <- redundant path
    mapM_ putStrLn files
