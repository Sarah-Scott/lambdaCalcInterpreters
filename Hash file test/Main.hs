#!/usr/bin/env stack
-- stack --resolver lts-9.3 script




{-
--hashes a file given statically using MD5


import qualified Data.ByteString.Lazy as LB
import Data.Digest.Pure.MD5

main :: IO ()
main = do
    fileContent <- LB.readFile "test1.txt"
    let md5Digest = md5 fileContent
    print md5Digest
-}

{-
--hashes a string given at runtime using SHA256

import           Crypto.Hash             (hashWith, SHA256 (..))
import           Data.ByteString         (ByteString)
import           Data.Text.Encoding      (encodeUtf8)
import qualified Data.Text.IO            as TIO
import           System.IO               (hFlush, stdout)

main :: IO ()
main = do
  putStr "Enter some text: "
  hFlush stdout
  text <- TIO.getLine
  let bs = encodeUtf8 text
  putStrLn $ "You entered: " ++ show bs
  let digest = hashWith SHA256 bs
  putStrLn $ "SHA256 hash: " ++ show digest
-}



--getLine :: IO Text
--encodeUtf8 :: Text -> ByteString
--hashWith :: (ByteArrayAccess ba, HashAlgorithm alg) => alg -> ba -> Digest alg
--readFile :: FilePath -> IO ByteString



--main can't have parameters
--so getting the filename has to be hardcoded, got during compile time, or asked for during runtime




--hashes a file given at runtime using SHA256

import           Crypto.Hash             (hashWith, SHA256 (..))
import qualified Data.ByteString         as B

main :: IO ()
main = do
  putStrLn "Give the name of the file: "
  fileName <- getLine
  fileContent <- B.readFile fileName
  putStrLn $ "The file contains: " ++ show fileContent
  let digest = hashWith SHA256 fileContent
  print digest

