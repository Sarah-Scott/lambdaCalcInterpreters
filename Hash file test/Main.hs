#!/usr/bin/env stack
-- stack --resolver lts-9.3 script

--getLine :: IO Text
--encodeUtf8 :: Text -> ByteString
--hashWith :: (ByteArrayAccess ba, HashAlgorithm alg) => alg -> ba -> Digest alg
--readFile :: FilePath -> IO ByteString



--do I have to call each of these main?
--how do I have multiple functions at a time?
--I don't understand this main/IO business


{-
--hashes a file given statically using SHA256

import           Crypto.Hash             (hashWith, SHA256 (..))
import qualified Data.ByteString         as B

main :: IO ()
main = do
  fileContent <- B.readFile "testMultiLine.txt"
  print fileContent
  let digest = hashWith SHA256 fileContent
  print digest
-}

{-
--generates a nonce

import qualified Crypto.Nonce as CN
import qualified Data.ByteString as B

main :: IO ()
main = do
  g <- CN.new
  n <- CN.nonce128 g 
  print n
-}  


--signs a file's contents then verifies it

import Crypto.Sign.Ed25519
import qualified Data.ByteString as B

main :: IO ()
main = do
  (pk0,sk0) <- createKeypair
  (pk1,sk1) <- createKeypair
  fileContent <- B.readFile "test1.txt"
  let msg = sign sk0 fileContent
  print $ verify pk0 msg
  print $ verify pk1 msg
