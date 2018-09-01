#!/usr/bin/env stack
-- stack --resolver lts-9.3 script


import Crypto.Sign.Ed25519
import qualified Data.ByteString as B
import qualified Crypto.Nonce as CN
import           Crypto.Hash             (hashWith, SHA256 (..))

--getLine :: IO Text
--encodeUtf8 :: Text -> ByteString
--hashWith :: (ByteArrayAccess ba, HashAlgorithm alg) => alg -> ba -> Digest alg
--readFile :: FilePath -> IO ByteString


--hashes a file given statically using SHA256
doHash :: IO ()
doHash = do
  fileContent <- B.readFile "testMultiLine.txt"
  print fileContent
  let digest = hashWith SHA256 fileContent
  print digest



--generates a nonce
doNonce :: IO ()
doNonce = do
  g <- CN.new
  n <- CN.nonce128 g 
  print n
--should this generate something with only numbers?
 


--signs a file's contents then verifies it
doSign :: IO ()
doSign = do
  (pk0,sk0) <- createKeypair
  (pk1,sk1) <- createKeypair
  fileContent <- B.readFile "test1.txt"
  let msg = sign sk0 fileContent
  print $ verify pk0 msg
  print $ verify pk1 msg
--what am I supposed to be signing??

