module VigenereCipher where

import Data.Char

shiftEnc p k =
    chr (ord 'A' + ((ord p - ord 'A' + ord k - ord 'A') `mod` 26))

shiftDec c k =
    chr (ord 'A' + ((ord c - ord 'A' - (ord k - ord 'A')) `mod` 26))

vigenereEncrypt text key = go (map toUpper text) (cycle (map toUpper key))
  where
    go [] _ = []
    go (c:cs) (k:ks)
      | isUpper c = shiftEnc c k : go cs ks
      | otherwise = c : go cs (k:ks) 

vigenereDecrypt text key = go (map toUpper text) (cycle (map toUpper key))
  where
    go [] _ = []
    go (c:cs) (k:ks)
      | isUpper c = shiftDec c k : go cs ks
      | otherwise = c : go cs (k:ks)

main :: IO ()
main = do
    putStrLn "Enter message (uppercase letters will be processed):"
    msg <- getLine
    putStrLn "Enter key:"
    key <- getLine

    let enc = vigenereEncrypt msg key
    putStrLn $ "Encrypted: " ++ enc

    let dec = vigenereDecrypt enc key
    putStrLn $ "Decrypted (check): " ++ dec
