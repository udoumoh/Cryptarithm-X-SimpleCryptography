module CaesarCipher where

import Data.Char

main :: IO ()
main = do
  putStrLn "Enter the message:"
  msg <- getLine

  putStrLn "Enter the key (integer):"
  keyInput <- getLine
  let key = read keyInput :: Int

  let c = caesarEncrypt key msg
  putStrLn $ "Encrypted: " ++ c
  putStrLn $ "Decrypted (for check): " ++ caesarDecrypt key c


shiftChar :: Int -> Char -> Char
shiftChar k c
  | isUpper c = chr $ base + ((ord c - base + k) `mod` 26)
  | otherwise = c
  where base = ord 'A'

caesarEncrypt :: Int -> String -> String
caesarEncrypt k = map (shiftChar k)

caesarDecrypt :: Int -> String -> String
caesarDecrypt k = map (shiftChar (-k))
