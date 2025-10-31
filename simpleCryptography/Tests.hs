module Tests where

import Data.Char (toUpper)
import qualified Data.Set as S

import CaesarCipher 
import VigenereCipher     
import CryptAnalysisTool 

------------------------------------------------------------
-- 1. Caesar round-trip test
--    Encrypt with a key, decrypt with the same key, expect original text.
------------------------------------------------------------

test_caesarRoundTrip :: IO ()
test_caesarRoundTrip = do
  let key    = 3
      plain  = "HELLO WORLD"
      enc    = caesarEncrypt key plain
      dec    = caesarDecrypt key enc

  putStrLn "[Caesar Round-Trip]"
  putStrLn $ "  Plain: " ++ plain
  putStrLn $ "  Enc  : " ++ enc
  putStrLn $ "  Dec  : " ++ dec
  if dec == plain
    then putStrLn "  Result: OK\n"
    else putStrLn "  Result: FAIL\n"

------------------------------------------------------------
-- 2. Vigenère round-trip test
--    Encrypt with a key, decrypt with the same key, expect original text.
--    We normalise case because vigenereEncrypt may uppercase output.
------------------------------------------------------------

test_vigenereRoundTrip :: IO ()
test_vigenereRoundTrip = do
  let key    = "KEY"
      plain  = "HELLO WORLD"
      enc    = vigenereEncrypt plain key
      dec    = vigenereDecrypt enc key
      norm   = map toUpper plain

  putStrLn "[Vigenere Round-Trip]"
  putStrLn $ "  Plain: " ++ norm
  putStrLn $ "  Enc  : " ++ enc
  putStrLn $ "  Dec  : " ++ dec
  if dec == norm
    then putStrLn "  Result: OK\n"
    else putStrLn "  Result: FAIL\n"

------------------------------------------------------------
-- 3. Punctuation / spacing behaviour
------------------------------------------------------------

test_punctuationPreserved :: IO ()
test_punctuationPreserved = do
  let key    = 5
      plain  = "HELLO, WORLD!"
      enc    = caesarEncrypt key plain
      dec    = caesarDecrypt key enc

  putStrLn "[Punctuation Preservation - Caesar]"
  putStrLn $ "  Plain: " ++ plain
  putStrLn $ "  Enc  : " ++ enc
  putStrLn $ "  Dec  : " ++ dec
  if dec == plain
    then putStrLn "  Result: OK (punctuation preserved)\n"
    else putStrLn "  Result: FAIL\n"

------------------------------------------------------------
-- 4. Attacker test for Caesar
--
--    Process:
--    - Takes a known plaintext and a secret key.
--    - Encrypts it with Caesar.
--    - Calls bruteForce + bestGuess.
--    - Checks that the "best guess" the tool returns matches the original.
--
--    This demonstrates that the dictionary-scoring brute force
--    isn't just printing 26 lines, it's actually identifying the
--    correct decryption as the most likely English text.
------------------------------------------------------------

test_caesarAttacker :: IO ()
test_caesarAttacker = do
  dict <- loadDictionary "../words.txt"

  let secretKey = 7
      plain     = "HELLO WORLD THIS IS A TEST"
      cipher    = caesarEncrypt secretKey plain

  let candidates = bruteForce cipher
      (bestKey, bestPlain, score) = bestGuess dict candidates

  putStrLn "[Caesar Attacker Test]"
  putStrLn $ "  Original plaintext : " ++ plain
  putStrLn $ "  Ciphertext (key 7) : " ++ cipher
  putStrLn $ "  Recovered best key : " ++ show bestKey
  putStrLn $ "  Recovered plaintext: " ++ bestPlain
  putStrLn $ "  Score              : " ++ show score

  if map toUpper plain == bestPlain && bestKey == secretKey
    then putStrLn "  Result: OK (attacker recovered key and plaintext)\n"
    else putStrLn "  Result: FAIL (attacker did not match)\n"


runAllTests :: IO ()
runAllTests = do
  test_caesarRoundTrip
  test_vigenereRoundTrip
  test_punctuationPreserved
  test_caesarAttacker
