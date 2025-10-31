module CryptAnalysisTool where


import Data.Char (chr, ord, isUpper, toUpper, isAlpha)
import qualified Data.Set as S
import Data.List (sortBy, maximumBy)
import Data.Ord (comparing)
import Control.Monad (forM_)

shiftChar key letter
  | isUpper letter =
      let base = ord 'A'
      in chr $ base + ((ord letter - base + key) `mod` 26)
  | otherwise = letter

-- Apply a Caesar shift to the entire string
caesarShift key = map (shiftChar key)

-- Try all 26 possible shifts (0..25) and return (key, plaintext)
bruteForce cipher =
  [ (key, caesarShift (-key) cipherUpper) | key <- [0..25] ]
  where
    cipherUpper = map toUpper cipher

-- Break text into a list of words made only of uppercase letters.
tokenize [] = []
tokenize s  =
  let (letters, rest) = span isAlpha s
  in case letters of
       []   -> tokenize (drop 1 rest)
       word -> map toUpper word : tokenize rest

-- Count how many of the plaintext's tokens are real English words
scorePlaintext dict plaintext =
  length [ word | word <- tokenize plaintext, word `S.member` dict ]

-- Pick the words that look more like english words using dictionary scores.
bestGuess dict candidates =
  maximumBy (comparing (\(_,_,sc) -> sc))
    [ (key, txt, scorePlaintext dict txt)
    | (key, txt) <- candidates
    ]

-- Print all candidates ranked by score (best first)
printRanked dict candidates = do
  let ranked = sortBy (flip $ comparing (\(_,txt) -> scorePlaintext dict txt)) candidates
  putStrLn "\nAll candidates ranked by English score (best first):\n"
  forM_ ranked $ \(k, txt) -> do
    let sc = scorePlaintext dict txt
    putStrLn $ "Key " ++ show k ++ " [score " ++ show sc ++ "]: " ++ txt

-- Load the dictionary file and convert each line in the file to uppercase.
loadDictionary path = do
  raw <- readFile path
  let ws      = lines raw    
      wsUpper = map (map toUpper) ws
  return (S.fromList wsUpper)


main :: IO ()
main = do
  putStrLn "Loading dictionary..."
  dict <- loadDictionary "../words.txt"

  putStrLn "Enter the ciphertext:"
  cipher <- getLine

  let candidates = bruteForce cipher
      (bestK, bestTxt, bestScore) = bestGuess dict candidates

  putStrLn "\nMost likely decryption (based on English word hits):\n"
  putStrLn $ "Best key = " ++ show bestK ++ " (score " ++ show bestScore ++ ")"
  putStrLn bestTxt

  printRanked dict candidates
