import Data.Char (chr, ord, isUpper, toUpper)
import Data.List (maximumBy)
import Data.Ord (comparing)
import qualified Data.Map.Strict as M

shiftChar k c
  | isUpper c = chr $ base + ((ord c - base + k) `mod` 26)
  | otherwise = c
  where base = ord 'A'

caesarShift k = map (shiftChar k)

letterCounts txt = foldr go M.empty (map toUpper txt)
  where
    go ch m
      | ch >= 'A' && ch <= 'Z' = M.insertWith (+) ch 1 m
      | otherwise = m

mostFrequentLetter txt =
  if null countsList then Nothing else Just (maximumBy (comparing snd) countsList)
  where
    counts = letterCounts txt
    countsList = M.toList counts

freqGuessCaesar cipher = case mostFrequentLetter cipher of
  Nothing -> Nothing
  Just (mostFreqChar, freq) ->
    let base = ord 'A'
        cIdx = ord mostFreqChar - base
        eIdx = ord 'E' - base
        key = (cIdx - eIdx) `mod` 26 
        decrypted = caesarShift (-key) cipher
    in Just (key, mostFreqChar, freq, decrypted)

printFreqAnalysis cipher = do
  putStrLn "\nFrequency analysis (assuming most frequent letter -> 'E'):\n"
  case mostFrequentLetter cipher of
    Nothing -> putStrLn "No alphabetic characters found in input."
    Just (ch, count) -> do
      putStrLn $ "Most frequent ciphertext letter: '" ++ [ch] ++ "' (count = " ++ show count ++ ")"
      case freqGuessCaesar cipher of
        Nothing -> putStrLn "Could not produce a guess."
        Just (key, mf, freq, guess) -> do
          putStrLn $ "Assuming '" ++ [mf] ++ "' -> 'E'"
          putStrLn $ "Inferred key (encryption shift): " ++ show key
          putStrLn $ "Decrypted guess: " ++ guess

bruteForce cipher = do
  putStrLn "\nBrute-force candidates (Key -> Plaintext):\n"
  let cipherUpper = map toUpper cipher
  mapM_ (\k -> putStrLn $ "Key = " ++ show k ++ " -> " ++ caesarShift (-k) cipherUpper) [0..25]

main :: IO ()
main = do
  putStrLn "Enter the ciphertext:"
  cipher <- getLine

  printFreqAnalysis cipher

  putStrLn "\nWould you like to see all brute-force candidates as well? (y/N)"
  ans <- getLine
  if not (null ans) && (toUpper (head ans) == 'Y')
    then bruteForce cipher
    else putStrLn "Done."
