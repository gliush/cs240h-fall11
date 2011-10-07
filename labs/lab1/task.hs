import Data.Char(toLower)
import Data.List(sortBy)
import Control.Monad(mapM)
import Data.Map(empty,toList,alter,Map)

main = do
    lines <- getContents
    let strings = analyze lines
    mapM_ putStrLn strings

analyze :: String -> [String]
analyze lines = strings 
    where
        allwords = words $ map toLower $ rmPunct lines
        wordsFreq = freq allwords
        wordsFreqNorm = normalize wordsFreq
        strings = map stringRepr wordsFreqNorm

rmPunct :: String -> String
rmPunct [] = []
rmPunct (x:xs) | isPunct x = ' ':rmPunct xs
                | otherwise = x:rmPunct xs

isPunct x = x `elem` ",.:;-!@#$%^&*()+=|]}[{`~\"?/><"

freq :: [String] -> [(String, Int)]
freq allwords = sortit  $ freq' allwords empty
    where sortit = sortBy (\(_, a) (_, b) -> compare b a)

freq' :: [String] -> Map String Int -> [(String, Int)]
freq' [] acc = toList acc
freq' (word:words) acc = freq' words newacc
    where
        f (Just v) = Just (v+1)
        f Nothing = Just 1
        newacc = alter f word acc

normalize :: [(String, Int)] -> [(String, Int, Int)]
normalize input = 
    [(word, maxWord, n) | (word,c) <- input, let n = number c, n > 0 ] 
    where
        maxWord = maximum $ map (length.fst) input
        maxCount = maximum $ map snd input
        mult = fromIntegral (79 - maxWord) / fromIntegral maxCount
        number c = truncate (fromIntegral c * mult)
  
stringRepr :: (String, Int, Int) -> String
stringRepr (word, maxLen, num) = 
    word ++ replicate (1+maxLen-length word) ' ' ++ replicate num '#'
