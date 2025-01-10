import Data.List

-- rleEncode
getCount :: String -> (Integer, Char)
getCount [] = (0, '.')
getCount str = (fromIntegral (length str), str !! 0)

getCounts :: [String] -> [(Integer, Char)]
getCounts [] = [(0, '.')]
getCounts s = map getCount s

-- rleEncode "aaaabbaaa" -> [(4,'a'), (2, 'b'), (3, 'a')]
rleEncode :: String -> [(Integer, Char)]
rleEncode inp = getCounts (group inp)
-- rleEncode inp = [0, group (inp)]

-- rleDecode
decodeStep :: (Integer, Char) -> String
decodeStep encoded = replicate (fromIntegral (fst encoded)) (snd encoded)

-- rleDecode [(4,'a'), (2, 'b'), (3, 'a')] -> "aaaabbaaa"
rleDecode :: [(Integer, Char)] -> String
rleDecode [] = " " 
rleDecode encoded = concat (map (decodeStep) encoded)
-- rleDecode (s : str) = (replicate (fromIntegral (fst s)) (snd s)) : rleDecode str

main :: IO ()
main = do
  let code = "aaaabbaaa"
  print $ rleEncode code
  let encoded = rleEncode code
  print $ snd ((rleEncode code) !! 0)
  print $ decodeStep (encoded !! 0)
  print $ rleDecode (rleEncode code)