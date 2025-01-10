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

-- rleDecode [(4,'a'), (2, 'b'), (3, 'a')] -> "aaaabbaaa"
rleDecode :: [(Integer, Char)] -> String
rleDecode inp = replicate inp

main :: IO ()
main = do
  let code = "aaaabbaaa"
  print $ rleEncode code
  print $ rleDecode (rleEncode code)