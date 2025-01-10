import Data.List

-- rleEncode
getCount :: String -> (Integer, Char)
getCount [] = (0, '.')
getCount str = (fromIntegral (length str), (str) !! 0)

getCounts :: [String] -> [(Integer, Char)]
getCounts [] = [(0, '.')]
getCounts s = map (getCount) s

-- rleEncode "aaaabbaaa" -> [(4,'a'), (2, 'b'), (3, 'a')]
rleEncode :: String -> [(Integer, Char)]
-- rleEncode _ = [(0, 'a')]
rleEncode inp = getCounts (group inp)
-- rleEncode inp = [0, group (inp)]

-- rleDecode [(4,'a'), (2, 'b'), (3, 'a')] -> "aaaabbaaa"

main :: IO ()
main = do
  print $ rleEncode "aaaabbaaa"
  print $ group "aaaabbaaa"
  print $ getCount ((group "aaaabbaaa") !! 0)
  print $ getCounts (group "aaaabbaaa")