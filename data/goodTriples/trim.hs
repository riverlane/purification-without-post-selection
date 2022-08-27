test ([c1,tr1,p1],[c2,tr2,p2],[c3,tr3,p3])
  | (c1,tr1) /= (c2,tr2) = True -- first
  | (c3,tr3) /= (c2,tr2) = True -- or last in column
  | p2 > p1 + 0.00015 = True -- after a jump
  | p2 < p3 - 0.00015 = True -- before a jump
  | otherwise = False

middle (_,y,_) = y

read' s = (c,tr,p) where
  [c,tr,p] = map read $ words s

main = do
  contents <- getContents
  let c_tr_ps = map (map read . words) $ lines contents :: [[Double]]
  let z3 = zip3 c_tr_ps (tail c_tr_ps) (tail $ tail c_tr_ps)
  let filtered = map middle $ filter test z3
  putStrLn $ unlines $ map (unwords . map show) filtered
