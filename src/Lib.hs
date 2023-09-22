module Lib
    ( someFunc
    , extractBankStmts
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- extract concerned bankstatements with checking amt of a PaymentID in ledger table 
-- checking -> [checking] (same day) -> [bankstmt] --> [concerned bankstmt]
extractBankStmts' :: Int -> [Int] -> [Int] -> [Int]
extractBankStmts' r rs bs =
    case match1to1 r bs of    -- 1 checking <--> 1 bankstmt
        rr1@(_:_) -> rr1
        [] -> case matchNto1 rs bs of  -- all checking on a same day <--> 1 bankstmt (merged case)
            rr2@(_:_) -> rr2
            [] -> case match1to2 r bs of  -- 1 checking <--> 2 bankstmts (split case)
                rr3@(_:_) -> rr3
                [] -> case matchNto2 rs bs of -- all checking on a samde day<--> 2 bankstmts (split case)
                    rr4@(_:_) -> rr4
                    [] -> case matchNtoN rs bs of -- all checking on a samde day <--> all bankstmts (merged case) 
                        rr5@(_:_) -> rr5
                        [] -> []

-- checking -> [checking] (same day) -> [bankstmt] --> [concerned bankstmt]
matchTo1BankStmt :: (Num a, Eq a) => a -> [a] -> [a] -> [a]
matchTo1BankStmt r rs bs = concat [ matchNto1 xs bs | xs <- allCombs r rs ]


matchTo2BankStmts :: (Num a, Eq a) => a -> [a] -> [a] -> [a]
matchTo2BankStmts r rs bs = concat [ matchNto2 xs bs | xs <- allCombs r rs ]

extractBankStmts :: (Num a, Eq a) => a -> [a] -> [a] -> [a]
extractBankStmts r rs bs =
  case concat [ matchNto1 xs bs | xs <- allCombs r rs ] of
    rr1@(_:_) -> rr1
    [] -> case concat [ matchNto2 xs bs | xs <- allCombs r rs ] of
      rr2@(_:_) -> rr2
      [] -> []
  
-- simple case: 1 pid = 1 bankstmt
match1to1 :: Eq a => a -> [a] -> [a]
match1to1 r = filter (==r)

-- merged case: N pid = 1 bankstmt
matchNto1 :: (Num a, Eq a) => [a] -> [a] -> [a]
matchNto1 rs = filter (==sum rs)

-- split case 1: 1 pid = 2 bankstmts (only first 3 bankstmts considered)
match1to2 :: (Num a, Eq a) => a -> [a] -> [a]
match1to2 r xs 
  | not (null matched) = head matched
  | otherwise = []
  where matched = filter (\x -> head x + last x == r) (combination 2 xs)

-- split case 2: sum(N pid) = 2 bankstmts (only first 3 bankstmts considered)
matchNto2 :: (Num a, Eq a) => [a] -> [a] -> [a]
matchNto2 r xs 
  | not (null matched) = head matched
  | otherwise = []
  where matched = filter (\x -> head x + last x == sum(r)) (combination 2 xs)

-- split case 3: sum(N pid) == sum(bankstmts)
matchNtoN :: (Num a, Eq a) => [a] -> [a] -> [a]
matchNtoN rs xs = if sum(rs) == sum(xs) then xs else []

-- make combination: nCr 
combination :: Num a => Int -> [a] -> [[a]]
combination 0 _ = [[]]
combination _ [] = []
combination n (x:xs) = map (x:) (combination (n-1) xs) ++ combination n xs

-- combinations having concerned amt:  checking -> all checkings --> [all combinations include concerned checking amt]
allCombs :: (Num a, Eq a) => a -> [a] -> [[a]]
allCombs 0 _ = []
allCombs _ [] = []
allCombs r rs =
  filter (elem r) $ cs
  where cnt = length rs
        cs = concat $ [ combination n rs | n <- [1 .. cnt] ]

match1to2' :: Int -> [Int] -> [Int]
match1to2' r (x:y:z:_) 
  | r == x + y = [x, y]
  | r == x + z = [x, z]
  | r == y + z = [y, z]
  | otherwise = []
match1to2' r (x:y:_) = if r == x + y then [x,y] else []
match1to2' _ _ = []

-- split case 2: sum(N pid) = 2 bankstmts (only first 3 bankstmts considered)
matchNto2' :: [Int] -> [Int] -> [Int]
matchNto2' rs (x:y:z:_)
  | sum(rs) == x + y = [x, y]
  | sum(rs) == x + z = [x, z]
  | sum(rs) == y + z = [y, z]
  | otherwise = []
matchNto2' rs (x:y:_) = if sum(rs) == x + y then [x,y] else []   
matchNto2' _ _ = []

 


