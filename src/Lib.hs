module Lib
    ( extractBankStmts
    ) where

-- extract concerned bankstatements with checking amt of a PaymentID in ledger table
-- checking -> [checking] (same day) -> [bankstmt] --> [concerned bankstmt]
extractBankStmts :: (Num a, Eq a) => a -> [a] -> [a] -> [a]
extractBankStmts r rs bs =
    case concat [matchNto1 xs bs | xs <- allCombs r rs] of
        rr1@(_:_) -> rr1
        [] ->
            case concat [matchNto2 xs bs | xs <- allCombs r rs] of
                rr2@(_:_) -> rr2
                [] -> []

-- merged case: N payment = 1 bankstmt
matchNto1 :: (Num a, Eq a) => [a] -> [a] -> [a]
matchNto1 rs = filter (== sum rs)

-- split case 2: sum(N payment) = 2 bankstmts (only first 3 bankstmts considered)
matchNto2 :: (Num a, Eq a) => [a] -> [a] -> [a]
matchNto2 r xs
    | not (null matched) = head matched
    | otherwise = []
  where
    matched = filter (\x -> head x + last x == sum r) (combination 2 xs)

-- make combination: nCr
combination :: Num a => Int -> [a] -> [[a]]
combination 0 _ = [[]]
combination _ [] = []
combination n (x:xs) = map (x :) (combination (n - 1) xs) ++ combination n xs

-- combinations having concerned amt:  checking -> all checkings --> [all combinations include concerned checking amt]
allCombs :: (Num a, Eq a) => a -> [a] -> [[a]]
allCombs 0 _ = []
allCombs _ [] = []
allCombs r rs = filter (elem r) $ cs
  where
    cnt = length rs
    cs = concat $ [combination n rs | n <- [1 .. cnt]]
