module Main (main) where
    
import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
    describe "Extract bank statements" $ do
        it "can handle simple case  NK_SP_20230816: 76326484 [76326484] [76326484]" $ do
            extractBankStmts 76326484 [76326484] [76326484] `shouldBe` ([76326484] :: [Int])

        it "can handle merged case  NK_SP_20230820: 57093789 [53723969, 68353888, 57093789] [179171646]" $ do
            extractBankStmts 57093789 [53723969, 68353888, 57093789] [179171646] `shouldBe` ([179171646] :: [Int])
  
        it "can handle split case1  NP_SP_20230816: 315632478 [315632478] [129016435, 186616043]" $ do
            extractBankStmts 315632478 [315632478] [129016435, 186616043] `shouldBe` ([129016435, 186616043] :: [Int])

        it "can handle split case2a SW_LJ_OFF_2023811: 9598554 [9598554, 49366127] [9566395, 32159, 49366127]" $ do
            extractBankStmts 9598554 [9598554, 49366127] [9566395, 32159, 49366127] `shouldBe` ([9566395, 32159] :: [Int])    

        it "can handle split case2b SW_LJ_OFF_2023816: 49366127 [9598554, 49366127] [9566395, 32159, 49366127]" $ do
            extractBankStmts 49366127 [9598554, 49366127] [9566395, 32159, 49366127] `shouldBe` ([49366127] :: [Int])    
