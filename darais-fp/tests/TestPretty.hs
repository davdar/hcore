module Main where

import Prelude()
import FP
import qualified FP.Pretty as P

test1 :: IO ()
test1 = do
  let e = 
        [ [ [ [ 123::Int ] ] ]
        , [ []
          , [ [ 123 ]
            , []
            , [ 123 ]
            ]
          , [ [ 123 , 123 , 123 , 123 , 123 ] ]
          ]
        , [ [ [ 123 , 123 , 123 ] ] ]
        ]
  putStrLn $ printf "printing: %s" $ show e
  pprintLn e

data Exp = Plus Exp Exp | Times Exp Exp | Num Int
  deriving (Eq, Ord, Show)
instance Pretty Exp where
  precLattice Proxy = compile [ ("*", "+") ]
  pretty (Plus e1 e2) = P.guardLevel (level "+") $ do
    -- l <- askView P.precLevel
    P.hsep
      [ P.bump $ pretty e1
      , P.punctuation $ P.string $ "+" -- ++ "<" ++ show l ++ ">"
      , pretty e2
      ]
  pretty (Times e1 e2) = P.guardLevel (level "*") $ do
    -- l <- askView P.precLevel
    P.hsep
      [ P.bump $ pretty e1
      , P.punctuation $ P.string $ "*" -- ++ "<" ++ show l ++ ">"
      , pretty e2
      ]
  pretty (Num i) = P.literal $ P.string $ show i

test2 :: IO ()
test2 = do
  let e = Plus (Plus (Times (Times (Num 1) 
                                   (Plus (Num 2) 
                                         (Num 3))) 
                            (Num 4)) 
                     (Num 5))
               (Plus (Num 6) 
                     (Plus (Num 7) 
                           (Times (Num 8) 
                                  (Num 9))))
  putStrLn $ printf "printing: %s" $ show e
  putStrLn "expecting:"
  putStrLn "((1 * (2 + 3)) * 4 + 5) + 6 + 7 + 8 * 9"
  pprintLn e

main :: IO ()
main = test2
