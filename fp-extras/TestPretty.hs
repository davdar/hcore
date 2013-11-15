module Main where

import Prelude()
import FP

main :: IO ()
main = do
  let x = 
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
  putStrLn $ printf "printing: %s" $ show x
  pprintLnWith (localViewSet layoutWidthL 10) x
