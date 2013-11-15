{-# LANGUAGE TypeOperators #-}

module Data.Compose where

data (:.:) c d a = Compose { runCompose :: c (d a) }
