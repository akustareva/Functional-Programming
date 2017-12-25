{-# LANGUAGE TemplateHaskell #-}

import TemplateHaskell (showTextFunc)

data MyData = MyData
     { foo :: String
     , bar :: Int
     }
     deriving(Show)

showTextFunc ''MyData

-- print $ MyData { foo = "bar", bar = 5 }