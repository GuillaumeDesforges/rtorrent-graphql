module Main where

import Network.Wai.Handler.Warp (run)
import Server (getDefaultServer)

main :: IO ()
main = run 3000 =<< getDefaultServer