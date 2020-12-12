{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Tests where

import Network.HTTP.Types (methodPost)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Application)
import Server (getDefaultServer)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (ProcessHandle, spawnProcess, terminateProcess)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Wai (request, shouldRespondWith, with)
import Test.Hspec.Wai.JSON (json)
import Test.Tasty (TestTree, defaultMain, testGroup, withResource)
import Test.Tasty.Hspec (testSpec)

main :: IO ()
main = tests >>= defaultMain

tests :: IO TestTree
tests =
  let integrationTests :: IO TestTree
      integrationTests =
        let openRtorrent :: IO ProcessHandle
            openRtorrent = do
              -- spawn rtorrent
              spawnProcess
                "rtorrent"
                [ "-n", -- do not load ~/.rtorrent.rc
                  "-o import=./rtorrent.rc" -- load ./rtorrent.rc instead
                ]

            closeRtorrent :: ProcessHandle -> IO ()
            closeRtorrent handle = terminateProcess handle

            apiResponds :: IO Application -> Spec
            apiResponds app = with app $ do
              describe "Query .hello" $ do
                it "returns data" $ do
                  let testRequest = request methodPost "/api" [(hContentType, "application/json")] [json|{query: "{hello}"}|]
                  testRequest `shouldRespondWith` [json|{data: {hello: "Hello world"}}|]
         in return $
              withResource openRtorrent closeRtorrent $ \_ ->
                withResource getDefaultServer mempty $ \app -> unsafePerformIO $ testSpec "Integration" (apiResponds app)
   in testGroup "Tests" . (: []) <$> integrationTests