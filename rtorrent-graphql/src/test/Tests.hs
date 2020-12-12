{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Tests where

import Network.HTTP.Types (methodPost)
import Network.HTTP.Types.Header (hContentType)
import Server (getDefaultServer)
import System.Process (ProcessHandle, spawnProcess, terminateProcess)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Wai (request, shouldRespondWith, with)
import Test.Hspec.Wai.JSON (json)
import Test.Tasty (TestTree, defaultMain, testGroup, withResource)
import Test.Tasty.Hspec (testSpec)

main :: IO ()
main = tests >>= defaultMain

tests :: IO TestTree
tests = do
  let integrationTests :: IO TestTree
      integrationTests = do
        -- Test the GraphQL API using Test.Hspec.Wai
        let -- The GraphQL server to be tested
            app = getDefaultServer
            -- Tests
            apiResponds :: Spec
            apiResponds = with app $ do
              describe "Query .hello" $ do
                it "returns data" $ do
                  let testRequest = request methodPost "/api" [(hContentType, "application/json")] [json|{query: "{hello}"}|]
                  testRequest `shouldRespondWith` [json|{data: {hello: "Hello world"}}|]
        -- Build tasty tests from HSpec
        spec <- testSpec "Integration" apiResponds
        -- Those tests will require rTorrent to be running
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
        -- Integration tests with rTorrent
        return $ withResource openRtorrent closeRtorrent $ const spec
   in testGroup "Tests" . (: []) <$> integrationTests