{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Tests where

import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as LBS (ByteString)
import Network.HTTP.Types (methodPost)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai.Test (SResponse)
import Server (getDefaultServer)
import System.Process (ProcessHandle, spawnProcess, terminateProcess)
import Test.Hspec (Spec, context, describe, it)
import Test.Hspec.Wai (WaiSession, request, shouldRespondWith, with)
import Test.Hspec.Wai.JSON (json)
import Test.Tasty (TestTree, defaultMain, testGroup, withResource)
import Test.Tasty.Hspec (testSpec)

main :: IO ()
main = tests >>= defaultMain

tests :: IO TestTree
tests = testGroup "Tests" . (: []) <$> integrationTests

-- Integration tests with rTorrent
integrationTests :: IO TestTree
integrationTests = do
  -- Build tasty tests from HSpec
  spec <- testSpec "Integration" specs
  return $ withResource openRtorrent closeRtorrent $ const spec
  where
    -- Test uses the same default WAI Application as dev or prod
    app = getDefaultServer

    -- Handle rTorrent as a tasty resource
    openRtorrent :: IO ProcessHandle
    openRtorrent = do
      -- spawn rtorrent
      spawnProcess
        "rtorrent"
        [ "-n", -- do not load ~/.rtorrent.rc
          "-o import=./rtorrent.rc" -- load ./rtorrent.rc instead
        ]
    closeRtorrent :: ProcessHandle -> IO ()
    closeRtorrent handle = terminateProcess handle

    -- Tests

    -- List all tests
    specs :: Spec
    specs = with app $ do
      describe "Login" $ do
        it "fails on empty payload" $ do
          postJson "/login" [json|{}|] `shouldRespondWith` 400
        it "returns credentials on good payload" $ do
          postJson "/login" [json|{username:"someUsername",password:"somePassword"}|] `shouldRespondWith` [json|{token:"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.e30.QtmAB4MXp1A9X0yJXTtag0bXsmj9A4p1l9MgHFDCOnY"}|]

      describe "Query .hello" $ do
        it "returns data" $ do
          postJson "/api" [json|{query: "{hello}"}|] `shouldRespondWith` [json|{data: {hello: "Hello world"}}|]

postJson :: BS.ByteString -> LBS.ByteString -> WaiSession st SResponse
postJson route = request methodPost route [(hContentType, "application/json")]