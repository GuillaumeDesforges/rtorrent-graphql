module Tests where

import Test.HTTP
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [integrationTests]

integrationTests :: TestTree
integrationTests = testGroup "Integration" [apiResponds]

apiResponds = httpTestCase "API responds" "http://localhost:3000" $
  do
    post "/api" "{\"query\": \"{ hello }\"}"
    return ()