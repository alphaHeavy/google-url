{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Url
import Test.Tasty
import Test.Tasty.HUnit

typeTestUnknown :: TestTree
typeTestUnknown = testCase "Unknown Scheme" $ do
  let url = parseUrl "something:///HOSTNAME.com/"
      r = show (url :: Url)
  assertBool "Url Parsed Incorrectly" $ "something:///HOSTNAME.com/" == r

typeTestKnown :: TestTree
typeTestKnown = testCase "Known Scheme" $ do
  let correct = "http://hostname.com/"
      url1 = parseUrl "http:HOSTNAME.com"
      url2 = parseUrl "http:/HOSTNAME.com"
      url3 = parseUrl "http://HOSTNAME.com"
      url4 = parseUrl "http:///HOSTNAME.com"
  correct @=? show url1
  correct @=? show url2
  correct @=? show url3
  correct @=? show url4

typeTests :: TestTree
typeTests = testGroup "Type Tests" [typeTestUnknown, typeTestKnown]

componentTest :: TestTree
componentTest = testCase "Components" $ do
  let url = parseUrl "http://user:pass@google.com:99/foo;bar?q=a#ref"
  True @=? isValid url
  Http @=? getScheme url
  (Just "user") @=? getUsername url
  (Just "pass") @=? getPassword url

componentTests :: TestTree
componentTests = testGroup "Component Tests" [componentTest]

allTests :: TestTree
allTests = testGroup "Google Url Tests" [typeTests,componentTests]

main :: IO ()
main = defaultMain allTests
