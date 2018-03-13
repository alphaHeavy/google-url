{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Url
import qualified Data.Text as T
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

testParseRelative :: TestTree
testParseRelative = testCase "Parse Relative" $ do
  let url = parseUrl "test.html"
  case url of
    r@(RelativeUrl _) -> do
      False @=? isValid url
    _ -> assertFailure "Url returned was not a relative url"

testParseResolveUrl :: TestTree
testParseResolveUrl = testCase "Parse Resolve Url" $ do
  let url = unsafeParseFullyQualifiedUrl "https://www.cnbc.com/id/105058064?__source=twitter%7Cmain"
      location = "/2018/03/12/dropbox-sets-valuation-as-high-as-8-billion.html?__source=twitter%7Cmain"
      finalUrl = parseUrl "https://www.cnbc.com/2018/03/12/dropbox-sets-valuation-as-high-as-8-billion.html?__source=twitter%7Cmain"
      result = parseResolveUrl url location
  finalUrl @=? result


parseTests :: TestTree
parseTests = testGroup "Parsing Tests" [testParseRelative, testParseResolveUrl]

allTests :: TestTree
allTests = testGroup "Google Url Tests" [typeTests,componentTests,parseTests]

main :: IO ()
main = defaultMain allTests
