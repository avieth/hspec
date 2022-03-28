{-# LANGUAGE OverloadedStrings #-}

module Test.Hspec.Core.Formatters.JSON (
  itemToJSON
, failureRecordToJSON
, writeFailureReport
) where

import qualified Data.Aeson as A
import Data.Maybe (mapMaybe)
import Data.String (fromString)
import Test.Hspec.Core.Format
import Test.Hspec.Core.Formatters.Internal
import Test.Hspec.Core.Util (joinPath)
import qualified Test.Hspec.Core.Formatters.V2 as V2

writeFailureReport :: [(Path, V2.Item)] -> FilePath -> IO ()
writeFailureReport xs path = A.encodeFile path
    (A.toJSON (failureRecordToJSON <$> records))
  where
    records = flip mapMaybe xs $ \(path', item) -> case itemResult item of
      Success -> Nothing
      Pending _ _ -> Nothing
      Failure mLoc reason -> Just $ FailureRecord
        { failureRecordLocation = mLoc
        , failureRecordPath = path'
        , failureRecordMessage = reason
        }

itemToJSON :: Item -> A.Value
itemToJSON it = A.object
  [ "location" A..= (locationToJSON       <$> itemLocation it)
  , "duration" A..= (secondsToJSON         $  itemDuration it)
  , "info"     A..= (A.String . fromString $  itemInfo it)
  , "result"   A..= (resultToJSON          $  itemResult it)
  ]

failureRecordToJSON :: FailureRecord -> A.Value
failureRecordToJSON fr = A.object
  [ "location" A..= (locationToJSON      <$> failureRecordLocation fr)
  , "path"     A..= (pathToJSON           $  failureRecordPath fr)
  , "reason"   A..= (failureReasonToJSON  $  failureRecordMessage fr)
  ]

pathToJSON :: Path -> A.Value
pathToJSON = A.String . fromString . joinPath

locationToJSON :: Location -> A.Value
locationToJSON loc = A.object
  [ "file"   A..= (A.String . fromString $ locationFile loc)
  , "line"   A..= (A.toJSON              $ locationLine loc)
  , "column" A..= (A.toJSON              $ locationColumn loc)
  ]

secondsToJSON :: Seconds -> A.Value
secondsToJSON (Seconds d) = A.toJSON d

resultToJSON :: Result -> A.Value
resultToJSON Success = A.object
  [ "type" A..= A.String "success" ]
resultToJSON (Pending mLoc mStr) = A.object
  [ "type"     A..= (A.String               $ "pending")
  , "location" A..= (locationToJSON        <$> mLoc)
  , "reason"   A..= (A.String . fromString <$> mStr)
  ]
resultToJSON (Failure mLoc reason) = A.object
  [ "type"     A..= (A.String        $  "failure")
  , "location" A..= (locationToJSON <$> mLoc)
  , "reason"   A..= (failureReasonToJSON    $  reason)
  ]

failureReasonToJSON :: FailureReason -> A.Value
failureReasonToJSON NoReason = A.object
  [ "type" A..= A.String "none" ]
failureReasonToJSON (Reason message) = A.object
  [ "type"    A..= A.String "message"
  , "message" A..= A.String (fromString message)
  ]
failureReasonToJSON (ExpectedButGot mStr1 str2 str3) = A.object
  [ "type"     A..= (A.String               $  "expectation")
  , "message"  A..= (A.String . fromString <$> mStr1)
  , "expected" A..= (A.String . fromString  $  str2)
  , "got"      A..= (A.String . fromString  $  str3)
  ]
failureReasonToJSON (Error mStr exception) = A.object
  [ "type"    A..= (A.String                      $ "error")
  , "message" A..= (A.String . fromString        <$> mStr)
  , "error"   A..= (A.String . fromString . show  $  exception)
  ]
