module Test.Hspec.Core.Formatters.GithubAction (
  formatter
) where

import Test.Hspec.Core.Format
import Test.Hspec.Core.Formatters.Internal

formatter :: Formatter
formatter = Formatter {
  formatterStarted = pure ()
, formatterGroupStarted = const (pure ())
, formatterGroupDone = const (pure ())
, formatterProgress = \_ _ -> pure ()
, formatterItemStarted = const (pure ())
, formatterItemDone = const itemDone
, formatterDone = pure ()
}

-- | Lines are of the form
--
--   <file>:<line>:<column>:<message>\NUL
--
-- so that it can be matched this regex in a github actions problem matcher
--
--   {
--     "regexp": "^(.+):(\\d+):(\\d+):(.+)\\0",
--     "file": 1,
--     "line": 2,
--     "column": 3,
--     "message": 4
--   }
--
-- Any appearance of \NUL in the message will be removed, so that they
-- aren't truncated. That should be fine though because we would never want
-- a \NUL in the github alert message anyway.
--
-- The location of the program that actually failed is used, unless it's
-- not available, in which case the location of the spec is used. If neither
-- location is available then nothing is output.
itemDone :: Item -> FormatM ()
itemDone item = case itemResult item of
    Success -> pure ()
    Pending _ _ -> pure ()
    Failure mFailureLoc failureReason -> formatFailure (itemLocation item) mFailureLoc failureReason

  where

    -- The second first argument is the location of the "it" spec.

    formatFailure :: Maybe Location -> Maybe Location -> FailureReason -> FormatM ()

    -- No location info means it's useless to us.
    formatFailure Nothing Nothing _ = pure ()

    -- We have the location of the failing spec but nothing else.
    formatFailure (Just loc) Nothing reason = writeLine
      (formatLocation loc ++ ":" ++ formatReason reason)

    formatFailure Nothing (Just loc) reason = writeLine
      (formatLocation loc ++ ":" ++ formatReason reason)

    -- If we have both locations, then we print the location of the failure
    -- as the message at the location of the spec "it"
    formatFailure (Just loc) (Just loc') reason = mapM_ writeLine
      [ (formatLocation loc  ++ ":" ++ formatLocation loc' ++ "\NUL")
      , (formatLocation loc' ++ ":" ++ formatReason reason)
      ]

formatReason :: FailureReason -> String
formatReason NoReason = "\NUL"
formatReason (Reason string) = removeNULs string ++ "\NUL"
formatReason (ExpectedButGot mStr1 str2 str3) = removeNULs (unlines lines') ++ "\NUL"
  where
    -- FIXME what is the optional string and where should it go?
    lines' = maybe id (:) mStr1 ["expected: " ++ str2, " but got: " ++ str3]
formatReason (Error mStr someException) = removeNULs (unlines lines') ++ "\NUL"
  where
    -- FIXME what is the optional string and where should it go?
    lines' = maybe id (:) mStr [show someException]

formatLocation :: Location -> String
formatLocation (Location file line column) = file ++ ":" ++ show line ++ ":" ++ show column

removeNULs :: String -> String
removeNULs = filter ((/=) '\NUL')
