#!/usr/bin/env stack
{- stack
    runghc
    --package basic-prelude
    --package shake
 -}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import BasicPrelude
import Development.Shake
import Development.Shake.FilePath

main :: IO ()
main = do

  -- Call shake with defaults and an updated version on file contents chage.
  version <- getHashedShakeVersion [ "Shakefile.hs" ]
  shakeArgs shakeOptions { shakeVersion = version } $ do



    -- Build the "dependencies" file if necessary.
    want [ "dependencies" ]
