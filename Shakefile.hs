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



    -- "dependencies" recipe - Collect "dependencies" files for each project and merge them together.
    "dependencies" %> \out -> do

      -- List out projects under "projects", tracking the result as a shake dependency.
      projects <- getDirectoryDirs "projects"

      -- Build the "dependencies" file (without versions) for each project if necessary.
      let dependencies = map (\project -> addExtension project "dependencies.versionless") projects
      need dependencies

      -- Merge "dependencies" files for each project.
      cmd_ ("sort" :: String) (FileStdout out) ("-u" : dependencies)



    -- "*.dependencies" recipe - Collect dependencies for a project.
    "*.dependencies" %> \out ->

      -- List external dependencies for project without base dependencies.
      cmd_ ("stack" :: String) (Cwd ("projects" </> dropExtension out)) (FileStdout out) [ "ls" :: String, "dependencies", "--external", "--no-include-base" ]



    -- "*.dependencies.versionless" recipe - Collect dependencies for a project without versions.
    "*.dependencies.versionless" %> \out -> do

      -- Build the "dependencies" file for each project if necessary.
      need [ dropExtension out ]

      -- Cut off the version column.
      cmd_ ("cut" :: String) (FileStdin (dropExtension out)) (FileStdout out) [ "-d" :: String, " ", "-f1" ]
