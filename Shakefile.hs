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



    -- Build the "Dockerfile" file if necessary.
    want [ "Dockerfile" ]



    -- "dependencies" recipe - Collect "dependencies" files for each project and merge them together.
    "dependencies" %> \out -> do

      -- List out projects under "projects", tracking the result as a shake dependency.
      projects <- getDirectoryDirs "projects"

      -- Ensure the projects are sync'd up to origin/master.
      let masters = map (\project -> addExtension project "master") projects
      need masters

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



    -- "Dockerfile" recipe - Buld a Dockerfile that builds all dependencies.
    "Dockerfile" %> \out -> do

      -- Base Docker image to use, read from a file, and trakced as a shake dependency.
      from <- readFile' "FROM"

      -- Stack resolver to use.
      resolver <- readFile' "RESOLVER"

      -- Get "dependencies" file contents, remove "rts".
      dependencies <- readFileLines "dependencies"
      let dependencies' = dependencies \\ [ "rts" ]

      -- Write the Dockerfile out.
      writeFileLines out $
        [ "FROM " <> from
        , "RUN stack build --resolver " <> resolver <> " \\"
        ]
        <> map (\dependency -> dependency <> " \\") (init dependencies')
        <> [ last dependencies' ]



    -- "build" phony recipe - build all of the dependencies locally.
    "build" ~> do

      -- Stack resolver to use.
      resolver <- readFile' "RESOLVER"

      -- Get "dependencies" file contents, remove "rts".
      dependencies <- readFileLines "dependencies"
      let dependencies' = dependencies \\ [ "rts" ]

      -- Build all dependencies.
      cmd_ ("stack" :: String) ("build" : "--resolver" : resolver : dependencies')



    -- "*.master" recipe - If local version does not match remote version, sync up.
    "*.master" %> \out -> do

      -- Get local version contents to compare with.
      local <- readFile' (addExtension (dropExtension out) "local")

      -- Get remote version contents to compare with.
      remote <- readFile' (addExtension (dropExtension out) "remote")

      -- If local version does not match remote version, sync local version to remote version.
      unless (local == remote) $
        cmd_ ("git" :: String) (Cwd ("projects" </> dropExtension out)) [ "reset" :: String, "--hard", "origin/master" ]

      -- Write out remote version.
      writeFileChanged out remote



    -- "*.local" recipe - Get local version of project.
    "*.local" %> \out ->

      -- Find the local origin/master version of the project.
      cmd ("git" :: String) (Cwd ("projects" </> dropExtension out)) (FileStdout out) [ "rev-parse" :: String, "origin/master" ]



    -- "*.remote" recipe - Get remote version of project.
    "*.remote" %> \out -> do

      -- Always run this recipe to force remote fetching.
      alwaysRerun

      -- Fetch from origin, then find remote origin/master version of the project.
      cmd_ ("git" :: String) (Cwd ("projects" </> dropExtension out)) [ "fetch" :: String, "origin" ]
      cmd ("git" :: String) (Cwd ("projects" </> dropExtension out)) (FileStdout out) [ "rev-parse" :: String, "origin/master" ]



    -- "clean" phony recipe - remove generated files.
    "clean" ~>

      -- Remove Dockerfile and generated files.
      removeFilesAfter "."
        [ "Dockerfile"
        , "dependencies"
        , "*.dependencies"
        , "*.versionless"
        , "*.master"
        , "*.local"
        , "*.remote"
        ]
