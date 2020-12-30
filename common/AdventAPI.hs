{-# LANGUAGE OverloadedStrings #-}

module AdventAPI (readInput, readInputDefaults) where

import Network.HTTP.Req
    ( (/:),
      bsResponse,
      defaultHttpConfig,
      header,
      https,
      req,
      responseBody,
      runReq,
      NoReqBody(NoReqBody),
      POST(POST) )
import System.Directory (doesFileExist)
import System.FilePath.Posix ((</>), (<.>))

import qualified Data.Text as Text (pack)
import qualified Data.ByteString.Char8 as ByteString (ByteString, pack, unpack)

-- | Call 'readInput' with default paths
readInputDefaults :: Int -> Int -> IO String
readInputDefaults year day = readInput year day "session-cookie.txt" "input"

-- | Read the input for the given day. If a file "input<day>.txt" exists
-- in 'inputsDir', it is simply read. Otherwise, uses 'sessionCookiePath'
-- to read the session cookie from disk and saves it as "input<day>.txt"
-- in 'inputsDir',
readInput :: Int -> Int -> FilePath -> FilePath -> IO String
readInput year day sessionCookiePath inputsDir = do
    let filename = inputsDir </> show year </> "input" ++ (show day) <.> ".txt"

    hasFile <- doesFileExist filename
    if hasFile
        then readFile filename
        else fetchAndSaveInput year day sessionCookiePath filename

-- | Read the session cookie from disk, fetch the input,
-- save it to a file and return its contents
fetchAndSaveInput :: Int -> Int -> FilePath -> FilePath -> IO String
fetchAndSaveInput year day sessionCookiePath filename = do
    sessionCookie <- readFile sessionCookiePath
    contents <- runInputRequest year day sessionCookie
    writeFile filename contents
    return contents

-- | Get the input for the given day from the server
-- via HTTP GET, using the given session cookie
runInputRequest :: Int -> Int -> String -> IO String
runInputRequest year day sessionCookie = runReq defaultHttpConfig $ do
    let f = Text.pack . show

    r <-
        req
            POST -- method
            (https "adventofcode.com" /: (f year) /: "day" /: (f day) /: "input") -- safe by construction URL
            NoReqBody -- empty body
            bsResponse -- strict ByteString response
            $ header "Cookie" (ByteString.pack $ "session=" ++ sessionCookie)
    return $ ByteString.unpack (responseBody r :: ByteString.ByteString)
