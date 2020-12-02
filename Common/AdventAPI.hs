{-# LANGUAGE OverloadedStrings #-}

module Common.AdventAPI (readInput) where

import Network.HTTP.Req
import System.Directory (doesFileExist)
import System.FilePath.Posix ((</>), (<.>))

import qualified Data.Text as Text (pack)
import qualified Data.ByteString.Char8 as ByteString (ByteString, pack, unpack)

-- | Read the input for the given day. If a file "input<day>.txt" exists
-- in 'inputsDir', it is simply read. Otherwise, uses 'sessionCookiePath'
-- to read the session cookie from disk and saves it as "input<day>.txt"
-- in 'inputsDir',
readInput :: Int -> FilePath -> FilePath -> IO String
readInput day sessionCookiePath inputsDir = do
    let filename = inputsDir </> "input" ++ (show day) <.> ".txt"

    hasFile <- doesFileExist filename
    if hasFile
        then readFile filename
        else fetchAndSaveInput day sessionCookiePath filename

-- | Read the session cookie from disk, fetch the input,
-- save it to a file and return its contents
fetchAndSaveInput :: Int -> FilePath -> FilePath -> IO String
fetchAndSaveInput day sessionCookiePath filename = do
    sessionCookie <- readFile sessionCookiePath
    contents <- runInputRequest day sessionCookie
    writeFile filename contents
    return contents

-- | Get the input for the given day from the server
-- via HTTP GET, using the given session cookie
runInputRequest :: Int -> String -> IO String
runInputRequest day sessionCookie = runReq defaultHttpConfig $ do
    r <-
        req
            POST -- method
            (https "adventofcode.com" /: "2020" /: "day" /: (Text.pack . show $ day) /: "input") -- safe by construction URL
            NoReqBody -- empty body
            bsResponse -- strict ByteString response
            $ header "Cookie" (ByteString.pack $ "session=" ++ sessionCookie)
    return $ ByteString.unpack (responseBody r :: ByteString.ByteString)
