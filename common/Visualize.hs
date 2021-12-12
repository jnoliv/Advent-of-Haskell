{-# LANGUAGE BlockArguments #-}

module Visualize (
    cursorUp, cursorDown, cursorRight, cursorLeft,
    eraseInDisplay, eraseInLine,
    scrollUp, scrollDown,
    saveCursorPosition, restoreCursorPosition,
    setCursorPosition, getCursorPosition,
    getTerminalDimensions
) where

import Data.Bifunctor (bimap)
import System.IO

-- TODO: remove this import, this is only for debugging.
import Control.Concurrent (threadDelay)

-- | Same as putStr but flushes stdout immediately.
putStr' :: String -> IO ()
putStr' s = putStr s >> hFlush stdout

-- | Writes an ANSI escape code of type "\ESC[nc".
controlSequenceIntroducer :: Int -> Char -> IO ()
controlSequenceIntroducer n c = putStr' $ "\ESC[" ++ show n ++ [c]

-- | Move cursor n cells in the given direction.
cursorUp, cursorDown, cursorRight, cursorLeft :: Int -> IO ()
cursorUp    n = controlSequenceIntroducer n 'A'
cursorDown  n = controlSequenceIntroducer n 'B'
cursorRight n = controlSequenceIntroducer n 'C'
cursorLeft  n = controlSequenceIntroducer n 'D'

-- | Depending on n, erase
-- * 0: from cursor to end of screen/line
-- * 1: from cursor to start of screen/line
-- * 2: entire screen/line 
eraseInDisplay, eraseInLine :: Int -> IO ()
eraseInDisplay n = controlSequenceIntroducer n 'J'
eraseInLine    n = controlSequenceIntroducer n 'K'

-- | Scrolls the entire screen up/down n lines.
-- New lines are added at the bottom/top.
scrollUp, scrollDown :: Int -> IO ()
scrollUp   n = controlSequenceIntroducer n 'S'
scrollDown n = controlSequenceIntroducer n 'T'

-- | Saves and restore cursor position.
saveCursorPosition, restoreCursorPosition :: IO ()
saveCursorPosition    = putStr' "\ESC[s"
restoreCursorPosition = putStr' "\ESC[u"

-- | Move cursor to row n, column n. The values are 1 based!
setCursorPosition :: Int -> Int -> IO ()
setCursorPosition n m =
    putStr' $ "\ESC[" ++ show n ++ ";" ++ show m ++ "H"

-- | Returns the current cursor position as a (row, col) tuple.
getCursorPosition :: IO (Int, Int)
getCursorPosition = do
    buffering <- hGetBuffering stdin
    hSetBuffering stdin NoBuffering

    echo <- hGetEcho stdin
    hSetEcho stdin False

    --saveCursorPosition

    -- Request cursor position
    putStr' "\ESC[6n"

    hSetEcho stdin echo

    -- Read reported position
    (row, col) <- bimap read read <$> getDSR "" ""

    -- Clear report from terminal
    --restoreCursorPosition
    --eraseInDisplay 0

    hSetBuffering stdin buffering

    return (row, col)

getDSR :: String -> String -> IO (String, String)
getDSR row col = do
    c <- getChar

    case c of
        '\ESC' -> do
            _ <- getChar
            getDSR "" ""

        ';'    -> do
            c' <- getChar
            getDSR row [c']

        'R'    -> return (row, col)

        c'     -> if null col
            then getDSR (row ++ [c']) ""
            else getDSR row (col ++ [c'])

-- | Returns the terminal dimensions as a (row, col) tuple.
getTerminalDimensions :: IO (Int, Int)
getTerminalDimensions = do
    (row, col) <- getCursorPosition

    setCursorPosition 9999 9999
    dims <- getCursorPosition

    setCursorPosition row col

    return dims