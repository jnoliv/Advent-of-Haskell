import Control.Monad (forM_)
import Data.List (sort)
import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.FilePath.Posix (FilePath, (</>), isExtensionOf)
import Test.DocTest (doctest)

srcDirs :: [FilePath]
srcDirs = sort ["src/2015", "src/2020"]

relativeSrc :: FilePath -> [FilePath] -> [FilePath]
relativeSrc dir = sort . map (dir </>) . filter (".hs" `isExtensionOf`)

main :: IO ()
main = do
    args <- getArgs

    if not (null args)
        then
            doctest args
        else
            forM_ srcDirs $ \dir -> do
                files <- relativeSrc dir <$> listDirectory dir

                forM_ files $ \file -> do
                    putStrLn ("Testing " ++ file ++ ":")
                    doctest [file]
