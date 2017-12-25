{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RankNTypes        #-}

module FSLens where

import           Control.Lens        (Traversal', makeLenses, makePrisms, traversed, filtered, (^.), (%~))
import           System.Directory    (doesFileExist, listDirectory)
import           System.FilePath     (splitPath, takeFileName, (</>), replaceExtension)

data FS 
    = Dir 
          { _name     :: FilePath  -- название папки, не полный путь
          , _contents :: [FS]
          }
    | File
          { _name     :: FilePath  -- название5 файла, не полный путь
          }
    deriving(Show)

scan :: FilePath -> IO FS
scan fp = do
    isFile <- doesFileExist fp
    if isFile then pure $ File $ takeFileName fp
    else do
        content <- listDirectory fp
        Dir (last $ splitPath fp) <$> mapM (scan . (</>) fp) content

makeLenses ''FS
makePrisms ''FS

isFile :: FS -> Bool
isFile (File _) = True
isFile _        = False

isDir :: FS -> Bool
isDir = not . isFile

cd :: FilePath -> Traversal' FS FS
cd dr = contents . traversed . filtered (\x -> isDir x && x ^. name == dr)

ls :: Traversal' FS [FS]
ls = contents

file :: FilePath -> Traversal' FS FS
file fn = contents . traversed . filtered (\x -> isFile x && x ^. name == fn)

-- x <- scan "."
-- x ^. cd "Algo .ls
-- x ^. ls

replaceExtension' :: String -> FS -> FS
replaceExtension' newExtension =  contents . traversed . filtered isFile . name %~ (`replaceExtension` newExtension)
