{-# LANGUAGE ScopedTypeVariables #-}

module Data.Mnist.Internal.Download
    ( download
    , datasetDir
    , fileNames
    ) where

import           Control.Exception    (SomeException, throwIO, try)
import           Control.Lens         ((^.))
import           Control.Monad        (forM_, unless)
import qualified Data.ByteString.Lazy as BL
import           Network.Wreq         (get, responseBody)
import           System.Directory     (createDirectoryIfMissing, doesFileExist)
import           System.IO.Error      (userError)

datasetDir :: FilePath
datasetDir = "dataset/"

fileNames :: [String]
fileNames =
    [ "train-images-idx3-ubyte.gz"
    , "train-labels-idx1-ubyte.gz"
    , "t10k-images-idx3-ubyte.gz"
    , "t10k-labels-idx1-ubyte.gz"
    ]

baseUrl :: String
baseUrl = "http://yann.lecun.com/exdb/mnist/"

download :: IO ()
download = do
    createDirectoryIfMissing False datasetDir
    forM_ fileNames $ \fn -> do
        let url = baseUrl ++ fn
        let gzPath = datasetDir ++ fn
        exist <- doesFileExist gzPath
        unless exist $ do
            putStrLn $ "Downloading: " ++ url
            eitherRes <- try $ get url
            case eitherRes of
                Left (_ :: SomeException) -> throwIO $ userError $ "Failed: " ++ url
                Right res -> do
                    let body = res ^. responseBody
                    BL.writeFile gzPath body
