module Data.Mnist
    ( Mnist(..)
    , loadMnist
    , toOneHotLabel
    ) where

import           Codec.Compression.GZip       (decompress)
import           Control.Monad                (unless)
import           Data.Binary                  (decode, encode)
import qualified Data.ByteString.Lazy         as BL
import           Numeric.LinearAlgebra        (Matrix, R, Vector, cmap,
                                               fromLists, matrix, toList,
                                               vector)
import           System.Directory             (doesFileExist)

import           Data.Mnist.Internal.Download (datasetDir, download, fileNames)

data Mnist = Mnist
    { trainImage :: Matrix R
    , trainLabel :: Vector R
    , testImage  :: Matrix R
    , testLabel  :: Vector R
    }

picklePath :: FilePath
picklePath = datasetDir ++ "mnist.pkl"

imageSize :: Int
imageSize = 28 * 28

toDoubleList :: BL.ByteString -> [Double]
toDoubleList = fmap fromIntegral . BL.unpack

loadImage :: FilePath -> IO (Matrix R)
loadImage fp = do
    c <- BL.drop 16 <$> decompress <$> BL.readFile fp
    return . (matrix imageSize) $ toDoubleList c

loadLabel :: FilePath -> IO (Vector R)
loadLabel fp = do
    c <- BL.drop 8 <$> decompress <$> BL.readFile fp
    return . vector $ toDoubleList c

createPickle :: IO ()
createPickle = do
    exist <- doesFileExist picklePath
    unless exist $ do
        putStrLn $ "Create: " ++ picklePath
        let train_image = datasetDir ++ fileNames !! 0
        let train_label = datasetDir ++ fileNames !! 1
        let test_image  = datasetDir ++ fileNames !! 2
        let test_label  = datasetDir ++ fileNames !! 3
        m <- (,,,) <$> loadImage train_image
                   <*> loadLabel train_label
                   <*> loadImage test_image
                   <*> loadLabel test_label
        BL.writeFile picklePath (encode m)

loadPickle :: IO Mnist
loadPickle = do
    putStrLn $ "Load: " ++ picklePath
    (a, b, c, d) <- decode <$> BL.readFile picklePath
    return $ Mnist a b c d

normalize' :: Mnist -> Mnist
normalize' m = m {trainImage = t1, testImage = t2}
  where
    t1 = cmap (/ 255) (trainImage m)
    t2 = cmap (/ 255) (testImage m)

toOneHotLabel :: Vector R -> Matrix R
toOneHotLabel = fromLists . map doubleToVec . toList
  where
    doubleToVec n = [if (round n) == x then 1 else 0 | x <- [0..9 :: Int]]

loadMnist :: Bool     -- normalize
          -> IO Mnist
loadMnist _n = do
    download
    createPickle
    m <- loadPickle
    let fp = zip [normalize'] [_n]
    let opt = foldr (.) id [f | (f, p) <- fp, p == True]
    return (opt m)
