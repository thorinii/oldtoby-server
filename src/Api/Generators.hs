{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Generators (randomId)
where

import Data.Char (isAlphaNum)
import qualified Data.Text as T
import           System.Random

import Api.Types



randomId :: Char -> IO Id
randomId prefix = do
  suffix <- randomString 20
  return $ Id (prefix `T.cons` suffix)



randomString :: Int -> IO T.Text
randomString n = randomStringGen n T.empty
  where randomStringGen :: Int -> T.Text -> IO T.Text
        randomStringGen 0 acc = return acc
        randomStringGen n acc = do
          char <- randomChar
          if isAlphaNum char
            then randomStringGen (n-1) (char `T.cons` acc)
            else randomStringGen n acc


randomChar :: IO Char
randomChar = getStdRandom $ randomR('0', 'z')