{-# LANGUAGE OverloadedStrings #-}

module Problem (
    Difficulty(..)
  , ProblemType(..)
  , Problem(..)
  , randomProblem
) where

import System.Random
import Data.Text
import Data.Monoid ((<>))

randomInt :: (Int, Int) -> IO Int
randomInt = randomRIO

data Difficulty = VeryEasy
                | Easy
                | Medium
                | Hard
                | VeryHard
                | Insane
                deriving (Eq, Ord, Enum, Show)

data ProblemType = Addition
                 | Subtraction
                 | Multiplication
                 | Division
                 deriving (Eq, Show, Enum)

data Problem = Problem {
    difficulty :: Difficulty
  , answer     :: Text
  , string     :: Text
  , kind       :: ProblemType
}

addition :: Difficulty -> IO Problem
addition d = do
  a <- randomInt (low, high)
  b <- randomInt (low, high)
  return $ Problem d ((pack . show) (a + b)) ((pack . show) a <> " + " <> (pack . show) b) Addition
  where
    (low, high) = case d of
      VeryEasy -> (1,    10)
      Easy     -> (1,    50)
      Medium   -> (10,   100)
      Hard     -> (50,   300)
      VeryHard -> (100,  3000)
      Insane   -> (1000, 15000)

subtraction :: Difficulty -> IO Problem
subtraction d = do
  a <- randomInt (low, high)
  b <- randomInt (low, high)
  return $ Problem d ((pack . show) (a - b)) ((pack . show) a <> " - " <> (pack . show) b) Subtraction
  where
    (low, high) = case d of
      VeryEasy -> (1,    10)
      Easy     -> (1,    50)
      Medium   -> (10,   100)
      Hard     -> (50,   300)
      VeryHard -> (100,  3000)
      Insane   -> (1000, 15000)
  

multiplication :: Difficulty -> IO Problem
multiplication d = do
  a <- randomInt (low, high)
  b <- randomInt (low, high)
  return $ Problem d ((pack . show) (a * b)) ((pack . show) a <> " × " <> (pack . show) b) Multiplication
  where
    (low, high) = case d of
      VeryEasy -> (2,  5)
      Easy     -> (2,  10)
      Medium   -> (2,  20)
      Hard     -> (3,  50)
      VeryHard -> (5,  100)
      Insane   -> (10, 300)

division :: Difficulty -> IO Problem
division d = do
  a <- randomInt (low, high)
  b <- randomInt (low, high)
  if a `rem` b == 0 && a /= b
  then return $ Problem d ((pack . show) (a `div` b)) ((pack . show) a <> " ÷ " <> (pack . show) b) Division
  else division d
  where
    (low, high) = case d of
      VeryEasy -> (2,  5)
      Easy     -> (2,  10)
      Medium   -> (2,  20)
      Hard     -> (3,  50)
      VeryHard -> (5,  100)
      Insane   -> (10, 300)

randomProblem :: Difficulty -> IO Problem
randomProblem d = do
  roll <- randomInt (1, 4)
  case roll of
    1 -> addition d
    2 -> subtraction d
    3 -> multiplication d
    4 -> division d

join :: Problem -> Problem -> Problem
join p1 p2 = undefined
