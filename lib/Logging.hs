{-# LANGUAGE OverloadedStrings #-}

module Logging (
	Log(..),
	Level(..),
	err,
	warn,
	report,
	printLog
) where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Control.Monad (forM_)

-- A monoid for logging at various levels
data Log = Log {
	errors :: [Text],
	warnings :: [Text],
	reports :: [Text]
}

data Level = Report | Warning | Error deriving (Eq, Ord, Show)

-- Monoid on concatenation of each level of logging
instance Semigroup Log where
	Log e1 w1 r1 <> Log e2 w2 r2 = Log (e1 <> e2) (w1 <> w2) (r1 <> r2)

instance Monoid Log where
	mempty = Log [] [] []

-- Create a log with an error
err :: Text -> Log
err e = Log [e] [] []

-- Create a log with a warning
warn :: Text -> Log
warn w = Log [] [w] []

-- Create a log with a report
report :: Text -> Log
report r = Log [] [] [r]

-- Output indented
putIndent :: Text -> IO ()
putIndent x = TIO.putStrLn $ "\t" <> x

-- Print logs above a certain level, printing and returning the highest level
printLog :: Log -> Level -> IO (Maybe Level)
printLog (Log [] _ _) Error = return Nothing
printLog (Log [] [] _) Warning = return Nothing
printLog (Log [] [] []) _ = return Nothing
printLog (Log e@(_:_) _ _) _= do
	putStrLn ""
	putStrLn "The following errors occurred:"
	forM_ e putIndent
	putStrLn ""
	return $ Just Error
printLog (Log [] w@(_:_) _) _ = do
	putStrLn ""
	putStrLn "The following warnings have triggered:"
	forM_ w putIndent
	putStrLn ""
	return $ Just Warning
printLog (Log [] [] r@(_:_)) _ = Just Report <$ forM_ r putIndent
