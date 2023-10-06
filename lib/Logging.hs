{-# LANGUAGE OverloadedStrings #-}

module Logging
	( Log(Log, errors, warnings, reports)
	, Level(Report, Warning, Error)
	, err
	, warn
	, report
	, printLog
	) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Foldable (traverse_)
import System.IO (Handle, hPutStrLn)

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
err e = Log (T.lines e) [] []

-- Create a log with a warning
warn :: Text -> Log
warn w = Log [] (T.lines w) []

-- Create a log with a report
report :: Text -> Log
report r = Log [] [] (T.lines r)

-- Output indented
putIndent :: Handle -> Text -> IO ()
putIndent h x = TIO.hPutStrLn h $ "\t" <> x

-- Print logs above a certain level, printing and returning the highest level
printLog :: Handle -> Log -> Level -> IO (Maybe Level)
printLog _ (Log [] _ _) Error = pure Nothing
printLog _ (Log [] [] _) Warning = pure Nothing
printLog _ (Log [] [] []) _ = pure Nothing
printLog h (Log e@(_:_) _ _) _= do
	hPutStrLn h ""
	hPutStrLn h "The following errors occurred:"
	traverse_ (putIndent h) e
	hPutStrLn h ""
	pure $ Just Error
printLog h (Log [] w@(_:_) _) _ = do
	hPutStrLn h ""
	hPutStrLn h "The following warnings have triggered:"
	traverse_ (putIndent h) w
	hPutStrLn h ""
	pure $ Just Warning
printLog h (Log [] [] r@(_:_)) _ = do
	hPutStrLn h ""
	traverse_ (putIndent h) r
	hPutStrLn h ""
	pure $ Just Report
