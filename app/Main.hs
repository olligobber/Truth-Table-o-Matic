{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.IO (hFlush, stdout, stderr)
import qualified Control.Monad.Writer as W
import Control.Monad (foldM)
import Data.String (fromString)
import System.Exit (exitFailure)
import Control.Applicative ((<|>))

import TruthTable (TruthTable, addProp, addForm, addDef, empty)
import Logging (Log, Level(Report, Warning, Error), printLog, err)
import WFFParser (parseWFF)
import WFF (WFF(Proposition))
import Render (renderText, putText)

-- Simple prompt for text input
prompt :: String -> IO Text
prompt p =
	(putStr p *> hFlush stdout *> TIO.getLine) <|>
	("" <$ putStrLn "")

-- Prompt for a new columns to add, returning a truth table after all are added
promptCols :: TruthTable Text -> IO (TruthTable Text)
promptCols table = do
	userInput <- T.splitOn ":" <$> prompt
		"Enter a column (leave blank to finish): "
	if userInput == [""] then
		return table
	else case traverse (parseWFF "Input") userInput of
			Right [] -> error "Impossible output from splitOn"
			Right [Proposition x] -> do
				let (newtable, l) = W.runWriter $ addProp table x
				_ <- printLog stdout l Report
				promptCols newtable
			Right [w] -> do
				let (newtable, l) = W.runWriter $ addForm table w
				_ <- printLog stdout l Report
				promptCols newtable
			Right [Proposition x, w] -> do
				let (newtable, l) = W.runWriter $ addDef table x w
				_ <- printLog stdout l Report
				promptCols newtable
			Right [w, Proposition x] -> do
				let (newtable, l) = W.runWriter $ addDef table x w
				_ <- printLog stdout l Report
				promptCols newtable
			Right _ ->
				putStrLn "Invalid format for new column" *> promptCols table
			-- Parsing error, print to user
			Left e -> do
				_ <- printLog stdout (err $ fromString $ show e) Report
				promptCols table

-- Make a truth table from an indexed list of columns to add
makeTable :: [(Integer, Text)] -> W.Writer Log (TruthTable Text)
makeTable = foldM
	( \table (index, arg) ->
	if arg == "" then do
		W.tell $ err $ "Empty argument: " <> renderText index
		return table
	else case
		traverse (parseWFF $ "Argument " <> show index) $ T.splitOn ":" arg
	of
		Right [] -> error "Impossible output from splitOn"
		Right [Proposition x] -> addProp table x
		Right [w] -> addForm table w
		Right [Proposition x, w] -> addDef table x w
		Right [w, Proposition x] -> addDef table x w
		Right _ -> do
			W.tell $ err $
				"Invalid format for new column in argument " <>
				renderText index
			return table
		Left e -> do
			-- Parsing error, send to user
			W.tell $ err $ fromString $ show e
			return table
	)
	empty


main :: IO ()
main = do
	args <- getArgs
	case args of
		-- No arguments -> interactive mode
		[] -> promptCols empty >>= putText
		-- Arguments provided
		_ -> do
			-- Parse the arguments
			let (table, l) =
					W.runWriter $ makeTable $ zip [1..] $ fromString <$> args
			-- Print out warnings and errors
			logtype <- printLog stderr l Warning
			-- Print truth table if no errors occurred
			if logtype == Just Error then
				exitFailure
			else
				putText table
