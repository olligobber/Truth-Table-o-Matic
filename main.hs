{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import qualified Control.Monad.Writer as W
import Control.Monad (foldM, when)
import Data.String (fromString)

import TruthTable
import Logging
import WFFParser
import WFF (WFF(..))
import Render (render, putRender)

prompt :: String -> IO Text
prompt p = putStr p >> hFlush stdout >> TIO.getLine

promptCols :: TruthTable Text -> IO (TruthTable Text)
promptCols table = do
    userInput <- T.splitOn ":" <$> prompt
        "Enter a column (leave blank to finish): "
    if userInput == [""] then
        return table
    else case traverse (parseWFF "Input") userInput of
            Right [] -> error "Impossible output from splitOn"
            Right [Prop x] -> do
                let (newtable, l) = W.runWriter $ addProp table x
                _ <- printLog l Report
                promptCols newtable
            Right [w] -> do
                let (newtable, l) = W.runWriter $ addForm table w
                _ <- printLog l Report
                promptCols newtable
            Right [Prop x, w] -> do
                let (newtable, l) = W.runWriter $ addDef table x w
                _ <- printLog l Report
                promptCols newtable
            Right [w, Prop x] -> do
                let (newtable, l) = W.runWriter $ addDef table x w
                _ <- printLog l Report
                promptCols newtable
            Right _ -> putStrLn "Too many colons in input" >> promptCols table
            Left e -> print e >> promptCols table

makeTable :: [(Integer, Text)] -> W.Writer Log (TruthTable Text)
makeTable = foldM
    ( \table (index, arg) -> if arg == "" then do
        W.tell $ err $ "Empty argument: " <> render index
        return table
    else case traverse (parseWFF $ "Argument " <> show index) $
        T.splitOn ":" arg of
            Right [] -> error "Impossible output from splitOn"
            Right [Prop x] -> addProp table x
            Right [w] -> addForm table w
            Right [Prop x, w] -> addDef table x w
            Right [w, Prop x] -> addDef table x w
            Right _ -> do
                W.tell $ err $ "Too many colons in argument " <> render index
                return table
            Left e -> do
                W.tell $ err $ fromString $ show e
                return table
    )
    empty


main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> promptCols empty >>= putRender
        _ -> do
            let (table, l) = W.runWriter $ makeTable $ zip [1..] $
                    fromString <$> args
            logtype <- printLog l Warning
            when (logtype /= Just Error) $ putRender table
