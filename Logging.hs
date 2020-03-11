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

data Log = Log {
    errors :: [Text],
    warnings :: [Text],
    reports :: [Text]
}

data Level = Report | Warning | Error deriving (Eq, Ord, Show)

instance Semigroup Log where
    Log e1 w1 r1 <> Log e2 w2 r2 = Log (e1 <> e2) (w1 <> w2) (r1 <> r2)

instance Monoid Log where
    mempty = Log [] [] []

err :: Text -> Log
err e = Log [e] [] []

warn :: Text -> Log
warn w = Log [] [w] []

report :: Text -> Log
report r = Log [] [] [r]

printLog :: Log -> Level -> IO (Maybe Level)
printLog (Log [] _ _) Error = return Nothing
printLog (Log [] [] _) Warning = return Nothing
printLog (Log [] [] []) _ = return Nothing
printLog (Log e@(_:_) _ _) _= do
    putStrLn "The following errors occurred:"
    putStrLn ""
    forM_ e TIO.putStrLn
    return $ Just Error
printLog (Log [] w@(_:_) _) _ = do
    putStrLn "The following warnings have triggered:"
    putStrLn ""
    forM_ w TIO.putStrLn
    return $ Just Warning
printLog (Log [] [] r@(_:_)) _ = Just Report <$ forM_ r TIO.putStrLn
