{-# LANGUAGE OverloadedStrings #-}

module TruthTable (
	TruthTable,
	empty,
	addProp,
	addDef,
	addForm
) where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Lazy (Map, (!))
import qualified Data.Map.Lazy as M
import qualified Data.Text as T
import Control.Monad.Writer (Writer)
import qualified Control.Monad.Writer as W
import Control.Monad (foldM, replicateM)
import Data.Foldable (toList)

import Render (Renderable(render))
import WFF (WFF, applyMap)
import Logical (evaluate)
import Logging (Log, err, warn, report)

{-
	A truth table with columns for propositions, named formulas, and unnamed
	formulas
-}
data TruthTable a = TruthTable {
	propositions :: Set a,
	definitions :: Map a (WFF a), -- What the user entered
	flatdefs :: Map a (WFF a), -- Replaced names with formulas
	formulas :: Set (WFF a)
} deriving Show

-- Pretty rendering
instance (Ord a, Renderable a) => Renderable (TruthTable a) where
	render table = T.unlines $ titles:seperators:formatted where
		colnames = concat
			[ render <$> toList (propositions table)
			, (\(k,v) -> render k <> " : " <> render v) <$>
				M.assocs (definitions table)
			, render <$> toList (formulas table)
			]
		colvals = concat
			[ pure <$> toList (propositions table)
			, toList (flatdefs table)
			, (>>= \x -> maybe (pure x) id $ M.lookup x $ flatdefs table) <$>
				toList (formulas table)
			]
		colWidths = T.length <$> colnames

		titles = T.intercalate " │ " colnames
		seperators = T.intercalate "─┼─" $ flip T.replicate "─" <$> colWidths
		formatted = do
			avals <- replicateM (length $ propositions table) [True, False]
			let
				assignment = M.fromListWith undefined $
					zip (toList $ propositions table) avals
				acols = evaluate . fmap (assignment !) <$> colvals
				rendcols = zipWith
					(\l v -> T.center l ' ' $ if l < 4 then T.take 1 v else v)
					colWidths $ render <$> acols
			return $ T.intercalate " │ " rendcols

-- Set of valid propositions in formulas
symbols :: Ord a => TruthTable a -> Set a
symbols table = propositions table `S.union` M.keysSet (definitions table)

-- Table with nothing in it
empty :: TruthTable a
empty = TruthTable S.empty M.empty M.empty S.empty

-- Add a proposition column to a table if the name isn't in use
addProp :: (Renderable a, Ord a) => TruthTable a -> a ->
	Writer Log (TruthTable a)
addProp table prop
	| prop `M.member` definitions table =  do
		W.tell $ err $ "Name clash between new proposition " <> render prop <>
			" and existing definition"
		return table
	| prop `S.member` propositions table = do
		W.tell $ warn $ "Proposition " <> render prop <> " is already added"
		return table
	| otherwise = do
		W.tell $ report $ "Added proposition " <> render prop
		return $ TruthTable
			(prop `S.insert` propositions table)
			(definitions table)
			(flatdefs table)
			(formulas table)

{-
	Add a named formula to a table if the name isn't used by another formula,
	and the definition isn't recursive
-}
addDef :: (Renderable a, Ord a) => TruthTable a -> a -> WFF a ->
	Writer Log (TruthTable a)
addDef table symbol def
	| symbol `M.member` definitions table = do
		W.tell $ err $ "Name " <> render symbol <> " is already defined"
		return table
	| symbol `elem` flatdef = do
		W.tell $ err $
			"Recursive definition: " <> render symbol <> " : " <> render flatdef
		return table
	| symbol `S.member` propositions table = do
		W.tell $ warn $ "Overwriting proposition " <> render symbol <>
			" with new definition"
		addDef withoutSymbol symbol def
	| def `S.member` formulas table = do
		W.tell $ warn $ "Replacing column " <> render def <>
			" with new definition as " <> render symbol
		addDef withoutDefinition symbol def
	| otherwise = do
		newtable <- foldM addProp table newprops
		W.tell $ report $ "Added definition of " <> render symbol
		return $ TruthTable
			(propositions newtable)
			(M.insert symbol def $ definitions newtable)
			(M.insert symbol flatdef $ newflats newtable)
			(formulas newtable)
	where
		-- Delete the name from the set of propositions
		withoutSymbol = TruthTable
			(S.delete symbol $ propositions table)
			(definitions table)
			(flatdefs table)
			(formulas table)
		-- Delete the formula from the set of formulas
		withoutDefinition = TruthTable
			(propositions table)
			(definitions table)
			(flatdefs table)
			(S.delete def $ formulas table)
		-- Replace all names with corresponding formulas
		flatdef = applyMap (flatdefs table) def
		-- Propositions in the formula that aren't already defined
		newprops = S.fromList (toList def) `S.difference` symbols table
		-- Replace new name with new definition in old formulas
		newflats t =
			(>>= \x -> if x == symbol then flatdef else pure x) <$> flatdefs t

-- Add a formula to a table
addForm :: (Renderable a, Ord a) => TruthTable a -> WFF a ->
	Writer Log (TruthTable a)
addForm table form
	| form `elem` formulas table = do
		W.tell $ warn $ "Column " <> render form <> " is already added"
		return table
	| form `elem` definitions table = do
		W.tell $ warn $ "Column " <> render form <>
			" already exists as a defined symbol"
		return table
	| otherwise = do
		newtable <- foldM addProp table newprops
		W.tell $ report $ "Added column for formula " <> render form
		return $ TruthTable
			(propositions newtable)
			(definitions table)
			(flatdefs table)
			(form `S.insert` formulas table)
	where
		-- Propositions in the formula that aren't already defined
		newprops = S.fromList (toList form) `S.difference` symbols table
