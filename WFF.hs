{-# LANGUAGE OverloadedStrings #-}

module WFF(
    WFF(..),
    applyMap
) where

import Data.Function (on)
import Control.Applicative (liftA2)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Control.Monad (ap)
import Data.Traversable (foldMapDefault)
import Data.Text (Text)

import Render (Renderable(..))

-- Logical connectives
infix 5 :|:
infix 5 :&:
infix 5 :>:
infix 5 :=:

-- Logical formula datatype
data WFF c =
    Prop c |                -- Proposition
    Not (WFF c) |           -- Negation
    (:|:) (WFF c) (WFF c) | -- Disjunction
    (:&:) (WFF c) (WFF c) | -- Conjunction
    (:>:) (WFF c) (WFF c) | -- Implication
    (:=:) (WFF c) (WFF c)   -- Equivalence
    deriving (Eq, Ord)

-- Get the infix constructors to render properly
instance Show c => Show (WFF c) where
    showsPrec prec (Prop prop) =
        showParen (prec>10) $ showString "Prop " . showsPrec 11 prop
    showsPrec prec (Not wff) =
        showParen (prec>10) $ showString "Not " . showsPrec 11 wff
    showsPrec prec (wff1 :|: wff2) =
        showParen (prec>5) $
            showsPrec 6 wff1 . showString " :|: " . showsPrec 6 wff2
    showsPrec prec (wff1 :&: wff2) =
        showParen (prec>5) $
            showsPrec 6 wff1 . showString " :&: " . showsPrec 6 wff2
    showsPrec prec (wff1 :>: wff2) =
        showParen (prec>5) $
            showsPrec 6 wff1 . showString " :>: " . showsPrec 6 wff2
    showsPrec prec (wff1 :=: wff2) =
        showParen (prec>5) $
            showsPrec 6 wff1 . showString " :=: " . showsPrec 6 wff2

instance Functor WFF where -- Use Monad instance to define this
    fmap f m = m >>= (return . f)

instance Applicative WFF where -- Use Monad instance to define this
    pure = return
    (<*>) = ap

{-
    The Monad and Applicative instances basically nest later structures into
    earlier structures. The main point of this is that >>= can be used to
    substitute propositions for formulas
-}
instance Monad WFF where
    return = Prop
    (Prop prop)     >>= f   = f prop
    (Not wff)       >>= f   = Not $ wff >>= f
    (wff1 :|: wff2) >>= f   = ((:|:) `on` (>>= f)) wff1 wff2
    (wff1 :&: wff2) >>= f   = ((:&:) `on` (>>= f)) wff1 wff2
    (wff1 :>: wff2) >>= f   = ((:>:) `on` (>>= f)) wff1 wff2
    (wff1 :=: wff2) >>= f   = ((:=:) `on` (>>= f)) wff1 wff2

instance Foldable WFF where -- Use Traversable instance to define this
    foldMap = foldMapDefault

-- Traversable on propositions
instance Traversable WFF where
    sequenceA (Prop prop) = Prop <$> prop
    sequenceA (Not wff) = Not <$> sequenceA wff
    sequenceA (wff1 :|: wff2) = (liftA2 (:|:) `on` sequenceA) wff1 wff2
    sequenceA (wff1 :&: wff2) = (liftA2 (:&:) `on` sequenceA) wff1 wff2
    sequenceA (wff1 :>: wff2) = (liftA2 (:>:) `on` sequenceA) wff1 wff2
    sequenceA (wff1 :=: wff2) = (liftA2 (:=:) `on` sequenceA) wff1 wff2

-- Text version of ShowS functions
showParenT :: Bool -> (Text -> Text) -> Text -> Text
showParenT False t = t
showParenT True t = ("(" <>) . t . (")" <>)

showText :: Text -> (Text -> Text)
showText = (<>)

-- Nice rendering for the user
rendersPrec :: Int -> (c -> Text) -> WFF c -> Text -> Text
rendersPrec _ rend (Prop prop) = showText $ rend prop
rendersPrec prec rend (Not wff) = showParenT (prec>2) $
    showText "¬" . rendersPrec 2 rend wff
rendersPrec prec rend (wff1 :|: wff2) = showParenT (prec>1) $
    rendersPrec 2 rend wff1 . showText "∨" . rendersPrec 2 rend wff2
rendersPrec prec rend (wff1 :&: wff2) = showParenT (prec>1) $
    rendersPrec 2 rend wff1 . showText "∧" . rendersPrec 2 rend wff2
rendersPrec prec rend (wff1 :>: wff2) = showParenT (prec>1) $
    rendersPrec 2 rend wff1 . showText "→" . rendersPrec 2 rend wff2
rendersPrec prec rend (wff1 :=: wff2) = showParenT (prec>1) $
    rendersPrec 2 rend wff1 . showText "↔" . rendersPrec 2 rend wff2

instance Renderable x => Renderable (WFF x) where
    render wff = rendersPrec 2 render wff ""

-- Apply a mapping from match to some formula
applyMap :: Ord x => Map x (WFF x) -> WFF x -> WFF x
applyMap m w = w >>= \p -> case M.lookup p m of
    Nothing -> Prop p
    Just n -> n
