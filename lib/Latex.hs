module Latex
	( LatexMath
	, LatexInline
	, inlineMath
	, LatexParagraphs
	, displayMath
	, paragraph
	, LatexPages
	, page
	, LatexDocument
	, simpleDocument
	, makePDF
	) where

import Data.Set (Set)
import qualified Data.Set as Set
import Text.LaTeX
	( PackageName
	, par, pagebreak, documentclass, article, a4paper, document
	, usepackage, mathDisplay, renderFile, hfill, (%:)
	)
import Text.LaTeX.Base.Syntax ( LaTeX(TeXMath), MathType(Parentheses) )
import Text.LaTeX.Packages.AMSMath (amsmath)
import Data.Coerce (coerce)
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import System.Process (callCommand)
import System.Directory (copyFile)

-- Latex content with a set of packages it requires
data LatexWithPackages = LatexWithPackages
	{ code :: LaTeX
	, packages :: Set PackageName
	}

instance Semigroup LatexWithPackages where
	l1 <> l2 = LatexWithPackages
		(code l1 <> code l2)
		(packages l1 <> packages l2)

instance Monoid LatexWithPackages where
	mempty = LatexWithPackages mempty mempty

-- Latex math environment content
-- When math is embedded anywhere, the amsmath package will always be added
-- so there is no need to explicitly include it in the package list
newtype LatexMath = LatexMath { fromMath :: LatexWithPackages }

instance Semigroup LatexMath where
	(<>) = coerce $ (<>) @LatexWithPackages

instance Monoid LatexMath where
	mempty = LatexMath mempty

-- TODO add commands to create various maths symbols

-- Latex inline content
newtype LatexInline = LatexInline { fromInline :: LatexWithPackages }

instance Semigroup LatexInline where
	(<>) = coerce $ (<>) @LatexWithPackages

instance Monoid LatexInline where
	mempty = LatexInline mempty

inlineMath :: LatexMath -> LatexInline
inlineMath (LatexMath math) = LatexInline $ LatexWithPackages
	(TeXMath Parentheses $ code math)
	(packages math <> Set.singleton amsmath)

-- TODO add commands to write plain sentences

-- Latex paragraphed content
newtype LatexParagraphs = LatexParagraphs { fromParagraph :: LatexWithPackages }

instance Semigroup LatexParagraphs where
	LatexParagraphs l1 <> LatexParagraphs l2 =
		LatexParagraphs $ LatexWithPackages
			(code l1 <> par <> code l2)
			(packages l1 <> packages l2)

-- instance Monoid LatexParagraphs where
-- 	mempty = LatexParagraphs mempty

displayMath :: LatexMath -> LatexParagraphs
displayMath (LatexMath math) = LatexParagraphs $ LatexWithPackages
	(mathDisplay $ code math)
	(packages math <> Set.singleton amsmath)

paragraph :: LatexInline -> LatexParagraphs
paragraph = coerce

-- Latex pages
newtype LatexPages = LatexPages { fromPage :: LatexWithPackages }

instance Semigroup LatexPages where
	LatexPages l1 <> LatexPages l2 = LatexPages $ LatexWithPackages
		(code l1 <> pagebreak Nothing <> code l2)
		(packages l1 <> packages l2)

-- instance Monoid LatexPages where
-- 	mempty = LatexPages mempty

page :: LatexParagraphs -> LatexPages
page = coerce

-- Latex documents
newtype LatexDocument = LatexDocument { fromDocument :: LaTeX }

-- Puts the document in an a4paper article
simpleDocument :: LatexPages -> LatexDocument
simpleDocument (LatexPages latex) = LatexDocument $
	documentclass [a4paper] article <>
	foldMap (usepackage []) (packages latex) <>
	document
		(if code latex == mempty
			then hfill %: "cannot have empty document, so use an hfill to pad"
			else code latex)

-- Turns a latex document into a pdf file at the given location
-- Note this requires a working latex and rubber installation
makePDF :: LatexDocument -> FilePath -> IO ()
makePDF latex filename = withSystemTempDirectory "LatexBuild" $ \dir -> do
	renderFile (dir </> "output.tex") $ fromDocument latex
	callCommand $ "cd " <> dir <> "; rubber --pdf output.tex"
	copyFile (dir </> "output.pdf") filename