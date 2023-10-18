module Latex
	( LatexMath
	, top
	, bot
	, neg
	, land
	, lor
	, rightarrow
	, leftrightarrow
	, uparrow
	, downarrow
	, lt
	, gt
	, bracket
	, mathLetters
	, LatexInline
	, inlineMath
	, LatexParagraphs
	, displayMath
	, paragraph
	, LatexPages
	, simplePage
	, fittingPage
	, LatexDocument
	, simpleDocument
	, getCode
	, makePDF
	) where

import Data.Set (Set)
import qualified Data.Set as Set
import Text.LaTeX
	( PackageName
	, par, pagebreak, documentclass, article, a4paper, document, autoParens
	, usepackage, mathDisplay, renderFile, hfill, (%:), raw, render
	)
import Text.LaTeX.Base.Syntax ( LaTeX(TeXMath), MathType(Parentheses) )
import Text.LaTeX.Packages.AMSMath (amsmath)
import Text.LaTeX.Base.Class (comm0, env0)
import Data.Coerce (coerce)
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import System.Process (callCommand)
import System.Directory (copyFile)
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Char (isAlpha)

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

latexNoPackages :: LaTeX -> LatexWithPackages
latexNoPackages = flip LatexWithPackages mempty

-- Latex math environment content
-- When math is embedded anywhere, the amsmath package will always be added
-- so there is no need to explicitly include it in the package list
newtype LatexMath = LatexMath { fromMath :: LatexWithPackages }

instance Semigroup LatexMath where
	(<>) = coerce $ (<>) @LatexWithPackages

instance Monoid LatexMath where
	mempty = LatexMath mempty

top :: LatexMath
top = LatexMath $ latexNoPackages $ comm0 "top"

bot :: LatexMath
bot = LatexMath $ latexNoPackages $ comm0 "bot"

neg :: LatexMath
neg = LatexMath $ latexNoPackages $ comm0 "neg"

land :: LatexMath
land = LatexMath $ latexNoPackages $ comm0 "land"

lor :: LatexMath
lor = LatexMath $ latexNoPackages $ comm0 "lor"

rightarrow :: LatexMath
rightarrow = LatexMath $ latexNoPackages $ comm0 "rightarrow"

leftrightarrow :: LatexMath
leftrightarrow = LatexMath $ latexNoPackages $ comm0 "leftrightarrow"

uparrow :: LatexMath
uparrow = LatexMath $ latexNoPackages $ comm0 "uparrow"

downarrow :: LatexMath
downarrow = LatexMath $ latexNoPackages $ comm0 "downarrow"

gt :: LatexMath
gt = LatexMath $ latexNoPackages $ raw ">"

lt :: LatexMath
lt = LatexMath $ latexNoPackages $ raw "<"

bracket :: LatexMath -> LatexMath
bracket (LatexMath latex) = LatexMath $ LatexWithPackages
	(autoParens $ code latex)
	(packages latex)

mathLetters :: Text -> LatexMath
mathLetters c | Text.all isAlpha c = LatexMath $ latexNoPackages $ raw c
mathLetters _ = error "Cannot user mathLetters for non-letters"

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

-- Latex paragraphed content
newtype LatexParagraphs = LatexParagraphs { fromParagraph :: LatexWithPackages }

instance Semigroup LatexParagraphs where
	LatexParagraphs l1 <> LatexParagraphs l2 =
		LatexParagraphs $ LatexWithPackages
			(code l1 <> par <> code l2)
			(packages l1 <> packages l2)

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

simplePage :: LatexParagraphs -> LatexPages
simplePage = coerce

fittingPage :: LatexInline -> LatexPages
fittingPage (LatexInline latex) = LatexPages $ LatexWithPackages
	(env0 "inctext" $ raw "[/igr/border=1em]" <> code latex)
	(packages latex <> Set.singleton "incgraph")

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

getCode :: LatexDocument -> Text
getCode = render . fromDocument

-- Turns a latex document into a pdf file at the given location
-- Note this requires a working latex and rubber installation
makePDF :: LatexDocument -> FilePath -> IO ()
makePDF latex filename = withSystemTempDirectory "LatexBuild" $ \dir -> do
	renderFile (dir </> "output.tex") $ fromDocument latex
	callCommand $ "cd " <> dir <> "; rubber --pdf output.tex"
	copyFile (dir </> "output.pdf") filename