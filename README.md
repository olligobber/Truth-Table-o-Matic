# The Truth-Table-o-Matic

Creates truth tables for propositional logic.

## Usage

The program takes in a list of instructions on how to build the truth table. These can be either provided as command line arguments, or via an interactive mode. To use cabal to run in interactive mode, simply type `cabal run`. To use cabal to provide command line arguments, type `cabal run ttom -- args`. After installation, run the command `ttom` to run in interactive mode, and `ttom args` to provide arguments.

Each instruction is either a new proposition variable, such as `P` or `prop_var1`, a formula such as `A /\ B`, or a named formula such as `eq : (A -> B) & (B --> A)`, whose name can be used in other formulas such as `eq <-> (A = B)`. Entering a formula with new propositions will add those propositions as new variables. Recursive formulas are not allowed.

## Example Usage

```
$ cabal run ttom -- "A" "C: A && AA" "A -> C"
A │ AA │ C : A∧AA │ A→C
──┼────┼──────────┼────
T │  T │   True   │  T
T │  F │   False  │  F
F │  T │   False  │  T
F │  F │   False  │  T
```

```
$ cabal run ttom -- "invalid-proposition" "recursion : recursion & 1" "bad : col : spec"

The following errors occurred:
	"Argument 1" (line 1, column 9):
	unexpected "p"
	expecting "-" or ">"
	Recursive definition: recursion : recursion∧1
	Invalid format for new column in argument 3

```

```
$ cabal run
Enter a column (leave blank to finish): a

	 Added proposition a

Enter a column (leave blank to finish): a : b /\ c

The following warnings have triggered:
	Overwriting proposition a with new definition

Enter a column (leave blank to finish): a : a /\ d

The following errors occurred:
	Name a is already defined

Enter a column (leave blank to finish):
b │ c │ a : b∧c
──┼───┼────────
T │ T │   True
T │ F │  False
F │ T │  False
F │ F │  False
```

## Formula Syntax

The following table demonstrates how to input the various components of a formula

Symbol | Input
----|----
Propositions | A string of any alphanumeric characters
Brackets | `()`, `[]`, or `{}`
Verum | `⊥`, or `_\|_`
Falsum | `⊤`, or `%`
Not | `-`, `~`, `!`, or `¬`
And | `/\`, `&`, `&&`, `*`, or `∧`
Or | `\/`, `\|`, `\|\|`, or `∨`
Implies | `->`, `-->`, `=>`, `==>`, or `→`
Equals | `=`, `==`, `<->`, `<=>`, or `↔`
Greater Than | `>`
Xor | `+`, `<>`, `⊻`, `~=`, `!=`, or `/=`
Nand | `/\|\`, `!&`, `~&`, or `↑`
Nor | `\\|/`, `!\|`, `~\|`, or `↓`