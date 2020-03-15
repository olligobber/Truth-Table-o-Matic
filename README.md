# The Truth-Table-o-Matic

Creates truth tables for propositional logic.

## Usage

Once compiled, main can accept the list of columns as command line arguments, or these can be entered in interactive mode by giving no command line arguments.

Each column is either a new proposition variable, such as `P` or `prop_var1`, a formula such as `A /\ B`, or a named formula such as `eq : (A -> B) & (B --> A)`, whose name can be used in other formulas such as `eq <-> (A = B)`. Entering a formula with new propositions will add those propositions as new variables. Recursive formulas are not allowed.

## Example Usage

```
$ ./main "A" "C: A && A'" "A -> C"
A │ A' │ C : (A∧A') │ (A→C)
──┼────┼────────────┼──────
T │  T │    True    │  True
T │  F │    False   │ False
F │  T │    False   │  True
F │  F │    False   │  True
```

```
$ ./main "invalid-proposition" "recursion : recursion & 1" "bad : col : spec"

The following errors occurred:
    "Argument 1" (line 1, column 9):
unexpected "p"
expecting ">" or "->"
    Recursive definition: recursion : (recursion∧1)
    Invalid format for new column in argument 3
```

```
$ ./main
Enter a column (leave blank to finish): a
    Added proposition a
Enter a column (leave blank to finish): a : b /\ c

The following warnings have triggered:
    Overwriting proposition a with new definition

Enter a column (leave blank to finish): a : a /\ d

The following errors occurred:
    Name a is already defined

Enter a column (leave blank to finish):
b │ c │ a : (b∧c)
──┼───┼──────────
T │ T │    True  
T │ F │   False  
F │ T │   False  
F │ F │   False  
```
