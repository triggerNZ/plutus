# Marlex Semantics (Marlowe Extended)

## Syntax ENBF

```ebnf
toplevel = { def } | expr;
def      = identifier { identifier } '=' expr;
expr     = app | term;
app      = term term { term };
term     = lam | let | ifelse | literal | name | '(' expr ')';
lam      = '\' { identifier } '->' expr;
let      = 'let' '{' binds '}' 'in' expr;
binds    = bind ';' binds | bind;
bind     = identifier '=' expr;
ifelse   = 'if' expr 'then' expr 'else' expr;
name     = ident-with-dots | identifier;
literal  = integer | string;
```

## Builtin identifiers

All names available after importing following modules:

```haskell
import Prelude (foldl, foldr, map, filter)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Language.Marlowe
```

## Examples

```haskell
Close -- simple Marlowe expression
alice = Role "alice" -- simple toplevel binding
account role = AccountId 0 (Role role)
account = \role -> AccountId 0 (Role role)
sum = let {
  a = 5;
  b = 6 } in a + b
```
