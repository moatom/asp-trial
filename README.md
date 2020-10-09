# asp-trial
Try using asp (a parser combinator, https://github.com/yallop/ocaml-asp, PLDI 2019)

I wrote a parser for type expression by using asp. Its sytax is the following.
```:Syntax
exp ::= var | "int" | "bool" | "unit" | "flot" | "string" | ...
        exp "ref" | exp "list" | exp "array" | ...
        exp "->" exp | exp "*" exp | ...
        "(" exp ")"
var ::= '[a-zA-Z0-9][a-zA-Z0-9_']*
```

For handling postfix operators, a re-written syntax like the following is used.

```:Syntax'
var  ::= '[a-zA-Z0-9][a-zA-Z0-9_']*
atom ::= var | "int" | "bool" | "unit" | "flot" | "string" | ...
postfix_exp ::= (atom | "(" exp ")")("ref" | "list" | "array")*
exp ::= postfix_exp "->" postfix_exp | postfix_exp "*" postfix_exp | ...
```

See https://qiita.com/moatom/items/4d783c53308878376682 (Japanese) for more detail.
