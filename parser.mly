%{
  open Lambda;;
%}

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LET
%token LETREC
%token FIX
%token IN
%token CONCAT
%token NIL
%token CONS
%token ISNIL
%token HEAD
%token TAIL
%token BOOL
%token NAT
%token STRING
%token LIST

%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET
%token DOT
%token COMMA
%token EQ
%token COLON
%token ARROW
%token EOF

%token <int> INTV
%token <string> IDV
%token <string> STRINGV

%start s
%type <Lambda.command> s

%%

s :
    IDV EQ term EOF
      { Bind ($1, $3) }
  | term EOF
      { Eval $1 }

term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | LAMBDA IDV COLON ty DOT term
      { TmAbs ($2, $4, $6) }
  | LET IDV EQ term IN term
      { TmLetIn ($2, $4, $6) }
  | LETREC IDV COLON ty EQ term IN term
      { TmLetIn ($2, TmFix (TmAbs($2, $4, $6)), $8) }

appTerm :
    pathTerm
      { $1 }
  | SUCC pathTerm
      { TmSucc $2 }
  | PRED pathTerm
      { TmPred $2 }
  | ISZERO pathTerm
      { TmIsZero $2 }
  | FIX pathTerm
      { TmFix $2 }
  | CONCAT pathTerm pathTerm
      { TmConcat ($2, $3) }
  | appTerm pathTerm
      { TmApp ($1, $2) }

pathTerm :
  | pathTerm DOT IDV
      { TmProj ($1, $3) }
  | pathTerm DOT INTV
      { TmProj ($1, string_of_int $3) }
  | atomicTerm
      { $1 }

atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | LBRACE tupleTerm RBRACE
      { TmTuple $2 }
  | LBRACE recordTerm RBRACE
      { TmRecord $2 }
  | NIL LBRACKET ty RBRACKET
      { TmNil $3 }
  | CONS LBRACKET ty RBRACKET atomicTerm atomicTerm
      { TmCons ($3, $5, $6) }
  | ISNIL LBRACKET ty RBRACKET atomicTerm
      { TmIsnil ($3, $5) }
  | HEAD LBRACKET ty RBRACKET atomicTerm
      { TmHead ($3, $5) }
  | TAIL LBRACKET ty RBRACKET atomicTerm
      { TmTail ($3, $5) }
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | IDV
      { TmVar $1 }
  | INTV
      { let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f $1 }
  | STRINGV
      { TmString $1 }


ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }

atomicTy :
    LPAREN ty RPAREN
      { $2 }
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | STRING
      { TyString }
  | LIST LBRACKET ty RBRACKET
      { TyList $3 }

tupleTerm :
    term
      { [$1] }
  | term COMMA tupleTerm
      { $1 :: $3 }

recordTerm :
    IDV EQ term
      { [($1, $3)] }
  | IDV EQ term COMMA recordTerm
      { ($1, $3) :: $5 }
