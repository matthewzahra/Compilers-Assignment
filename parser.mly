/* lab4/parser.mly */
/* Copyright (c) 2017 J. M. Spivey */

%{
open Optree
open Dict
open Tree
%}

%token <Dict.ident>     IDENT
%token <Optree.op>      MULOP ADDOP RELOP
%token <int>            NUMBER 
%token <char>           CHAR
%token <Optree.symbol * int> STRING

/* punctuation */
%token                  SEMI DOT COLON LPAR RPAR COMMA SUB BUS
%token                  EQUAL MINUS ASSIGN VBAR ARROW
%token                  BADTOK IMPOSSIBLE

/* keywords */
%token                  ARRAY BEGIN CONST DO ELSE END IF OF
%token                  PROC RECORD RETURN THEN TO TYPE
%token                  VAR WHILE NOT POINTER NIL
%token                  REPEAT UNTIL FOR ELSIF CASE

%type <Tree.program>    program
%start                  program

%{
let const n t = make_expr (Constant (n, t))
%}

%%

program :       
    block DOT                           { Prog ($1, ref []) } ;

block : 
    decl_list BEGIN stmts END           { make_block ($1, $3) } ;

decl_list :     
    /* empty */                         { [] }
  | decl decl_list                      { $1 @ $2 } ;

decl :  
    CONST const_decls                   { $2 }
  | VAR var_decls                       { $2 }
  | proc_decl                           { [$1] }
  | TYPE type_decls                     { [TypeDecl $2] } ;

const_decls :
    const_decl                          { [$1] }
  | const_decl const_decls              { $1 :: $2 } ;

const_decl :
    IDENT EQUAL expr SEMI               { ConstDecl ($1, $3) } ;

type_decls :
    type_decl                           { [$1] }
  | type_decl type_decls                { $1 :: $2 } ;

type_decl :     
    IDENT EQUAL typexpr SEMI            { ($1, $3) } ;

var_decls :
    var_decl                            { [$1] }
  | var_decl var_decls                  { $1 :: $2 } ;

var_decl :
    ident_list COLON typexpr SEMI       { VarDecl (VarDef, $1, $3) } ;

proc_decl :
    proc_heading SEMI block SEMI        { ProcDecl ($1, $3) } ;

proc_heading :  
    PROC name params return_type        { Heading ($2, $3, $4) } ;

params :
    LPAR RPAR                           { [] }
  | LPAR formal_decls RPAR              { $2 } ;

formal_decls :  
    formal_decl                         { [$1] }
  | formal_decl SEMI formal_decls       { $1 :: $3 } ;

formal_decl :   
    ident_list COLON openArray          { VarDecl (OpenArrayParamDef, $1, $3) }
  | VAR ident_list COLON openArray      { VarDecl (OpenArrayParamDef, $2, $4) }
  | ident_list COLON typexpr            { VarDecl (CParamDef, $1, $3) }
  | VAR ident_list COLON typexpr        { VarDecl (VParamDef, $2, $4) }
  | proc_heading                        { PParamDecl $1 } ;

return_type :
    /* empty */                         { None }
  | COLON typexpr                       { Some $2 } ;

stmts : 
    stmt_list                           { seq $1 } ;

stmt_list :
    stmt                                { [$1] }
  | stmt SEMI stmt_list                 { $1 :: $3 } ;

stmt :  
    line stmt1                          { make_stmt ($2, $1) }
  | /* A trick to force the right line number */
    IMPOSSIBLE                          { failwith "impossible" } ;

line :
    /* empty */                         { !Lexer.lineno } ;

stmt1 :
    /* empty */                         { Skip }
  | variable ASSIGN expr                { Assign ($1, $3) }
  | name actuals                        { ProcCall ($1, $2) }
  | RETURN expr_opt                     { Return $2 }
  | IF expr THEN stmts elses END        { IfStmt ($2, $4, $5) }
  | WHILE expr DO stmts END             { WhileStmt ($2, $4) } 
  | REPEAT stmts UNTIL expr             { RepeatStmt ($2, $4) }
  | FOR name ASSIGN expr TO expr DO stmts END 
                                        { let v = make_expr (Variable $2) in
                                          ForStmt (v, $4, $6, $8, ref None) } 
  | CASE expr OF arms else_part END     { CaseStmt ($2, $4, $5) } ;

elses :
    /* empty */                         { make_stmt (Skip, 0) }
  | ELSE stmts                          { $2 }
  | ELSIF line expr THEN stmts elses    { make_stmt (IfStmt ($3,$5,$6), $2) } ;

arms :
    arm                                 { [$1] }
  | arm VBAR arms                       { $1 :: $3 } ;

arm :
    expr COLON stmts                    { ($1, $3) };

else_part :
    /* empty */                         { make_stmt (Skip, 0) }
  | ELSE stmts                          { $2 } ;

ident_list :    
    IDENT                               { [$1] }
  | IDENT COMMA ident_list              { $1 :: $3 } ;

expr_opt :      
    /* empty */                         { None }
  | expr                                { Some $1 } ;

expr :
    simple                              { $1 }
  | expr RELOP simple                   { make_expr (Binop ($2, $1, $3)) }
  | expr EQUAL simple                   { make_expr (Binop (Eq, $1, $3)) } ;

simple :
    term                                { $1 }
  | simple ADDOP term                   { make_expr (Binop ($2, $1, $3)) }
  | simple MINUS term                   { make_expr (Binop (Minus, $1, $3)) } ;

term :
    factor                              { $1 }
  | term MULOP factor                   { make_expr (Binop ($2, $1, $3)) } ;

factor :
    variable                            { $1 }
  | NUMBER                              { const $1 integer }
  | STRING                              { let (lab, len) = $1 in
                                          make_expr (String (lab, len)) }
  | CHAR                                { const (int_of_char $1) character }
  | NIL                                 { make_expr Nil }
  | name actuals                        { make_expr (FuncCall ($1, $2)) }
  | NOT factor                          { make_expr (Monop (Not, $2)) }
  | MINUS factor                        { make_expr (Monop (Uminus, $2)) }
  | LPAR expr RPAR                      { $2 } ;

actuals :       
    LPAR RPAR                           { [] }
  | LPAR expr_list RPAR                 { $2 } ;

expr_list :     
    expr                                { [$1] }
  | expr COMMA expr_list                { $1 :: $3 } ;

variable :      
    name                                { make_expr (Variable $1) }
  | variable SUB expr DOT DOT expr RPAR { make_expr (Slice ($1,$3,$6)) }
  | variable SUB name DOT DOT expr RPAR { make_expr (Slice ($1,make_expr (Variable $3),$6)) }
  | variable SUB expr BUS               { make_expr (Sub ($1, $3)) }
  | variable DOT name                   { make_expr (Select ($1, $3)) }
  | variable ARROW                      { make_expr (Deref $1) } ;


openArray :
  ARRAY OF typexpr                    { OpenArray $3 }

typexpr :       
    name                                { TypeName $1 }
  | ARRAY expr OF typexpr               { Array ($2, $4) }
  | RECORD fields END                   { Record $2 }
  | POINTER TO openArray                { Pointer $3 }
  | POINTER TO typexpr                  { Pointer $3 } ;

fields :
    field_decl opt_semi                 { [$1] }
  | field_decl SEMI fields              { $1 :: $3 } ;

field_decl :    
    ident_list COLON typexpr            { VarDecl (FieldDef, $1, $3) } ;

opt_semi :
    SEMI                                { () }
  | /* empty */                         { () } ;

name :  
    IDENT                               { make_name ($1, !Lexer.lineno) } ;
