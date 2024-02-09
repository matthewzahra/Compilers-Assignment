type token =
  | IDENT of (Dict.ident)
  | MULOP of (Optree.op)
  | ADDOP of (Optree.op)
  | RELOP of (Optree.op)
  | NUMBER of (int)
  | CHAR of (char)
  | STRING of (Optree.symbol * int)
  | SEMI
  | DOT
  | COLON
  | LPAR
  | RPAR
  | COMMA
  | SUB
  | BUS
  | EQUAL
  | MINUS
  | ASSIGN
  | VBAR
  | ARROW
  | BADTOK
  | IMPOSSIBLE
  | ARRAY
  | BEGIN
  | CONST
  | DO
  | ELSE
  | END
  | IF
  | OF
  | PROC
  | RECORD
  | RETURN
  | THEN
  | TO
  | TYPE
  | VAR
  | WHILE
  | NOT
  | POINTER
  | NIL
  | REPEAT
  | UNTIL
  | FOR
  | ELSIF
  | CASE

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Tree.program
