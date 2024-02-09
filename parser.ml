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

open Parsing;;
let _ = parse_error;;
# 5 "parser.mly"
open Optree
open Dict
open Tree
# 31 "parser.mly"
let const n t = make_expr (Constant (n, t))
# 58 "parser.ml"
let yytransl_const = [|
  264 (* SEMI *);
  265 (* DOT *);
  266 (* COLON *);
  267 (* LPAR *);
  268 (* RPAR *);
  269 (* COMMA *);
  270 (* SUB *);
  271 (* BUS *);
  272 (* EQUAL *);
  273 (* MINUS *);
  274 (* ASSIGN *);
  275 (* VBAR *);
  276 (* ARROW *);
  277 (* BADTOK *);
  278 (* IMPOSSIBLE *);
  279 (* ARRAY *);
  280 (* BEGIN *);
  281 (* CONST *);
  282 (* DO *);
  283 (* ELSE *);
  284 (* END *);
  285 (* IF *);
  286 (* OF *);
  287 (* PROC *);
  288 (* RECORD *);
  289 (* RETURN *);
  290 (* THEN *);
  291 (* TO *);
  292 (* TYPE *);
  293 (* VAR *);
  294 (* WHILE *);
  295 (* NOT *);
  296 (* POINTER *);
  297 (* NIL *);
  298 (* REPEAT *);
  299 (* UNTIL *);
  300 (* FOR *);
  301 (* ELSIF *);
  302 (* CASE *);
    0|]

let yytransl_block = [|
  257 (* IDENT *);
  258 (* MULOP *);
  259 (* ADDOP *);
  260 (* RELOP *);
  261 (* NUMBER *);
  262 (* CHAR *);
  263 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\003\000\003\000\005\000\005\000\005\000\005\000\
\006\000\006\000\010\000\009\000\009\000\012\000\007\000\007\000\
\014\000\008\000\016\000\018\000\018\000\020\000\020\000\021\000\
\021\000\021\000\021\000\021\000\019\000\019\000\004\000\023\000\
\023\000\024\000\024\000\025\000\026\000\026\000\026\000\026\000\
\026\000\026\000\026\000\026\000\026\000\030\000\030\000\030\000\
\031\000\031\000\033\000\032\000\032\000\015\000\015\000\029\000\
\029\000\011\000\011\000\011\000\034\000\034\000\034\000\035\000\
\035\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
\036\000\036\000\028\000\028\000\037\000\037\000\027\000\027\000\
\027\000\027\000\027\000\027\000\022\000\013\000\013\000\013\000\
\013\000\013\000\038\000\038\000\039\000\040\000\040\000\017\000\
\000\000"

let yylen = "\002\000\
\002\000\004\000\000\000\002\000\002\000\002\000\001\000\002\000\
\001\000\002\000\004\000\001\000\002\000\004\000\001\000\002\000\
\004\000\004\000\004\000\002\000\003\000\001\000\003\000\003\000\
\004\000\003\000\004\000\001\000\000\000\002\000\001\000\001\000\
\003\000\002\000\001\000\000\000\000\000\003\000\002\000\002\000\
\006\000\005\000\004\000\009\000\006\000\000\000\002\000\006\000\
\001\000\003\000\003\000\000\000\002\000\001\000\003\000\000\000\
\001\000\001\000\003\000\003\000\001\000\003\000\003\000\001\000\
\003\000\001\000\001\000\001\000\001\000\001\000\002\000\002\000\
\002\000\003\000\002\000\003\000\001\000\003\000\001\000\007\000\
\007\000\004\000\003\000\002\000\003\000\001\000\004\000\003\000\
\003\000\003\000\002\000\003\000\003\000\001\000\000\000\001\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\097\000\000\000\
\000\000\000\000\007\000\000\000\000\000\005\000\000\000\096\000\
\000\000\000\000\008\000\000\000\000\000\006\000\000\000\000\000\
\001\000\000\000\004\000\000\000\000\000\010\000\000\000\000\000\
\000\000\013\000\000\000\016\000\000\000\035\000\000\000\031\000\
\000\000\000\000\000\000\067\000\069\000\068\000\000\000\000\000\
\000\000\070\000\000\000\000\000\000\000\000\000\000\000\064\000\
\020\000\000\000\000\000\028\000\000\000\000\000\000\000\019\000\
\000\000\000\000\000\000\000\000\086\000\055\000\000\000\002\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\034\000\000\000\018\000\000\000\073\000\072\000\000\000\011\000\
\000\000\000\000\071\000\000\000\000\000\084\000\000\000\000\000\
\000\000\000\000\000\000\021\000\000\000\030\000\000\000\000\000\
\000\000\000\000\000\000\014\000\017\000\033\000\000\000\000\000\
\040\000\000\000\000\000\000\000\000\000\039\000\000\000\074\000\
\000\000\000\000\075\000\000\000\000\000\083\000\000\000\000\000\
\000\000\000\000\065\000\000\000\000\000\026\000\024\000\023\000\
\000\000\000\000\088\000\000\000\091\000\090\000\089\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\076\000\000\000\
\082\000\000\000\027\000\025\000\000\000\087\000\093\000\092\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\078\000\
\000\000\000\000\085\000\000\000\036\000\000\000\042\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\047\000\000\000\
\041\000\000\000\051\000\053\000\045\000\050\000\080\000\081\000\
\000\000\000\000\000\000\000\000\000\000\044\000\048\000"

let yydgoto = "\002\000\
\007\000\008\000\009\000\039\000\010\000\014\000\022\000\011\000\
\019\000\015\000\103\000\020\000\068\000\023\000\024\000\012\000\
\052\000\032\000\064\000\061\000\062\000\135\000\040\000\041\000\
\042\000\081\000\053\000\091\000\113\000\174\000\166\000\179\000\
\167\000\054\000\055\000\056\000\125\000\105\000\106\000\141\000"

let yysindex = "\021\000\
\198\255\000\000\038\255\057\255\064\255\100\255\000\000\106\255\
\105\255\198\255\000\000\156\255\149\255\000\000\038\255\000\000\
\158\255\152\255\000\000\064\255\163\255\000\000\100\255\168\255\
\000\000\165\255\000\000\198\255\024\001\000\000\026\255\181\255\
\011\255\000\000\100\255\000\000\011\255\000\000\170\255\000\000\
\199\255\137\255\208\255\000\000\000\000\000\000\024\001\024\001\
\024\001\000\000\146\255\190\255\191\255\007\255\204\255\000\000\
\000\000\100\255\209\255\000\000\206\255\212\255\011\255\000\000\
\024\001\100\255\186\255\216\255\000\000\000\000\217\255\000\000\
\165\255\024\001\024\001\024\001\165\255\057\255\024\001\190\255\
\000\000\056\000\000\000\116\255\000\000\000\000\024\001\000\000\
\024\001\014\255\000\000\057\255\024\001\000\000\024\001\024\001\
\024\001\230\255\157\255\000\000\036\255\000\000\012\255\237\255\
\224\255\228\255\157\255\000\000\000\000\000\000\052\255\130\255\
\000\000\056\255\210\255\233\255\019\255\000\000\024\001\000\000\
\007\255\007\255\000\000\180\255\246\255\000\000\098\255\124\255\
\204\255\204\255\000\000\157\255\010\001\000\000\000\000\000\000\
\011\255\011\255\000\000\100\255\000\000\000\000\000\000\165\255\
\165\255\024\001\024\001\024\001\130\255\024\001\000\000\245\255\
\000\000\251\255\000\000\000\000\011\255\000\000\000\000\000\000\
\240\254\234\255\130\255\254\254\028\000\241\255\002\000\000\000\
\024\001\024\001\000\000\165\255\000\000\253\255\000\000\024\001\
\165\255\165\255\017\000\024\001\024\000\035\000\000\000\024\001\
\000\000\123\255\000\000\000\000\000\000\000\000\000\000\000\000\
\096\255\165\255\165\255\018\000\240\254\000\000\000\000"

let yyrindex = "\000\000\
\003\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\003\000\000\000\000\000\000\000\000\000\178\255\000\000\
\000\000\000\000\000\000\008\001\020\000\000\000\046\001\000\000\
\000\000\166\255\000\000\003\000\000\000\000\000\000\000\073\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\040\000\242\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\229\255\007\000\159\000\045\000\000\000\
\000\000\000\000\000\000\000\000\000\000\031\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\051\255\000\000\134\000\000\000\184\255\000\000\000\000\094\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\038\000\000\000\000\000\000\000\000\000\000\000\172\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\197\000\235\000\000\000\044\000\000\000\000\000\000\000\052\001\
\083\000\121\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\041\000\000\000\000\000\000\000\115\255\
\166\255\000\000\000\000\000\000\210\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\049\000\000\000\015\001\000\000\000\000\050\000\060\255\000\000\
\000\000\000\000\000\000\166\255\000\000\000\000\000\000\000\000\
\144\255\166\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\166\255\115\255\000\000\049\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\035\001\071\001\187\255\000\000\067\001\061\001\000\000\
\069\001\000\000\227\255\000\000\223\255\000\000\238\255\226\255\
\255\255\000\000\000\000\249\000\000\000\193\255\028\001\000\000\
\180\000\000\000\062\001\025\001\000\000\165\000\183\000\000\000\
\000\000\176\255\015\000\213\255\221\000\232\000\000\000\000\000"

let yytablesize = 595
let yytable = "\051\000\
\060\000\087\000\017\000\071\000\085\000\086\000\121\000\115\000\
\122\000\095\000\172\000\016\000\059\000\089\000\016\000\087\000\
\070\000\084\000\044\000\045\000\046\000\001\000\087\000\096\000\
\047\000\123\000\021\000\089\000\173\000\102\000\048\000\069\000\
\176\000\065\000\089\000\069\000\021\000\057\000\013\000\098\000\
\080\000\137\000\066\000\143\000\111\000\112\000\114\000\104\000\
\148\000\117\000\067\000\036\000\049\000\131\000\050\000\087\000\
\004\000\016\000\036\000\087\000\124\000\069\000\058\000\127\000\
\018\000\134\000\004\000\089\000\156\000\036\000\060\000\089\000\
\058\000\142\000\161\000\162\000\116\000\036\000\036\000\036\000\
\029\000\145\000\059\000\036\000\029\000\144\000\049\000\049\000\
\036\000\149\000\126\000\128\000\036\000\036\000\036\000\036\000\
\036\000\069\000\155\000\087\000\021\000\087\000\183\000\158\000\
\159\000\069\000\152\000\187\000\188\000\129\000\130\000\089\000\
\153\000\089\000\025\000\036\000\163\000\164\000\165\000\087\000\
\124\000\104\000\036\000\171\000\196\000\197\000\087\000\120\000\
\026\000\195\000\069\000\089\000\154\000\087\000\090\000\069\000\
\069\000\016\000\089\000\181\000\182\000\036\000\036\000\036\000\
\036\000\089\000\186\000\036\000\194\000\087\000\165\000\036\000\
\036\000\088\000\193\000\069\000\036\000\016\000\036\000\036\000\
\036\000\089\000\036\000\028\000\029\000\074\000\036\000\033\000\
\031\000\075\000\036\000\036\000\036\000\036\000\076\000\035\000\
\036\000\037\000\077\000\133\000\078\000\036\000\079\000\087\000\
\036\000\036\000\038\000\036\000\066\000\036\000\063\000\036\000\
\150\000\036\000\036\000\089\000\067\000\072\000\036\000\092\000\
\090\000\009\000\009\000\036\000\093\000\097\000\073\000\036\000\
\009\000\036\000\094\000\036\000\036\000\009\000\009\000\083\000\
\036\000\100\000\099\000\101\000\107\000\036\000\003\000\108\000\
\109\000\036\000\036\000\036\000\004\000\036\000\079\000\079\000\
\079\000\005\000\006\000\140\000\079\000\079\000\079\000\132\000\
\079\000\079\000\079\000\079\000\079\000\079\000\138\000\079\000\
\079\000\037\000\147\000\139\000\146\000\169\000\079\000\079\000\
\079\000\151\000\079\000\170\000\037\000\175\000\079\000\079\000\
\066\000\066\000\066\000\178\000\037\000\037\000\066\000\079\000\
\066\000\079\000\066\000\066\000\180\000\066\000\066\000\066\000\
\185\000\066\000\003\000\087\000\037\000\054\000\037\000\087\000\
\066\000\066\000\066\000\191\000\066\000\177\000\087\000\089\000\
\066\000\066\000\022\000\089\000\189\000\198\000\192\000\061\000\
\061\000\066\000\089\000\066\000\061\000\061\000\061\000\077\000\
\061\000\061\000\032\000\061\000\061\000\061\000\043\000\061\000\
\092\000\095\000\032\000\032\000\094\000\093\000\061\000\061\000\
\061\000\119\000\061\000\094\000\046\000\052\000\061\000\061\000\
\027\000\030\000\032\000\036\000\032\000\062\000\062\000\061\000\
\034\000\061\000\062\000\062\000\062\000\136\000\062\000\062\000\
\184\000\062\000\062\000\062\000\110\000\062\000\079\000\082\000\
\118\000\199\000\190\000\079\000\062\000\062\000\062\000\079\000\
\062\000\079\000\168\000\160\000\062\000\062\000\000\000\000\000\
\000\000\000\000\000\000\063\000\063\000\062\000\000\000\062\000\
\063\000\063\000\063\000\000\000\063\000\063\000\000\000\063\000\
\063\000\063\000\000\000\063\000\000\000\056\000\000\000\000\000\
\000\000\000\000\063\000\063\000\063\000\000\000\063\000\000\000\
\056\000\000\000\063\000\063\000\000\000\000\000\000\000\000\000\
\056\000\056\000\058\000\063\000\000\000\063\000\058\000\058\000\
\058\000\000\000\058\000\058\000\000\000\058\000\058\000\000\000\
\056\000\058\000\056\000\057\000\000\000\000\000\000\000\000\000\
\058\000\058\000\058\000\000\000\058\000\000\000\057\000\000\000\
\058\000\058\000\000\000\000\000\000\000\000\000\057\000\057\000\
\059\000\058\000\000\000\058\000\059\000\059\000\059\000\000\000\
\059\000\059\000\000\000\059\000\059\000\000\000\057\000\059\000\
\057\000\038\000\000\000\000\000\000\000\000\000\059\000\059\000\
\059\000\000\000\059\000\000\000\038\000\000\000\059\000\059\000\
\000\000\000\000\000\000\000\000\038\000\038\000\060\000\059\000\
\000\000\059\000\060\000\060\000\060\000\000\000\060\000\060\000\
\000\000\060\000\060\000\000\000\038\000\060\000\038\000\000\000\
\000\000\000\000\000\000\000\000\060\000\060\000\060\000\000\000\
\060\000\000\000\016\000\000\000\060\000\060\000\044\000\045\000\
\046\000\000\000\000\000\000\000\047\000\060\000\043\000\060\000\
\016\000\000\000\048\000\000\000\044\000\045\000\046\000\012\000\
\012\000\043\000\047\000\000\000\000\000\000\000\012\000\157\000\
\048\000\043\000\043\000\012\000\012\000\000\000\000\000\000\000\
\049\000\000\000\050\000\000\000\000\000\079\000\079\000\079\000\
\000\000\043\000\000\000\043\000\000\000\000\000\049\000\000\000\
\050\000\079\000\079\000\079\000\079\000\015\000\015\000\079\000\
\000\000\000\000\000\000\000\000\015\000\000\000\000\000\000\000\
\000\000\015\000\015\000"

let yycheck = "\029\000\
\031\000\004\001\004\000\037\000\048\000\049\000\087\000\077\000\
\089\000\003\001\027\001\001\001\031\000\016\001\001\001\004\001\
\035\000\047\000\005\001\006\001\007\001\001\000\004\001\017\001\
\011\001\012\001\001\001\016\001\045\001\063\000\017\001\033\000\
\035\001\023\001\016\001\037\000\001\001\012\001\001\001\058\000\
\042\000\030\001\032\001\107\000\074\000\075\000\076\000\066\000\
\030\001\079\000\040\001\001\001\039\001\097\000\041\001\004\001\
\031\001\001\001\008\001\004\001\090\000\063\000\037\001\093\000\
\001\001\099\000\031\001\016\001\132\000\019\001\101\000\016\001\
\037\001\107\000\144\000\145\000\078\000\027\001\028\001\029\001\
\008\001\026\001\101\000\033\001\012\001\034\001\027\001\028\001\
\038\001\119\000\092\000\093\000\042\001\043\001\044\001\045\001\
\046\001\099\000\132\000\004\001\001\001\004\001\172\000\137\000\
\138\000\107\000\009\001\177\000\178\000\095\000\096\000\016\001\
\015\001\016\001\009\001\001\001\146\000\147\000\148\000\004\001\
\150\000\140\000\008\001\157\000\194\000\195\000\004\001\012\001\
\024\001\034\001\132\000\016\001\009\001\004\001\011\001\137\000\
\138\000\001\001\016\001\169\000\170\000\027\001\028\001\029\001\
\001\001\016\001\176\000\033\001\026\001\004\001\180\000\008\001\
\038\001\008\001\184\000\157\000\042\001\001\001\044\001\045\001\
\046\001\016\001\019\001\008\001\016\001\029\001\001\001\016\001\
\011\001\033\001\027\001\028\001\029\001\008\001\038\001\013\001\
\033\001\010\001\042\001\023\001\044\001\038\001\046\001\004\001\
\001\001\042\001\022\001\044\001\032\001\046\001\010\001\008\001\
\013\001\028\001\029\001\016\001\040\001\028\001\033\001\009\001\
\011\001\024\001\025\001\038\001\014\001\002\001\008\001\042\001\
\031\001\044\001\020\001\046\001\029\001\036\001\037\001\008\001\
\033\001\012\001\010\001\008\001\035\001\038\001\025\001\008\001\
\008\001\042\001\043\001\044\001\031\001\046\001\002\001\003\001\
\004\001\036\001\037\001\008\001\008\001\009\001\010\001\010\001\
\012\001\013\001\014\001\015\001\016\001\017\001\010\001\019\001\
\020\001\008\001\018\001\028\001\043\001\009\001\026\001\027\001\
\028\001\012\001\030\001\009\001\019\001\028\001\034\001\035\001\
\002\001\003\001\004\001\027\001\027\001\028\001\008\001\043\001\
\010\001\045\001\012\001\013\001\019\001\015\001\016\001\017\001\
\028\001\019\001\024\001\004\001\043\001\010\001\045\001\004\001\
\026\001\027\001\028\001\012\001\030\001\010\001\004\001\016\001\
\034\001\035\001\012\001\016\001\028\001\028\001\012\001\003\001\
\004\001\043\001\016\001\045\001\008\001\009\001\010\001\012\001\
\012\001\013\001\019\001\015\001\016\001\017\001\028\000\019\001\
\009\001\028\001\027\001\028\001\028\001\014\001\026\001\027\001\
\028\001\018\001\030\001\020\001\028\001\028\001\034\001\035\001\
\010\000\015\000\043\001\023\000\045\001\003\001\004\001\043\001\
\020\000\045\001\008\001\009\001\010\001\101\000\012\001\013\001\
\173\000\015\001\016\001\017\001\073\000\019\001\009\001\042\000\
\080\000\197\000\180\000\014\001\026\001\027\001\028\001\018\001\
\030\001\020\001\150\000\140\000\034\001\035\001\255\255\255\255\
\255\255\255\255\255\255\003\001\004\001\043\001\255\255\045\001\
\008\001\009\001\010\001\255\255\012\001\013\001\255\255\015\001\
\016\001\017\001\255\255\019\001\255\255\008\001\255\255\255\255\
\255\255\255\255\026\001\027\001\028\001\255\255\030\001\255\255\
\019\001\255\255\034\001\035\001\255\255\255\255\255\255\255\255\
\027\001\028\001\004\001\043\001\255\255\045\001\008\001\009\001\
\010\001\255\255\012\001\013\001\255\255\015\001\016\001\255\255\
\043\001\019\001\045\001\008\001\255\255\255\255\255\255\255\255\
\026\001\027\001\028\001\255\255\030\001\255\255\019\001\255\255\
\034\001\035\001\255\255\255\255\255\255\255\255\027\001\028\001\
\004\001\043\001\255\255\045\001\008\001\009\001\010\001\255\255\
\012\001\013\001\255\255\015\001\016\001\255\255\043\001\019\001\
\045\001\008\001\255\255\255\255\255\255\255\255\026\001\027\001\
\028\001\255\255\030\001\255\255\019\001\255\255\034\001\035\001\
\255\255\255\255\255\255\255\255\027\001\028\001\004\001\043\001\
\255\255\045\001\008\001\009\001\010\001\255\255\012\001\013\001\
\255\255\015\001\016\001\255\255\043\001\019\001\045\001\255\255\
\255\255\255\255\255\255\255\255\026\001\027\001\028\001\255\255\
\030\001\255\255\001\001\255\255\034\001\035\001\005\001\006\001\
\007\001\255\255\255\255\255\255\011\001\043\001\008\001\045\001\
\001\001\255\255\017\001\255\255\005\001\006\001\007\001\024\001\
\025\001\019\001\011\001\255\255\255\255\255\255\031\001\030\001\
\017\001\027\001\028\001\036\001\037\001\255\255\255\255\255\255\
\039\001\255\255\041\001\255\255\255\255\002\001\003\001\004\001\
\255\255\043\001\255\255\045\001\255\255\255\255\039\001\255\255\
\041\001\014\001\015\001\016\001\017\001\024\001\025\001\020\001\
\255\255\255\255\255\255\255\255\031\001\255\255\255\255\255\255\
\255\255\036\001\037\001"

let yynames_const = "\
  SEMI\000\
  DOT\000\
  COLON\000\
  LPAR\000\
  RPAR\000\
  COMMA\000\
  SUB\000\
  BUS\000\
  EQUAL\000\
  MINUS\000\
  ASSIGN\000\
  VBAR\000\
  ARROW\000\
  BADTOK\000\
  IMPOSSIBLE\000\
  ARRAY\000\
  BEGIN\000\
  CONST\000\
  DO\000\
  ELSE\000\
  END\000\
  IF\000\
  OF\000\
  PROC\000\
  RECORD\000\
  RETURN\000\
  THEN\000\
  TO\000\
  TYPE\000\
  VAR\000\
  WHILE\000\
  NOT\000\
  POINTER\000\
  NIL\000\
  REPEAT\000\
  UNTIL\000\
  FOR\000\
  ELSIF\000\
  CASE\000\
  "

let yynames_block = "\
  IDENT\000\
  MULOP\000\
  ADDOP\000\
  RELOP\000\
  NUMBER\000\
  CHAR\000\
  STRING\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    Obj.repr(
# 37 "parser.mly"
                                        ( Prog (_1, ref []) )
# 450 "parser.ml"
               : Tree.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'decl_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 40 "parser.mly"
                                        ( make_block (_1, _3) )
# 458 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "parser.mly"
                                        ( [] )
# 464 "parser.ml"
               : 'decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decl_list) in
    Obj.repr(
# 44 "parser.mly"
                                        ( _1 @ _2 )
# 472 "parser.ml"
               : 'decl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'const_decls) in
    Obj.repr(
# 47 "parser.mly"
                                        ( _2 )
# 479 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'var_decls) in
    Obj.repr(
# 48 "parser.mly"
                                        ( _2 )
# 486 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'proc_decl) in
    Obj.repr(
# 49 "parser.mly"
                                        ( [_1] )
# 493 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'type_decls) in
    Obj.repr(
# 50 "parser.mly"
                                        ( [TypeDecl _2] )
# 500 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'const_decl) in
    Obj.repr(
# 53 "parser.mly"
                                        ( [_1] )
# 507 "parser.ml"
               : 'const_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'const_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'const_decls) in
    Obj.repr(
# 54 "parser.mly"
                                        ( _1 :: _2 )
# 515 "parser.ml"
               : 'const_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Dict.ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 57 "parser.mly"
                                        ( ConstDecl (_1, _3) )
# 523 "parser.ml"
               : 'const_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'type_decl) in
    Obj.repr(
# 60 "parser.mly"
                                        ( [_1] )
# 530 "parser.ml"
               : 'type_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'type_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'type_decls) in
    Obj.repr(
# 61 "parser.mly"
                                        ( _1 :: _2 )
# 538 "parser.ml"
               : 'type_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Dict.ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typexpr) in
    Obj.repr(
# 64 "parser.mly"
                                        ( (_1, _3) )
# 546 "parser.ml"
               : 'type_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'var_decl) in
    Obj.repr(
# 67 "parser.mly"
                                        ( [_1] )
# 553 "parser.ml"
               : 'var_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'var_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'var_decls) in
    Obj.repr(
# 68 "parser.mly"
                                        ( _1 :: _2 )
# 561 "parser.ml"
               : 'var_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'ident_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typexpr) in
    Obj.repr(
# 71 "parser.mly"
                                        ( VarDecl (VarDef, _1, _3) )
# 569 "parser.ml"
               : 'var_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'proc_heading) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    Obj.repr(
# 74 "parser.mly"
                                        ( ProcDecl (_1, _3) )
# 577 "parser.ml"
               : 'proc_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'name) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'params) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'return_type) in
    Obj.repr(
# 77 "parser.mly"
                                        ( Heading (_2, _3, _4) )
# 586 "parser.ml"
               : 'proc_heading))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
                                        ( [] )
# 592 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'formal_decls) in
    Obj.repr(
# 81 "parser.mly"
                                        ( _2 )
# 599 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_decl) in
    Obj.repr(
# 84 "parser.mly"
                                        ( [_1] )
# 606 "parser.ml"
               : 'formal_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_decl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal_decls) in
    Obj.repr(
# 85 "parser.mly"
                                        ( _1 :: _3 )
# 614 "parser.ml"
               : 'formal_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'openArray) in
    Obj.repr(
# 88 "parser.mly"
                                        ( VarDecl (OpenArrayParamDef, _1, _3) )
# 622 "parser.ml"
               : 'formal_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'ident_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'openArray) in
    Obj.repr(
# 89 "parser.mly"
                                        ( VarDecl (OpenArrayParamDef, _2, _4) )
# 630 "parser.ml"
               : 'formal_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typexpr) in
    Obj.repr(
# 90 "parser.mly"
                                        ( VarDecl (CParamDef, _1, _3) )
# 638 "parser.ml"
               : 'formal_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'ident_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'typexpr) in
    Obj.repr(
# 91 "parser.mly"
                                        ( VarDecl (VParamDef, _2, _4) )
# 646 "parser.ml"
               : 'formal_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'proc_heading) in
    Obj.repr(
# 92 "parser.mly"
                                        ( PParamDecl _1 )
# 653 "parser.ml"
               : 'formal_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "parser.mly"
                                        ( None )
# 659 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'typexpr) in
    Obj.repr(
# 96 "parser.mly"
                                        ( Some _2 )
# 666 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 99 "parser.mly"
                                        ( seq _1 )
# 673 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 102 "parser.mly"
                                        ( [_1] )
# 680 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 103 "parser.mly"
                                        ( _1 :: _3 )
# 688 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'line) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt1) in
    Obj.repr(
# 106 "parser.mly"
                                        ( make_stmt (_2, _1) )
# 696 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "parser.mly"
                                        ( failwith "impossible" )
# 702 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 111 "parser.mly"
                                        ( !Lexer.lineno )
# 708 "parser.ml"
               : 'line))
; (fun __caml_parser_env ->
    Obj.repr(
# 114 "parser.mly"
                                        ( Skip )
# 714 "parser.ml"
               : 'stmt1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'variable) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "parser.mly"
                                        ( Assign (_1, _3) )
# 722 "parser.ml"
               : 'stmt1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'name) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'actuals) in
    Obj.repr(
# 116 "parser.mly"
                                        ( ProcCall (_1, _2) )
# 730 "parser.ml"
               : 'stmt1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr_opt) in
    Obj.repr(
# 117 "parser.mly"
                                        ( Return _2 )
# 737 "parser.ml"
               : 'stmt1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'stmts) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'elses) in
    Obj.repr(
# 118 "parser.mly"
                                        ( IfStmt (_2, _4, _5) )
# 746 "parser.ml"
               : 'stmt1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 119 "parser.mly"
                                        ( WhileStmt (_2, _4) )
# 754 "parser.ml"
               : 'stmt1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'stmts) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "parser.mly"
                                        ( RepeatStmt (_2, _4) )
# 762 "parser.ml"
               : 'stmt1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 122 "parser.mly"
                                        ( let v = make_expr (Variable _2) in
                                          ForStmt (v, _4, _6, _8, ref None) )
# 773 "parser.ml"
               : 'stmt1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'arms) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'else_part) in
    Obj.repr(
# 124 "parser.mly"
                                        ( CaseStmt (_2, _4, _5) )
# 782 "parser.ml"
               : 'stmt1))
; (fun __caml_parser_env ->
    Obj.repr(
# 127 "parser.mly"
                                        ( make_stmt (Skip, 0) )
# 788 "parser.ml"
               : 'elses))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmts) in
    Obj.repr(
# 128 "parser.mly"
                                        ( _2 )
# 795 "parser.ml"
               : 'elses))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'line) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'elses) in
    Obj.repr(
# 129 "parser.mly"
                                        ( make_stmt (IfStmt (_3,_5,_6), _2) )
# 805 "parser.ml"
               : 'elses))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arm) in
    Obj.repr(
# 132 "parser.mly"
                                        ( [_1] )
# 812 "parser.ml"
               : 'arms))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arms) in
    Obj.repr(
# 133 "parser.mly"
                                        ( _1 :: _3 )
# 820 "parser.ml"
               : 'arms))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'stmts) in
    Obj.repr(
# 136 "parser.mly"
                                        ( (_1, _3) )
# 828 "parser.ml"
               : 'arm))
; (fun __caml_parser_env ->
    Obj.repr(
# 139 "parser.mly"
                                        ( make_stmt (Skip, 0) )
# 834 "parser.ml"
               : 'else_part))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmts) in
    Obj.repr(
# 140 "parser.mly"
                                        ( _2 )
# 841 "parser.ml"
               : 'else_part))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Dict.ident) in
    Obj.repr(
# 143 "parser.mly"
                                        ( [_1] )
# 848 "parser.ml"
               : 'ident_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Dict.ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ident_list) in
    Obj.repr(
# 144 "parser.mly"
                                        ( _1 :: _3 )
# 856 "parser.ml"
               : 'ident_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 147 "parser.mly"
                                        ( None )
# 862 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 148 "parser.mly"
                                        ( Some _1 )
# 869 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple) in
    Obj.repr(
# 151 "parser.mly"
                                        ( _1 )
# 876 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Optree.op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'simple) in
    Obj.repr(
# 152 "parser.mly"
                                        ( make_expr (Binop (_2, _1, _3)) )
# 885 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'simple) in
    Obj.repr(
# 153 "parser.mly"
                                        ( make_expr (Binop (Eq, _1, _3)) )
# 893 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 156 "parser.mly"
                                        ( _1 )
# 900 "parser.ml"
               : 'simple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'simple) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Optree.op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 157 "parser.mly"
                                        ( make_expr (Binop (_2, _1, _3)) )
# 909 "parser.ml"
               : 'simple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'simple) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 158 "parser.mly"
                                        ( make_expr (Binop (Minus, _1, _3)) )
# 917 "parser.ml"
               : 'simple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 161 "parser.mly"
                                        ( _1 )
# 924 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Optree.op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 162 "parser.mly"
                                        ( make_expr (Binop (_2, _1, _3)) )
# 933 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'variable) in
    Obj.repr(
# 165 "parser.mly"
                                        ( _1 )
# 940 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 166 "parser.mly"
                                        ( const _1 integer )
# 947 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Optree.symbol * int) in
    Obj.repr(
# 167 "parser.mly"
                                        ( let (lab, len) = _1 in
                                          make_expr (String (lab, len)) )
# 955 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 169 "parser.mly"
                                        ( const (int_of_char _1) character )
# 962 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    Obj.repr(
# 170 "parser.mly"
                                        ( make_expr Nil )
# 968 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'name) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'actuals) in
    Obj.repr(
# 171 "parser.mly"
                                        ( make_expr (FuncCall (_1, _2)) )
# 976 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 172 "parser.mly"
                                        ( make_expr (Monop (Not, _2)) )
# 983 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 173 "parser.mly"
                                        ( make_expr (Monop (Uminus, _2)) )
# 990 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 174 "parser.mly"
                                        ( _2 )
# 997 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    Obj.repr(
# 177 "parser.mly"
                                        ( [] )
# 1003 "parser.ml"
               : 'actuals))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 178 "parser.mly"
                                        ( _2 )
# 1010 "parser.ml"
               : 'actuals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 181 "parser.mly"
                                        ( [_1] )
# 1017 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list) in
    Obj.repr(
# 182 "parser.mly"
                                        ( _1 :: _3 )
# 1025 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 185 "parser.mly"
                                        ( make_expr (Variable _1) )
# 1032 "parser.ml"
               : 'variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'variable) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 186 "parser.mly"
                                        ( make_expr (Slice (_1,_3,_6)) )
# 1041 "parser.ml"
               : 'variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'variable) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'name) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 187 "parser.mly"
                                        ( make_expr (Slice (_1,make_expr (Variable _3),_6)) )
# 1050 "parser.ml"
               : 'variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'variable) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 188 "parser.mly"
                                        ( make_expr (Sub (_1, _3)) )
# 1058 "parser.ml"
               : 'variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'variable) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 189 "parser.mly"
                                        ( make_expr (Select (_1, _3)) )
# 1066 "parser.ml"
               : 'variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'variable) in
    Obj.repr(
# 190 "parser.mly"
                                        ( make_expr (Deref _1) )
# 1073 "parser.ml"
               : 'variable))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typexpr) in
    Obj.repr(
# 194 "parser.mly"
                                      ( OpenArray _3 )
# 1080 "parser.ml"
               : 'openArray))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 197 "parser.mly"
                                        ( TypeName _1 )
# 1087 "parser.ml"
               : 'typexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'typexpr) in
    Obj.repr(
# 198 "parser.mly"
                                        ( Array (_2, _4) )
# 1095 "parser.ml"
               : 'typexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'fields) in
    Obj.repr(
# 199 "parser.mly"
                                        ( Record _2 )
# 1102 "parser.ml"
               : 'typexpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'openArray) in
    Obj.repr(
# 200 "parser.mly"
                                        ( Pointer _3 )
# 1109 "parser.ml"
               : 'typexpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typexpr) in
    Obj.repr(
# 201 "parser.mly"
                                        ( Pointer _3 )
# 1116 "parser.ml"
               : 'typexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'field_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'opt_semi) in
    Obj.repr(
# 204 "parser.mly"
                                        ( [_1] )
# 1124 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'field_decl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fields) in
    Obj.repr(
# 205 "parser.mly"
                                        ( _1 :: _3 )
# 1132 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typexpr) in
    Obj.repr(
# 208 "parser.mly"
                                        ( VarDecl (FieldDef, _1, _3) )
# 1140 "parser.ml"
               : 'field_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 211 "parser.mly"
                                        ( () )
# 1146 "parser.ml"
               : 'opt_semi))
; (fun __caml_parser_env ->
    Obj.repr(
# 212 "parser.mly"
                                        ( () )
# 1152 "parser.ml"
               : 'opt_semi))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Dict.ident) in
    Obj.repr(
# 215 "parser.mly"
                                        ( make_name (_1, !Lexer.lineno) )
# 1159 "parser.ml"
               : 'name))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Tree.program)
