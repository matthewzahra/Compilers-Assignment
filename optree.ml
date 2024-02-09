(* lab4/optree.ml *)
(* Copyright (c) 2017 J. M. Spivey *)

open Print

(* CODE LABELS *)

type codelab = int

let nolab = -1

(* |lab| -- last used code label *)
let lab = ref 0

(* |label| -- allocate a code label *)
let label () = incr lab; !lab

(* |fLab| -- format a code label for printf *)
let fLab n = fMeta ".L$" [fNum n]

type symbol = string


(* OPERATORS *)

(* |op| -- type of picoPascal operators *)
type op = Plus | Minus | Times | Div | Mod | Eq 
  | Uminus | Lt | Gt | Leq | Geq | Neq | And | Or | Not | Lsl
  | Lsr | Asr | BitAnd | BitOr | BitNot

let op_name =
  function
      Plus -> "Plus" | Minus -> "Minus" | Times -> "Times"
    | Div -> "Div" | Mod -> "Mod" | Eq -> "Eq"
    | Uminus -> "Uminus" | Lt -> "Lt" | Gt -> "Gt" 
    | Leq -> "Leq" | Geq -> "Geq" | Neq -> "Neq" 
    | And -> "And" | Or -> "Or" | Not -> "Not"
    | Lsl -> "Lsl" | Lsr -> "Lsr" | Asr -> "Asr" 
    | BitAnd -> "BitAnd" | BitOr -> "BitOr" | BitNot -> "BitNot"

let fOp w = fStr (op_name w)


(* IR INSTUCTIONS *)

(* |inst| -- type of intermediate instructions *)
type inst =
    CONST of int                (* Constant (value) *)
  | GLOBAL of symbol            (* Constant (symbol) *)
  | LOCAL of int                (* Local address (offset) *)
  | REGVAR of int               (* Register (index) *)
  | LOADC                       (* Load char *)
  | LOADW                       (* Load word *)
  | STOREC                      (* Store char *)
  | STOREW                      (* Store word *)
  | ARG of int                  (* Pass argument (index) *)
  | STATLINK                    (* Pass static link *)
  | CALL of int                 (* Call procedure (nparams) *)
  | RESULTW                     (* Procedure result *)
  | MONOP of op                 (* Perform unary operation (op) *)
  | BINOP of op                 (* Perform binary operation (op) *)
  | OFFSET                      (* Add address and offset *)
  | BOUND                       (* Array bound check *)
  | SLICE
  | NCHECK                      (* Null pointer check *)
  | LABEL of codelab            (* Set code label *)
  | JUMP of codelab             (* Unconditional branch (dest) *)
  | JUMPC of op * codelab       (* Conditional branch (cond, dest) *)
  | JCASE of codelab list * codelab (* Jump table *)

  (* Extra instructions *)
  | LINE of int                 (* Line number *)
  | NOP
  | SEQ
  | AFTER                       (* Expression with side effect *)
  | DEFTEMP of int              (* Define temp *)
  | TEMP of int                 (* Temporary *)

let fInst =
  function
      CONST x ->        fMeta "CONST $" [fNum x]
    | GLOBAL a ->       fMeta "GLOBAL $" [fStr a]
    | LOCAL n ->        fMeta "LOCAL $" [fNum n]
    | REGVAR i ->       fMeta "REGVAR $" [fNum i]
    | LOADC ->          fStr "LOADC"
    | LOADW ->          fStr "LOADW"
    | STOREC ->         fStr "STOREC"
    | STOREW ->         fStr "STOREW"
    | ARG n ->          fMeta "ARG $" [fNum n]
    | STATLINK ->       fStr "STATLINK"
    | CALL n ->         fMeta "CALL $" [fNum n]
    | RESULTW ->        fStr "RESULTW"
    | MONOP w ->        fMeta "MONOP $" [fOp w]
    | BINOP w ->        fMeta "BINOP $" [fOp w]
    | OFFSET ->         fStr "OFFSET"
    | BOUND ->          fStr "BOUND"
    | SLICE ->           fStr "SLICE"
    | NCHECK ->         fStr "NCHECK"
    | LABEL l ->        fMeta "LABEL $" [fLab l]
    | JUMP l ->         fMeta "JUMP $" [fLab l]
    | JUMPC (w, l) ->   fMeta "JUMPC $ $" [fOp w; fLab l]
    | JCASE (labs, def) -> fMeta "JCASE $ $" [fNum (List.length labs); fLab def]
    | LINE n ->         fMeta "LINE $" [fNum n]
    | NOP ->            fStr "NOP"
    | SEQ ->            fStr "SEQ"
    | AFTER ->          fStr "AFTER"
    | DEFTEMP n ->      fMeta "DEFTEMP $" [fNum n]
    | TEMP n ->         fMeta "TEMP $" [fNum n]

let int_of_bool b = if b then 1 else 0

(* |do_monop| -- evaluate unary operators *)
let do_monop w x =
  match w with
      Uminus -> - x
    | Not -> if x <> 0 then 0 else 1
    | BitNot -> lnot x
    | _ -> failwith "do_monop"

(* |do_binop| -- evaluate binary operators *)
let do_binop w x y =
  match w with
      Plus -> x + y
    | Minus -> x - y
    | Times -> x * y
    | Div -> x / y
    | Mod -> x mod y
    | Eq -> int_of_bool (x = y)
    | Lt -> int_of_bool (x < y)
    | Gt -> int_of_bool (x > y)
    | Leq -> int_of_bool (x <= y)
    | Geq -> int_of_bool (x >= y)
    | Neq -> int_of_bool (x <> y)
    | And -> if x <> 0 then y else 0
    | Or -> if x <> 0 then 1 else y
    | BitAnd -> x land y
    | BitOr -> x lor y
    | Lsl -> x lsl y
    | Lsr -> x lsr y
    | Asr -> x asr y
    | _ -> failwith "do_binop"

(* |negate| -- negation of a comparison *)
let negate = 
  function Eq -> Neq | Neq -> Eq | Lt  -> Geq
    | Leq -> Gt | Gt  -> Leq | Geq -> Lt
    | _ -> failwith "negate"


(* OPERATOR TREES *)

type optree = Node of inst * optree list

let rec canon_app t us =
  match t with
      <SEQ, @ts> -> List.fold_right canon_app ts us
    | <NOP> -> us
    | <LINE n> -> if n = 0 then us else <LINE n> :: set_line n us
    | _ -> effects t (result t :: us)

and set_line n ts =
  match ts with 
      [] -> []
    | <LINE m> :: us -> if n <> m then ts else us
    | u :: us -> u :: set_line n us

and effects t us =
  match t with
      <AFTER, t1, t2> -> canon_app t1 (effects t2 us)
    | <w, @ts> -> List.fold_right effects ts us

and result =
  function
      <AFTER, t1, t2> -> result t2
    | <w, @ts> -> <w, @(List.map result ts)>

let canonicalise t = canon_app t []

let flat =
  function
      <CALL n, @(fn::args)> -> 
        List.rev args @ [<CALL n, fn>]
    | <DEFTEMP k, <CALL n, @(fn::args)>> ->
        List.rev args @ [<DEFTEMP k, <CALL n, fn>>]
    | t -> [t]

let flatten ts = List.concat (List.map flat ts)

let rec fTree <x, @ts> = 
  fMeta "<$$>" [fInst x;
      if ts = [] then fStr "" else fMeta ", $" [fList(fTree) ts]]

let print_optree pfx t =
  match t with
      <LINE n> ->
        Print.printf "$$\n" [fStr pfx; fStr (Source.get_line n)]
    | _ ->
        fgrindf stdout pfx "$" [fTree t];
