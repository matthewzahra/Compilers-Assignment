(* lab4/dict.ml *)
(* Copyright (c) 2017 J. M. Spivey *)

open Print
open Mach
open Optree

(* IDENTIFIERS *)

(* Identifiers are represented by integers, with a hash table that maps
   strings to the corresponding integer, and a vector that maps the
   other way. *)

(* |ident| -- type of identifiers *)
type ident = int

let nids = ref 0
let idhash = Hashtbl.create 100
let idvec = Growvect.create 100

(* |intern| -- make identifier from string *)
let intern s =
  try Hashtbl.find idhash s with
    Not_found ->
      let x = !nids in
      incr nids;
      Hashtbl.add idhash s x;
      Growvect.append idvec s;
      x

(* |spelling| -- convert identifier to string *)
let spelling x = Growvect.get idvec x

(* |fId| -- format identifier for printing *)
let fId x = fStr (spelling x)

(* |libid| -- type of picoPascal library procedures *)
type libid = ChrFun | OrdFun | PrintNum | PrintChar | PrintString 
  | NewLine | ReadChar | ExitProc | NewProc | ArgcFun | ArgvProc
  | OpenIn | CloseIn | Operator of Optree.op | Len | NewRowProc

(* |lib_name| -- name of a library procedure *)
let lib_name x = 
  match x with
      PrintNum -> "print_num" | PrintChar -> "print_char" 
    | PrintString -> "print_string" | NewLine -> "newline"
    | ReadChar -> "read_char" | ChrFun -> "chr" | OrdFun -> "ord"
    | ExitProc -> "exit" | NewProc -> "new"
    | ArgcFun -> "argc" | ArgvProc -> "argv"
    | OpenIn -> "open_in" | CloseIn -> "close_in"
    | Operator op -> sprintf "$" [Optree.fOp op]
    | Len -> "len"
    | NewRowProc -> "newrow"

(* |fLibId| -- format library name for printing *)
let fLibId l = fStr (lib_name l)


(*
Environments are represented using O'Caml's library module that
implements mappings using balanced binary trees.

The top block is also kept separately as a list, to check for multiple
declarations, and so that it can be returned by the top_block function.
This is used for formal parameter lists and lists of fields in
record types.  The list is kept in reverse order internally, so that
an element can be added in constant time.
*)

(* |location| -- runtime locations *)
type location =
    Global of Optree.symbol     (* Global (label) *)
  | Local of int                (* Local (offset) *)
  | Register of int             (* Register *)
  | Nowhere                     (* Compile-time only *)

(* |def_kind| -- kinds of definition *)
type def_kind = 
    ConstDef of int             (* Constant (value) *)
  | StringDef                   (* String *)
  | TypeDef                     (* Type *)
  | VarDef                      (* Variable *)
  | CParamDef                   (* Value parameter *)
  | VParamDef                   (* Var parameter *)
  | FieldDef                    (* Field of record *)
  | ProcDef                     (* Procedure *)
  | PParamDef                   (* Proc parameter *)
  | LibDef of libproc           (* Lib proc (data) *)
  | HoleDef of ptype ref        (* Pending type *)
  | DummyDef                    (* Dummy *)
  | OpenArrayParamDef

(* |def| -- definitions in environment *)
and def = 
  { d_tag: ident;               (* Name *)
    d_kind: def_kind;           (* Kind of object *)
    d_type: ptype;              (* Type *)
    d_level: int;               (* Static level *)
    mutable d_mem: bool;        (* Whether addressible *)
    mutable d_addr: location }  (* Run-time location *)

(* |basic_type| -- basic built-in types *)
and basic_type = VoidType | IntType | CharType | BoolType | AddrType

(* |ptype| -- type of picoPascal types *)
and ptype = 
  { t_id: int;                  (* Unique identifier *)
    t_guts: type_guts;          (* Shape of the type *)
    t_rep: Mach.metrics }

(* |type_guts| -- internal structure of a type *)
and type_guts =
    BasicType of basic_type
  | ArrayType of int * ptype
  | OpenArrayType of ptype
  | RecordType of def list
  | ProcType of proc_data
  | PointerType of ptype ref

(* |proc_data| -- data about a procedure type *)
and proc_data =
  { p_fparams: def list;
    p_pcount: int;
    p_result: ptype }

(* |libproc| -- data about a library procedure *)
and libproc =
  { q_id: libid;
    q_nargs: int;
    q_argtypes: ptype list }

(* |IdMap| -- module for mappings from identifiers to another type *)
module IdMap = 
  Map.Make(struct 
    type t = ident  
    let compare = compare 
  end)

(* |enviroment| -- type of symbol tables *)
type environment = Env of (def list * def IdMap.t)

(* An environment tracks its top layer as a list, for use in making
formal parameter lists *)

(* |add_def| -- add definition to environment *)
let add_def d m = IdMap.add d.d_tag d m

(* |add_block| -- reinstate a previous top layer *)
let add_block b (Env (b0, m)) =
  Env (List.rev b, Util.accum add_def b m)

(* |top_block| -- fetch top layer as a list *)
let top_block (Env (b, m)) = List.rev b

(* |new_block| -- add empty top layer *)
let new_block (Env (b0, m)) = Env ([], m)

(* |find_def| -- find definition in a list or raise |Not_found| *)
let find_def x ds =
  let rec search =
    function
        [] -> raise Not_found
      | d::ds -> 
          if x = d.d_tag then d else search ds in
  search ds

(* |can| -- test if application returns without raising |Not_found| *)
let can f x = try f x; true with Not_found -> false

(* |define| -- add definition to environment, raise |Exit| on clash *)
let define d (Env (b, m)) = 
  if can (find_def d.d_tag) b then raise Exit;
  Env (d::b, add_def d m)

(* |replace| -- replace definiyion with an alternative *)
let replace d (Env (b, m)) =
  let rec repl =
    function
        [] -> failwith "replace"
      | d'::ds -> 
          if d.d_tag = d'.d_tag then d::ds else d' :: repl ds in
  Env (repl b, add_def d m)

(* |lookup| -- look up definition or raise |Not_found| *)
let lookup x (Env (b, m)) = IdMap.find x m

(* |empty| -- empty environment *)
let empty = Env ([], IdMap.empty)


(* TYPES *)

let n_types = ref 0

let mk_type t r =
  incr n_types;
  { t_id = !n_types; t_guts = t; t_rep = r }

let voidtype =  mk_type (BasicType VoidType) void_rep
let integer =   mk_type (BasicType IntType) int_rep
let character = mk_type (BasicType CharType) char_rep
let boolean =   mk_type (BasicType BoolType) bool_rep
let addrtype =  mk_type (BasicType AddrType) addr_rep

let row n t =
  let r = t.t_rep in 
  mk_type (ArrayType (n, t)) { r_size = n * r.r_size; r_align = r.r_align }

let openArrayRow t = 
  mk_type (OpenArrayType t) addr_rep

let discrete t =
  match t.t_guts with
      BasicType (IntType | CharType | BoolType) -> true
    | _ -> false

let scalar t =
  match t.t_guts with
      BasicType (IntType | CharType | BoolType) -> true
    | PointerType _ -> true
    | _ -> false

let is_pointer t =
  match t.t_guts with
      PointerType t1 -> true
    | _ -> false

let is_array t =
  match t.t_guts with
    ArrayType (n,t1) -> true
    | _ -> false

let is_openArray t =
  match t.t_guts with
    OpenArrayType t1 -> true
    | _ -> false

let bound t =
  match t.t_guts with
      ArrayType (n, t1) -> n
    | _ -> failwith "bound"

let base_type t =
  match t.t_guts with
      PointerType t1 -> !t1
    | ArrayType (n, t1) -> t1
    | OpenArrayType t1 -> t1
    | _ -> failwith "base_type"

let rec get_final_pointer t = 
  match t.t_guts with 
      PointerType t1 -> get_final_pointer !t1
    | x -> x

let get_proc t =
  match t.t_guts with
      ProcType p -> p
    | _ -> failwith "get_proc"

let rec same_type t1 t2 = 
  match (t1.t_guts, t2.t_guts) with
      (ProcType p1, ProcType p2) ->
        match_args p1.p_fparams p2.p_fparams 
        && same_type p1.p_result p2.p_result
    | (ArrayType (n1, u1), ArrayType(n2, u2)) ->
        n1 = n2 && same_type u1 u2
    | (OpenArrayType t1, OpenArrayType t2) -> same_type t1 t2
    | (OpenArrayType t1, ArrayType(n2, t2)) -> same_type t1 t2
    | (ArrayType(n1, t1), OpenArrayType t2) ->  same_type t1 t2
    | (OpenArrayType _, PointerType p) -> same_type t1 !p
    | (ArrayType(_,_), PointerType p) -> same_type t1 !p
    | (PointerType p, OpenArrayType _) -> same_type !p t2
    | (PointerType p, ArrayType(_,_)) -> same_type !p t2
    | (PointerType _, BasicType x) -> x = AddrType
    | (BasicType x, PointerType _) -> x = AddrType
    | (PointerType p, PointerType q) -> same_type !p !q
    | (_, _) -> t1.t_id = t2.t_id

and match_args fp1 fp2 = 
  match (fp1, fp2) with
      ([], []) -> true
    | (f1::fp1', f2::fp2') ->
        f1.d_kind = f2.d_kind && same_type f1.d_type f2.d_type
          && match_args fp1' fp2'
    | _ -> false

let is_string t =
  match t.t_guts with
      ArrayType (n, t1) -> same_type t1 character
    | OpenArrayType t1 -> same_type t1 character
    | _ -> false

let symbol_of d =
  match d.d_addr with
      Global x -> x
    | _ -> failwith "symbol_of"

let offset_of d =
  match d.d_addr with
      Local o -> o
    | _ -> failwith "offset_of"
