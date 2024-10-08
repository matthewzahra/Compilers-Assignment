(* lab4/tgen.ml *)
(* Copyright (c) 2017 J. M. Spivey *)

open Dict
open Tree
open Mach
open Optree
open Lexer
open Print

let boundchk = ref false
let optlevel = ref 0
let debug = ref 0


(* |level| -- nesting level of current procedure *)
let level = ref 0

(* |retlab| -- label to return from current procedure *)
let retlab = ref nolab

(* |size_of| -- calculate size of type *)
let size_of t = t.t_rep.r_size

(* |get_value| -- get constant value or fail *)
let get_value e =
  match e.e_value with
      Some v -> v
    | None -> failwith "get_value"

(* |schain| -- code to follow N links of static chain *)
let rec schain n =
  if n = 0 then
    <LOCAL 0>
  else
    <LOADW, <OFFSET, schain (n-1), <CONST stat_link>>>

(* |address| -- code to push address of an object *)
let address d =
  match d.d_addr with
      Global x ->
        <GLOBAL x>
    | Local off -> 
        <OFFSET, schain (!level - d.d_level), <CONST off>>
    | Register i ->
        <REGVAR i>
    | Nowhere -> 
        failwith (sprintf "address $" [fId d.d_tag])

(* 3 cases - normal array (size in def), openarray parameter (size at +4) or pointer to open array (size at -4)*)
let size_of_array e1 = 
    let deepestExpr = deref_chain e1 in
    match e1.e_type.t_guts with
        OpenArrayType t -> 
                          begin
                                match deepestExpr.e_guts with
                                    Variable x ->
                                                  begin
                                                        match (get_def x).d_kind with
                                                            OpenArrayParamDef -> <LOADW, <OFFSET, address (get_def x), <CONST 4>>>
                                                          | _ -> <LOADW, <OFFSET, <LOADW, address (get_def x)>, <CONST (-4)>>>
                                                  end
                                  | _ -> failwith "can't take length of non-variable"
                          end
            
      | ArrayType(n,t) -> <CONST n> 
      | _ -> failwith "not a valid array"


(* |gen_closure| -- two trees for a (code, envt) pair *)
let gen_closure d =
  match d.d_kind with
      ProcDef ->
        (<GLOBAL (symbol_of d)>,
          if d.d_level = 0 then <CONST 0> else schain (!level - d.d_level))
    | PParamDef | OpenArrayParamDef->
        (<LOADW, address d>,
          <LOADW, <OFFSET, address d, <CONST addr_rep.r_size>>>)
    | _ -> failwith "missing closure"

let rec numargs i =
  function
      [] -> []
    | (x::xs) -> <ARG i, x> :: numargs (i+1) xs

(* |libcall| -- code for library call *)
let libcall sym args rtype =
  let n = List.length args in
  <CALL n, @(<GLOBAL sym> :: numargs 0 args)>

(* |gen_copy| -- generate code to copy a fixed-size chunk *)
let gen_copy dst src n =
  libcall "memcpy" [dst; src; <CONST n>] voidtype

let rec gen_addr_bound a = 
    let temp = Regs.new_temp() in
    match get_final_pointer a.e_type with
        ArrayType(n,t) -> 
                          let deepestExpr = deref_chain a in 
                          begin
                                match deepestExpr.e_guts with
                                    Variable x -> 
                                                  let baseAddrTree = <AFTER, <DEFTEMP temp, gen_addr deepestExpr>, <TEMP temp>> in
                                                  let boundTree = <CONST n> in
                                                  (baseAddrTree,boundTree)
                                  | _ -> failwith "non-variable is not suitable for bounds checking!" 
                          end

      | OpenArrayType t -> 
                          let deepestExpr = deref_chain a in (*need to determine if it is an open array parameter or if it is the dereference value of some pointer (or chain of pointers)*)
                          begin
                              match deepestExpr.e_guts with
                                  Variable x -> let d = get_def x in
                                                begin
                                                      match d.d_kind with
                                                          OpenArrayParamDef -> 
                                                                              let addr = address d in
                                                                              let baseAddrTree = <AFTER, <DEFTEMP temp, addr>, <LOADW, <TEMP temp>>> in
                                                                              let boundTree = <LOADW, <OFFSET, <TEMP temp>, <CONST 4>>>
                                                                              in (baseAddrTree,boundTree)
                                                        |_ -> (* pointer to open array *)
                                                                              let addr = address d in
                                                                              let baseAddrTree = <AFTER, <DEFTEMP temp, <LOADW, addr>>, <TEMP temp>> in
                                                                              let boundTree = <LOADW, <OFFSET, <TEMP temp>, <CONST (-4)>>>
                                                                              in (baseAddrTree,boundTree)
                                                end
                                | _ -> failwith "non-variable is not suitable for bounds checking"
                          end


(* |gen_addr| -- code for the address of a variable *)
and gen_addr v = 
  match v.e_guts with
      Variable x ->
        let d = get_def x in
        begin
          match d.d_kind with
              VarDef ->
                address d
            | VParamDef ->
                <LOADW, address d>
            | CParamDef ->
                if scalar d.d_type || is_pointer d.d_type then 
                  address d
                else
                  <LOADW, address d>
            | StringDef ->
                address d
            | OpenArrayParamDef ->
              <LOADW, address d>
            | _ -> 
                failwith "load_addr"
        end
    | Sub (a, i) ->
        begin
            match a.e_type.t_guts with
                ArrayType(n,t) ->     if not !boundchk then 
                                          <OFFSET, gen_addr a, <BINOP Times, gen_expr i, <CONST (size_of v.e_type)>>>
                                      else 
                                          let (baseAddrTree,boundTree) = gen_addr_bound a in
                                          <OFFSET, baseAddrTree, <BINOP Times, <BOUND, gen_expr i, boundTree>, <CONST (size_of v.e_type)>>>
              | OpenArrayType t ->  
                                      if not !boundchk then 
                                          <OFFSET, gen_addr a, <BINOP Times, gen_expr i, <CONST (size_of v.e_type)>>>
                                      else
                                          let (baseAddrTree,boundTree) = gen_addr_bound a in
                                           <OFFSET, baseAddrTree, <BINOP Times, <BOUND, gen_expr i, boundTree>, <CONST (size_of v.e_type)>>>
        end 
    | Select (r, x) ->
        let d = get_def x in
        <OFFSET, gen_addr r, <CONST (offset_of d)>>
    | Deref p ->
        let null_check t =
          if not !boundchk then t else <NCHECK, t> in
        null_check (gen_expr (deref_chain p))
    | String (lab, n) -> <GLOBAL lab>
    | _ -> failwith "gen_addr!"

(* |gen_expr| -- tree for the value of an expression *)
and gen_expr e =
  match e.e_value with
      Some v -> 
        <CONST v>
    | None -> 
        begin
          match e.e_guts with
              Variable _ | Sub _ | Select _ | Deref _ | Slice _->
                let ld = if size_of e.e_type = 1 then LOADC else LOADW in
                <ld, gen_addr e>
            | Monop (w, e1) ->
                <MONOP w, gen_expr e1>
            | Binop (Div, e1, e2) ->
                libcall "int_div" [gen_expr e1; gen_expr e2] integer
            | Binop (Mod, e1, e2) ->
                libcall "int_mod" [gen_expr e1; gen_expr e2] integer
            | Binop (w, e1, e2) ->
                <BINOP w, gen_expr e1, gen_expr e2>
            | FuncCall (p, args) -> 
                gen_call p args
            | _ -> failwith "gen_expr"
        end

(* |gen_call| -- generate code to call a procedure *)
and gen_call x args =
  let d = get_def x in
  match d.d_kind with
      LibDef q ->
        gen_libcall q args
    | _ ->
        let p = get_proc d.d_type in
        let (fn, sl) = gen_closure d in
        let args = List.concat (List.map2 gen_arg p.p_fparams args) in
        <CALL p.p_pcount, @(fn :: <STATLINK, sl> :: numargs 0 args)>

(* |gen_arg| -- generate code for a procedure argument *)
and gen_arg f a = 
  match f.d_kind with
      CParamDef ->
        if scalar f.d_type || is_pointer f.d_type then 
          [gen_expr a]
        else 
          [gen_addr a]
    | VParamDef ->
        [gen_addr a]
    | PParamDef ->
        begin
          match a.e_guts with 
              Variable x -> 
                let (fn, sl) = gen_closure (get_def x) in [fn; sl]
            | _ -> 
                failwith "bad funarg"
        end
    | OpenArrayParamDef ->
      begin
        match a.e_guts with
          Variable x ->
            let def = get_def x in
            let kind = def.d_kind in
            begin 
            
              match kind with
                OpenArrayParamDef -> let (addr, length) = gen_closure def in [addr;length] (*Only these have *)

                | _ -> 
                
                    begin
                          match a.e_type.t_guts with (*if it is not an open array parameter, can only be a normal array or a pointer to an (open) array*)
                              ArrayType(n,t) -> [gen_addr a; <CONST n>]
                            | PointerType p -> 
                                                let (baseAddrTree,boundTree) = gen_addr_bound (deref_chain a) in
                                                [baseAddrTree; boundTree]
                            | _ -> failwith "illegal argument"
                    end         
            end
          | Deref e -> gen_arg f (deref_chain e)
          | Slice(ar,lb,ub) ->
              begin
                    match ar.e_type.t_guts with
                        ArrayType _ | OpenArrayType _ -> 
                                              let lbTree = gen_expr lb in
                                              let ubTree = gen_expr ub in
                                              let (baseAddrTree,boundTree) = gen_addr_bound ar in (* to avoid repeating side effects *)

                                              if (lb.e_value != None && get_value lb == 0) then (* trivial case *)
                                                  let arrLength = <SLICE, lbTree, ubTree, boundTree> in
                                                  [baseAddrTree; arrLength]
                                              else
                                                  let baseAddr = <OFFSET, baseAddrTree, <BINOP Times, lbTree, <CONST (size_of(base_type ar.e_type))>>> in
                                                  let arrLength = <SLICE, lbTree, ubTree, boundTree> in 
                                                  [baseAddr; arrLength]
                    | _ -> failwith "not an array"
              end



          | _ -> failwith "bad"
      end
    | _ -> failwith "bad arg!"

(* |gen_libcall| -- generate code to call a built-in procedure *)
and gen_libcall q args =
  match (q.q_id, args) with
      (ChrFun, [e]) -> gen_expr e
    | (OrdFun, [e]) -> gen_expr e
    | (PrintString, [e]) ->
        begin
            match e.e_type.t_guts with
                ArrayType(n,t) ->  libcall "print_string" [gen_addr e; <CONST (bound e.e_type)>] voidtype
          
              | OpenArrayType t -> libcall "print_string" [gen_addr e; size_of_array e] voidtype
        end

    | (ReadChar, [e]) ->
        libcall "read_char" [gen_addr e] voidtype
    | (NewProc, [e]) ->
        let size = size_of (base_type e.e_type) in
        <STOREW, libcall "palloc" [<CONST size>] addrtype, gen_addr e>
    | (NewRowProc, [e1;arrLength]) ->
        let e1 = deref_chain e1 in 
        let elemSize = size_of (base_type e1.e_type) in
        <STOREW, libcall "palloc2" [<CONST (get_value arrLength)>; <CONST elemSize>] addrtype, gen_addr e1>
    | (ArgcFun, []) ->
        libcall "argc" [] integer
    | (ArgvProc, [e1; e2]) ->
        libcall "argv"
          [gen_expr e1; gen_addr e2; <CONST (bound e2.e_type)>] voidtype
    | (OpenIn, [e]) ->
        libcall "open_in"
          [gen_addr e; <CONST (bound e.e_type)>] voidtype
    | (Operator op, [e1]) ->
        <MONOP op, gen_expr e1>
    | (Operator op, [e1; e2]) ->
        <BINOP op, gen_expr e1, gen_expr e2>
    | (Len, [e]) ->     (*different cases for normal array, open array parameter and pointer to (open) array*)
        size_of_array e
        
    | (_, _) ->
        let proc = sprintf "$" [fLibId q.q_id] in
        libcall proc (List.map gen_expr args) voidtype

(* |gen_cond| -- generate code to branch on a condition *)
let rec gen_cond test tlab flab =
  match test.e_value with
      Some v ->
        if v <> 0 then <JUMP tlab> else <JUMP flab>
    | None ->
        begin match test.e_guts with
            Monop (Not, e) ->
              gen_cond e flab tlab
          | Binop (Or, e1, e2) ->
              let l1 = label () in
              <SEQ,
                gen_cond e1 tlab l1,
                <LABEL l1>,
                gen_cond e2 tlab flab>
          | Binop (And, e1, e2) ->
              let l1 = label () in
              <SEQ,
                gen_cond e1 l1 flab,
                <LABEL l1>,
                gen_cond e2 tlab flab>
          | Binop ((Eq | Neq | Lt | Leq | Gt | Geq) as w, e1, e2) ->
              <SEQ,
                <JUMPC (w, tlab), gen_expr e1, gen_expr e2>,
                <JUMP flab>>
          | _ ->
              <SEQ,
                <JUMPC (Neq, tlab), gen_expr test, <CONST 0>>,
                <JUMP flab>>
        end

(* |gen_jtable| -- lay out jump table for case statement *)
let gen_jtable sel table0 deflab =
  if table0 = [] then
    <JUMP deflab>
  else begin
    let table = List.sort (fun (v1, l1) (v2, l2) -> compare v1 v2) table0 in
    let lobound = fst (List.hd table) in
    let rec tab u qs =
      match qs with
          [] -> []
        | (v, l) :: rs -> 
            if u = v then l :: tab (v+1) rs else deflab :: tab (u+1) qs in
    <JCASE (tab lobound table, deflab),
      <BINOP Minus, sel, <CONST lobound>>>
  end

(* |gen_stmt| -- generate code for a statement *)
let rec gen_stmt s = 
  let code =
    match s.s_guts with
        Skip -> <NOP>

      | Seq ss -> <SEQ, @(List.map gen_stmt ss)>

      | Assign (v, e) ->
          if scalar v.e_type || is_pointer v.e_type then begin
            let st = if size_of v.e_type = 1 then STOREC else STOREW in
            <st, gen_expr e, gen_addr v>
          end else begin
            gen_copy (gen_addr v) (gen_addr e) (size_of v.e_type)
          end

      | ProcCall (p, args) ->
          gen_call p args

      | Return res ->
          begin
            match res with
                Some e -> <SEQ, <RESULTW, gen_expr e>, <JUMP !retlab>>
              | None -> <JUMP !retlab>
          end

      | IfStmt (test, thenpt, elsept) ->
          let l1 = label () and l2 = label () and l3 = label() in
          <SEQ,
            gen_cond test l1 l2,
            <LABEL l1>,
            gen_stmt thenpt,
            <JUMP l3>,
            <LABEL l2>,
            gen_stmt elsept,
            <LABEL l3>>

      | WhileStmt (test, body) ->
          (* The test is at the top, improving the chances of finding
             common subexpressions between the test and loop body. *)
          let l1 = label () and l2 = label () and l3 = label() in
          <SEQ,
            <LABEL l1>,
            gen_cond test l2 l3,
            <LABEL l2>,
            gen_stmt body,
            <JUMP l1>,
            <LABEL l3>>

      | RepeatStmt (body, test) ->
          let l1 = label () and l2 = label () in
          <SEQ,
            <LABEL l1>,
            gen_stmt body, 
            gen_cond test l2 l1,
            <LABEL l2>>

      | ForStmt (var, lo, hi, body, upb) ->
          (* Use previously allocated temp variable to store upper bound.
             We could avoid this if the upper bound is constant. *)
          let tmp = match !upb with Some d -> d | _ -> failwith "for" in
          let l1 = label () and l2 = label () in
          <SEQ,
            <STOREW, gen_expr lo, gen_addr var>,
            <STOREW, gen_expr hi, address tmp>,
            <LABEL l1>,
            <JUMPC (Gt, l2), gen_expr var, <LOADW, address tmp>>,
            gen_stmt body,
            <STOREW, <BINOP Plus, gen_expr var, <CONST 1>>, gen_addr var>,
            <JUMP l1>,
            <LABEL l2>>

      | CaseStmt (sel, arms, deflt) ->
          (* Use one jump table, and hope it is reasonably compact *)
          let deflab = label () and donelab = label () in
          let labs = List.map (function x -> label ()) arms in
          let get_val (v, body) = get_value v in
          let table = List.combine (List.map get_val arms) labs in
          let gen_case lab (v, body) =
            <SEQ,
              <LABEL lab>,
              gen_stmt body,
              <JUMP donelab>> in
          <SEQ,
            gen_jtable (gen_expr sel) table deflab,
            <SEQ, @(List.map2 gen_case labs arms)>,
            <LABEL deflab>,
            gen_stmt deflt,
            <LABEL donelab>> in

   (* Label the code with a line number *)
   <SEQ, <LINE s.s_line>, code>

(* unnest -- move procedure calls to top level *)
let unnest code =
  let rec do_tree =
    function
        <CALL n, @args> ->
          let t = Regs.new_temp () in
          <AFTER, 
            <DEFTEMP t, <CALL n, @(List.map do_tree args)>>, 
            <TEMP t>>
      | <w, @args> ->
          <w, @(List.map do_tree args)> in
  let do_root =
    function <op, @args> -> <op, @(List.map do_tree args)> in
  Optree.canonicalise <SEQ, @(List.map do_root code)>

(* |count_refs| -- count references to temps *)
let rec count_refs =
  function
      <TEMP n> -> Regs.inc_temp n
    | <op, @args> -> List.iter count_refs args

let show label code =
  if !debug > 0 then begin
    printf "$$:\n" [fStr Mach.comment; fStr label];
    List.iter (Optree.print_optree Mach.comment) code;
    printf "\n" []
  end;
  code

(* |do_proc| -- generate code for a procedure and pass to the back end *)
let do_proc lab lev nargs (Block (_, body, fsize, nregv)) =
  try
    level := lev+1;
    retlab := label ();
    Regs.init ();
    let code0 = 
      show "Initial code"
        (Optree.canonicalise <SEQ, gen_stmt body, <LABEL !retlab>>) in
    let code1 =
      if !optlevel < 1 then code0 else
        show "After simplification" (Jumpopt.optimise (Simp.optimise code0)) in
    let code2 =
      if !optlevel < 2 then 
        show "After unnesting" (unnest code1) 
      else
        show "After sharing" (Share.traverse code1) in

    List.iter count_refs code2;
    Target.start_proc lab nargs !fsize;
    Regs.get_regvars !nregv;
    Tran.translate (flatten code2);
    Target.end_proc ()
  with
    Failure msg ->
      let bt = Printexc.get_raw_backtrace () in
      Printexc.raise_with_backtrace
        (Failure (sprintf "$ in procedure $" [fStr msg; fStr lab])) bt

let get_decls (Block (decls, _, _, _)) = decls

(* |gen_proc| -- translate a procedure, ignore other declarations *)
let rec gen_proc = 
  function
      ProcDecl (Heading (x, _, _), block) ->
        let d = get_def x in
        let p = get_proc d.d_type in
        let line = Source.get_line x.x_line in
        printf "$$\n" [fStr Mach.comment; fStr line];
        do_proc (symbol_of d) d.d_level p.p_pcount block;
        gen_procs block;
    | _ -> ()

(* |gen_procs| -- generate code for the procedures in a block *)
and gen_procs block =
  List.iter gen_proc (get_decls block)

(* |gen_global| -- generate declaration for global variable *)
let gen_global d =
  match d.d_kind with
      VarDef ->
        Target.emit_global (symbol_of d) (size_of d.d_type)
    | _ -> ()

(* |translate| -- generate code for the whole program *)
let translate (Prog (block, glodefs)) =
  Target.preamble ();
  gen_procs block;
  do_proc "pmain" 0 0 block;
  List.iter gen_global !glodefs;
  List.iter (fun (lab, s) -> Target.emit_string lab s) (string_table ());
  Target.postamble ()

