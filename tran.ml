(* lab4/tran.ml *)
(* Copyright (c) 2017 J. M. Spivey *)

open Optree
open Target
open Regs
open Print

let debug = ref 0

(* |release| -- release all registers used by a fragment *)
let release frag = List.iter release_reg (get_regs frag)

(* |gen| -- emit an instruction with register allocation *)
let gen fmt rands =
  List.iter release rands;
  emit_inst fmt R_none rands

(* |gen_reg| -- emit instruction with result in a register *)
let gen_reg fmt r rands =
  (* Assume the instruction does not write its output register
     before it has finished reading its inputs *)
  (* First release the input registers *)
  List.iter release rands;
  (* Then allocate the output register, so it can overlap an input *)
  let r' = get_reg r in
  emit_inst fmt r' rands;
  register r'

(* |gen_move| -- move value to specific register *)
let gen_move dst src =
  if dst = R_any || dst = R_temp || dst = src then
    register src
  else
    gen_reg "mov" dst [register src]


(* Tests for fitting in various immediate fields *)

(* |fits_offset| -- test for fitting in offset field of address *)
let fits_offset x = (-4096 < x && x < 4096)

(* |fits_immed| -- test for fitting in immediate field *)
let fits_immed x =
  (* A conservative approximation, using shifts instead of rotates *)
  let rec reduce r =
    if r land 3 <> 0 then r else reduce (r lsr 2) in
  x = 0 || x > 0 && reduce x < 256

(* |fits_move| -- test for fitting in immediate move *)
let fits_move x = fits_immed x || fits_immed (lnot x)

(* |fits_add| -- test for fitting in immediate add *)
let fits_add x = fits_immed x || fits_immed (-x)

(* |line| -- current line number *)
let line = ref 0

(* The main part of the code generator consists of a family of functions
   eval_X t, each generating code for a tree t, leaving the value in
   a register, or as an operand for another instruction, etc. *)

(* |eval_reg| -- evaluate expression with result in specified register *)
let rec eval_reg t r =
  (* Binary operation *)
  let binary op t1 t2 =
    let v1 = eval_reg t1 R_any in
    let v2 = eval_rand t2 in
    gen_reg op r [v1; v2]

  (* Shifts *)
  and shift op t1 t2 =
    let v1 = eval_reg t1 R_any in
    let v2 = eval_shift t2 in
    gen_reg op r [v1; v2]

  (* Unary operation *)
  and unary op t1 =
    let v1 = eval_reg t1 R_any in
    gen_reg op r [v1]

  (* Comparison with boolean result *)
  and compare op t1 t2 =
    let v1 = eval_reg t1 R_any in
    let v2 = eval_rand t2 in
    gen_reg "cmp $1, $2 / mov $0, #0 / mov$3 $0, #1"
      r [v1; v2; quote op] in

  match t with
      <CONST k> when fits_move k -> 
        gen_reg "mov $0, #$1" r [number k]
    | <CONST k> ->
        gen_reg "ldr $0, =$1" r [number k]
    | <LOCAL 0> ->
        gen_move r R_fp
    | <LOCAL n> when fits_add n ->
        gen_reg "add $0, fp, #$1" r [number n]
    | <LOCAL n> ->
        gen_reg "ldr ip, =$1 / add $0, fp, ip" r [number n]
    | <GLOBAL x> ->
        gen_reg "ldr $0, =$1" r [symbol x]
    | <TEMP n> ->
        gen_move r (Regs.use_temp n)
    | <(LOADW|LOADC), <REGVAR i>> ->
        let rv = List.nth stable i in
        reserve_reg rv; gen_move r rv
    | <LOADW, t1> -> 
        let v1 = eval_addr t1 in
        gen_reg "ldr" r [v1]
    | <LOADC, t1> -> 
        let v1 = eval_addr t1 in
        gen_reg "ldrb" r [v1]

    | <MONOP Uminus, t1> -> unary "neg" t1
    | <MONOP Not, t1> -> 
        let v1 = eval_reg t1 R_any in
        gen_reg "eor $0, $1, #1" r [v1]
    | <MONOP BitNot, t1> -> unary "mvn" t1

    | <OFFSET, t1, <CONST k>> when fits_add k ->
        (* Allow add for negative constants *)
        let v1 = eval_reg t1 R_any in
        gen_reg "add $0, $1, #$2" r [v1; number k]
    | <OFFSET, t1, t2> -> binary "add" t1 t2

    | <BINOP Plus, t1, t2> -> binary "add" t1 t2
    | <BINOP Minus, t1, t2> -> binary "sub" t1 t2
    | <BINOP And, t1, t2> -> binary "and" t1 t2
    | <BINOP Or, t1, t2> -> binary "orr" t1 t2
    | <BINOP Lsl, t1, t2> -> shift "lsl" t1 t2
    | <BINOP Lsr, t1, t2> -> shift "lsr" t1 t2
    | <BINOP Asr, t1, t2> -> shift "asr" t1 t2
    | <BINOP BitAnd, t1, t2> -> binary "and" t1 t2
    | <BINOP BitOr, t1, t2> -> binary "orr" t1 t2

    | <BINOP Times, t1, t2> ->
        (* The mul instruction needs both operands in registers *)
        let v1 = eval_reg t1 R_any in
        let v2 = eval_reg t2 R_any in
        gen_reg "mul" r [v1; v2]

    | <BINOP Eq, t1, t2> -> compare "eq" t1 t2
    | <BINOP Neq, t1, t2> -> compare "ne" t1 t2
    | <BINOP Gt, t1, t2> -> compare "gt" t1 t2
    | <BINOP Geq, t1, t2> -> compare "ge" t1 t2
    | <BINOP Lt, t1, t2> -> compare "lt" t1 t2
    | <BINOP Leq, t1, t2> -> compare "le" t1 t2

    | <BOUND, t1, t2> ->
        let v1 = eval_reg t1 r in
        let v2 = eval_rand t2 in
        gen_reg "cmp $1, $2 / ldrhs r0, =$3 / blhs check"
          (reg_of v1) [v1; v2; number !line]

    | <SLICE, i, j, n> ->
        let v1 = eval_reg i r in
        let v2 = eval_reg j R_any in
        let v3 = eval_reg n R_any in
        gen_reg "cmp $1, #0 / ldrmi r0, =$4 / blmi slicecheck / sub $1, $2, $1 / cmp $1, #0 / ldrmi r0, =$4 / blmi slicecheck / cmp $3, $2 / ldrmi r0, =$4 / blmi slicecheck"
          (reg_of v1) [v1; v2; v3; number !line]

    | <NCHECK, t1> ->
        let v1 = eval_reg t1 r in
        gen_reg "cmp $1, #0 / ldreq r0, =$2 / bleq nullcheck"
          (reg_of v1) [v1; number !line]

    | <w, @args> ->
        failwith (sprintf "eval $" [fInst w])

(* |eval_rand| -- evaluate to form second operand *)
and eval_rand t =
  match t with
      <CONST k> when fits_immed k -> frag "#$1" [number k]
    | _ -> eval_reg t R_any

(* |eval_shift| -- evaluate to form shift amount *)
and eval_shift =
  function
      <CONST k> when k >= 1 && k < 32 -> frag "#$1" [number k]
    | t -> eval_reg t R_any

(* |eval_addr| -- evaluate to form an address for ldr or str *)
and eval_addr =
  function
      <LOCAL n> when fits_offset n ->
        frag "[fp, #$1]" [number n]
    | <OFFSET, t1, <CONST k>> when fits_offset k ->
        let v1 = eval_reg t1 R_any in
        frag "[$1, #$2]" [v1; number k]
    | t ->
        let v1 = eval_reg t R_any in
        frag "[$1]" [v1]

(* |exec_call| -- execute procedure call *)
let exec_call =
  function
      <GLOBAL f> -> 
        gen "bl" [symbol f]
    | t -> 
        let v1 = eval_reg t R_any in
        gen "blx" [v1]

(* |exec_stmt| -- generate code to execute a statement *)
let exec_stmt t =
  (* Conditional jump *)
  let condj op lab t1 t2 =
    let v1 = eval_reg t1 R_any in
    let v2 = eval_rand t2 in
    gen "cmp $1, $2 / b$3 $4" [v1; v2; quote op; codelab lab] in

  (* Procedure call *)
  let call t =
    spill_temps volatile;           (* Spill any remaining temps *)
    exec_call t;              (* Call the function *)
    List.iter (function r ->  (* Release argument registers *)
      if not (is_free r) then release_reg r) volatile in

  match t with
      <CALL k, t1> -> 
        call t1

    | <DEFTEMP n, <CALL k, t1>> ->
        call t1;
        reserve_reg (R 0); 
        Regs.def_temp n (R 0)

    | <DEFTEMP n, t1> ->
        let v1 = eval_reg t1 R_temp in
        Regs.def_temp n (reg_of v1)

    | <(STOREW|STOREC), t1, <REGVAR i>> ->
        let rv = List.nth stable i in
        spill_temps [rv];
        release (eval_reg t1 rv)
    | <STOREW, t1, t2> -> 
        let v1 = eval_reg t1 R_any in
        let v2 = eval_addr t2 in
        gen "str" [v1; v2]
    | <STOREC, t1, t2> -> 
        let v1 = eval_reg t1 R_any in
        let v2 = eval_addr t2 in
        gen "strb" [v1; v2]

    | <RESULTW, t1> ->
        release (eval_reg t1 (R 0))

    | <LABEL lab> -> emit_lab lab

    | <JUMP lab> -> gen "b" [codelab lab]

    | <JUMPC (Eq, lab), t1, t2> -> condj "eq" lab t1 t2
    | <JUMPC (Lt, lab), t1, t2> -> condj "lt" lab t1 t2
    | <JUMPC (Gt, lab), t1, t2> -> condj "gt" lab t1 t2
    | <JUMPC (Leq, lab), t1, t2> -> condj "le" lab t1 t2
    | <JUMPC (Geq, lab), t1, t2> -> condj "ge" lab t1 t2
    | <JUMPC (Neq, lab), t1, t2> -> condj "ne" lab t1 t2

    | <JCASE (table, deflab), t1> ->
        (* This jump table code exploits the fact that on ARM,
           reading the pc gives a value 8 bytes beyond the
           current instruction, so in the ldrlo instruction
           below, pc points to the branch table. *)
        let v1 = eval_reg t1 R_any in
        gen "cmp $1, #$2 / ldrlo pc, [pc, $1, LSL #2] / b $3"
          [v1; number (List.length table); codelab deflab];
        List.iter (fun lab -> gen ".word $1" [codelab lab]) table

    | <ARG i, <TEMP k>> when i < 4 ->
        (* Avoid annoying spill and reload if the value is a temp
           already in the correct register: e.g. in f(g(x)). *)
        let r = R i in
        let r1 = Regs.use_temp k in
        spill_temps [r];
        ignore (gen_move r r1)
    | <ARG i, t1> when i < 4 ->
        spill_temps [R i];
        ignore (eval_reg t1 (R i))
    | <ARG i, t1> when i >= 4 ->
        need_stack (4*i-12);
        let v1 = eval_reg t1 R_any in
        gen "str $1, [sp, #$2]" [v1; number (4*i-16)]

    | <STATLINK, t1> ->
        let r = R 10 in
        spill_temps [r];
        ignore (eval_reg t1 r)

    | <w, @ts> -> 
        failwith (sprintf "exec_stmt $" [fInst w])

(* |process| -- generate code for a statement, or note a line number *)
let process =
  function
      <LINE n> ->
        if !line <> n then
          emit_comment (String.trim (Source.get_line n));
        line := n
    | t ->
        if !debug > 0 then emit_tree t;
        exec_stmt t;
        if !debug > 1 then emit_comment (Regs.dump_regs ())

(* |translate| -- translate a procedure body *)
let translate code =
  (try List.iter process code with exc -> 
    (* Code generation failed, but let's see how far we got *)
    Target.flush_proc (); raise exc)
