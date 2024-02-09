(* lab4/target.ml *)
(* Copyright (c) 2017 J. M. Spivey *)

open Optree
open Print

(* |reg| -- type of ARM registers *)
type reg = R of int | R_fp | R_any | R_temp | R_none

let reg_name =
  function
      R n -> sprintf "r$" [fNum n]
    | R_fp -> "fp"
    | R_any -> "*ANYREG*" 
    | R_temp -> "*TEMPREG*"
    | R_none -> "*NOREG*"

(* |fReg| -- format register for printing *)
let fReg r = fStr (reg_name r)

(* ARM register assignments:

   R0-3   arguments + scratch
   R4-R9  callee-save temps
   R10    static link
   R11=fp frame pointer
   R12=sp stack pointer
   R13=ip temp for linkage
   R14=lr link register
   R15=pc program counter 

*)

let volatile = [R 0; R 1; R 2; R 3; R 10]
let stable = [R 4; R 5; R 6; R 7; R 8; R 9]


(* CODE FRAGMENTS *)

(* In this simple compiler, it's enough to reduce fragments to strings
   immediately, without waiting for offsets to be adjusted. *)

type fragment = string * reg list

let get_regs (_, regs) = regs

let fFrag (arg, _) = fStr arg

let reg_of v =
  match get_regs v with
      [r] -> r
    | _ -> failwith "reg_of"

let register r = (reg_name r, [r])
let number n = (string_of_int n, [])
let symbol x = (x, [])
let codelab lab = (sprintf "$" [fLab lab], [])
let quote s = (s, [])

let fTemplate tmpl r args =
  fExt (fun prf ->
    let i = ref 0 in
    while !i < String.length tmpl do
      let c = tmpl.[!i] in incr i;
      if c <> '$' then
        prf "$" [fChr c]
      else begin
        try
          let x = tmpl.[!i] in incr i;
          begin match x with
              '0' -> prf "$" [fReg r]
            | '1'..'9' ->
                let i = Char.code x - Char.code '1' in
                prf "$" [fFrag (List.nth args i)]
            | _ -> raise Not_found
          end
        with
          _ -> failwith (sprintf "fTemplate $ $ [$]"
                  [fStr tmpl; fReg r; fList(fFrag) args])
      end
    done)

let frag fmt rands =
  ( sprintf "$" [fTemplate fmt R_none rands],
    List.concat_map get_regs rands )


(* CODE OUTPUT *)

(* |seg| -- type of assembler segments *)
type seg = Text | Data | Unknown

(* |current_seg| -- current output segment *)
let current_seg = ref Unknown

(* |segment| -- emit segment directive if needed *)
let segment s =
  if !current_seg <> s then begin
    let seg_name = 
      match s with 
        Text -> ".text" | Data -> ".data" | Unknown -> "*unknown*" in
    printf "\t$\n" [fStr seg_name];
    current_seg := s
  end

(* |preamble| -- emit start of assembler file *)
let preamble () =
  printf "@ picoPascal compiler output\n" [];
  printf "\t.global pmain\n\n" []

type item = 
    Instr of string
  | Label of codelab
  | Comment of string
  | Tree of Optree.optree

let code = Queue.create ()
let icount = ref 0

let frame = ref 0
let stack = ref 0

let instr text =
  incr icount;
  Queue.add (Instr text) code

(* |emit_inst| -- emit an assembly language instruction *)
let emit_inst fmt r rands =
  if String.index_opt fmt '$' <> None then begin
    (* An explicit template maybe with several instructions split by / *)
    let parts = String.split_on_char '/' fmt in
    List.iter (fun x ->
      let y = String.trim x in
      instr (sprintf "$" [fTemplate y r rands])) parts
  end else begin
    (* Just a mnemonic, to be followed by operands *)
    let args = if r = R_none then rands else register r :: rands in
    if args = [] then
      instr fmt
    else
      instr (sprintf "$ $" [fStr fmt; fList(fFrag) args])
  end

(* |emit_move| -- emit reg-to-reg move *)
let emit_move dst src =
  emit_inst "mov" dst [register src]

(* |emit_lab| -- emit a label *)
let emit_lab lab =
  Queue.add (Label lab) code

let emit_comment cmnt =
  Queue.add (Comment cmnt) code

let emit_tree t =
  Queue.add (Tree t) code

let need_stack n =
  stack := max n !stack

let flush () =
  let put =
    function
        Instr s -> 
          printf "\t$\n" [fStr s]
      | Label lab ->
          printf "$:\n" [fLab lab] 
      | Comment cmnt ->
          printf "@ $\n" [fStr cmnt] 
      | Tree t ->
          Optree.print_optree "@ " t in
  Queue.iter put code;
  Queue.clear code

(* |start_proc| -- emit start of procedure *)
let start_proc lab nargs fram =
  segment Text;
  printf "$:\n" [fStr lab];
  printf "\tmov ip, sp\n" [];
  if nargs > 0 then begin
    let save = if nargs <= 2 then "{r0-r1}" else "{r0-r3}" in
    printf "\tstmfd sp!, $\n" [fStr save]
  end;
  printf "\tstmfd sp!, {r4-r10, fp, ip, lr}\n" [];
  printf "\tmov fp, sp\n" [];
  frame := fram

(* |flush_proc| -- output procedure fragment, perhaps after error *)
let flush_proc () =
  (* Round up frame space for stack alignment *)
  let space = 8 * ((!frame + !stack + 7)/8) in
  if space <= 1024 then
    (* Since space is a multiple of 8, we can fit values up to 1024 *)
    (if space > 0 then printf "\tsub sp, sp, #$\n" [fNum space])
  else begin
    printf "\tldr ip, =$\n" [fNum space];
    printf "\tsub sp, sp, ip\n" []
  end;
  flush ();
  stack := 0

(* |end_proc| -- emit end of procedure *)
let end_proc () =
  flush_proc ();
  printf "\tldmfd fp, {r4-r10, fp, sp, pc}\n" [];
  printf "\t.pool\n\n" []               (* Output the literal table *)

(* |emit_string| -- output a string constant *)
let emit_string lab s =
  segment Data;
  printf "$:" [fStr lab];
  let n = String.length s in
  for k = 0 to n-1 do
    let c = int_of_char s.[k] in
    if k mod 10 = 0 then 
      printf "\n\t.byte $" [fNum c]
    else
      printf ", $" [fNum c]
  done;
  printf "\n\t.byte 0\n" []

(* |emit_global| -- output a global variable *)
let emit_global sym n =
  printf "\t.comm $, $, 4\n" [fStr sym; fNum n]

(* |postamble| -- finish the assembler file *)
let postamble () =
  fprintf stderr "$ instructions\n" [fNum !icount];
  (* Tell the linker we *don't* need execstack *)
  printf "\t.section .note.GNU-stack\n" [];
  printf "@ End\n" []

