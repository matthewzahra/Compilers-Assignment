(* lab4/target.mli *)
(* Copyright (c) 2017 J. M. Spivey *)

open Optree

(* |reg| -- ARM registers *)
type reg = R of int | R_fp | R_any | R_temp | R_none

(* |fReg| -- format register for printing *)
val fReg : reg -> Print.arg

(* |volatile| -- list of caller-save registers *)
val volatile : reg list

(* |stable| -- list of callee-save registers *)
val stable : reg list



(* FRAGMENTS *)

type fragment

(* Basic fragments *)
val register : reg -> fragment
val number : int -> fragment
val symbol : symbol -> fragment
val codelab : codelab -> fragment
val quote : string -> fragment

(* Join multiple fragments into one *)
val frag : string -> fragment list -> fragment

val reg_of : fragment -> reg
val get_regs : fragment -> reg list


(* CODE OUTPUT *)

(* |emit_inst| -- emit an assembly language instruction *)
val emit_inst : string -> reg -> fragment list -> unit

(* |emit_move| -- emit a reg-to-reg move *)
val emit_move : reg -> reg -> unit

(* |emit_lab| -- place a label *)
val emit_lab : codelab -> unit

(* |emit_comment| -- insert a comment *)
val emit_comment : string -> unit

(* |emit_tree| -- Print an optree as a comment *)
val emit_tree : optree -> unit

(* |need_stack| -- ensure stack space *)
val need_stack : int -> unit

(* |preamble| -- emit first part of assembly language output *)
val preamble : unit -> unit

(* |postamble| -- emit last part of assembly language output *)
val postamble : unit -> unit

(* |start_proc| -- emit beginning of procedure *)
val start_proc : symbol -> int -> int -> unit

(* |end_proc| -- emit end of procedure *)
val end_proc : unit -> unit

(* |flush_proc| -- dump out code after failure *)
val flush_proc : unit -> unit

(* |emit_string| -- emit assembler code for string constant *)
val emit_string : symbol -> string -> unit

(* |emit_global| -- emit assembler code to define global variable *)
val emit_global : symbol -> int -> unit
