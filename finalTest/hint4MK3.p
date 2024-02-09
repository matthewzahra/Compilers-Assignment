var p : pointer to array of integer;

begin
    newrow(p,3);
    p^[0] := 1;
    print_num(p^[0]); newline();
end.

(* address of p evaluated into a temporary register
@     p^[0] := 1;
@ <DEFTEMP 1, <LOADW, <GLOBAL _p>>>
@ <STOREW,
@   <CONST 1>,
@   <OFFSET,
@     <TEMP 1>,
@     <BINOP
@       Times,
@       <BOUND, <CONST 0>, <LOADW, <OFFSET, <TEMP 1>, <CONST -4>>>>,
@       <CONST 4>>>>
*)