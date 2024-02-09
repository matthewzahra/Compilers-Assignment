(* show that passing in a pointer to an open array as an open array parameter doesn't lead to repeated
evaluation (so not to duplicate side effects - AFTER node not present due to "Optree.canonicalise" ) *)

var p : pointer to array of integer;

proc sum(b : array of integer) : integer;
    var i, s: integer;
begin
    s := 0;
    for i := 0 to len(b)-1 do
        s := s + b[i];
    end;
    return s
end;

begin
    newrow(p,5);
    p^[0] := 1; p^[1] := 2; p^[2] := 3; p^[3] := 4; p^[4] := 5;
    print_num(sum(p^));
    newline();
end.

(* See how address gets evaluated into TEMP 6 and then reused
@     print_num(sum(p^));
@ <DEFTEMP 6, <LOADW, <GLOBAL _p>>>
@ <CALL
@   1,
@   <GLOBAL print_num>,
@   <ARG
@     0,
@     <CALL
@       2,
@       <GLOBAL _sum>,
@       <STATLINK, <CONST 0>>,
@       <ARG 0, <TEMP 6>>,
@       <ARG 1, <LOADW, <OFFSET, <TEMP 6>, <CONST -4>>>>>>>
*)