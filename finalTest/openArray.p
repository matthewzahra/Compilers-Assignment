(* demonstrates that functions with open arrays work for arrays of arbitrary size 
including len function*)

var b : array 5 of integer;
var c : array 6 of integer;

proc sum(a :array of integer) : integer;
    var i,s : integer;
begin
    s := 0;
    for i := 0 to len(a) - 1 do
        s := s + a[i]
    end;
    return s
end;

begin
    b[0] := 1; b[1] := 2; b[2] := 3; b[3] := 4; b[4] := 5;
    c[0] := 1; c[1] := 2; c[2] := 3; c[3] := 4; c[4] := 5; c[5] := 6;
    print_num(sum(b));
    newline();
    print_num(sum(c));
    newline();
end.