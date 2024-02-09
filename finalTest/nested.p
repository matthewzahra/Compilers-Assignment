(* showing open array parameters can be passed into functions as well *)

var b : array 5 of integer;

proc f(a: array of integer): integer;
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
    return sum(a)
end;

begin
    b[0] := 1; b[1] := 2; b[2] := 3; b[3] := 4; b[4] := 5;
    print_num(f(b));
    newline();
end.