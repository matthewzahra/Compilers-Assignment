(* demonstrates that bounds checking works for normal arrays, open arrays
and open arrays accessed via deferencing a pointer*)

var a : array 3 of integer;
var b : pointer to array of integer;

proc f(b: array of integer) : integer;
begin
    return b[2];
end;

begin
    a[0] := 1; a[1] := 2; a[2] := 3;
    print_num(f(a)); newline();
    print_num(a[3]); newline();
    newrow(b,3);
    b^[0] := 1; b^[1] := 2; b^[2] := 33;
    print_num(f(b^)); newline();
    newrow(b,2);
    print_num(f(b^)); newline();
    print_num(b^[3]);
    newline();
end.