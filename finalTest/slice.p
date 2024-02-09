var a : array 5 of integer;
var b : pointer to array of integer;
var i : integer ;
var j : integer;

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
    i := 2;
    j := 4;
    a[0]:=1;a[1]:=2;a[i]:=3;a[3]:=4;a[4]:=5;

    print_num(sum(a[2..5)));
    newline();

    print_num(sum(a[i..j)));
    newline();

    newrow(b,3);
    b^[0] := 1; b^[1] := 2; b^[2] := 3;
    print_num(sum(b^[i-1..j-1)));
    newline();

    (* common case - efficient code *)
    print_num(sum(a[0..4)));
    newline();

    (* this is essentially an empty array *)
    print_num(sum(a[0..0))); 
    newline();


    (* checking validity of slices *)
    print_num(sum(a[0..6)));
    newline();


    print_num(sum(a[3..1)));
    newline();
end.