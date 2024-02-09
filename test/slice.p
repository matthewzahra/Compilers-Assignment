var a : array 5 of integer;
var i : integer ;
var j : integer;


proc sum(b : array of integer) : integer;
    var i, s: integer;
begin
    s := 0;

    for i := 0 to len(b)-1 do
    print_num(i);
    newline();
        s := s + b[i];
    end;
    return s
end;

begin
    i := 2;
    j := 4;
    a[0]:=1;a[1]:=2;a[i]:=3;a[3]:=4;a[4]:=5;

    print_num(sum(a[i..j)));

    newline();
end.