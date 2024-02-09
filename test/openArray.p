var a : array 6 of integer;

proc f(c: array of integer) : integer;
begin
    return sum(c)
end;

proc sum(b : array of integer) : integer;
    var i, s: integer;
begin
    s := 0;
    print_num(len(b));
    newline();
    for i := 0 to len(b)-1 do
    print_num(b[i]);
    newline();
        s := s + b[i];
    end;
    return s
end;
var i, s: integer;
begin
    a[0]:=1;a[1]:=11;a[2]:=100;
    print_num(len(a));
    newline();
    print_num(f(a));
    newline();
end.