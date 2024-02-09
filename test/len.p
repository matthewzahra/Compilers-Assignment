var a : array 5 of integer;

proc find_len(b : array of integer; c : array 5 of integer) : integer;
begin
    print_num(len(b));
    newline();
    print_num(len(c));
    newline();
    return len(b)
end;

begin
    print_num(len(a));
    a[0] := 6;
    newline();
    print_num(find_len(a,a));
    newline();
end.