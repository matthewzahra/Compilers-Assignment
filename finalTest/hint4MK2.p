var a : array 2 of pointer to array of integer;

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
    new(a[0]);new(a[1]);
    newrow(a[0],2);newrow(a[1],3);
    a[0]^[0] := 1; a[0]^[1] := 2;
    a[1]^[0] := 3; a[1]^[1] := 4; a[1]^[2] := 5;
    print_num(sum(a[0]^));
    newline();

end.