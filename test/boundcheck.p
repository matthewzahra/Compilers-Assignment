var a : pointer to array of integer;


proc f(b: array of integer) : integer;
begin
    return b[5]
end;

begin
    newrow(a,5);
    print_num(a^[5]);
    newline();
    print_num(f(a^));
    newline();
end.