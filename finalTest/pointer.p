(* demonstrates pointers and arbitrary chains of pointers
 that len works for pointers and that pointers to open arrays can be 
 dereferenced and successfully passed to functions *)

var p: pointer to array of integer;
var q: pointer to pointer to array of integer;
var x: pointer to pointer to pointer to array of integer;

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
    newrow(p,3);
    new(q);
    newrow(q^,4);
    new(x);
    new(x^);
    newrow(x^^,5);
    print_num(len(p^));
    newline();
    print_num(len(q^^));
    newline();
    print_num(len(x^^^));
    newline();

    x^^^[0] := 1; x^^^[1] := 2; x^^^[2] := 3; x^^^[3] := 4; x^^^[4] := 5;
    print_num(sum(x^^^));
    newline();
end.