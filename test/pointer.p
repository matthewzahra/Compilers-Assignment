var p : pointer to array of integer;
var q : pointer to pointer to array of integer;
var x : integer; 
var a : pointer to pointer to pointer to pointer to array of integer;
var b : pointer to array 60 of integer;
var c : array 60 of integer;
proc f(a: array of integer) : integer;
begin
    a[0] := 69;
    (*print_num(a[0]);
    newline();
    print_num(a[45]);
    newline();*)
    return len(a)
end;

begin
    new(b); 
    b^[0] := 678;
    print_num(len(b^));
    newline();
    print_num(f(b^));
    newline();
(*
    newrow(p,55);
    print_num(f(p^));
    newline();


    newrow(p,50);
    p^[0] := 36;


    print_num(len(p^));
    newline();

    new(q);
    newrow(q^,50);


    print_num(len(q^^));
    newline();

    print_num(f(q^^));
    newline();

    q^^[0] := 45;
    new(a);
    new(a^);
    new(a^^);
    newrow(a^^^,55);
    print_num(f(a^^^));
    newline();
*)
end.