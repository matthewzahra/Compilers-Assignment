var i: integer;

var a: pointer to pointer to array 15 of integer;
var c: pointer to array of integer;

proc sum(b : array of integer): integer;
 var i, s: integer;
 
 begin
    print_num(len(b));
    newline();
    s := 0;
    for i := 0 to (len(b) -1) do
        s := s + b[i]
    end;
    return s
end;

begin
  i := 2; new(a); new(a^); a^^[0] := 1; a^^[1] := 1; a^^[2] := 13; a^^[6] := 12; newrow(c,14);
  c^[0] := 12; c^[5]:= 31;
  print_num(len(c^));
  newline();
  print_num(sum(c^));
  newline();
  print_num(sum(a^^));
  newline();
end.