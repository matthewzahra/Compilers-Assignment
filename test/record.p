(* Another version of phone book using a record type *)

type string = array 11 of char;

type rec = record name: string; age: integer end;

var 
  db: array 20 of rec;
  N: integer;

proc equal(x, y: string): boolean;
  var i: integer;
begin
  i := 0;
  while i < 10 do
    if x[i] <> y[i] then
      return false
    end;
    i := i+1
  end;
  return true
end;

proc store(n: string; a: integer);
begin
  db[N].name := n;
  db[N].age := a;
  N := N+1
end;

proc recall(n: string): integer;
  var i: integer;
begin
  i := 0;
  while i < N do
    if equal(db[i].name, n) then
      return db[i].age
    end;
    i := i+1
  end;
  return 999
end;

begin
  N := 0;

  store("bill      ", 23);
  store("george    ", 34);

  print_num(recall("george    ")); newline();
  print_num(recall("fred      ")); newline()
end.

(*<<
34
999
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

@ proc equal(x, y: string): boolean;
	.text
_equal:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@ i := 0;
	mov r4, #0
.L2:
@ while i < 10 do
	cmp r4, #10
	bge .L4
@ if x[i] <> y[i] then
	ldr r0, [fp, #40]
	add r0, r0, r4
	ldrb r0, [r0]
	ldr r1, [fp, #44]
	add r1, r1, r4
	ldrb r1, [r1]
	cmp r0, r1
	beq .L7
@ return false
	mov r0, #0
	b .L1
.L7:
@ i := i+1
	add r4, r4, #1
	b .L2
.L4:
@ return true
	mov r0, #1
.L1:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.pool

@ proc store(n: string; a: integer);
_store:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@ db[N].name := n;
	ldr r4, =_db
	ldr r5, =_N
	mov r2, #11
	ldr r1, [fp, #40]
	ldr r0, [r5]
	lsl r0, r0, #4
	add r0, r4, r0
	bl memcpy
@ db[N].age := a;
	ldr r0, [fp, #44]
	ldr r1, [r5]
	lsl r1, r1, #4
	add r1, r4, r1
	str r0, [r1, #12]
@ N := N+1
	ldr r0, [r5]
	add r0, r0, #1
	str r0, [r5]
	ldmfd fp, {r4-r10, fp, sp, pc}
	.pool

@ proc recall(n: string): integer;
_recall:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@ i := 0;
	mov r4, #0
.L10:
@ while i < N do
	ldr r0, =_N
	ldr r0, [r0]
	cmp r4, r0
	bge .L12
@ if equal(db[i].name, n) then
	ldr r5, =_db
	ldr r1, [fp, #40]
	lsl r0, r4, #4
	add r0, r5, r0
	bl _equal
	cmp r0, #0
	beq .L15
@ return db[i].age
	lsl r0, r4, #4
	add r0, r5, r0
	ldr r0, [r0, #12]
	b .L9
.L15:
@ i := i+1
	add r4, r4, #1
	b .L10
.L12:
@ return 999
	ldr r0, =999
.L9:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.pool

pmain:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@ N := 0;
	mov r0, #0
	ldr r1, =_N
	str r0, [r1]
@ store("bill      ", 23);
	mov r1, #23
	ldr r0, =__s1
	bl _store
@ store("george    ", 34);
	mov r1, #34
	ldr r0, =__s2
	bl _store
@ print_num(recall("george    ")); newline();
	ldr r0, =__s3
	bl _recall
	bl print_num
	bl newline
@ print_num(recall("fred      ")); newline()
	ldr r0, =__s4
	bl _recall
	bl print_num
	bl newline
	ldmfd fp, {r4-r10, fp, sp, pc}
	.pool

	.comm _db, 320, 4
	.comm _N, 4, 4
	.data
__s1:
	.byte 98, 105, 108, 108, 32, 32, 32, 32, 32, 32
	.byte 0
__s2:
	.byte 103, 101, 111, 114, 103, 101, 32, 32, 32, 32
	.byte 0
__s3:
	.byte 103, 101, 111, 114, 103, 101, 32, 32, 32, 32
	.byte 0
__s4:
	.byte 102, 114, 101, 100, 32, 32, 32, 32, 32, 32
	.byte 0
	.section .note.GNU-stack
@ End
]]*)
