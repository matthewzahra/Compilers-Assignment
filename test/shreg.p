var x: integer;

proc f();
  var y: integer;
begin
  y := 3;
  x := y;
  y := 4;
  g(x)
end;

proc g(t: integer);
begin
  print_num(t); newline()
end;

begin
  f()
end.

(*<<
3
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

@ proc f();
	.text
_f:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@ y := 3;
	mov r4, #3
@ x := y;
	ldr r0, =_x
	str r4, [r0]
@ y := 4;
	mov r4, #4
@ g(x)
	ldr r0, =_x
	ldr r5, [r0]
	mov r0, r5
	bl _g
	ldmfd fp, {r4-r10, fp, sp, pc}
	.pool

@ proc g(t: integer);
_g:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@ print_num(t); newline()
	ldr r0, [fp, #40]
	bl print_num
	bl newline
	ldmfd fp, {r4-r10, fp, sp, pc}
	.pool

pmain:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@ f()
	bl _f
	ldmfd fp, {r4-r10, fp, sp, pc}
	.pool

	.comm _x, 4, 4
	.section .note.GNU-stack
@ End
]]*)
