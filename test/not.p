(* Boolean negation *)

var b: boolean;

begin
  b := not b;
  if b then print_string("ok"); newline() end
end.

(*<<
ok
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

	.text
pmain:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@ b := not b;
	ldr r4, =_b
	ldrb r0, [r4]
	eor r5, r0, #1
	strb r5, [r4]
@ if b then print_string("ok"); newline() end
	cmp r5, #0
	beq .L1
	mov r1, #3
	ldr r0, =__s1
	bl print_string
	bl newline
.L1:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.pool

	.comm _b, 1, 4
	.data
__s1:
	.byte 111, 107
	.byte 0
	.section .note.GNU-stack
@ End
]]*)
