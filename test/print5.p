(* Printing strings *)

begin
  print_string("five"); newline()
end.

(*<<
five
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

	.text
pmain:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@ print_string("five"); newline()
	mov r1, #5
	ldr r0, =__s1
	bl print_string
	bl newline
	ldmfd fp, {r4-r10, fp, sp, pc}
	.pool

	.data
__s1:
	.byte 102, 105, 118, 101
	.byte 0
	.section .note.GNU-stack
@ End
]]*)
