@ picoPascal compiler output
	.global pmain

@ proc sum(b : array of integer) : integer;
	.text
_sum:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@ s := 0;
	mov r5, #0
@ for i := 0 to len(b)-1 do
	mov r4, #0
	add r0, fp, #40
	ldr r0, [r0, #4]
	sub r6, r0, #1
.L2:
	cmp r4, r6
	bgt .L3
@ s := s + b[i];
	ldr r0, [fp, #40]
	mov r1, #4
	mul r1, r4, r1
	add r0, r0, r1
	ldr r0, [r0]
	add r5, r5, r0
@ end;
	add r4, r4, #1
	b .L2
.L3:
@ return s
	mov r0, r5
	b .L1
.L1:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.pool

pmain:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@ i := 2;
	mov r0, #2
	ldr r1, =_i
	str r0, [r1]
@ j := 4;
	mov r0, #4
	ldr r1, =_j
	str r0, [r1]
@ a[0]:=1;a[1]:=2;a[i]:=3;a[3]:=4;a[4]:=5;
	mov r0, #1
	ldr r1, =_a
	mov r2, #0
	mov r3, #4
	mul r2, r2, r3
	add r1, r1, r2
	str r0, [r1]
	mov r0, #2
	ldr r1, =_a
	mov r2, #1
	mov r3, #4
	mul r2, r2, r3
	add r1, r1, r2
	str r0, [r1]
	mov r0, #3
	ldr r1, =_a
	ldr r2, =_i
	ldr r2, [r2]
	mov r3, #4
	mul r2, r2, r3
	add r1, r1, r2
	str r0, [r1]
	mov r0, #4
	ldr r1, =_a
	mov r2, #3
	mov r3, #4
	mul r2, r2, r3
	add r1, r1, r2
	str r0, [r1]
	mov r0, #5
	ldr r1, =_a
	mov r2, #4
	mov r3, #4
	mul r2, r2, r3
	add r1, r1, r2
	str r0, [r1]
@ print_num(sum(a[2..5)));
	ldr r4, =_a
	mov r1, #2
	mov r0, #5
	mov r2, #5
	cmp r1, #0
	ldrmi r0, =21
	blmi slicecheck
	sub r1, r0, r1
	cmp r1, #0
	ldrmi r0, =21
	blmi slicecheck
	cmp r2, r0
	ldrmi r0, =21
	blmi slicecheck
	mov r0, #2
	mov r2, #4
	mul r0, r0, r2
	add r0, r4, r0
	mov r10, #0
	bl _sum
	bl print_num
@ newline();
	bl newline
@ print_num(sum(a[i..j)));
	ldr r4, =_a
	ldr r0, =_i
	ldr r1, [r0]
	ldr r0, =_j
	ldr r0, [r0]
	mov r2, #5
	cmp r1, #0
	ldrmi r0, =24
	blmi slicecheck
	sub r1, r0, r1
	cmp r1, #0
	ldrmi r0, =24
	blmi slicecheck
	cmp r2, r0
	ldrmi r0, =24
	blmi slicecheck
	ldr r0, =_i
	ldr r0, [r0]
	mov r2, #4
	mul r0, r0, r2
	add r0, r4, r0
	mov r10, #0
	bl _sum
	bl print_num
@ newline();
	bl newline
@ newrow(b,3);
	mov r1, #4
	mov r0, #3
	bl palloc2
	ldr r1, =_b
	str r0, [r1]
@ b^[0] := 1; b^[1] := 2; b^[2] := 3;
	mov r0, #1
	ldr r1, =_b
	ldr r1, [r1]
	mov r2, #0
	mov r3, #4
	mul r2, r2, r3
	add r1, r1, r2
	str r0, [r1]
	mov r0, #2
	ldr r1, =_b
	ldr r1, [r1]
	mov r2, #1
	mov r3, #4
	mul r2, r2, r3
	add r1, r1, r2
	str r0, [r1]
	mov r0, #3
	ldr r1, =_b
	ldr r1, [r1]
	mov r2, #2
	mov r3, #4
	mul r2, r2, r3
	add r1, r1, r2
	str r0, [r1]
@ print_num(sum(b^[i-1..j-1)));
	ldr r0, =_b
	ldr r4, [r0]
	ldr r0, =_i
	ldr r0, [r0]
	sub r1, r0, #1
	ldr r0, =_j
	ldr r0, [r0]
	sub r0, r0, #1
	ldr r2, [r4, #-4]
	cmp r1, #0
	ldrmi r0, =29
	blmi slicecheck
	sub r1, r0, r1
	cmp r1, #0
	ldrmi r0, =29
	blmi slicecheck
	cmp r2, r0
	ldrmi r0, =29
	blmi slicecheck
	ldr r0, =_i
	ldr r0, [r0]
	sub r0, r0, #1
	mov r2, #4
	mul r0, r0, r2
	add r0, r4, r0
	mov r10, #0
	bl _sum
	bl print_num
@ newline();
	bl newline
@ print_num(sum(a[0..4)));
	ldr r4, =_a
	mov r1, #0
	mov r0, #4
	mov r2, #5
	cmp r1, #0
	ldrmi r0, =33
	blmi slicecheck
	sub r1, r0, r1
	cmp r1, #0
	ldrmi r0, =33
	blmi slicecheck
	cmp r2, r0
	ldrmi r0, =33
	blmi slicecheck
	mov r0, r4
	mov r10, #0
	bl _sum
	bl print_num
@ newline();
	bl newline
@ print_num(sum(a[0..0)));
	ldr r4, =_a
	mov r1, #0
	mov r0, #0
	mov r2, #5
	cmp r1, #0
	ldrmi r0, =37
	blmi slicecheck
	sub r1, r0, r1
	cmp r1, #0
	ldrmi r0, =37
	blmi slicecheck
	cmp r2, r0
	ldrmi r0, =37
	blmi slicecheck
	mov r0, r4
	mov r10, #0
	bl _sum
	bl print_num
@ newline();
	bl newline
@ print_num(sum(a[0..6)));
	ldr r4, =_a
	mov r1, #0
	mov r0, #6
	mov r2, #5
	cmp r1, #0
	ldrmi r0, =42
	blmi slicecheck
	sub r1, r0, r1
	cmp r1, #0
	ldrmi r0, =42
	blmi slicecheck
	cmp r2, r0
	ldrmi r0, =42
	blmi slicecheck
	mov r0, r4
	mov r10, #0
	bl _sum
	bl print_num
@ newline();
	bl newline
@ print_num(sum(a[3..1)));
	ldr r4, =_a
	mov r1, #3
	mov r0, #1
	mov r2, #5
	cmp r1, #0
	ldrmi r0, =46
	blmi slicecheck
	sub r1, r0, r1
	cmp r1, #0
	ldrmi r0, =46
	blmi slicecheck
	cmp r2, r0
	ldrmi r0, =46
	blmi slicecheck
	mov r0, #3
	mov r2, #4
	mul r0, r0, r2
	add r0, r4, r0
	mov r10, #0
	bl _sum
	bl print_num
@ newline();
	bl newline
@ end.
.L4:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.pool

	.comm _a, 20, 4
	.comm _b, 4, 4
	.comm _i, 4, 4
	.comm _j, 4, 4
	.section .note.GNU-stack
@ End
