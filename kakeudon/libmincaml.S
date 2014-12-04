	li	$sp, 0x00400000
	li	$gp, 0x00030000
	mtc1	$zero, $f31
	jal	_min_caml_start
endloop:
	jal	recv_byte
	j	endloop

	.globl	min_caml_read_char
min_caml_read_char:
	mov	$a1, $ra
	jal	recv_byte
	mov	$a0, $v0
	mov	$ra, $a1
	jr	$ra

	.globl	min_caml_print_char
min_caml_print_char:
	j	send_byte

	.globl	min_caml_print_int
min_caml_print_int:
	mov	$a2, $ra
	mov	$a1, $a0
	srl	$a0, $a1, 24
	jal	send_byte
	srl	$a0, $a1, 16
	jal	send_byte
	srl	$a0, $a1, 8
	jal	send_byte
	srl	$a0, $a1, 0
	jal	send_byte
	mov	$ra, $a2
	jr	$ra

	.globl	min_caml_create_float_array
min_caml_create_float_array:
	mfc1	$a1, $f0
	mov	$v0, $gp
create_float_array_fill_loop:
	beq	$a0, $zero, create_float_array_fill_loop_end
	sw	$a1, 0($gp)
	addiu	$a0, $a0, -1
	addiu	$gp, $gp, 4
	j	create_float_array_fill_loop
create_float_array_fill_loop_end:
	mov	$a0, $v0
	jr	$ra

	.globl	min_caml_create_array
min_caml_create_array:
	mov	$v0, $gp
create_array_fill_loop:
	beq	$a0, $zero, create_array_fill_loop_end
	sw	$a1, 0($gp)
	addiu	$a0, $a0, -1
	addiu	$gp, $gp, 4
	j	create_array_fill_loop
create_array_fill_loop_end:
	mov	$a0, $v0
	jr	$ra

	.globl	min_caml_int_of_float
min_caml_int_of_float:
	cvt.w.s	$f0, $f0
	mfc1	$a0, $f0
	jr	$ra

	.globl	min_caml_float_of_int
min_caml_float_of_int:
	mtc1	$a0, $f0
	cvt.s.w	$f0, $f0
	jr	$ra

	.globl	min_caml_sqrt
min_caml_sqrt:
	sqrt.s	$f0, $f0
	jr	$ra

recv_byte:
	li	$t0, 0xffff0000
rd_poll:
	lw	$v0, 0($t0)
	andi	$v0, $v0, 0x01
	beq	$v0, $zero, rd_poll
	lw	$v0, 4($t0)
	andi	$v0, $v0, 0xff
	jr	$ra

send_byte:
	li	$t0, 0xffff0000
wr_poll:
	lw	$v0, 8($t0)
	andi	$v0, $v0, 0x01
	beq	$v0, $zero, wr_poll
	sw	$a0, 12($t0)
	jr	$ra
