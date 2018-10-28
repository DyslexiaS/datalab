	.file	"bits.c"
	.text
	.globl	absVal
	.type	absVal, @function
absVal:
.LFB0:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	sarl	$31, %eax
	movl	%eax, -4(%rbp)
	movl	-20(%rbp), %eax
	xorl	-4(%rbp), %eax
	subl	-4(%rbp), %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	absVal, .-absVal
	.globl	addOK
	.type	addOK, @function
addOK:
.LFB1:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %edx
	movl	-24(%rbp), %eax
	addl	%edx, %eax
	shrl	$31, %eax
	movl	%eax, -12(%rbp)
	movl	-20(%rbp), %eax
	shrl	$31, %eax
	movl	%eax, -8(%rbp)
	movl	-24(%rbp), %eax
	shrl	$31, %eax
	movl	%eax, -4(%rbp)
	movl	-12(%rbp), %eax
	xorl	-8(%rbp), %eax
	movl	%eax, %edx
	movl	-12(%rbp), %eax
	xorl	-4(%rbp), %eax
	andl	%edx, %eax
	testl	%eax, %eax
	sete	%al
	movzbl	%al, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE1:
	.size	addOK, .-addOK
	.globl	allEvenBits
	.type	allEvenBits, @function
allEvenBits:
.LFB2:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	$16, %eax
	andl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	$8, %eax
	andl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	$4, %eax
	andl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	$2, %eax
	andl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	andl	$1, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2:
	.size	allEvenBits, .-allEvenBits
	.globl	allOddBits
	.type	allOddBits, @function
allOddBits:
.LFB3:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	$16, %eax
	andl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	$8, %eax
	andl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	$4, %eax
	andl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	$2, %eax
	andl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	%eax
	andl	$1, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE3:
	.size	allOddBits, .-allOddBits
	.globl	anyEvenBit
	.type	anyEvenBit, @function
anyEvenBit:
.LFB4:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	$16, %eax
	orl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	$8, %eax
	orl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	$4, %eax
	orl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	$2, %eax
	orl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	andl	$1, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE4:
	.size	anyEvenBit, .-anyEvenBit
	.globl	anyOddBit
	.type	anyOddBit, @function
anyOddBit:
.LFB5:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	$16, %eax
	orl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	$8, %eax
	orl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	$4, %eax
	orl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	$2, %eax
	orl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	%eax
	andl	$1, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE5:
	.size	anyOddBit, .-anyOddBit
	.globl	bang
	.type	bang, @function
bang:
.LFB6:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	$16, %eax
	orl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	$8, %eax
	orl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	$4, %eax
	orl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	$2, %eax
	orl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	%eax
	orl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	andl	$1, %eax
	testl	%eax, %eax
	sete	%al
	movzbl	%al, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE6:
	.size	bang, .-bang
	.globl	bitAnd
	.type	bitAnd, @function
bitAnd:
.LFB7:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	-4(%rbp), %eax
	andl	-8(%rbp), %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE7:
	.size	bitAnd, .-bitAnd
	.globl	bitCount
	.type	bitCount, @function
bitCount:
.LFB8:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	$42, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE8:
	.size	bitCount, .-bitCount
	.globl	bitMask
	.type	bitMask, @function
bitMask:
.LFB9:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-24(%rbp), %eax
	notl	%eax
	movl	%eax, %edx
	movl	-20(%rbp), %eax
	addl	%edx, %eax
	addl	$1, %eax
	shrl	$31, %eax
	movl	%eax, -8(%rbp)
	movl	-20(%rbp), %eax
	movl	$1, %edx
	movl	%eax, %ecx
	sall	%cl, %edx
	movl	-24(%rbp), %eax
	movl	$1, %esi
	movl	%eax, %ecx
	sall	%cl, %esi
	movl	%esi, %eax
	subl	%eax, %edx
	movl	-20(%rbp), %eax
	movl	$1, %esi
	movl	%eax, %ecx
	sall	%cl, %esi
	movl	%esi, %eax
	orl	%edx, %eax
	movl	%eax, -4(%rbp)
	movl	-8(%rbp), %eax
	subl	$1, %eax
	andl	-4(%rbp), %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE9:
	.size	bitMask, .-bitMask
	.globl	bitMatch
	.type	bitMatch, @function
bitMatch:
.LFB10:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	-8(%rbp), %eax
	xorl	-4(%rbp), %eax
	notl	%eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE10:
	.size	bitMatch, .-bitMatch
	.globl	bitNor
	.type	bitNor, @function
bitNor:
.LFB11:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	-4(%rbp), %eax
	orl	-8(%rbp), %eax
	notl	%eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE11:
	.size	bitNor, .-bitNor
	.globl	bitOr
	.type	bitOr, @function
bitOr:
.LFB12:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	-4(%rbp), %eax
	orl	-8(%rbp), %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE12:
	.size	bitOr, .-bitOr
	.globl	bitParity
	.type	bitParity, @function
bitParity:
.LFB13:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	$16, %eax
	xorl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	$8, %eax
	xorl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	$4, %eax
	xorl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	$2, %eax
	xorl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	%eax
	xorl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	andl	$1, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE13:
	.size	bitParity, .-bitParity
	.globl	bitReverse
	.type	bitReverse, @function
bitReverse:
.LFB14:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -20(%rbp)
	movl	$-16711936, -16(%rbp)
	movl	$-3856, -12(%rbp)
	movl	$-858993460, -8(%rbp)
	movl	$-1431655766, -4(%rbp)
	movl	-20(%rbp), %eax
	sarl	$16, %eax
	movl	%eax, %edx
	movl	-20(%rbp), %eax
	sall	$16, %eax
	orl	%edx, %eax
	movl	%eax, -20(%rbp)
	movl	-20(%rbp), %eax
	andl	-16(%rbp), %eax
	sarl	$8, %eax
	movl	%eax, %edx
	movl	-16(%rbp), %eax
	notl	%eax
	andl	-20(%rbp), %eax
	sall	$8, %eax
	orl	%edx, %eax
	movl	%eax, -20(%rbp)
	movl	-20(%rbp), %eax
	andl	-12(%rbp), %eax
	sarl	$4, %eax
	movl	%eax, %edx
	movl	-12(%rbp), %eax
	notl	%eax
	andl	-20(%rbp), %eax
	sall	$4, %eax
	orl	%edx, %eax
	movl	%eax, -20(%rbp)
	movl	-20(%rbp), %eax
	andl	-8(%rbp), %eax
	sarl	$2, %eax
	movl	%eax, %edx
	movl	-8(%rbp), %eax
	notl	%eax
	andl	-20(%rbp), %eax
	sall	$2, %eax
	orl	%edx, %eax
	movl	%eax, -20(%rbp)
	movl	-20(%rbp), %eax
	andl	-4(%rbp), %eax
	sarl	%eax
	movl	%eax, %edx
	movl	-4(%rbp), %eax
	notl	%eax
	andl	-20(%rbp), %eax
	addl	%eax, %eax
	orl	%edx, %eax
	movl	%eax, -20(%rbp)
	movl	-20(%rbp), %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE14:
	.size	bitReverse, .-bitReverse
	.globl	bitXor
	.type	bitXor, @function
bitXor:
.LFB15:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	-4(%rbp), %eax
	xorl	-8(%rbp), %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE15:
	.size	bitXor, .-bitXor
	.globl	byteSwap
	.type	byteSwap, @function
byteSwap:
.LFB16:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	%edx, -12(%rbp)
	movl	$42, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE16:
	.size	byteSwap, .-byteSwap
	.globl	conditional
	.type	conditional, @function
conditional:
.LFB17:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	%edx, -12(%rbp)
	cmpl	$0, -4(%rbp)
	sete	%al
	movzbl	%al, %eax
	movl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	negl	%eax
	andl	-12(%rbp), %eax
	movl	%eax, %edx
	movl	-4(%rbp), %eax
	subl	$1, %eax
	andl	-8(%rbp), %eax
	orl	%edx, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE17:
	.size	conditional, .-conditional
	.globl	countLeadingZero
	.type	countLeadingZero, @function
countLeadingZero:
.LFB18:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	$42, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE18:
	.size	countLeadingZero, .-countLeadingZero
	.globl	copyLSB
	.type	copyLSB, @function
copyLSB:
.LFB19:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	andl	$1, -4(%rbp)
	movl	-4(%rbp), %eax
	negl	%eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE19:
	.size	copyLSB, .-copyLSB
	.globl	distinctNegation
	.type	distinctNegation, @function
distinctNegation:
.LFB20:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	andl	$2147483647, -4(%rbp)
	cmpl	$0, -4(%rbp)
	setne	%al
	movzbl	%al, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE20:
	.size	distinctNegation, .-distinctNegation
	.globl	dividePower2
	.type	dividePower2, @function
dividePower2:
.LFB21:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	shrl	$31, %eax
	movl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	negl	%eax
	movl	%eax, %edx
	movl	-24(%rbp), %eax
	movl	$1, %esi
	movl	%eax, %ecx
	sall	%cl, %esi
	movl	%esi, %ecx
	movl	-20(%rbp), %eax
	addl	%ecx, %eax
	subl	$1, %eax
	andl	%eax, %edx
	movl	-4(%rbp), %eax
	subl	$1, %eax
	andl	-20(%rbp), %eax
	orl	%eax, %edx
	movl	-24(%rbp), %eax
	movl	%eax, %ecx
	sarl	%cl, %edx
	movl	%edx, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE21:
	.size	dividePower2, .-dividePower2
	.globl	evenBits
	.type	evenBits, @function
evenBits:
.LFB22:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	$85, -4(%rbp)
	movl	-4(%rbp), %eax
	sall	$16, %eax
	orl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	sall	$8, %eax
	orl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE22:
	.size	evenBits, .-evenBits
	.globl	ezThreeFourths
	.type	ezThreeFourths, @function
ezThreeFourths:
.LFB23:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	leal	0(,%rax,4), %edx
	movl	-4(%rbp), %eax
	addl	%edx, %eax
	sarl	$2, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE23:
	.size	ezThreeFourths, .-ezThreeFourths
	.globl	fitsBits
	.type	fitsBits, @function
fitsBits:
.LFB24:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	shrl	$31, %eax
	movl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	negl	%eax
	movl	%eax, %edx
	movl	-20(%rbp), %eax
	notl	%eax
	andl	%eax, %edx
	movl	-4(%rbp), %eax
	subl	$1, %eax
	andl	-20(%rbp), %eax
	orl	%edx, %eax
	movl	%eax, -20(%rbp)
	movl	-24(%rbp), %eax
	subl	$1, %eax
	movl	-20(%rbp), %edx
	movl	%eax, %ecx
	sarl	%cl, %edx
	movl	%edx, %eax
	testl	%eax, %eax
	sete	%al
	movzbl	%al, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE24:
	.size	fitsBits, .-fitsBits
	.globl	fitsShort
	.type	fitsShort, @function
fitsShort:
.LFB25:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	addl	$32768, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	$16, %eax
	testl	%eax, %eax
	sete	%al
	movzbl	%al, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE25:
	.size	fitsShort, .-fitsShort
	.globl	floatAbsVal
	.type	floatAbsVal, @function
floatAbsVal:
.LFB26:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	$42, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE26:
	.size	floatAbsVal, .-floatAbsVal
	.globl	floatFloat2Int
	.type	floatFloat2Int, @function
floatFloat2Int:
.LFB27:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	$42, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE27:
	.size	floatFloat2Int, .-floatFloat2Int
	.globl	floatInt2Float
	.type	floatInt2Float, @function
floatInt2Float:
.LFB28:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	$42, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE28:
	.size	floatInt2Float, .-floatInt2Float
	.globl	floatIsEqual
	.type	floatIsEqual, @function
floatIsEqual:
.LFB29:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	$42, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE29:
	.size	floatIsEqual, .-floatIsEqual
	.globl	floatIsLess
	.type	floatIsLess, @function
floatIsLess:
.LFB30:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	$42, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE30:
	.size	floatIsLess, .-floatIsLess
	.globl	floatNegate
	.type	floatNegate, @function
floatNegate:
.LFB31:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	$42, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE31:
	.size	floatNegate, .-floatNegate
	.globl	floatPower2
	.type	floatPower2, @function
floatPower2:
.LFB32:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	$42, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE32:
	.size	floatPower2, .-floatPower2
	.globl	floatScale1d2
	.type	floatScale1d2, @function
floatScale1d2:
.LFB33:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	$42, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE33:
	.size	floatScale1d2, .-floatScale1d2
	.globl	floatScale2
	.type	floatScale2, @function
floatScale2:
.LFB34:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	$42, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE34:
	.size	floatScale2, .-floatScale2
	.globl	floatScale64
	.type	floatScale64, @function
floatScale64:
.LFB35:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	$42, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE35:
	.size	floatScale64, .-floatScale64
	.globl	floatUnsigned2Float
	.type	floatUnsigned2Float, @function
floatUnsigned2Float:
.LFB36:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	$42, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE36:
	.size	floatUnsigned2Float, .-floatUnsigned2Float
	.globl	getByte
	.type	getByte, @function
getByte:
.LFB37:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	-8(%rbp), %eax
	sall	$3, %eax
	movl	-4(%rbp), %edx
	movl	%eax, %ecx
	sarl	%cl, %edx
	movl	%edx, %eax
	movzbl	%al, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE37:
	.size	getByte, .-getByte
	.globl	greatestBitPos
	.type	greatestBitPos, @function
greatestBitPos:
.LFB38:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	$42, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE38:
	.size	greatestBitPos, .-greatestBitPos
	.globl	howManyBits
	.type	howManyBits, @function
howManyBits:
.LFB39:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	$0, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE39:
	.size	howManyBits, .-howManyBits
	.globl	implication
	.type	implication, @function
implication:
.LFB40:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	cmpl	$0, -4(%rbp)
	sete	%al
	movzbl	%al, %eax
	orl	-8(%rbp), %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE40:
	.size	implication, .-implication
	.globl	intLog2
	.type	intLog2, @function
intLog2:
.LFB41:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	$42, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE41:
	.size	intLog2, .-intLog2
	.globl	isAsciiDigit
	.type	isAsciiDigit, @function
isAsciiDigit:
.LFB42:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	shrl	$31, %eax
	movl	%eax, -4(%rbp)
	movl	$47, %eax
	subl	-20(%rbp), %eax
	movl	%eax, %edx
	movl	$58, %eax
	subl	-20(%rbp), %eax
	xorl	%edx, %eax
	shrl	$31, %eax
	movl	%eax, %edx
	cmpl	$0, -4(%rbp)
	sete	%al
	movzbl	%al, %eax
	andl	%edx, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE42:
	.size	isAsciiDigit, .-isAsciiDigit
	.globl	isEqual
	.type	isEqual, @function
isEqual:
.LFB43:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	-4(%rbp), %eax
	cmpl	-8(%rbp), %eax
	sete	%al
	movzbl	%al, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE43:
	.size	isEqual, .-isEqual
	.globl	isGreater
	.type	isGreater, @function
isGreater:
.LFB44:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	$42, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE44:
	.size	isGreater, .-isGreater
	.globl	isLess
	.type	isLess, @function
isLess:
.LFB45:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	$42, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE45:
	.size	isLess, .-isLess
	.globl	isLessOrEqual
	.type	isLessOrEqual, @function
isLessOrEqual:
.LFB46:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	$42, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE46:
	.size	isLessOrEqual, .-isLessOrEqual
	.globl	isNegative
	.type	isNegative, @function
isNegative:
.LFB47:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	shrl	$31, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE47:
	.size	isNegative, .-isNegative
	.globl	isNonNegative
	.type	isNonNegative, @function
isNonNegative:
.LFB48:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	notl	%eax
	shrl	$31, %eax
	movzbl	%al, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE48:
	.size	isNonNegative, .-isNonNegative
	.globl	isNonZero
	.type	isNonZero, @function
isNonZero:
.LFB49:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	cmpl	$0, -4(%rbp)
	setne	%al
	movzbl	%al, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE49:
	.size	isNonZero, .-isNonZero
	.globl	isNotEqual
	.type	isNotEqual, @function
isNotEqual:
.LFB50:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	-4(%rbp), %eax
	cmpl	-8(%rbp), %eax
	setne	%al
	movzbl	%al, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE50:
	.size	isNotEqual, .-isNotEqual
	.globl	isPallindrome
	.type	isPallindrome, @function
isPallindrome:
.LFB51:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	$42, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE51:
	.size	isPallindrome, .-isPallindrome
	.globl	isPositive
	.type	isPositive, @function
isPositive:
.LFB52:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	notl	%eax
	shrl	$31, %eax
	movl	%eax, %edx
	cmpl	$0, -4(%rbp)
	setne	%al
	andl	%edx, %eax
	movzbl	%al, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE52:
	.size	isPositive, .-isPositive
	.globl	isPower2
	.type	isPower2, @function
isPower2:
.LFB53:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	$42, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE53:
	.size	isPower2, .-isPower2
	.globl	isTmax
	.type	isTmax, @function
isTmax:
.LFB54:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	cmpl	$2147483647, -4(%rbp)
	sete	%al
	movzbl	%al, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE54:
	.size	isTmax, .-isTmax
	.globl	isTmin
	.type	isTmin, @function
isTmin:
.LFB55:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	cmpl	$-2147483648, -4(%rbp)
	sete	%al
	movzbl	%al, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE55:
	.size	isTmin, .-isTmin
	.globl	isZero
	.type	isZero, @function
isZero:
.LFB56:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	cmpl	$0, -4(%rbp)
	sete	%al
	movzbl	%al, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE56:
	.size	isZero, .-isZero
	.globl	leastBitPos
	.type	leastBitPos, @function
leastBitPos:
.LFB57:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	cmpl	$0, -4(%rbp)
	jne	.L116
	movl	$0, %edx
	jmp	.L117
.L116:
	movl	$-1, %edx
.L117:
	movl	-4(%rbp), %eax
	subl	$1, %eax
	xorl	-4(%rbp), %eax
	andl	%edx, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE57:
	.size	leastBitPos, .-leastBitPos
	.globl	leftBitCount
	.type	leftBitCount, @function
leftBitCount:
.LFB58:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	$42, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE58:
	.size	leftBitCount, .-leftBitCount
	.globl	logicalNeg
	.type	logicalNeg, @function
logicalNeg:
.LFB59:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	$16, %eax
	orl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	$8, %eax
	orl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	$4, %eax
	orl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	$2, %eax
	orl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	%eax
	orl	%eax, -4(%rbp)
	andl	$1, -4(%rbp)
	movl	-4(%rbp), %eax
	xorl	$1, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE59:
	.size	logicalNeg, .-logicalNeg
	.globl	logicalShift
	.type	logicalShift, @function
logicalShift:
.LFB60:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	cmpl	$0, -24(%rbp)
	sete	%al
	movzbl	%al, %eax
	movl	%eax, -4(%rbp)
	movl	-24(%rbp), %eax
	movl	-20(%rbp), %edx
	movl	%eax, %ecx
	sarl	%cl, %edx
	movl	-4(%rbp), %eax
	negl	%eax
	movl	%eax, %esi
	movl	$32, %eax
	subl	-24(%rbp), %eax
	movl	$1, %edi
	movl	%eax, %ecx
	sall	%cl, %edi
	movl	%edi, %eax
	subl	$1, %eax
	orl	%esi, %eax
	andl	%edx, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE60:
	.size	logicalShift, .-logicalShift
	.globl	maximumOfTwo
	.type	maximumOfTwo, @function
maximumOfTwo:
.LFB61:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	$42, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE61:
	.size	maximumOfTwo, .-maximumOfTwo
	.globl	minimumOfTwo
	.type	minimumOfTwo, @function
minimumOfTwo:
.LFB62:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	$42, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE62:
	.size	minimumOfTwo, .-minimumOfTwo
	.globl	minusOne
	.type	minusOne, @function
minusOne:
.LFB63:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	$-1, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE63:
	.size	minusOne, .-minusOne
	.globl	multFiveEighths
	.type	multFiveEighths, @function
multFiveEighths:
.LFB64:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	$42, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE64:
	.size	multFiveEighths, .-multFiveEighths
	.globl	negate
	.type	negate, @function
negate:
.LFB65:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	negl	%eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE65:
	.size	negate, .-negate
	.globl	oddBits
	.type	oddBits, @function
oddBits:
.LFB66:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	$170, -4(%rbp)
	movl	-4(%rbp), %eax
	sall	$8, %eax
	orl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	sall	$16, %eax
	orl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE66:
	.size	oddBits, .-oddBits
	.globl	remainderPower2
	.type	remainderPower2, @function
remainderPower2:
.LFB67:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	shrl	$31, %eax
	movl	%eax, -16(%rbp)
	movl	-24(%rbp), %eax
	addl	$1, %eax
	movl	$1, %edx
	movl	%eax, %ecx
	sall	%cl, %edx
	movl	%edx, %eax
	subl	$1, %eax
	movl	%eax, -12(%rbp)
	movl	-20(%rbp), %eax
	sarl	$31, %eax
	movl	%eax, -8(%rbp)
	movl	-20(%rbp), %eax
	xorl	-8(%rbp), %eax
	subl	-8(%rbp), %eax
	movl	%eax, -20(%rbp)
	movl	-20(%rbp), %eax
	andl	-12(%rbp), %eax
	movl	%eax, -4(%rbp)
	movl	-16(%rbp), %eax
	negl	%eax
	andl	-4(%rbp), %eax
	movl	-16(%rbp), %edx
	leal	-1(%rdx), %ecx
	movl	-4(%rbp), %edx
	negl	%edx
	andl	%ecx, %edx
	orl	%edx, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE67:
	.size	remainderPower2, .-remainderPower2
	.globl	replaceByte
	.type	replaceByte, @function
replaceByte:
.LFB68:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	%edx, -12(%rbp)
	movl	$42, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE68:
	.size	replaceByte, .-replaceByte
	.globl	rotateLeft
	.type	rotateLeft, @function
rotateLeft:
.LFB69:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-24(%rbp), %eax
	movl	$1, %edx
	movl	%eax, %ecx
	sall	%cl, %edx
	movl	%edx, %eax
	leal	-1(%rax), %edx
	movl	$32, %eax
	subl	-24(%rbp), %eax
	movl	%eax, %ecx
	sall	%cl, %edx
	movl	%edx, %eax
	movl	%eax, -8(%rbp)
	movl	-20(%rbp), %eax
	andl	%eax, -8(%rbp)
	cmpl	$0, -24(%rbp)
	sete	%al
	movzbl	%al, %eax
	movl	%eax, -4(%rbp)
	movl	$32, %eax
	subl	-24(%rbp), %eax
	movl	-8(%rbp), %edx
	movl	%eax, %ecx
	sarl	%cl, %edx
	movl	-4(%rbp), %eax
	negl	%eax
	movl	%eax, %esi
	movl	-24(%rbp), %eax
	movl	$1, %edi
	movl	%eax, %ecx
	sall	%cl, %edi
	movl	%edi, %eax
	subl	$1, %eax
	orl	%esi, %eax
	andl	%edx, %eax
	movl	%eax, -8(%rbp)
	movl	-24(%rbp), %eax
	movl	-20(%rbp), %edx
	movl	%eax, %ecx
	sall	%cl, %edx
	movl	%edx, %eax
	orl	-8(%rbp), %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE69:
	.size	rotateLeft, .-rotateLeft
	.globl	rotateRight
	.type	rotateRight, @function
rotateRight:
.LFB70:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-24(%rbp), %eax
	movl	$1, %edx
	movl	%eax, %ecx
	sall	%cl, %edx
	movl	%edx, %eax
	subl	$1, %eax
	movl	%eax, -8(%rbp)
	movl	-8(%rbp), %eax
	andl	-20(%rbp), %eax
	movl	%eax, %edx
	movl	$32, %eax
	subl	-24(%rbp), %eax
	movl	%eax, %ecx
	sall	%cl, %edx
	movl	%edx, %eax
	movl	%eax, -8(%rbp)
	cmpl	$0, -24(%rbp)
	sete	%al
	movzbl	%al, %eax
	movl	%eax, -4(%rbp)
	movl	-24(%rbp), %eax
	movl	-20(%rbp), %edx
	movl	%eax, %ecx
	sarl	%cl, %edx
	movl	-4(%rbp), %eax
	negl	%eax
	movl	%eax, %esi
	movl	$32, %eax
	subl	-24(%rbp), %eax
	movl	$1, %edi
	movl	%eax, %ecx
	sall	%cl, %edi
	movl	%edi, %eax
	subl	$1, %eax
	orl	%esi, %eax
	andl	%edx, %eax
	movl	%eax, -20(%rbp)
	movl	-20(%rbp), %eax
	orl	-8(%rbp), %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE70:
	.size	rotateRight, .-rotateRight
	.globl	satAdd
	.type	satAdd, @function
satAdd:
.LFB71:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	$42, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE71:
	.size	satAdd, .-satAdd
	.globl	satMul2
	.type	satMul2, @function
satMul2:
.LFB72:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	$42, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE72:
	.size	satMul2, .-satMul2
	.globl	satMul3
	.type	satMul3, @function
satMul3:
.LFB73:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	leal	(%rax,%rax), %edx
	movl	-4(%rbp), %eax
	addl	%edx, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE73:
	.size	satMul3, .-satMul3
	.globl	sign
	.type	sign, @function
sign:
.LFB74:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	shrl	$31, %eax
	movl	%eax, -4(%rbp)
	cmpl	$0, -20(%rbp)
	setne	%dl
	cmpl	$0, -4(%rbp)
	sete	%al
	andl	%edx, %eax
	movzbl	%al, %eax
	movl	-4(%rbp), %edx
	negl	%edx
	orl	%edx, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE74:
	.size	sign, .-sign
	.globl	signMag2TwosComp
	.type	signMag2TwosComp, @function
signMag2TwosComp:
.LFB75:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	$42, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE75:
	.size	signMag2TwosComp, .-signMag2TwosComp
	.globl	specialBits
	.type	specialBits, @function
specialBits:
.LFB76:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	$-3522561, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE76:
	.size	specialBits, .-specialBits
	.globl	subtractionOK
	.type	subtractionOK, @function
subtractionOK:
.LFB77:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	$0, -12(%rbp)
	movl	$0, -8(%rbp)
	movl	$0, -4(%rbp)
	movl	-12(%rbp), %eax
	cmpl	-8(%rbp), %eax
	sete	%al
	movzbl	%al, %edx
	movl	-12(%rbp), %eax
	xorl	-4(%rbp), %eax
	orl	%edx, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE77:
	.size	subtractionOK, .-subtractionOK
	.globl	thirdBits
	.type	thirdBits, @function
thirdBits:
.LFB78:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	$585, -4(%rbp)
	movl	-4(%rbp), %eax
	sall	$12, %eax
	orl	-4(%rbp), %eax
	movl	-4(%rbp), %edx
	sall	$24, %edx
	orl	%edx, %eax
	movl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE78:
	.size	thirdBits, .-thirdBits
	.globl	tmax
	.type	tmax, @function
tmax:
.LFB79:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	$2147483647, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE79:
	.size	tmax, .-tmax
	.globl	tmin
	.type	tmin, @function
tmin:
.LFB80:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	$-2147483648, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE80:
	.size	tmin, .-tmin
	.globl	trueFiveEighths
	.type	trueFiveEighths, @function
trueFiveEighths:
.LFB81:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	$42, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE81:
	.size	trueFiveEighths, .-trueFiveEighths
	.globl	trueThreeFourths
	.type	trueThreeFourths, @function
trueThreeFourths:
.LFB82:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	shrl	$31, %eax
	movl	%eax, -8(%rbp)
	movl	-20(%rbp), %eax
	andl	$3, %eax
	movl	%eax, -4(%rbp)
	sarl	$2, -20(%rbp)
	movl	-20(%rbp), %eax
	addl	%eax, %eax
	addl	%eax, -20(%rbp)
	movl	-4(%rbp), %eax
	addl	%eax, %eax
	addl	%eax, -4(%rbp)
	sarl	$2, -4(%rbp)
	movl	-20(%rbp), %eax
	leal	3(%rax), %edx
	testl	%eax, %eax
	cmovs	%edx, %eax
	sarl	$2, %eax
	movl	%eax, %edx
	movl	%edx, %eax
	addl	%eax, %eax
	leal	(%rax,%rdx), %ecx
	movl	-8(%rbp), %eax
	negl	%eax
	movl	%eax, %esi
	movl	-4(%rbp), %edx
	movl	%edx, %eax
	addl	%eax, %eax
	addl	%edx, %eax
	imull	%esi, %eax
	leal	3(%rax), %edx
	testl	%eax, %eax
	cmovs	%edx, %eax
	sarl	$2, %eax
	addl	%ecx, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE82:
	.size	trueThreeFourths, .-trueThreeFourths
	.globl	twosComp2SignMag
	.type	twosComp2SignMag, @function
twosComp2SignMag:
.LFB83:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	$42, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE83:
	.size	twosComp2SignMag, .-twosComp2SignMag
	.globl	upperBits
	.type	upperBits, @function
upperBits:
.LFB84:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	$42, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE84:
	.size	upperBits, .-upperBits
	.ident	"GCC: (Ubuntu 7.3.0-27ubuntu1~18.04) 7.3.0"
	.section	.note.GNU-stack,"",@progbits
