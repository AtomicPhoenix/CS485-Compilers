.globl Bool..vtable
Bool..vtable:
	.quad string0
	.quad Bool..new
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl IO..vtable
IO..vtable:
	.quad string1
	.quad IO..new
	.quad IO.in_int
	.quad IO.out_int
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Int..vtable
Int..vtable:
	.quad string2
	.quad Int..new
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Object..vtable
Object..vtable:
	.quad string3
	.quad Object..new
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl String..vtable
String..vtable:
	.quad string4
	.quad String..new
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.p2align 4
	.globl	Bool..new
	.type	Bool..new, @function
Bool..new:
	subq	$8, %rsp
	movl	$4, %esi
	movl	$8, %edi
	call	calloc
	#Set class tag, object size, vtable pointer
	movq	$0, (%rax)
	movq	$4, 8(%rax)
	movq	$Bool..vtable, %r10
	movq	%r10, 16(%rax)
	movq	$0, 24(%rax)
	addq	$8, %rsp
	ret
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.p2align 4
	.globl	IO..new
	.type	IO..new, @function
IO..new:
	subq	$8, %rsp
	movl	$3, %esi
	movl	$8, %edi
	call	calloc
	#Set class tag, object size, vtable pointer
	movq	$8, (%rax)
	movq	$3, 8(%rax)
	movq	$IO..vtable, %r10
	movq	%r10, 16(%rax)
	addq	$8, %rsp
	ret
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.p2align 4
	.globl	Int..new
	.type	Int..new, @function
Int..new:
	subq	$8, %rsp
	movl	$4, %esi
	movl	$8, %edi
	call	calloc
	#Set class tag, object size, vtable pointer
	movq	$1, (%rax)
	movq	$4, 8(%rax)
	movq	$Int..vtable, %r10
	movq	%r10, 16(%rax)
	movq	$0, 24(%rax)
	addq	$8, %rsp
	ret
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.p2align 4
	.globl	Object..new
	.type	Object..new, @function
Object..new:
	subq	$8, %rsp
	movl	$3, %esi
	movl	$8, %edi
	call	calloc
	#Set class tag, object size, vtable pointer
	movq	$7, (%rax)
	movq	$3, 8(%rax)
	movq	$Object..vtable, %r10
	movq	%r10, 16(%rax)
	addq	$8, %rsp
	ret
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.p2align 4
	.globl	String..new
	.type	String..new, @function
String..new:
	subq	$8, %rsp
	movl	$4, %esi
	movl	$8, %edi
	call	calloc
	#Set class tag, object size, vtable pointer
	movq	$3, (%rax)
	movq	$4, 8(%rax)
	movq	$String..vtable, %r10
	movq	%r10, 16(%rax)
	movq	$empty.string, %r10
	movq	%r10, 24(%rax)
	addq	$8, %rsp
	ret
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.p2align 4
	.globl	IO.in_int
	.type	IO.in_int, @function
IO.in_int:
	pushq	%rbp
	pushq	%rbx
	subq	$4120, %rsp
	call	Int..new
	leaq	16(%rsp), %rbp
	movl	$4096, %esi
	movq	stdin(%rip), %rdx
	movq	%rbp, %rdi
	movq	%rax, %rbx
	call	fgets
	leaq	8(%rsp), %rdx
	movq	%rbp, %rdi
	xorl	%eax, %eax
	movq	$percent.ld, %rsi
	call	sscanf
	movq	8(%rsp), %rcx
	movl	$2147483648, %edx
	addq	%rcx, %rdx
	shrq	$32, %rdx
	jne	.in_int_zero
	testl	%eax, %eax
	jg	.in_int_nonzero
.in_int_zero:
	xorl	%ecx, %ecx
.in_int_nonzero:
	movq	%rcx, 24(%rbx)
	addq	$4120, %rsp
	movq	%rbx, %rax
	popq	%rbx
	popq	%rbp
	ret
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.p2align 4
	.globl	IO.out_int
	.type	IO.out_int, @function
IO.out_int:
	pushq	%rbx
	movq	24(%rsi), %rsi
	movq	%rdi, %rbx
	xorl	%eax, %eax
	movq	$percent.d, %rdi
	call	printf
	movq	%rbx, %rax
	popq	%rbx
	ret
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.p2align 4
	.globl	Main.main
	.type	Main.main, @function
Main.main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
#;comment start
	#Comment start
#start
	#Comment end
#;label Main_main_0
	#Label
Main_main_0:
#;t$4 <- int 1
	#iconst start
	call	Int..new
	movq	$1, 24(%rax)
	movq	%rax, -0(%rbp)
	#iconst end
#;t$5 <- int 3
	#iconst start
	call	Int..new
	movq	$3, 24(%rax)
	movq	%rax, -8(%rbp)
	#iconst end
#;t$2 <- + t$4 t$5
	#Plus start
	movq	-0(%rbp), %rax
	movq	24(%rax), %rax
	movq	-8(%rbp), %rdx
	movq	24(%rdx), %rdx
	addl	%edx, %eax
	pushq	%rbp
	pushq	%rax
	call	Int..new
	movq	%rax, %r10
	popq	%rax
	popq	%rbp
	movq	%rax, 24(%r10)
	movq	%r10, -16(%rbp)
	#Plus end
#;t$6 <- int 5
	#iconst start
	call	Int..new
	movq	$5, 24(%rax)
	movq	%rax, -24(%rbp)
	#iconst end
#;t$7 <- int 7
	#iconst start
	call	Int..new
	movq	$7, 24(%rax)
	movq	%rax, -32(%rbp)
	#iconst end
#;t$3 <- + t$6 t$7
	#Plus start
	movq	-24(%rbp), %rax
	movq	24(%rax), %rax
	movq	-32(%rbp), %rdx
	movq	24(%rdx), %rdx
	addl	%edx, %eax
	pushq	%rbp
	pushq	%rax
	call	Int..new
	movq	%rax, %r10
	popq	%rax
	popq	%rbp
	movq	%rax, 24(%r10)
	movq	%r10, -40(%rbp)
	#Plus end
#;t$1 <- + t$2 t$3
	#Plus start
	movq	-16(%rbp), %rax
	movq	24(%rax), %rax
	movq	-40(%rbp), %rdx
	movq	24(%rdx), %rdx
	addl	%edx, %eax
	pushq	%rbp
	pushq	%rax
	call	Int..new
	movq	%rax, %r10
	popq	%rax
	popq	%rbp
	movq	%rax, 24(%r10)
	movq	%r10, -48(%rbp)
	#Plus end
#;t$0 <- call out_int t$1
	#Call w/ args start
	movq	-48(%rbp), %rsi
	call	IO.out_int
	movq	%rax, -56(%rbp)
	#Call w/ args end
#;return t$0
	#Return start
	jmp	.main.end
	#Return end
.main.end:
	addq	$64, %rsp
	popq	%rbp
	ret
	.section	.rodata
string1:
	.string	"IO"
string3:
	.string	"Object"
string4:
	.string	"String"
string0:
	.string	"Bool"
string2:
	.string	"Int"
	.globl empty.string
empty.string:
	.string	""
	.globl percent.ld
percent.ld:
	.string	"%ld"
	.globl percent.d
percent.d:
	.string	"%d"
	.text
lt_handler:
	pushq	%r12
	movq	%rsi, %r12
	pushq	%rbp
	pushq	%rbx
	movq	%rdi, %rbx
	call	Bool..new
	movq	%rax, %rbp
	testq	%rbx, %rbx
	je	.lt_false
	testq	%r12, %r12
	je	.lt_false
	movq	(%r12), %rdx
	addq	(%rbx), %rdx
	testq	$-3, %rdx
	je	.lt_num
	xorl	%eax, %eax
	cmpq	$6, %rdx
	je	.lt_string
.lt_cleanup:
	movq	%rax, 24(%rbp)
	movq	%rbp, %rax
	popq	%rbx
	popq	%rbp
	popq	%r12
	ret
		.p2align 4,,10
		.p2align 3
.lt_num:
	movq	24(%r12), %rax
	cmpq	%rax, 24(%rbx)
	setl	%al
	movzbl	%al, %eax
	movq	%rax, 24(%rbp)
	movq	%rbp, %rax
	popq	%rbx
	popq	%rbp
	popq	%r12
	ret
		.p2align 4,,10
		.p2align 3
.lt_false:
	xorl	%eax, %eax
	movq	%rax, 24(%rbp)
	movq	%rbp, %rax
	popq	%rbx
	popq	%rbp
	popq	%r12
	ret
		.p2align 4,,10
		.p2align 3
.lt_string:
	movq	24(%r12), %rsi
	movq	24(%rbx), %rdi
	call	strcmp
	shrl	$31, %eax
	jmp	.lt_cleanup
		.size	lt_handler, .-lt_handler
		.p2align 4
		.globl	le_handler
		.type	le_handler, @function
le_handler:
	pushq	%r12
	pushq	%rbp
	movq	%rsi, %rbp
	pushq	%rbx
	movq	%rdi, %rbx
	call	Bool..new
	movq	%rax, %r12
	testq	%rbx, %rbx
	je	.le_false
	testq	%rbp, %rbp
	je	.le_false
	movq	0(%rbp), %rax
	addq	(%rbx), %rax
	testq	$-3, %rax
	je	.le_num
	xorl	%edx, %edx
	cmpq	%rbp, %rbx
	sete	%dl
	cmpq	$6, %rax
	je	.le_string
.le_cleanup:
	movq	%rdx, 24(%r12)
	movq	%r12, %rax
	popq	%rbx
	popq	%rbp
	popq	%r12
	ret
		.p2align 4,,10
		.p2align 3
.le_num:
	movq	24(%rbp), %rax
	xorl	%edx, %edx
	cmpq	%rax, 24(%rbx)
	movq	%r12, %rax
	setle	%dl
	movq	%rdx, 24(%r12)
	popq	%rbx
	popq	%rbp
	popq	%r12
	ret
		.p2align 4,,10
		.p2align 3
.le_false:
	xorl	%edx, %edx
	movq	%r12, %rax
	movq	%rdx, 24(%r12)
	popq	%rbx
	popq	%rbp
	popq	%r12
	ret
		.p2align 4,,10
		.p2align 3
.le_string:
	movq	24(%rbp), %rsi
	movq	24(%rbx), %rdi
	call	strcmp
	xorl	%edx, %edx
	testl	%eax, %eax
	setle	%dl
	jmp	.le_cleanup
		.size	le_handler, .-le_handler
		.p2align 4
		.globl	eq_handler
		.type	eq_handler, @function
eq_handler:
	pushq	%r12
	pushq	%rbp
	movq	%rsi, %rbp
	pushq	%rbx
	movq	%rdi, %rbx
	call	Bool..new
	movq	%rax, %r12
	testq	%rbx, %rbx
	je	.eq_false
	testq	%rbp, %rbp
	je	.eq_false
	movq	0(%rbp), %rax
	addq	(%rbx), %rax
	testq	$-3, %rax
	je	.eq_num
	xorl	%edx, %edx
	cmpq	%rbp, %rbx
	sete	%dl
	cmpq	$6, %rax
	je	.eq_string
.eq_cleanup:
	movq	%rdx, 24(%r12)
	movq	%r12, %rax
	popq	%rbx
	popq	%rbp
	popq	%r12
	ret
		.p2align 4,,10
		.p2align 3
.eq_num:
	movq	24(%rbp), %rax
	xorl	%edx, %edx
	cmpq	%rax, 24(%rbx)
	movq	%r12, %rax
	sete	%dl
	movq	%rdx, 24(%r12)
	popq	%rbx
	popq	%rbp
	popq	%r12
	ret
		.p2align 4,,10
		.p2align 3
.eq_false:
	xorl	%edx, %edx
	movq	%r12, %rax
	movq	%rdx, 24(%r12)
	popq	%rbx
	popq	%rbp
	popq	%r12
	ret
		.p2align 4,,10
		.p2align 3
.eq_string:
	movq	24(%rbp), %rsi
	movq	24(%rbx), %rdi
	call	strcmp
	xorl	%edx, %edx
	testl	%eax, %eax
	sete	%dl
	jmp	.eq_cleanup
		.size	eq_handler, .-eq_handler
	.globl start
start:
	.globl main
	.type main, @function
main:
	pushq	%rbp
	call	Main.main	
andq	$-16, %rsp
	xorq	%rdi, %rdi
	call	exit
