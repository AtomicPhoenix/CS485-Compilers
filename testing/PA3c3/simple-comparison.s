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
	movq	8(%rsp), %rax
	movl	$2147483648, %edx
	movl	$4294967295, %ecx
	addq	%rax, %rdx
	cmpq	%rdx, %rcx
	movl	$0, %edx
	cmovb	%rdx, %rax
	movq	%rax, 24(%rbx)
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
	subq	$8, %rsp
	movq	24(%rsi), %rsi
	movq	%rdi, %rbx
	xorl	%eax, %eax
	movq	$percent.d, %rdi
	call	printf
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	ret
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.p2align 4
	.globl	Main.main
	.type	Main.main, @function
Main.main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$224, %rsp
#;comment start
	#Comment start
#start
	#Comment end
#;label Main_main_0
	#Label
Main_main_0:
#;t$1 <- default Bool
	#Let No Init start
	pushq	%rbp
	pushq	%rax
	call	Bool..new
	movq	%rax, %r10
	popq	%rax
	popq	%rbp
	movq	%r10, -0(%rbp)
	#Let No Init end
#;t$2 <- default Bool
	#Let No Init start
	pushq	%rbp
	pushq	%rax
	call	Bool..new
	movq	%rax, %r10
	popq	%rax
	popq	%rbp
	movq	%r10, -8(%rbp)
	#Let No Init end
#;t$3 <- default Bool
	#Let No Init start
	pushq	%rbp
	pushq	%rax
	call	Bool..new
	movq	%rax, %r10
	popq	%rax
	popq	%rbp
	movq	%r10, -16(%rbp)
	#Let No Init end
#;t$1 <- bool true
	#bconst start
	call	Bool..new
	movq	$1, 24(%rax)
	movq	%rax, -0(%rbp)
	#bconst end
#;t$6 <- t$1
	#Assignment start
	movq	-0(%rbp), %rax
	movq	%rax, -24(%rbp)
	#Assignment end
#;t$2 <- bool false
	#bconst start
	call	Bool..new
	movq	$0, 24(%rax)
	movq	%rax, -8(%rbp)
	#bconst end
#;t$9 <- t$2
	#Assignment start
	movq	-8(%rbp), %rax
	movq	%rax, -32(%rbp)
	#Assignment end
#;t$3 <- bool true
	#bconst start
	call	Bool..new
	movq	$1, 24(%rax)
	movq	%rax, -16(%rbp)
	#bconst end
#;t$12 <- t$3
	#Assignment start
	movq	-16(%rbp), %rax
	movq	%rax, -40(%rbp)
	#Assignment end
#;t$14 <- t$1
	#Ident Expr start
	movq	-0(%rbp), %rax
	movq	%rax, -48(%rbp)
	#Ident Expr end
#;t$17 <- not t$14
	#Not start
	movq	-48(%rbp), %rax
	movq	24(%rax), %rax
	testq	%rax, %rax
	movl	$1, %eax
	movl	$0, %edx
	cmovel	%eax, %edx
	pushq	%rbp
	pushq	%rdx
	call	Bool..new
	popq	%rdx
	popq	%rbp
	movq	%rdx, 24(%rax)
	movq	%rax, -56(%rbp)
	#Not end
#;bt t$17 main_Main_2
	#Branch True start
	movq	-56(%rbp), %rax
	movq	24(%rax), %rax
	testq	%rax, %rax
	jne	main_Main_2
	#Branch True end
#;comment then branch
	#Comment start
#then branch
	#Comment end
#;label main_Main_1
	#Label
main_Main_1:
#;t$15 <- int 1
	#iconst start
	call	Int..new
	movq	$1, 24(%rax)
	movq	%rax, -64(%rbp)
	#iconst end
#;t$13 <- call out_int t$15
	#Call w/ args start
	pushq	%rax
	pushq	%rbx
	pushq	%rbp
	pushq	%rdi
	pushq	%rsi
	pushq	%rcx
	pushq	%rdx
	pushq	%r8
	pushq	%r9
	pushq	%r10
	pushq	%r11
	pushq	%r12
	pushq	%r13
	pushq	%r14
	movq	-64(%rbp), %rsi
	call	IO.out_int
	movq	%rax, -72(%rbp)
	popq	%r14
	popq	%r13
	popq	%r12
	popq	%r11
	popq	%r10
	popq	%r9
	popq	%r8
	popq	%rdx
	popq	%rcx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%rbx
	popq	%rax
	#Call w/ args end
#;jmp main_Main_3
	#Jump
	jmp	main_Main_3
#;comment else branch
	#Comment start
#else branch
	#Comment end
#;label main_Main_2
	#Label
main_Main_2:
#;t$16 <- int 0
	#iconst start
	call	Int..new
	movq	$0, 24(%rax)
	movq	%rax, -80(%rbp)
	#iconst end
#;t$13 <- call out_int t$16
	#Call w/ args start
	pushq	%rax
	pushq	%rbx
	pushq	%rbp
	pushq	%rdi
	pushq	%rsi
	pushq	%rcx
	pushq	%rdx
	pushq	%r8
	pushq	%r9
	pushq	%r10
	pushq	%r11
	pushq	%r12
	pushq	%r13
	pushq	%r14
	movq	-80(%rbp), %rsi
	call	IO.out_int
	movq	%rax, -72(%rbp)
	popq	%r14
	popq	%r13
	popq	%r12
	popq	%r11
	popq	%r10
	popq	%r9
	popq	%r8
	popq	%rdx
	popq	%rcx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%rbx
	popq	%rax
	#Call w/ args end
#;jmp main_Main_3
	#Jump
	jmp	main_Main_3
#;comment if-join
	#Comment start
#if-join
	#Comment end
#;label main_Main_3
	#Label
main_Main_3:
#;t$19 <- t$2
	#Ident Expr start
	movq	-8(%rbp), %rax
	movq	%rax, -88(%rbp)
	#Ident Expr end
#;t$22 <- not t$19
	#Not start
	movq	-88(%rbp), %rax
	movq	24(%rax), %rax
	testq	%rax, %rax
	movl	$1, %eax
	movl	$0, %edx
	cmovel	%eax, %edx
	pushq	%rbp
	pushq	%rdx
	call	Bool..new
	popq	%rdx
	popq	%rbp
	movq	%rdx, 24(%rax)
	movq	%rax, -96(%rbp)
	#Not end
#;bt t$22 main_Main_5
	#Branch True start
	movq	-96(%rbp), %rax
	movq	24(%rax), %rax
	testq	%rax, %rax
	jne	main_Main_5
	#Branch True end
#;comment then branch
	#Comment start
#then branch
	#Comment end
#;label main_Main_4
	#Label
main_Main_4:
#;t$20 <- int 1
	#iconst start
	call	Int..new
	movq	$1, 24(%rax)
	movq	%rax, -104(%rbp)
	#iconst end
#;t$18 <- call out_int t$20
	#Call w/ args start
	pushq	%rax
	pushq	%rbx
	pushq	%rbp
	pushq	%rdi
	pushq	%rsi
	pushq	%rcx
	pushq	%rdx
	pushq	%r8
	pushq	%r9
	pushq	%r10
	pushq	%r11
	pushq	%r12
	pushq	%r13
	pushq	%r14
	movq	-104(%rbp), %rsi
	call	IO.out_int
	movq	%rax, -112(%rbp)
	popq	%r14
	popq	%r13
	popq	%r12
	popq	%r11
	popq	%r10
	popq	%r9
	popq	%r8
	popq	%rdx
	popq	%rcx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%rbx
	popq	%rax
	#Call w/ args end
#;jmp main_Main_6
	#Jump
	jmp	main_Main_6
#;comment else branch
	#Comment start
#else branch
	#Comment end
#;label main_Main_5
	#Label
main_Main_5:
#;t$21 <- int 0
	#iconst start
	call	Int..new
	movq	$0, 24(%rax)
	movq	%rax, -120(%rbp)
	#iconst end
#;t$18 <- call out_int t$21
	#Call w/ args start
	pushq	%rax
	pushq	%rbx
	pushq	%rbp
	pushq	%rdi
	pushq	%rsi
	pushq	%rcx
	pushq	%rdx
	pushq	%r8
	pushq	%r9
	pushq	%r10
	pushq	%r11
	pushq	%r12
	pushq	%r13
	pushq	%r14
	movq	-120(%rbp), %rsi
	call	IO.out_int
	movq	%rax, -112(%rbp)
	popq	%r14
	popq	%r13
	popq	%r12
	popq	%r11
	popq	%r10
	popq	%r9
	popq	%r8
	popq	%rdx
	popq	%rcx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%rbx
	popq	%rax
	#Call w/ args end
#;jmp main_Main_6
	#Jump
	jmp	main_Main_6
#;comment if-join
	#Comment start
#if-join
	#Comment end
#;label main_Main_6
	#Label
main_Main_6:
#;t$23 <- t$3
	#Ident Expr start
	movq	-16(%rbp), %rax
	movq	%rax, -128(%rbp)
	#Ident Expr end
#;t$26 <- not t$23
	#Not start
	movq	-128(%rbp), %rax
	movq	24(%rax), %rax
	testq	%rax, %rax
	movl	$1, %eax
	movl	$0, %edx
	cmovel	%eax, %edx
	pushq	%rbp
	pushq	%rdx
	call	Bool..new
	popq	%rdx
	popq	%rbp
	movq	%rdx, 24(%rax)
	movq	%rax, -136(%rbp)
	#Not end
#;bt t$26 main_Main_8
	#Branch True start
	movq	-136(%rbp), %rax
	movq	24(%rax), %rax
	testq	%rax, %rax
	jne	main_Main_8
	#Branch True end
#;comment then branch
	#Comment start
#then branch
	#Comment end
#;label main_Main_7
	#Label
main_Main_7:
#;t$24 <- int 1
	#iconst start
	call	Int..new
	movq	$1, 24(%rax)
	movq	%rax, -144(%rbp)
	#iconst end
#;t$0 <- call out_int t$24
	#Call w/ args start
	pushq	%rax
	pushq	%rbx
	pushq	%rbp
	pushq	%rdi
	pushq	%rsi
	pushq	%rcx
	pushq	%rdx
	pushq	%r8
	pushq	%r9
	pushq	%r10
	pushq	%r11
	pushq	%r12
	pushq	%r13
	pushq	%r14
	movq	-144(%rbp), %rsi
	call	IO.out_int
	movq	%rax, -152(%rbp)
	popq	%r14
	popq	%r13
	popq	%r12
	popq	%r11
	popq	%r10
	popq	%r9
	popq	%r8
	popq	%rdx
	popq	%rcx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%rbx
	popq	%rax
	#Call w/ args end
#;jmp main_Main_9
	#Jump
	jmp	main_Main_9
#;comment else branch
	#Comment start
#else branch
	#Comment end
#;label main_Main_8
	#Label
main_Main_8:
#;t$25 <- int 0
	#iconst start
	call	Int..new
	movq	$0, 24(%rax)
	movq	%rax, -160(%rbp)
	#iconst end
#;t$0 <- call out_int t$25
	#Call w/ args start
	pushq	%rax
	pushq	%rbx
	pushq	%rbp
	pushq	%rdi
	pushq	%rsi
	pushq	%rcx
	pushq	%rdx
	pushq	%r8
	pushq	%r9
	pushq	%r10
	pushq	%r11
	pushq	%r12
	pushq	%r13
	pushq	%r14
	movq	-160(%rbp), %rsi
	call	IO.out_int
	movq	%rax, -152(%rbp)
	popq	%r14
	popq	%r13
	popq	%r12
	popq	%r11
	popq	%r10
	popq	%r9
	popq	%r8
	popq	%rdx
	popq	%rcx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%rbx
	popq	%rax
	#Call w/ args end
#;jmp main_Main_9
	#Jump
	jmp	main_Main_9
#;comment if-join
	#Comment start
#if-join
	#Comment end
#;label main_Main_9
	#Label
main_Main_9:
#;return t$0
	#Return start
	jmp	.main.end
	#Return end
.main.end:
	addq	$224, %rsp
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
