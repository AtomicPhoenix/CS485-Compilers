#; No vtable found for class Bool
#; No vtable found for class IO
#; No vtable found for class Int
#; No vtable found for class Object
#; No vtable found for class String
#;comment start
#;label Main_main_0
#;t$3 <- int 5
#;t$2 <- isvoid t$3
#;t$6 <- not t$2
#;bt t$6 main_Main_2
#;comment then branch
#;label main_Main_1
#;t$4 <- int 0
#;t$1 <- call out_int t$4
#;jmp main_Main_3
#;comment else branch
#;label main_Main_2
#;t$5 <- int 1
#;t$1 <- call out_int t$5
#;jmp main_Main_3
#;comment if-join
#;label main_Main_3
#;t$8 <- new A
#;t$7 <- isvoid t$8
#;t$11 <- not t$7
#;bt t$11 main_Main_5
#;comment then branch
#;label main_Main_4
#;t$9 <- int 0
#;t$0 <- call out_int t$9
#;jmp main_Main_6
#;comment else branch
#;label main_Main_5
#;t$10 <- int 1
#;t$0 <- call out_int t$10
#;jmp main_Main_6
#;comment if-join
#;label main_Main_6
#;return t$0
.globl Bool..vtable
Bool..vtable:
	.quad string0
	.quad Bool..new
	.quad Object.abort
	.quad Object.copy
	.quad Object.type_name
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl IO..vtable
IO..vtable:
	.quad string1
	.quad IO..new
	.quad Object.abort
	.quad Object.copy
	.quad Object.type_name
	.quad IO.in_int
	.quad IO.in_string
	.quad IO.out_int
	.quad IO.out_string
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Int..vtable
Int..vtable:
	.quad string2
	.quad Int..new
	.quad Object.abort
	.quad Object.copy
	.quad Object.type_name
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Object..vtable
Object..vtable:
	.quad string3
	.quad Object..new
	.quad Object.abort
	.quad Object.copy
	.quad Object.type_name
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl String..vtable
String..vtable:
	.quad string4
	.quad String..new
	.quad Object.abort
	.quad Object.copy
	.quad Object.type_name
	.quad String.concat
	.quad String.length
	.quad String.substr
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl A..vtable
A..vtable:
	.quad string9
	.quad Object.abort
	.quad Object.copy
	.quad Object.type_name
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Main..vtable
Main..vtable:
	.quad string10
	.quad Object.abort
	.quad Object.copy
	.quad Object.type_name
	.quad IO.in_int
	.quad IO.in_string
	.quad IO.out_int
	.quad IO.out_string
	.quad Main.main
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
	.globl	A..new
	.type	A..new, @function
A..new:
	subq	$8, %rsp
	movl	$0, %esi
	movl	$8, %edi
	call	calloc
	#Set class tag, object size, vtable pointer
	movq	$9, (%rax)
	movq	$0, 8(%rax)
	movq	$A..vtable, 16(%rax)
	addq	$8, %rsp
	ret
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.p2align 4
	.globl	Main..new
	.type	Main..new, @function
Main..new:
	subq	$8, %rsp
	movl	$0, %esi
	movl	$8, %edi
	call	calloc
	#Set class tag, object size, vtable pointer
	movq	$9, (%rax)
	movq	$0, 8(%rax)
	movq	$Main..vtable, 16(%rax)
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
.globl	IO.in_string
IO.in_string:
	## method definition
	pushq	%rbp
	movq	%rsp, %rbp
	movq	16(%rbp), %r12
	## stack room for temporaries: 2
	movq	$16, %r14
	subq	%r14, %rsp
	## return address handling
	## method body begins
	## new String
	pushq	%rbp
	pushq	%r12
	movq	$String..new, %r14
	call	*%r14
	popq	%r12
	popq	%rbp
	movq	%r13, %r14
	## guarantee 16-byte alignment before call
	andq	$0xFFFFFFFFFFFFFFF0, %rsp
	call	coolgetstr
	movq	%rax, %r13
	movq	%r13, 24(%r14)
	movq	%r14, %r13
.globl	IO.in_string.end
IO.in_string.end:
	## method body ends
	## return address handling
	movq	%rbp, %rsp
	popq	%rbp
	ret
	## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.p2align 4
	.globl	IO.out_string
IO.out_string:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	16(%rbp), %r12
	movq $16, %r14
	subq %r14, %rsp
	movq 24(%rbp), %r14
	movq 24(%r14), %r13
	andq $0xFFFFFFFFFFFFFFF0, %rsp
	movq %r13, %rdi
	call cooloutstr
	movq %r12, %r13
	movq %rbp, %rsp
	popq %rbp
	ret
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.globl	cooloutstr
	.type	cooloutstr, @function
	cooloutstr:
	.LFB6:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movl	$0, -4(%rbp)
	jmp	.L2
	.L5:
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	movzbl	(%rax), %eax
	cmpb	$92, %al
	jne	.L3
	movl	-4(%rbp), %eax
	cltq
	leaq	1(%rax), %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	movzbl	(%rax), %eax
	cmpb	$110, %al
	jne	.L3
	movq	stdout(%rip), %rax
	movq	%rax, %rsi
	movl	$10, %edi
	call	fputc@PLT
	addl	$2, -4(%rbp)
	jmp	.L2
	.L3:
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	movzbl	(%rax), %eax
	cmpb	$92, %al
	jne	.L4
	movl	-4(%rbp), %eax
	cltq
	leaq	1(%rax), %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	movzbl	(%rax), %eax
	cmpb	$116, %al
	jne	.L4
	movq	stdout(%rip), %rax
	movq	%rax, %rsi
	movl	$9, %edi
	call	fputc@PLT
	addl	$2, -4(%rbp)
	jmp	.L2
	.L4:
	movq	stdout(%rip), %rdx
	movl	-4(%rbp), %eax
	movslq	%eax, %rcx
	movq	-24(%rbp), %rax
	addq	%rcx, %rax
	movzbl	(%rax), %eax
	movsbl	%al, %eax
	movq	%rdx, %rsi
	movl	%eax, %edi
	call	fputc@PLT
	addl	$1, -4(%rbp)
	.L2:
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	movzbl	(%rax), %eax
	testb	%al, %al
	jne	.L5
	movq	stdout(%rip), %rax
	movq	%rax, %rdi
	call	fflush@PLT
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
	.LFE6:
	.size	cooloutstr, .-cooloutstr
	.globl	coolstrlen
	.type	coolstrlen, @function
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.section	.rodata
.LC1:
	.string	""
	.text
	.globl	coolgetstr
	.type	coolgetstr, @function
coolgetstr:
.LFB9:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movl	$1, %esi
	movl	$40960, %edi
	call	calloc@PLT
	movq	%rax, -8(%rbp)
	movl	$0, -16(%rbp)
.L21:
	movq	stdin(%rip), %rax
	movq	%rax, %rdi
	call	fgetc@PLT
	movl	%eax, -12(%rbp)
	cmpl	$-1, -12(%rbp)
	je	.L15
	cmpl	$10, -12(%rbp)
	jne	.L16
.L15:
	cmpl	$0, -16(%rbp)
	je	.L17
	leaq	.LC1(%rip), %rax
	jmp	.L18
.L17:
	movq	-8(%rbp), %rax
	jmp	.L18
.L16:
	cmpl	$0, -12(%rbp)
	jne	.L19
	movl	$1, -16(%rbp)
	jmp	.L21
.L19:
	movq	-8(%rbp), %rax
	movq	%rax, %rdi
	call	coolstrlen
	movl	%eax, %edx
	movq	-8(%rbp), %rax
	addq	%rdx, %rax
	movl	-12(%rbp), %edx
	movb	%dl, (%rax)
	jmp	.L21
.L18:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE9:
	.size	coolgetstr, .-coolgetstr
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.section	.rodata
.LC0:
	.string	"%s%s"
	.text
	.globl	coolstrcat
	.type	coolstrcat, @function
coolstrcat:
.LFB8:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$40, %rsp
	.cfi_offset 3, -24
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	cmpq	$0, -40(%rbp)
	jne	.L11
	movq	-48(%rbp), %rax
	jmp	.L12
.L11:
	cmpq	$0, -48(%rbp)
	jne	.L13
	movq	-40(%rbp), %rax
	jmp	.L12
.L13:
	movq	-40(%rbp), %rax
	movq	%rax, %rdi
	call	coolstrlen
	movl	%eax, %ebx
	movq	-48(%rbp), %rax
	movq	%rax, %rdi
	call	coolstrlen
	addl	%ebx, %eax
	addl	$1, %eax
	movl	%eax, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	movl	$1, %esi
	movq	%rax, %rdi
	call	calloc@PLT
	movq	%rax, -24(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax, %rsi
	movq	-48(%rbp), %rcx
	movq	-40(%rbp), %rdx
	movq	-24(%rbp), %rax
	movq	%rcx, %r8
	movq	%rdx, %rcx
	leaq	.LC0(%rip), %rdx
	movq	%rax, %rdi
	movl	$0, %eax
	call	snprintf@PLT
	movq	-24(%rbp), %rax
.L12:
	movq	-8(%rbp), %rbx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE8:
	.size	coolstrcat, .-coolstrcat
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.globl	coolstrlen
	.type	coolstrlen, @function
coolstrlen:
.LFB7:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -24(%rbp)
	movl	$0, -4(%rbp)
	jmp	.L7
.L8:
	movl	-4(%rbp), %eax
	addl	$1, %eax
	movl	%eax, -4(%rbp)
.L7:
	movl	-4(%rbp), %eax
	movl	%eax, %edx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	movzbl	(%rax), %eax
	testb	%al, %al
	jne	.L8
	movl	-4(%rbp), %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE7:
	.size	coolstrlen, .-coolstrlen
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
coolsubstr:
.LFB10:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$48, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	%rdx, -40(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	coolstrlen
	movl	%eax, -4(%rbp)
	cmpq	$0, -32(%rbp)
	js	.L23
	cmpq	$0, -40(%rbp)
	js	.L23
	movq	-32(%rbp), %rdx
	movq	-40(%rbp), %rax
	addq	%rax, %rdx
	movl	-4(%rbp), %eax
	cltq
	cmpq	%rax, %rdx
	jle	.L24
.L23:
	movl	$0, %eax
	jmp	.L25
.L24:
	movq	-40(%rbp), %rax
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rdx
	addq	%rcx, %rdx
	movq	%rax, %rsi
	movq	%rdx, %rdi
	call	strndup@PLT
.L25:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE10:
	.size	coolsubstr, .-coolsubstr
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.globl	Object.abort
Object.abort:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	16(%rbp), %r12
	movq	$16, %r14
	subq	%r14, %rsp
	movq	$string6, %r13
	andq	$0xFFFFFFFFFFFFFFF0, %rsp
	movq	%r13, %rdi
	call	cooloutstr
	andq	$0xFFFFFFFFFFFFFFF0, %rsp
	movl	$0, %edi
	call	exit
Object.abort.end:
	movq	%rbp, %rsp
	popq	%rbp
	ret
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.p2align 4
	.globl	Object.copy
Object.copy:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	16(%rbp), %r12
	movq	$16, %r14
	subq	%r14, %rsp
	movq	8(%r12), %r14
	andq	$0xFFFFFFFFFFFFFFF0, %rsp
	movq	$8, %rsi
	movq	%r14, %rdi
	call	calloc
	movq	%rax, %r13
	pushq	%r13
	.globl	Object.copy.end
Object.copy.end:
	movq	%rbp, %rsp
	popq	%rbp
	ret
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Object.type_name:
	.globl	Object.type_name
	pushq	%rbp
	movq	%rsp, %rbp
	movq	16(%rbp), %r12
	movq	$16, %r14
	subq	%r14, %rsp
	pushq	%rbp
	pushq	%r12
	movq	$String..new, %r14
	call	*%r14
	popq	%r12
	popq	%rbp
	movq	16(%r12), %r14
	movq	0(%r14), %r14
	movq	%r14, 24(%r13)
Object.type_name.end:
	movq	%rbp, %rsp
	popq	%rbp
	ret
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl String.length
String.length:
## method definition
	pushq	%rbp
	movq	%rsp, %rbp
	movq	16(%rbp), %r12
## stack room for temporaries: 2
	movq	$16, %r14
	subq	%r14, %rsp
## return address handling
## method body begins
## new Int
	pushq	%rbp
	pushq	%r12
	movq	$Int..new, %r14
	call	*%r14
	popq	%r12
	popq	%rbp
	movq	%r13, %r14
	movq	24(%r12), %r13
## guarantee 16-byte alignment before call
	andq	$0xFFFFFFFFFFFFFFF0, %rsp
	movq	%r13, %rdi
	movl	$0, %eax
	call	coolstrlen
	movq	%rax, %r13
	movq	%r13, 24(%r14)
	movq	%r14, %r13
.globl String.length.end
String.length.end:
## method body ends
## return address handling
	movq	%rbp, %rsp
	popq	%rbp
	ret
## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl String.substr
String.substr:
## method definition
	pushq	%rbp
	movq	%rsp, %rbp
	movq	16(%rbp), %r12
## stack room for temporaries: 2
	movq	$16, %r14
	subq	%r14, %rsp
## return address handling
## fp[4] holds argument i (Int)
## fp[3] holds argument l (Int)
## method body begins
## new String
	pushq	%rbp
	pushq	%r12
	movq	$String..new, %r14
	call	*%r14
	popq	%r12
	popq	%rbp
	movq	%r13, %r15
	movq	24(%rbp), %r14
	movq	24(%r14), %r14
	movq	32(%rbp), %r13
	movq	24(%r13), %r13
	movq	24(%r12), %r12
## guarantee 16-byte alignment before call
	andq	$0xFFFFFFFFFFFFFFF0, %rsp
	movq	%r12, %rdi
	movq	%r13, %rsi
	movq	%r14, %rdx
	call	coolsubstr
	movq	%rax, %r13
	cmpq	$0, %r13
	jne	l6
	movq	$string7, %r13
## guarantee 16-byte alignment before call
	andq	$0xFFFFFFFFFFFFFFF0, %rsp
	movq	%r13, %rdi
	call	cooloutstr
## guarantee 16-byte alignment before call
	andq	$0xFFFFFFFFFFFFFFF0, %rsp
	movl	$0, %edi
	call	exit
.globl l6
l6:
	movq	%r13, 24(%r15)
	movq	%r15, %r13
.globl String.substr.end
String.substr.end:
## method body ends
## return address handling
	movq	%rbp, %rsp
	popq	%rbp
	ret
## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
## global string constants
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl String.concat
String.concat:
## method definition
	pushq	%rbp
	movq	%rsp, %rbp
	movq	16(%rbp), %r12
## stack room for temporaries: 2
	movq	$16, %r14
	subq	%r14, %rsp
## return address handling
## fp[3] holds argument s (String)
## method body begins
## new String
	pushq	%rbp
	pushq	%r12
	movq	$String..new, %r14
	call	*%r14
	popq	%r12
	popq	%rbp
	movq	%r13, %r15
	movq	24(%rbp), %r14
	movq	24(%r14), %r14
	movq	24(%r12), %r13
## guarantee 16-byte alignment before call
	andq	$0xFFFFFFFFFFFFFFF0, %rsp
	movq	%r13, %rdi
	movq	%r14, %rsi
	call	coolstrcat
	movq	%rax, %r13
	movq	%r13, 24(%r15)
	movq	%r15, %r13
.globl String.concat.end
String.concat.end:
## method body ends
## return address handling
	movq	%rbp, %rsp
	popq	%rbp
	ret
## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.p2align 4
	.globl	Main.main
	.type	Main.main, @function
Main.main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$96, %rsp
#;comment start
	#Comment start
#start
	#Comment end
#;label Main_main_0
	#Label
Main_main_0:
#;t$3 <- int 5
	#iconst start
	call	Int..new
	movq	$5, 24(%rax)
	movq	%rax, -0(%rbp)
	#iconst end
#;t$2 <- isvoid t$3
	cmpq	$0, %rax
	je	l2
	 #false branch of isvoid
	pushq	%rbp
	pushq	%r12
	movq	$Bool..new, %r14
	call	*%r14
	popq	%r12
	popq	%rbp
	jmp	l3
.globl l2
l2:
	 #true branch of isvoid
	pushq	%rbp
	pushq	%r12
	movq	$Bool..new, %r14
	call	*%r14
	popq	%r12
	popq	%rbp
	movq	$1, %r14
	movq	%r14, 24(%rax)
	jmp	l3
.globl l3
l3:
#;t$6 <- not t$2
	#Not start
	movq	-8(%rbp), %rax
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
	movq	%rax, -16(%rbp)
	#Not end
#;bt t$6 main_Main_2
	#Branch True start
	movq	-16(%rbp), %rax
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
#;t$4 <- int 0
	#iconst start
	call	Int..new
	movq	$0, 24(%rax)
	movq	%rax, -24(%rbp)
	#iconst end
#;t$1 <- call out_int t$4
	#Call w/ args start
	movq	-24(%rbp), %rsi
	call	IO.out_int
	movq	%rax, -32(%rbp)
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
#;t$5 <- int 1
	#iconst start
	call	Int..new
	movq	$1, 24(%rax)
	movq	%rax, -40(%rbp)
	#iconst end
#;t$1 <- call out_int t$5
	#Call w/ args start
	movq	-40(%rbp), %rsi
	call	IO.out_int
	movq	%rax, -32(%rbp)
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
#;t$8 <- new A
	pushq	%rbp
	pushq	%r12
	movq	$A..new, %r14
	call	*%r14
	popq	%r12
	popq	%rbp
#;t$7 <- isvoid t$8
	cmpq	$0, %rax
	je	l4
	 #false branch of isvoid
	pushq	%rbp
	pushq	%r12
	movq	$Bool..new, %r14
	call	*%r14
	popq	%r12
	popq	%rbp
	jmp	l5
.globl l4
l4:
	 #true branch of isvoid
	pushq	%rbp
	pushq	%r12
	movq	$Bool..new, %r14
	call	*%r14
	popq	%r12
	popq	%rbp
	movq	$1, %r14
	movq	%r14, 24(%rax)
	jmp	l5
.globl l5
l5:
#;t$11 <- not t$7
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
#;bt t$11 main_Main_5
	#Branch True start
	movq	-56(%rbp), %rax
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
#;t$9 <- int 0
	#iconst start
	call	Int..new
	movq	$0, 24(%rax)
	movq	%rax, -64(%rbp)
	#iconst end
#;t$0 <- call out_int t$9
	#Call w/ args start
	movq	-64(%rbp), %rsi
	call	IO.out_int
	movq	%rax, -72(%rbp)
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
#;t$10 <- int 1
	#iconst start
	call	Int..new
	movq	$1, 24(%rax)
	movq	%rax, -80(%rbp)
	#iconst end
#;t$0 <- call out_int t$10
	#Call w/ args start
	movq	-80(%rbp), %rsi
	call	IO.out_int
	movq	%rax, -72(%rbp)
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
#;return t$0
	#Return start
	jmp	.main.end
	#Return end
.main.end:
	addq	$96, %rsp
	popq	%rbp
	ret
	.section	.rodata
string1:
	.string	"IO"
string10:
	.string	"Main"
string3:
	.string	"Object"
string4:
	.string	"String"
string0:
	.string	"Bool"
string6:
	.string	"abort"
string9:
	.string	"A"
string2:
	.string	"Int"
string7:
	.string	"ERROR: 0: Exception: String.substr out of range\n"
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
