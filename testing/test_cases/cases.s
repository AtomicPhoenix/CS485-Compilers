#; #; #; #; #; #; t$1#; t$2#; #; t$3#; t$4#; t$0#; t$5#; t$6#; t$0#; #; t$0#; t$0#; t$7#; t$0#; t$0#; t$0#; t$8#; t$0#; t$0#; t$0#; t$0#;comment start
#;label Main_main_0
#;t$1 <- my_attribute
#;t$2 <- classId t$1
#Cmp $0, t$2 -> jump to main_Main_1
#;t$3 <- classId Int
#Cmp t$3, t$2 -> jump to main_Main_2
#;comment case-join
#;t$5 <- classId String
#Cmp t$5, t$2 -> jump to main_Main_3
#;comment case-join
#;jmp main_Main_4
#VoidCase: main_Main_1
#;label main_Main_2
#;t$7 <- t$1
#;t$0 <- call out_int t$7
#;jmp Main_main_join
#;label main_Main_3
#;t$8 <- t$1
#;t$0 <- call out_string t$8
#;jmp Main_main_join
#EmptyCase: main_Main_4
#;label Main_main_join
#;return t$0
.globl Bool..vtable
Bool..vtable:
	.quad .string0
	.quad Bool..new
	.quad Object.abort
	.quad Object.copy
	.quad Object.type_name
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl IO..vtable
IO..vtable:
	.quad .string1
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
	.quad .string2
	.quad Int..new
	.quad Object.abort
	.quad Object.copy
	.quad Object.type_name
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Object..vtable
Object..vtable:
	.quad .string3
	.quad Object..new
	.quad Object.abort
	.quad Object.copy
	.quad Object.type_name
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl String..vtable
String..vtable:
	.quad .string4
	.quad String..new
	.quad Object.abort
	.quad Object.copy
	.quad Object.type_name
	.quad String.concat
	.quad String.length
	.quad String.substr
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Main..vtable
Main..vtable:
	.quad .string11
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
	movq	$Bool..vtable, %r11
	movq	%r11, 16(%rax)
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
	movq	$1, (%rax)
	movq	$3, 8(%rax)
	movq	$IO..vtable, %r11
	movq	%r11, 16(%rax)
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
	movq	$2, (%rax)
	movq	$4, 8(%rax)
	movq	$Int..vtable, %r11
	movq	%r11, 16(%rax)
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
	movq	$3, (%rax)
	movq	$3, 8(%rax)
	movq	$Object..vtable, %r11
	movq	%r11, 16(%rax)
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
	movq	$4, (%rax)
	movq	$4, 8(%rax)
	movq	$String..vtable, %r11
	movq	%r11, 16(%rax)
	movq	$empty.string, %r11
	movq	%r11, 24(%rax)
	addq	$8, %rsp
	ret
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.p2align 4
	.globl	Main..new
	.type	Main..new, @function
Main..new:
## constructor for Main
	pushq	%rbp
	movq	%rsp, %rbp
	## stack room for temporaries: ?
	subq	$16, %rsp
	## return address handling
	movq	$4, %rax
	## guarantee 16-byte alignment before call
	andq	$0xFFFFFFFFFFFFFFF0, %rsp
	movq	$8, %rsi
	movq	%rax, %rdi
	call	calloc
	## store class tag, object size and vtable pointer
	movq	$9, 0(%rax)
	movq	$4, %r14
	movq	%r14, 8(%rax)
	movq	$Main..vtable, %r14
	movq	%r14, 16(%rax)
	## return address handling
	## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	## initialize attributes
	## self[3] holds field x (Int)
	## new Int
	pushq	%rax
	pushq	%rbp
	pushq	%r12
	movq	$Int..new, %r14
	call	*%r14
	movq	%rax, %r13
	popq	%r12
	popq	%rbp
	pushq	%rax
	movq	%r13, 24(%rax)
	pushq	%rax
	pushq	%r13
#;comment attr start
	#Comment start
#attr start
	#Comment end
#;t$0 <- int 5
	#iconst start
	call	Int..new
	movq	$5, 24(%rax)
	movq	%rax, -0(%rbp)
	#iconst end
	movq	-0(%rbp), %r13
	popq	%rax
	movq	%r13, 24(%rax)
	popq	%r13
	movq	%rbp, %rsp
	popq	%rbp
	ret
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.p2align 4
	.globl	IO.in_int
	.type	IO.in_int, @function
IO.in_int:
	pushq	%rbx
	subq	$32, %rsp
	movq	stdin(%rip), %rdx
	leaq	8(%rsp), %rdi
	leaq	16(%rsp), %rsi
	movq	$0, 8(%rsp)
	movq	$0, 16(%rsp)
	movq	$0, 24(%rsp)
	call	getline
	movq	8(%rsp), %rdi
	cmpq	$-1, %rax
	je	.in_int_string_error
	testq	%rdi, %rdi
	je	.in_int_string_error
.in_int_bounds_check:
	leaq	24(%rsp), %rsi
	movl	$10, %edx
	call	strtol
	movl	$4294967295, %edx
	movq	%rax, %rbx
	movl	$2147483648, %eax
	addq	%rbx, %rax
	cmpq	%rax, %rdx
	movl	$0, %eax
	cmovb	%rax, %rbx
	call	Bool..new
	movq	%rbx, 24(%rax)
	addq	$32, %rsp
	popq	%rbx
	ret
	.p2align 4,,10
	.p2align 3
.in_int_string_error:
	call	free
	movl	$1, %edi
	movl	$1, %esi
	call	calloc
	movq	%rax, 8(%rsp)
	movq	%rax, %rdi
	jmp	.in_int_bounds_check
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.p2align 4
	.globl	IO.out_int
	.type	IO.out_int, @function
IO.out_int:
	pushq	%rbx
	movl	24(%rsi), %esi
	movq	%rdi, %rbx
	xorl	%eax, %eax
	movl	$.percent.d, %edi
	call	printf
	movq	%rbx, %rax
	popq	%rbx
	ret
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.p2align 4
	.globl	IO.in_string
	.type	IO.in_string, @function
IO.in_string:
	pushq	%rbp
	pushq	%rbx
	subq	$24, %rsp
	movq	stdin(%rip), %rdx
	leaq	8(%rsp), %rsi
	movq	%rsp, %rdi
	movq	$0, (%rsp)
	movq	$0, 8(%rsp)
	call	getline
	movq	(%rsp), %rbp
	cmpq	$-1, %rax
	je	.in_string_null
	testq	%rbp, %rbp
	je	.in_string_null
	xorl	%esi, %esi
	movq	%rax, %rdx
	movq	%rbp, %rdi
	movq	%rax, %rbx
	call	memchr
	testq	%rax, %rax
	jne	.in_string_null
	leaq	-1(%rbp,%rbx), %rax
	cmpb	$10, (%rax)
	jne	.in_string_newline
	movb	$0, (%rax)
	movq	(%rsp), %rbp
.in_string_resize:
	movq	%rbx, %rsi
	movq	%rbp, %rdi
	call	realloc
.in_string_end:
	movq	%rax, (%rsp)
	call	String..new
	movq	24(%rax), %rdi
	movq	%rax, %rbx
	call	free
	movq	(%rsp), %rax
	movq	%rax, 24(%rbx)
	addq	$24, %rsp
	movq	%rbx, %rax
	popq	%rbx
	popq	%rbp
	ret
	.p2align 4,,10
	.p2align 3
.in_string_null:
	movq	%rbp, %rdi
	call	free
	movl	$1, %esi
	movl	$1, %edi
	call	calloc
	jmp	.in_string_end
	.p2align 4,,10
	.p2align 3
.in_string_newline:
	addq	$1, %rbx
	jmp	.in_string_resize
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.p2align 4
	.globl	IO.out_string
	.type	IO.out_string, @function
IO.out_string:
	pushq	%r12
	movq	%rdi, %r12
	pushq	%rbp
	pushq	%rbx
	movq	24(%rsi), %rbx
	movsbl	(%rbx), %edi
	testb	%dil, %dil
	jne	.L44
	jmp	.L57
	.p2align 4,,10
	.p2align 3
.L61:
	call	putchar
.L49:
	movsbl	1(%rbx), %edi
	addq	$1, %rbx
	testb	%dil, %dil
	je	.L57
.L44:
	cmpb	$92, %dil
	jne	.L61
	movzbl	1(%rbx), %ebp
	addq	$1, %rbx
	testb	%bpl, %bpl
	je	.L51
	cmpb	$110, %bpl
	je	.L62
	cmpb	$116, %bpl
	je	.L46
	movl	$92, %edi
	addq	$1, %rbx
	call	putchar
	movsbl	%bpl, %edi
	call	putchar
	movsbl	(%rbx), %edi
	testb	%dil, %dil
	jne	.L44
.L57:
	movq	%r12, %rax
	popq	%rbx
	popq	%rbp
	popq	%r12
	ret
	.p2align 4,,10
	.p2align 3
.L62:
	movl	$10, %edi
	call	putchar
	jmp	.L49
	.p2align 4,,10
	.p2align 3
.L46:
	movl	$9, %edi
	call	putchar
	jmp	.L49
	.p2align 4,,10
	.p2align 3
.L51:
	movl	$92, %edi
	call	putchar
	movq	%r12, %rax
	popq	%rbx
	popq	%rbp
	popq	%r12
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
	.p2align 4
	.globl	cool_error
	.type	cool_error, @function
cool_error:
	subq	$8, %rsp
	cmpq	$4, %rdi
	ja	.error_exit
	jmp	*.jump_table(,%rdi,8)
	.section	.rodata
	.align 8
	.align 4
.jump_table:
	.quad	.error_dispatch_void
	.quad	.error_case_void
	.quad	.error_case_no_match
	.quad	.error_div_by_zero
	.quad	.error_substr_index_bad
	.text
.error_div_by_zero:
	movl	$.error_div_by_zero_string, %edi
	xorl	%eax, %eax
	call	printf
.error_exit:
	xorl	%edi, %edi
	call	exit
.error_substr_index_bad:
	movl	$.error_substr_index_bad_string, %edi
	xorl	%eax, %eax
	call	printf
	jmp	.error_exit
.error_dispatch_void:
	movl	$.error_dispatch_void_string, %edi
	xorl	%eax, %eax
	call	printf
	jmp	.error_exit
.error_case_void:
	movl	$.error_case_void_string, %edi
	xorl	%eax, %eax
	call	printf
	jmp	.error_exit
.error_case_no_match:
	movl	$.error_case_no_match_string, %edi
	xorl	%eax, %eax
	call	printf
	jmp	.error_exit
	.size	cool_error, .-cool_error
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.p2align 4
	.globl	Object.abort
	.type	Object.abort, @function
Object.abort:
	movl	$.abort_string, %edi
	subq	$8, %rsp
	call	puts
	xorl	%edi, %edi
	call	exit
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.p2align 4
	.globl	Object.copy
	.type	Object.copy, @function
Object.copy:
	pushq	%rbp
	pushq	%rbx
	movq	%rdi, %rbx
	subq	$8, %rsp
	movq	8(%rdi), %rax
	leaq	0(,%rax,8), %rbp
	movq	%rbp, %rdi
	call	malloc
	addq	$8, %rsp
	movq	%rbp, %rdx
	movq	%rbx, %rsi
	movq	%rax, %rdi
	popq	%rbx
	popq	%rbp
	jmp	memcpy
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.p2align 4
	.globl	Object.type_name
	.type	Object.type_name, @function
Object.type_name:
	pushq	%rbx
	movq	%rdi, %rbx
	call	String..new
	movq	16(%rbx), %rdx
	movq	(%rdx), %rdx
	movq	%rdx, 24(%rax)
	popq	%rbx
	ret
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.p2align 4
	.globl	String.length
	.type	String.length, @function
String.length:
	pushq	%rbp
	movq	%rdi, %rbp
	pushq	%rbx
	subq	$8, %rsp
	call	Int..new
	movq	24(%rbp), %rdi
	movq	%rax, %rbx
	call	strlen
	movq	%rax, 24(%rbx)
	addq	$8, %rsp
	movq	%rbx, %rax
	popq	%rbx
	popq	%rbp
	ret
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
String.substr:
	pushq	%r13
	movq	%rdx, %r13
	pushq	%r12
	movq	%rsi, %r12
	pushq	%rbp
	movq	%rdi, %rbp
	pushq	%rbx
	subq	$8, %rsp
	movq	24(%rdi), %rdi
	call	strlen
	movq	%rax, %rdx
	movq	24(%r13), %rax
	addq	24(%r12), %rax
	cmpq	%rax, %rdx
	jb	.substr_error
	call	String..new
	movq	24(%rax), %rdi
	movq	%rax, %rbx
	call	free
	movq	24(%r13), %rsi
	movq	24(%r12), %rdi
	addq	24(%rbp), %rdi
	call	strndup
	movq	%rax, 24(%rbx)
	addq	$8, %rsp
	movq	%rbx, %rax
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	ret
	.p2align 4,,10
	.p2align 3
.substr_error:
	xorl	%esi, %esi
	movl	$4, %edi
	call	cool_error
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.p2align 4
	.globl	String.concat
	.type	String.concat, @function
String.concat:
	pushq	%r13
	pushq	%r12
	movq	%rsi, %r12
	pushq	%rbp
	movq	%rdi, %rbp
	pushq	%rbx
	subq	$8, %rsp
	call	String..new
	movq	24(%rbp), %rdi
	movq	%rax, %rbx
	call	strlen
	movq	24(%r12), %rdi
	movq	%rax, %r13
	call	strlen
	movq	24(%rbx), %rdi
	leaq	1(%r13,%rax), %rsi
	call	realloc
	movq	%rax, 24(%rbx)
	movq	24(%rbp), %rsi
	movq	%rax, %rdi
	movq	24(%r12), %r12
	call	stpcpy
	movq	%r12, %rsi
	movq	%rax, %rdi
	call	strcpy
	addq	$8, %rsp
	movq	%rbx, %rax
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	ret
	#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.p2align 4
	.globl	Main.main
	.type	Main.main, @function
Main.main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$80, %rsp
#;comment start
	#Comment start
#start
	#Comment end
#;label Main_main_0
	#Label
Main_main_0:
#;t$1 <- my_attribute
	#Ident Expr start
	movq	24(%rdi), %rax
	movq	%rax, -8(%rbp)
	#Ident Expr end
#;t$2 <- classId t$1
	movq	-8(%rbp), %r13
	movq	0(%r13), %r13
	movq	%r13, -16(%rbp)
#Cmp $0, t$2 -> jump to main_Main_1
	pushq	%r13
	pushq	%r14
	movq	$0, %r13
	movq	-16(%rbp), %r14
	cmpq	%r13, %r14
	popq	%r14
	popq	%r13
	je	main_Main_1
#;t$3 <- classId Int
	movq	$2, -24(%rbp)
#Cmp t$3, t$2 -> jump to main_Main_2
	pushq	%r13
	pushq	%r14
	movq	-24(%rbp), %r13
	movq	-16(%rbp), %r14
	cmpq	%r13, %r14
	popq	%r14
	popq	%r13
	je	main_Main_2
#;comment case-join
	#Comment start
#case-join
	#Comment end
#;t$5 <- classId String
	movq	$4, -32(%rbp)
#Cmp t$5, t$2 -> jump to main_Main_3
	pushq	%r13
	pushq	%r14
	movq	-32(%rbp), %r13
	movq	-16(%rbp), %r14
	cmpq	%r13, %r14
	popq	%r14
	popq	%r13
	je	main_Main_3
#;comment case-join
	#Comment start
#case-join
	#Comment end
#;jmp main_Main_4
	#Jump
	jmp	main_Main_4
#VoidCase: main_Main_1
main_Main_1:
## case expression: error case
	movq	 $.string9, %r13
## guarantee 16-byte alignment before call
	andq	 $0xFFFFFFFFFFFFFFF0, %rsp
	movq	 %r13, %rdi
	call	 cooloutstr
## guarantee 16-byte alignment before call
	andq	 $0xFFFFFFFFFFFFFFF0, %rsp
	movl	 $0, %edi
	call	 exit
#;label main_Main_2
	#Label
main_Main_2:
#;t$7 <- t$1
	#Ident Expr start
	movq	-8(%rbp), %rax
	movq	%rax, -40(%rbp)
	#Ident Expr end
#;t$0 <- call out_int t$7
	#Call w/ args start
	movq	-40(%rbp), %rsi
	call	IO.out_int
	movq	%rax, -0(%rbp)
	#Call w/ args end
#;jmp Main_main_join
	#Jump
	jmp	Main_main_join
#;label main_Main_3
	#Label
main_Main_3:
#;t$8 <- t$1
	#Ident Expr start
	movq	-8(%rbp), %rax
	movq	%rax, -48(%rbp)
	#Ident Expr end
#;t$0 <- call out_string t$8
	#Call w/ args start
	movq	-48(%rbp), %rsi
	call	IO.out_string
	movq	%rax, -0(%rbp)
	#Call w/ args end
#;jmp Main_main_join
	#Jump
	jmp	Main_main_join
#EmptyCase: main_Main_4
main_Main_4:
## case expression: error case
	movq	 $.string8, %r13
## guarantee 16-byte alignment before call
	andq	 $0xFFFFFFFFFFFFFFF0, %rsp
	movq	 %r13, %rdi
	call	 cooloutstr
## guarantee 16-byte alignment before call
	andq	 $0xFFFFFFFFFFFFFFF0, %rsp
	movl	 $0, %edi
	call	 exit
#;label Main_main_join
	#Label
Main_main_join:
#;return t$0
	#Return start
	jmp	.main.end
	#Return end
.main.end:
	addq	$80, %rsp
	popq	%rbp
	ret
	.section	.rodata
	.align 8
.globl .string1
.string1:
	.string	"IO"
	.align 8
.globl .string11
.string11:
	.string	"Main"
	.align 8
.globl .string9
.string9:
	.string	"ERROR: 6: Exception: case on void\n"
	.align 8
.globl .string3
.string3:
	.string	"Object"
	.align 8
.globl .string4
.string4:
	.string	"String"
	.align 8
.globl .string0
.string0:
	.string	"Bool"
	.align 8
.globl .string8
.string8:
	.string	"ERROR: 6: Exception: case without matching branch\n"
	.align 8
.globl .string6
.string6:
	.string	"abort"
	.align 8
.globl .string2
.string2:
	.string	"Int"
	.align 8
.globl .string7
.string7:
	.string	"ERROR: 0: Exception: String.substr out of range\n"
	.align 8
	.globl empty.string
empty.string:
	.string	""
	.align 8
	.globl .percent.ld
.percent.ld:
	.string	"%ld"
	.align 8
	.globl .percent.d
.percent.d:
	.string	"%d"
	.align 8
	.globl .error_dispatch_void_string
.error_dispatch_void_string:
	.string	"ERROR: %zd: Exception: dispatch on void\n"
	.align 8
	.globl .error_case_void_string
.error_case_void_string:
	.string	"ERROR: %zd: Exception: case on void\n"
	.align 8
	.globl .error_case_no_match_string
.error_case_no_match_string:
	.string	"ERROR: %zd: Exception: case without matching branch\n"
	.align 8
	.globl .error_div_by_zero_string
.error_div_by_zero_string:
	.string	"ERROR: %zd: Exception: division by zero\n"
	.align 8
	.globl .error_substr_index_bad_string
.error_substr_index_bad_string:
	.string	"ERROR: %zd: Exception: String.substr out of range\n"
	.align 8
	.globl .abort_string
.abort_string:
	.string	"abort"
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
start:                  ## program begins here
.globl main
.type main, @function
main:
	movq	$Main..new, %r14
	pushq	%rbp
	call	*%r14
	movq	%rax, %rdi
	pushq	%rbp
	pushq	%r13
	movq	$Main.main, %r14
	call	*%r14
## guarantee 16-byte alignment before call
	andq	$0xFFFFFFFFFFFFFFF0, %rsp
	movl	$0, %edi
	call	exit
