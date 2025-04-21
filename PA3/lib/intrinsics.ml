open Asm

let cool_error =
  [
    Line "\t.p2align 4";
    Instruction (".globl", "cool_error", "", "");
    Instruction (".type", "cool_error", "@function", "");
    Line "cool_error:";
    Instruction ("subq", "$8", "%rsp", "");
    Instruction ("cmpq", "$4", "%rdi", "");
    Instruction ("ja", ".error_exit", "", "");
    Instruction ("jmp", "*.jump_table(,%rdi,8)", "", "");
    Instruction (".section", ".rodata", "", "");
    Line "\t.align 8";
    Line "\t.align 4";
    Line ".jump_table:";
    Instruction (".quad", ".error_dispatch_void", "", "");
    Instruction (".quad", ".error_case_void", "", "");
    Instruction (".quad", ".error_case_no_match", "", "");
    Instruction (".quad", ".error_div_by_zero", "", "");
    Instruction (".quad", ".error_substr_index_bad", "", "");
    Line "\t.text";
    Line ".error_div_by_zero:";
    Instruction ("movl", "$.error_div_by_zero_string", "%edi", "");
    Instruction ("xorl", "%eax", "%eax", "");
    Instruction ("call", "printf", "", "");
    Line ".error_exit:";
    Instruction ("xorl", "%edi", "%edi", "");
    Instruction ("call", "exit", "", "");
    Line ".error_substr_index_bad:";
    Instruction ("movl", "$.error_substr_index_bad_string", "%edi", "");
    Instruction ("xorl", "%eax", "%eax", "");
    Instruction ("call", "printf", "", "");
    Instruction ("jmp", ".error_exit", "", "");
    Line ".error_dispatch_void:";
    Instruction ("movl", "$.error_dispatch_void_string", "%edi", "");
    Instruction ("xorl", "%eax", "%eax", "");
    Instruction ("call", "printf", "", "");
    Instruction ("jmp", ".error_exit", "", "");
    Line ".error_case_void:";
    Instruction ("movl", "$.error_case_void_string", "%edi", "");
    Instruction ("xorl", "%eax", "%eax", "");
    Instruction ("call", "printf", "", "");
    Instruction ("jmp", ".error_exit", "", "");
    Line ".error_case_no_match:";
    Instruction ("movl", "$.error_case_no_match_string", "%edi", "");
    Instruction ("xorl", "%eax", "%eax", "");
    Instruction ("call", "printf", "", "");
    Instruction ("jmp", ".error_exit", "", "");
    Instruction (".size", "cool_error", ".-cool_error", "");
  ]

let abort =
  [
    Line "\t.p2align 4";
    Instruction (".globl", "Object.abort", "", "");
    Instruction (".type", "Object.abort", "@function", "");
    Line "Object.abort:";
    Instruction ("movl", "$.abort_string", "%edi", "");
    Instruction ("subq", "$8", "%rsp", "");
    Instruction ("call", "puts", "", "");
    Instruction ("xorl", "%edi", "%edi", "");
    Instruction ("call", "exit", "", "");
    (*Instruction (".size", "Object.abort", ".-Object.abort", "");*)
  ]

let copy =
  [
    Line "\t.p2align 4";
    Instruction (".globl", "Object.copy", "", "");
    Instruction (".type", "Object.copy", "@function", "");
    Line "Object.copy:";
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("movq", "%rdi", "%rbx", "");
    Instruction ("subq", "$8", "%rsp", "");
    Instruction ("movq", "8(%rdi)", "%rax", "");
    Instruction ("leaq", "0(,%rax,8)", "%rbp", "");
    Instruction ("movq", "%rbp", "%rdi", "");
    Instruction ("call", "malloc", "", "");
    Instruction ("addq", "$8", "%rsp", "");
    Instruction ("movq", "%rbp", "%rdx", "");
    Instruction ("movq", "%rbx", "%rsi", "");
    Instruction ("movq", "%rax", "%rdi", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("jmp", "memcpy", "", "");
    (*Instruction (".size", "Object.copy", ".-Object.copy", "");*)
  ]

let type_name =
  [
    Line "\t.p2align 4";
    Instruction (".globl", "Object.type_name", "", "");
    Instruction (".type", "Object.type_name", "@function", "");
    Line "Object.type_name:";
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("movq", "%rdi", "%rbx", "");
    Instruction ("call", "String..new", "", "");
    Instruction ("movq", "16(%rbx)", "%rdx", "");
    Instruction ("movq", "(%rdx)", "%rdx", "");
    Instruction ("movq", "%rdx", "24(%rax)", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("ret", "", "", "");
    (*Instruction (".size", "Object.type_name", ".-Object.type_name", "");*)
  ]

let in_int =
  [
    Line "\t.p2align 4";
    Instruction (".globl", "IO.in_int", "", "");
    Instruction (".type", "IO.in_int", "@function", "");
    Line "IO.in_int:";
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("subq", "$32", "%rsp", "");
    Instruction ("movq", "stdin(%rip)", "%rdx", "");
    Instruction ("leaq", "8(%rsp)", "%rdi", "");
    Instruction ("leaq", "16(%rsp)", "%rsi", "");
    Instruction ("movq", "$0", "8(%rsp)", "");
    Instruction ("movq", "$0", "16(%rsp)", "");
    Instruction ("movq", "$0", "24(%rsp)", "");
    Instruction ("call", "getline", "", "");
    Instruction ("movq", "8(%rsp)", "%rdi", "");
    Instruction ("cmpq", "$-1", "%rax", "");
    Instruction ("je", ".in_int_string_error", "", "");
    Instruction ("testq", "%rdi", "%rdi", "");
    Instruction ("je", ".in_int_string_error", "", "");
    Line ".in_int_bounds_check:";
    Instruction ("leaq", "24(%rsp)", "%rsi", "");
    Instruction ("movl", "$10", "%edx", "");
    Instruction ("call", "strtol", "", "");
    Instruction ("movl", "$4294967295", "%edx", "");
    Instruction ("movq", "%rax", "%rbx", "");
    Instruction ("movl", "$2147483648", "%eax", "");
    Instruction ("addq", "%rbx", "%rax", "");
    Instruction ("cmpq", "%rax", "%rdx", "");
    Instruction ("movl", "$0", "%eax", "");
    Instruction ("cmovb", "%rax", "%rbx", "");
    Instruction ("call", "Int..new", "", "");
    Instruction ("movq", "%rbx", "24(%rax)", "");
    Instruction ("addq", "$32", "%rsp", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("ret", "", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".in_int_string_error:";
    Instruction ("call", "free", "", "");
    Instruction ("movl", "$1", "%edi", "");
    Instruction ("movl", "$1", "%esi", "");
    Instruction ("call", "calloc", "", "");
    Instruction ("movq", "%rax", "8(%rsp)", "");
    Instruction ("movq", "%rax", "%rdi", "");
    Instruction ("jmp", ".in_int_bounds_check", "", "");
    (*Instruction (".size", "IO.in_int", ".-IO.in_int", "");*)
  ]

let out_int =
  [
    Line "\t.p2align 4";
    Instruction (".globl", "IO.out_int", "", "");
    Instruction (".type", "IO.out_int", "@function", "");
    Line "IO.out_int:";
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("movl", "24(%rsi)", "%esi", "");
    Instruction ("movq", "%rdi", "%rbx", "");
    Instruction ("xorl", "%eax", "%eax", "");
    Instruction ("movl", "$.percent.d", "%edi", "");
    Instruction ("call", "printf", "", "");
    Instruction ("movq", "%rbx", "%rax", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("ret", "", "", "");
    (*Instruction (".size", "IO.out_int", ".-IO.out_int", "");*)
  ]

let in_string =
  [
    Line "\t.p2align 4";
    Instruction (".globl", "IO.in_string", "", "");
    Instruction (".type", "IO.in_string", "@function", "");
    Line "IO.in_string:";
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("subq", "$24", "%rsp", "");
    Instruction ("movq", "stdin(%rip)", "%rdx", "");
    Instruction ("leaq", "8(%rsp)", "%rsi", "");
    Instruction ("movq", "%rsp", "%rdi", "");
    Instruction ("movq", "$0", "(%rsp)", "");
    Instruction ("movq", "$0", "8(%rsp)", "");
    Instruction ("call", "getline", "", "");
    Instruction ("movq", "(%rsp)", "%rbp", "");
    Instruction ("cmpq", "$-1", "%rax", "");
    Instruction ("je", ".in_string_null", "", "");
    Instruction ("testq", "%rbp", "%rbp", "");
    Instruction ("je", ".in_string_null", "", "");
    Instruction ("xorl", "%esi", "%esi", "");
    Instruction ("movq", "%rax", "%rdx", "");
    Instruction ("movq", "%rbp", "%rdi", "");
    Instruction ("movq", "%rax", "%rbx", "");
    Instruction ("call", "memchr", "", "");
    Instruction ("testq", "%rax", "%rax", "");
    Instruction ("jne", ".in_string_null", "", "");
    Instruction ("leaq", "-1(%rbp,%rbx)", "%rax", "");
    Instruction ("cmpb", "$10", "(%rax)", "");
    Instruction ("jne", ".in_string_newline", "", "");
    Instruction ("movb", "$0", "(%rax)", "");
    Instruction ("movq", "(%rsp)", "%rbp", "");
    Line ".in_string_resize:";
    Instruction ("movq", "%rbx", "%rsi", "");
    Instruction ("movq", "%rbp", "%rdi", "");
    Instruction ("call", "realloc", "", "");
    Line ".in_string_end:";
    Instruction ("movq", "%rax", "(%rsp)", "");
    Instruction ("call", "String..new", "", "");
    Instruction ("movq", "24(%rax)", "%rdi", "");
    Instruction ("movq", "%rax", "%rbx", "");
    Instruction ("call", "free", "", "");
    Instruction ("movq", "(%rsp)", "%rax", "");
    Instruction ("movq", "%rax", "24(%rbx)", "");
    Instruction ("addq", "$24", "%rsp", "");
    Instruction ("movq", "%rbx", "%rax", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("ret", "", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".in_string_null:";
    Instruction ("movq", "%rbp", "%rdi", "");
    Instruction ("call", "free", "", "");
    Instruction ("movl", "$1", "%esi", "");
    Instruction ("movl", "$1", "%edi", "");
    Instruction ("call", "calloc", "", "");
    Instruction ("jmp", ".in_string_end", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".in_string_newline:";
    Instruction ("addq", "$1", "%rbx", "");
    Instruction ("jmp", ".in_string_resize", "", "");
    (*Instruction (".size", "IO.in_string", ".-IO.in_string", "");*)
    (*Line "\t## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;";*)
  ]

let out_string =
  [
    Line "\t.p2align 4";
    Instruction (".globl", "IO.out_string", "", "");
    Instruction (".type", "IO.out_string", "@function", "");
    Line "IO.out_string:";
    Instruction ("pushq", "%r12", "", "");
    Instruction ("movq", "%rdi", "%r12", "");
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("movq", "24(%rsi)", "%rbx", "");
    Instruction ("movsbl", "(%rbx)", "%edi", "");
    Instruction ("testb", "%dil", "%dil", "");
    Instruction ("jne", ".L44", "", "");
    Instruction ("jmp", ".L57", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".L61:";
    Instruction ("call", "putchar", "", "");
    Line ".L49:";
    Instruction ("movsbl", "1(%rbx)", "%edi", "");
    Instruction ("addq", "$1", "%rbx", "");
    Instruction ("testb", "%dil", "%dil", "");
    Instruction ("je", ".L57", "", "");
    Line ".L44:";
    Instruction ("cmpb", "$92", "%dil", "");
    Instruction ("jne", ".L61", "", "");
    Instruction ("movzbl", "1(%rbx)", "%ebp", "");
    Instruction ("addq", "$1", "%rbx", "");
    Instruction ("testb", "%bpl", "%bpl", "");
    Instruction ("je", ".L51", "", "");
    Instruction ("cmpb", "$110", "%bpl", "");
    Instruction ("je", ".L62", "", "");
    Instruction ("cmpb", "$116", "%bpl", "");
    Instruction ("je", ".L46", "", "");
    Instruction ("movl", "$92", "%edi", "");
    Instruction ("addq", "$1", "%rbx", "");
    Instruction ("call", "putchar", "", "");
    Instruction ("movsbl", "%bpl", "%edi", "");
    Instruction ("call", "putchar", "", "");
    Instruction ("movsbl", "(%rbx)", "%edi", "");
    Instruction ("testb", "%dil", "%dil", "");
    Instruction ("jne", ".L44", "", "");
    Line ".L57:";
    Instruction ("movq", "%r12", "%rax", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("ret", "", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".L62:";
    Instruction ("movl", "$10", "%edi", "");
    Instruction ("call", "putchar", "", "");
    Instruction ("jmp", ".L49", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".L46:";
    Instruction ("movl", "$9", "%edi", "");
    Instruction ("call", "putchar", "", "");
    Instruction ("jmp", ".L49", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".L51:";
    Instruction ("movl", "$92", "%edi", "");
    Instruction ("call", "putchar", "", "");
    Instruction ("movq", "%r12", "%rax", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("ret", "", "", "");
    (*Instruction (".size", "IO.out_string", ".-IO.out_string", "");*)
  ]

(*let cooloutstr =*)
(*[*)
(*Instruction (".globl\tcooloutstr", "", "", "");*)
(*Instruction (".type\tcooloutstr, @function", "", "", "");*)
(*Instruction ("cooloutstr:", "", "", "");*)
(*Instruction (".LFB6:", "", "", "");*)
(*Instruction (".cfi_startproc", "", "", "");*)
(*Instruction ("endbr64", "", "", "");*)
(*Instruction ("pushq\t%rbp", "", "", "");*)
(*Instruction (".cfi_def_cfa_offset 16", "", "", "");*)
(*Instruction (".cfi_offset 6, -16", "", "", "");*)
(*Instruction ("movq\t%rsp, %rbp", "", "", "");*)
(*Instruction (".cfi_def_cfa_register 6", "", "", "");*)
(*Instruction ("subq\t$32, %rsp", "", "", "");*)
(*Instruction ("movq\t%rdi, -24(%rbp)", "", "", "");*)
(*Instruction ("movl\t$0, -4(%rbp)", "", "", "");*)
(*Instruction ("jmp\t.L2", "", "", "");*)
(*Instruction (".L5:", "", "", "");*)
(*Instruction ("movl\t-4(%rbp), %eax", "", "", "");*)
(*Instruction ("movslq\t%eax, %rdx", "", "", "");*)
(*Instruction ("movq\t-24(%rbp), %rax", "", "", "");*)
(*Instruction ("addq\t%rdx, %rax", "", "", "");*)
(*Instruction ("movzbl\t(%rax), %eax", "", "", "");*)
(*Instruction ("cmpb\t$92, %al", "", "", "");*)
(*Instruction ("jne\t.L3", "", "", "");*)
(*Instruction ("movl\t-4(%rbp), %eax", "", "", "");*)
(*Instruction ("cltq", "", "", "");*)
(*Instruction ("leaq\t1(%rax), %rdx", "", "", "");*)
(*Instruction ("movq\t-24(%rbp), %rax", "", "", "");*)
(*Instruction ("addq\t%rdx, %rax", "", "", "");*)
(*Instruction ("movzbl\t(%rax), %eax", "", "", "");*)
(*Instruction ("cmpb\t$110, %al", "", "", "");*)
(*Instruction ("jne\t.L3", "", "", "");*)
(*Instruction ("movq\tstdout(%rip), %rax", "", "", "");*)
(*Instruction ("movq\t%rax, %rsi", "", "", "");*)
(*Instruction ("movl\t$10, %edi", "", "", "");*)
(*Instruction ("call\tfputc@PLT", "", "", "");*)
(*Instruction ("addl\t$2, -4(%rbp)", "", "", "");*)
(*Instruction ("jmp\t.L2", "", "", "");*)
(*Instruction (".L3:", "", "", "");*)
(*Instruction ("movl\t-4(%rbp), %eax", "", "", "");*)
(*Instruction ("movslq\t%eax, %rdx", "", "", "");*)
(*Instruction ("movq\t-24(%rbp), %rax", "", "", "");*)
(*Instruction ("addq\t%rdx, %rax", "", "", "");*)
(*Instruction ("movzbl\t(%rax), %eax", "", "", "");*)
(*Instruction ("cmpb\t$92, %al", "", "", "");*)
(*Instruction ("jne\t.L4", "", "", "");*)
(*Instruction ("movl\t-4(%rbp), %eax", "", "", "");*)
(*Instruction ("cltq", "", "", "");*)
(*Instruction ("leaq\t1(%rax), %rdx", "", "", "");*)
(*Instruction ("movq\t-24(%rbp), %rax", "", "", "");*)
(*Instruction ("addq\t%rdx, %rax", "", "", "");*)
(*Instruction ("movzbl\t(%rax), %eax", "", "", "");*)
(*Instruction ("cmpb\t$116, %al", "", "", "");*)
(*Instruction ("jne\t.L4", "", "", "");*)
(*Instruction ("movq\tstdout(%rip), %rax", "", "", "");*)
(*Instruction ("movq\t%rax, %rsi", "", "", "");*)
(*Instruction ("movl\t$9, %edi", "", "", "");*)
(*Instruction ("call\tfputc@PLT", "", "", "");*)
(*Instruction ("addl\t$2, -4(%rbp)", "", "", "");*)
(*Instruction ("jmp\t.L2", "", "", "");*)
(*Instruction (".L4:", "", "", "");*)
(*Instruction ("movq\tstdout(%rip), %rdx", "", "", "");*)
(*Instruction ("movl\t-4(%rbp), %eax", "", "", "");*)
(*Instruction ("movslq\t%eax, %rcx", "", "", "");*)
(*Instruction ("movq\t-24(%rbp), %rax", "", "", "");*)
(*Instruction ("addq\t%rcx, %rax", "", "", "");*)
(*Instruction ("movzbl\t(%rax), %eax", "", "", "");*)
(*Instruction ("movsbl\t%al, %eax", "", "", "");*)
(*Instruction ("movq\t%rdx, %rsi", "", "", "");*)
(*Instruction ("movl\t%eax, %edi", "", "", "");*)
(*Instruction ("call\tfputc@PLT", "", "", "");*)
(*Instruction ("addl\t$1, -4(%rbp)", "", "", "");*)
(*Instruction (".L2:", "", "", "");*)
(*Instruction ("movl\t-4(%rbp), %eax", "", "", "");*)
(*Instruction ("movslq\t%eax, %rdx", "", "", "");*)
(*Instruction ("movq\t-24(%rbp), %rax", "", "", "");*)
(*Instruction ("addq\t%rdx, %rax", "", "", "");*)
(*Instruction ("movzbl\t(%rax), %eax", "", "", "");*)
(*Instruction ("testb\t%al, %al", "", "", "");*)
(*Instruction ("jne\t.L5", "", "", "");*)
(*Instruction ("movq\tstdout(%rip), %rax", "", "", "");*)
(*Instruction ("movq\t%rax, %rdi", "", "", "");*)
(*Instruction ("call\tfflush@PLT", "", "", "");*)
(*Instruction ("nop", "", "", "");*)
(*Instruction ("leave", "", "", "");*)
(*Instruction (".cfi_def_cfa 7, 8", "", "", "");*)
(*Instruction ("ret", "", "", "");*)
(*Instruction (".cfi_endproc", "", "", "");*)
(*Instruction (".LFE6:", "", "", "");*)
(*Instruction (".size\tcooloutstr, .-cooloutstr", "", "", "");*)
(*Instruction (".globl\tcoolstrlen", "", "", "");*)
(*Instruction (".type\tcoolstrlen, @function", "", "", "");*)
(*Instruction ("", "", "", "");*)
(*]*)

let string_length =
  [
    Line "\t.p2align 4";
    Instruction (".globl", "String.length", "", "");
    Instruction (".type", "String.length", "@function", "");
    Line "String.length:";
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("movq", "%rdi", "%rbp", "");
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("subq", "$8", "%rsp", "");
    Instruction ("call", "Int..new", "", "");
    Instruction ("movq", "24(%rbp)", "%rdi", "");
    Instruction ("movq", "%rax", "%rbx", "");
    Instruction ("call", "strlen", "", "");
    Instruction ("movq", "%rax", "24(%rbx)", "");
    Instruction ("addq", "$8", "%rsp", "");
    Instruction ("movq", "%rbx", "%rax", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("ret", "", "", "");
    (*Instruction (".size", "String.length", ".-String.length", "");*)
  ]

let concat =
  [
    Line "\t.p2align 4";
    Instruction (".globl", "String.concat", "", "");
    Instruction (".type", "String.concat", "@function", "");
    Line "String.concat:";
    Instruction ("pushq", "%r13", "", "");
    Instruction ("pushq", "%r12", "", "");
    Instruction ("movq", "%rsi", "%r12", "");
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("movq", "%rdi", "%rbp", "");
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("subq", "$8", "%rsp", "");
    Instruction ("call", "String..new", "", "");
    Instruction ("movq", "24(%rbp)", "%rdi", "");
    Instruction ("movq", "%rax", "%rbx", "");
    Instruction ("call", "strlen", "", "");
    Instruction ("movq", "24(%r12)", "%rdi", "");
    Instruction ("movq", "%rax", "%r13", "");
    Instruction ("call", "strlen", "", "");
    Instruction ("movq", "24(%rbx)", "%rdi", "");
    Instruction ("leaq", "1(%r13,%rax)", "%rsi", "");
    Instruction ("call", "realloc", "", "");
    Instruction ("movq", "%rax", "24(%rbx)", "");
    Instruction ("movq", "24(%rbp)", "%rsi", "");
    Instruction ("movq", "%rax", "%rdi", "");
    Instruction ("movq", "24(%r12)", "%r12", "");
    Instruction ("call", "stpcpy", "", "");
    Instruction ("movq", "%r12", "%rsi", "");
    Instruction ("movq", "%rax", "%rdi", "");
    Instruction ("call", "strcpy", "", "");
    Instruction ("addq", "$8", "%rsp", "");
    Instruction ("movq", "%rbx", "%rax", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("popq", "%r13", "", "");
    Instruction ("ret", "", "", "");
    (*Instruction (".size", "String.concat", ".-String.concat", "");*)
  ]

(*let coolgetstr =*)
(*[*)
(*Line "\t.section\t.rodata";*)
(*Line ".LC1:";*)
(*Line "\t.string\t\"\"";*)
(*Line "\t.text";*)
(*Line "\t.globl\tcoolgetstr";*)
(*Line "\t.type\tcoolgetstr, @function";*)
(*Line "coolgetstr:";*)
(*Line ".LFB9:";*)
(*Line "\t.cfi_startproc";*)
(*Instruction ("endbr64", "", "", "");*)
(*Instruction ("pushq", "%rbp", "", "");*)
(*Line "\t.cfi_def_cfa_offset 16";*)
(*Line "\t.cfi_offset 6, -16";*)
(*Instruction ("movq", "%rsp", "%rbp", "");*)
(*Line "\t.cfi_def_cfa_register 6";*)
(*Instruction ("subq", "$16", "%rsp", "");*)
(*Instruction ("movl", "$1", "%esi", "");*)
(*Instruction ("movl", "$40960", "%edi", "");*)
(*Instruction ("call", "calloc@PLT", "", "");*)
(*Instruction ("movq", "%rax", "-8(%rbp)", "");*)
(*Instruction ("movl", "$0", "-16(%rbp)", "");*)
(*Line ".L21:";*)
(*Instruction ("movq", "stdin(%rip)", "%rax", "");*)
(*Instruction ("movq", "%rax", "%rdi", "");*)
(*Instruction ("call", "fgetc@PLT", "", "");*)
(*Instruction ("movl", "%eax", "-12(%rbp)", "");*)
(*Instruction ("cmpl", "$-1", "-12(%rbp)", "");*)
(*Instruction ("je", ".L15", "", "");*)
(*Instruction ("cmpl", "$10", "-12(%rbp)", "");*)
(*Instruction ("jne", ".L16", "", "");*)
(*Line ".L15:";*)
(*Instruction ("cmpl", "$0", "-16(%rbp)", "");*)
(*Instruction ("je", ".L17", "", "");*)
(*Instruction ("leaq", ".LC1(%rip)", "%rax", "");*)
(*Instruction ("jmp", ".L18", "", "");*)
(*Line ".L17:";*)
(*Instruction ("movq", "-8(%rbp)", "%rax", "");*)
(*Instruction ("jmp", ".L18", "", "");*)
(*Line ".L16:";*)
(*Instruction ("cmpl", "$0", "-12(%rbp)", "");*)
(*Instruction ("jne", ".L19", "", "");*)
(*Instruction ("movl", "$1", "-16(%rbp)", "");*)
(*Instruction ("jmp", ".L21", "", "");*)
(*Line ".L19:";*)
(*Instruction ("movq", "-8(%rbp)", "%rax", "");*)
(*Instruction ("movq", "%rax", "%rdi", "");*)
(*Instruction ("call", "coolstrlen", "", "");*)
(*Instruction ("movl", "%eax", "%edx", "");*)
(*Instruction ("movq", "-8(%rbp)", "%rax", "");*)
(*Instruction ("addq", "%rdx", "%rax", "");*)
(*Instruction ("movl", "-12(%rbp)", "%edx", "");*)
(*Instruction ("movb", "%dl", "(%rax)", "");*)
(*Instruction ("jmp", ".L21", "", "");*)
(*Line ".L18:";*)
(*Instruction ("leave", "", "", "");*)
(*Line "\t.cfi_def_cfa 7, 8";*)
(*Instruction ("ret", "", "", "");*)
(*Line "\t.cfi_endproc";*)
(*Line ".LFE9:";*)
(*Line "\t.size\tcoolgetstr, .-coolgetstr";*)
(*]*)

let string_substr =
  [
    Line "String.substr:";
    Instruction ("pushq", "%r13", "", "");
    Instruction ("movq", "%rdx", "%r13", "");
    Instruction ("pushq", "%r12", "", "");
    Instruction ("movq", "%rsi", "%r12", "");
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("movq", "%rdi", "%rbp", "");
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("subq", "$8", "%rsp", "");
    Instruction ("movq", "24(%rdi)", "%rdi", "");
    Instruction ("call", "strlen", "", "");
    Instruction ("movq", "%rax", "%rdx", "");
    Instruction ("movq", "24(%r13)", "%rax", "");
    Instruction ("addq", "24(%r12)", "%rax", "");
    Instruction ("cmpq", "%rax", "%rdx", "");
    Instruction ("jb", ".substr_error", "", "");
    Instruction ("call", "String..new", "", "");
    Instruction ("movq", "24(%rax)", "%rdi", "");
    Instruction ("movq", "%rax", "%rbx", "");
    Instruction ("call", "free", "", "");
    Instruction ("movq", "24(%r13)", "%rsi", "");
    Instruction ("movq", "24(%r12)", "%rdi", "");
    Instruction ("addq", "24(%rbp)", "%rdi", "");
    Instruction ("call", "strndup", "", "");
    Instruction ("movq", "%rax", "24(%rbx)", "");
    Instruction ("addq", "$8", "%rsp", "");
    Instruction ("movq", "%rbx", "%rax", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("popq", "%r13", "", "");
    Instruction ("ret", "", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".substr_error:";
    Instruction ("xorl", "%esi", "%esi", "");
    Instruction ("movl", "$4", "%edi", "");
    Instruction ("call", "cool_error", "", "");
    (*Instruction (".size", "String.substr", ".-String.substr", "");*)
  ]

(*let concat =*)
(*[*)
(*Line ".globl String.concat";*)
(*Line "String.concat:";*)
(*Line "## method definition";*)
(*Instruction ("pushq", "%rbp", "", "");*)
(*Instruction ("movq", "%rsp", "%rbp", "");*)
(*Instruction ("movq", "16(%rbp)", "%r12", "");*)
(*Line "## stack room for temporaries: 2";*)
(*Instruction ("movq", "$16", "%r14", "");*)
(*Instruction ("subq", "%r14", "%rsp", "");*)
(*Line "## return address handling";*)
(*Line "## fp[3] holds argument s (String)";*)
(*Line "## method body begins";*)
(*Line "## new String";*)
(*Instruction ("pushq", "%rbp", "", "");*)
(*Instruction ("pushq", "%r12", "", "");*)
(*Instruction ("movq", "$String..new", "%r14", "");*)
(*Instruction ("call", "*%r14", "", "");*)
(*Instruction ("popq", "%r12", "", "");*)
(*Instruction ("popq", "%rbp", "", "");*)
(*Instruction ("movq", "%r13", "%r15", "");*)
(*Instruction ("movq", "24(%rbp)", "%r14", "");*)
(*Instruction ("movq", "24(%r14)", "%r14", "");*)
(*Instruction ("movq", "24(%r12)", "%r13", "");*)
(*Line "## guarantee 16-byte alignment before call";*)
(*Instruction ("andq", "$0xFFFFFFFFFFFFFFF0", "%rsp", "");*)
(*Instruction ("movq", "%r13", "%rdi", "");*)
(*Instruction ("movq", "%r14", "%rsi", "");*)
(*Instruction ("call", "coolstrcat", "", "");*)
(*Instruction ("movq", "%rax", "%r13", "");*)
(*Instruction ("movq", "%r13", "24(%r15)", "");*)
(*Instruction ("movq", "%r15", "%r13", "");*)
(*Line ".globl String.concat.end";*)
(*Line "String.concat.end:";*)
(*Line "## method body ends";*)
(*Line "## return address handling";*)
(*Instruction ("movq", "%rbp", "%rsp", "");*)
(*Instruction ("popq", "%rbp", "", "");*)
(*Instruction ("ret", "", "", "");*)
(*Line "## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;";*)
(*]*)

(*let string_length =*)
(*[*)
(*Line ".globl String.length";*)
(*Line "String.length:";*)
(*Line "## method definition";*)
(*Instruction ("pushq", "%rbp", "", "");*)
(*Instruction ("movq", "%rsp", "%rbp", "");*)
(*Instruction ("movq", "16(%rbp)", "%r12", "");*)
(*Line "## stack room for temporaries: 2";*)
(*Instruction ("movq", "$16", "%r14", "");*)
(*Instruction ("subq", "%r14", "%rsp", "");*)
(*Line "## return address handling";*)
(*Line "## method body begins";*)
(*Line "## new Int";*)
(*Instruction ("pushq", "%rbp", "", "");*)
(*Instruction ("pushq", "%r12", "", "");*)
(*Instruction ("movq", "$Int..new", "%r14", "");*)
(*Instruction ("call", "*%r14", "", "");*)
(*Instruction ("popq", "%r12", "", "");*)
(*Instruction ("popq", "%rbp", "", "");*)
(*Instruction ("movq", "%r13", "%r14", "");*)
(*Instruction ("movq", "24(%r12)", "%r13", "");*)
(*Line "## guarantee 16-byte alignment before call";*)
(*Instruction ("andq", "$0xFFFFFFFFFFFFFFF0", "%rsp", "");*)
(*Instruction ("movq", "%r13", "%rdi", "");*)
(*Instruction ("movl", "$0", "%eax", "");*)
(*Instruction ("call", "coolstrlen", "", "");*)
(*Instruction ("movq", "%rax", "%r13", "");*)
(*Instruction ("movq", "%r13", "24(%r14)", "");*)
(*Instruction ("movq", "%r14", "%r13", "");*)
(*Line ".globl String.length.end";*)
(*Line "String.length.end:";*)
(*Line "## method body ends";*)
(*Line "## return address handling";*)
(*Instruction ("movq", "%rbp", "%rsp", "");*)
(*Instruction ("popq", "%rbp", "", "");*)
(*Instruction ("ret", "", "", "");*)
(*Line "## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;";*)
(*]*)

(*let string_substr =*)
(*[*)
(*Line ".globl String.substr";*)
(*Line "String.substr:";*)
(*Line "## method definition";*)
(*Instruction ("pushq", "%rbp", "", "");*)
(*Instruction ("movq", "%rsp", "%rbp", "");*)
(*Instruction ("movq", "16(%rbp)", "%r12", "");*)
(*Line "## stack room for temporaries: 2";*)
(*Instruction ("movq", "$16", "%r14", "");*)
(*Instruction ("subq", "%r14", "%rsp", "");*)
(*Line "## return address handling";*)
(*Line "## fp[4] holds argument i (Int)";*)
(*Line "## fp[3] holds argument l (Int)";*)
(*Line "## method body begins";*)
(*Line "## new String";*)
(*Instruction ("pushq", "%rbp", "", "");*)
(*Instruction ("pushq", "%r12", "", "");*)
(*Instruction ("movq", "$String..new", "%r14", "");*)
(*Instruction ("call", "*%r14", "", "");*)
(*Instruction ("popq", "%r12", "", "");*)
(*Instruction ("popq", "%rbp", "", "");*)
(*Instruction ("movq", "%r13", "%r15", "");*)
(*Instruction ("movq", "24(%rbp)", "%r14", "");*)
(*Instruction ("movq", "24(%r14)", "%r14", "");*)
(*Instruction ("movq", "32(%rbp)", "%r13", "");*)
(*Instruction ("movq", "24(%r13)", "%r13", "");*)
(*Instruction ("movq", "24(%r12)", "%r12", "");*)
(*Line "## guarantee 16-byte alignment before call";*)
(*Instruction ("andq", "$0xFFFFFFFFFFFFFFF0", "%rsp", "");*)
(*Instruction ("movq", "%r12", "%rdi", "");*)
(*Instruction ("movq", "%r13", "%rsi", "");*)
(*Instruction ("movq", "%r14", "%rdx", "");*)
(*Instruction ("call", "coolsubstr", "", "");*)
(*Instruction ("movq", "%rax", "%r13", "");*)
(*Instruction ("cmpq", "$0", "%r13", "");*)
(*Instruction ("jne", "l6", "", "");*)
(*Instruction ("movq", "$string7", "%r13", "");*)
(*Line "## guarantee 16-byte alignment before call";*)
(*Instruction ("andq", "$0xFFFFFFFFFFFFFFF0", "%rsp", "");*)
(*Instruction ("movq", "%r13", "%rdi", "");*)
(*Instruction ("call", "cooloutstr", "", "");*)
(*Line "## guarantee 16-byte alignment before call";*)
(*Instruction ("andq", "$0xFFFFFFFFFFFFFFF0", "%rsp", "");*)
(*Instruction ("movl", "$0", "%edi", "");*)
(*Instruction ("call", "exit", "", "");*)
(*Line ".globl l6";*)
(*Line "l6:";*)
(*Instruction ("movq", "%r13", "24(%r15)", "");*)
(*Instruction ("movq", "%r15", "%r13", "");*)
(*Line ".globl String.substr.end";*)
(*Line "String.substr.end:";*)
(*Line "## method body ends";*)
(*Line "## return address handling";*)
(*Instruction ("movq", "%rbp", "%rsp", "");*)
(*Instruction ("popq", "%rbp", "", "");*)
(*Instruction ("ret", "", "", "");*)
(*Line "## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;";*)
(*Line "## global string constants";*)
(*]*)

let intrinsic_funcs =
  [
    in_int;
    out_int;
    in_string;
    out_string;
    (*cooloutstr;*)
    cool_error;
    (*coolgetstr;*)
    (*coolstrcat;*)
    (*coolstrlen;*)
    (*coolsubstr;*)
    abort;
    copy;
    type_name;
    string_length;
    string_substr;
    concat;
  ]

let handlers =
  [
    Line "\t.p2align 4";
    Instruction (".globl", "lt_handler", "", "");
    Instruction (".type", "lt_handler", "@function", "");
    Line "lt_handler:";
    Instruction ("pushq", "%r12", "", "");
    Instruction ("movq", "%rsi", "%r12", "");
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("movq", "%rdi", "%rbp", "");
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("call", "Bool..new", "", "");
    Instruction ("movq", "%rax", "%rbx", "");
    Instruction ("testq", "%rbp", "%rbp", "");
    Instruction ("je", ".lt_false", "", "");
    Instruction ("testq", "%r12", "%r12", "");
    Instruction ("je", ".lt_false", "", "");
    Instruction ("movq", "(%r12)", "%rdx", "");
    Instruction ("addq", "0(%rbp)", "%rdx", "");
    Instruction ("leaq", "-2(%rdx)", "%rax", "");
    Instruction ("testq", "$-3", "%rax", "");
    Instruction ("je", ".lt_num", "", "");
    Instruction ("xorl", "%eax", "%eax", "");
    Instruction ("cmpq", "$10", "%rdx", "");
    Instruction ("je", ".lt_string", "", "");
    Line ".lt_cleanup:";
    Instruction ("movq", "%rax", "24(%rbx)", "");
    Instruction ("movq", "%rbx", "%rax", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("ret", "", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".lt_false:";
    Instruction ("xorl", "%eax", "%eax", "");
    Instruction ("movq", "%rax", "24(%rbx)", "");
    Instruction ("movq", "%rbx", "%rax", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("ret", "", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".lt_num:";
    Instruction ("movq", "24(%r12)", "%rax", "");
    Instruction ("cmpq", "%rax", "24(%rbp)", "");
    Instruction ("setl", "%al", "", "");
    Instruction ("movzbl", "%al", "%eax", "");
    Instruction ("jmp", ".lt_cleanup", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".lt_string:";
    Instruction ("movq", "24(%r12)", "%rsi", "");
    Instruction ("movq", "24(%rbp)", "%rdi", "");
    Instruction ("call", "strcmp", "", "");
    Instruction ("cltq", "", "", "");
    Instruction ("shrq", "$63", "%rax", "");
    Instruction ("jmp", ".lt_cleanup", "", "");
    Instruction (".size", "lt_handler", ".-lt_handler", "");
    Line "\t.p2align 4";
    Instruction (".globl", "le_handler", "", "");
    Instruction (".type", "le_handler", "@function", "");
    Line "le_handler:";
    Instruction ("pushq", "%r12", "", "");
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("movq", "%rsi", "%rbp", "");
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("movq", "%rdi", "%rbx", "");
    Instruction ("call", "Bool..new", "", "");
    Instruction ("movq", "%rax", "%r12", "");
    Instruction ("cmpq", " %rbx, %rbp", "", "");
    Instruction ("je", " .le_true", "", "");
    Instruction ("testq", "%rbx", "%rbx", "");
    Instruction ("je", ".le_false", "", "");
    Instruction ("testq", "%rbp", "%rbp", "");
    Instruction ("je", ".le_false", "", "");
    Instruction ("movq", "0(%rbp)", "%rax", "");
    Instruction ("addq", "(%rbx)", "%rax", "");
    Instruction ("leaq", "-2(%rax)", "%rdx", "");
    Instruction ("testq", "$-3", "%rdx", "");
    Instruction ("je", ".le_num", "", "");
    Instruction ("xorl", "%edx", "%edx", "");
    Instruction ("cmpq", "%rbp", "%rbx", "");
    Instruction ("sete", "%dl", "", "");
    Instruction ("cmpq", "$10", "%rax", "");
    Instruction ("je", ".le_string", "", "");
    Line ".le_cleanup:";
    Instruction ("movq", "%rdx", "24(%r12)", "");
    Instruction ("movq", "%r12", "%rax", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("ret", "", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".le_false:";
    Instruction ("xorl", "%edx", "%edx", "");
    Instruction ("movq", "%r12", "%rax", "");
    Instruction ("movq", "%rdx", "24(%r12)", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("ret", "", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".le_true:";
    Instruction ("movq\t$1,", " %rdx", "", "");
    Instruction ("movq\t%r12,", " %rax", "", "");
    Instruction ("movq\t%rdx,", " 24(%r12)", "", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("ret", "", "", "");
    Line ".p2align 4,,10";
    Line ".p2align 3";
    Line ".le_num:";
    Instruction ("xorl", "%edx", "%edx", "");
    Instruction ("movq", "24(%rbp)", "%rax", "");
    Instruction ("cmpq", "%rax", "24(%rbx)", "");
    Instruction ("setle", "%dl", "", "");
    Instruction ("jmp", ".le_cleanup", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".le_string:";
    Instruction ("movq", "24(%rbp)", "%rsi", "");
    Instruction ("movq", "24(%rbx)", "%rdi", "");
    Instruction ("call", "strcmp", "", "");
    Instruction ("xorl", "%edx", "%edx", "");
    Instruction ("testl", "%eax", "%eax", "");
    Instruction ("setle", "%dl", "", "");
    Instruction ("jmp", ".le_cleanup", "", "");
    Instruction (".size", "le_handler", ".-le_handler", "");
    Line "\t.p2align 4";
    Instruction (".globl", "eq_handler", "", "");
    Instruction (".type", "eq_handler", "@function", "");
    Line "eq_handler:";
    Instruction ("pushq", "%r12", "", "");
    Instruction ("pushq", "%rbp", "", "");
    Instruction ("movq", "%rsi", "%rbp", "");
    Instruction ("pushq", "%rbx", "", "");
    Instruction ("movq", "%rdi", "%rbx", "");
    Instruction ("call", "Bool..new", "", "");
    Instruction ("movq", "%rax", "%r12", "");
    Instruction ("movq", "%rax", "%r12", "");
    Instruction ("cmpq", " %rbx, %rbp", "", "");
    Instruction ("je", " .le_true", "", "");
    Instruction ("testq", "%rbx", "%rbx", "");
    Instruction ("je", ".eq_false", "", "");
    Instruction ("testq", "%rbp", "%rbp", "");
    Instruction ("je", ".eq_false", "", "");
    Instruction ("movq", "0(%rbp)", "%rax", "");
    Instruction ("addq", "(%rbx)", "%rax", "");
    Instruction ("leaq", "-2(%rax)", "%rdx", "");
    Instruction ("testq", "$-3", "%rdx", "");
    Instruction ("je", ".eq_num", "", "");
    Instruction ("xorl", "%edx", "%edx", "");
    Instruction ("cmpq", "%rbp", "%rbx", "");
    Instruction ("sete", "%dl", "", "");
    Instruction ("cmpq", "$10", "%rax", "");
    Instruction ("je", ".eq_string", "", "");
    Line ".eq_cleanup:";
    Instruction ("movq", "%rdx", "24(%r12)", "");
    Instruction ("movq", "%r12", "%rax", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("ret", "", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".eq_false:";
    Instruction ("xorl", "%edx", "%edx", "");
    Instruction ("movq", "%r12", "%rax", "");
    Instruction ("movq", "%rdx", "24(%r12)", "");
    Instruction ("popq", "%rbx", "", "");
    Instruction ("popq", "%rbp", "", "");
    Instruction ("popq", "%r12", "", "");
    Instruction ("ret", "", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".eq_num:";
    Instruction ("xorl", "%edx", "%edx", "");
    Instruction ("movq", "24(%rbp)", "%rax", "");
    Instruction ("cmpq", "%rax", "24(%rbx)", "");
    Instruction ("sete", "%dl", "", "");
    Instruction ("jmp", ".eq_cleanup", "", "");
    Line "\t.p2align 4,,10";
    Line "\t.p2align 3";
    Line ".eq_string:";
    Instruction ("movq", "24(%rbp)", "%rsi", "");
    Instruction ("movq", "24(%rbx)", "%rdi", "");
    Instruction ("call", "strcmp", "", "");
    Instruction ("xorl", "%edx", "%edx", "");
    Instruction ("testl", "%eax", "%eax", "");
    Instruction ("sete", "%dl", "", "");
    Instruction ("jmp", ".eq_cleanup", "", "");
    Instruction (".size", "eq_handler", ".-eq_handler", "");
  ]
