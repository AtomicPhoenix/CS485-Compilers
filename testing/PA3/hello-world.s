#start
Main_main_0:
movq my_attribute 0(%rbp) 
call $Int..new  
movq $5 24(%rax) 
movq %rax 8(%rbp) 
movq 0(%rbp) %rax 
movq 24(%rax) %rax 
movq 8(%rbp) %rdx 
movq 24(%rdx) %rdx 
addl %edx %eax 
pushq %rbp  
pushq %rax  
call $Int..new  
movq %rax %r10 
popq %rax  
popq %rbp  
movq %rax 24(%r10) 
movq %r10 16(%rbp) 
pusha   
pushq t$2  
andq $0xFFFFFFFFFFFFFFF0 %rsp 
call out_int  
movq %rax 24(%rbp) 
popa   
call $String..new  
movq $.string1 24(%rax) 
movq %rax 32(%rbp) 
pusha   
pushq t$5  
andq $0xFFFFFFFFFFFFFFF0 %rsp 
call out_string  
movq %rax 40(%rbp) 
popa   
movq %rbp %rsp 
popq %rbp %rsp 
ret   
