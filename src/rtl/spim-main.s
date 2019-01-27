        .text
        .globl main
        .globl _gc_root_stack_limit

main:   la $a0, _gc_root_stack_limit # initialize the top-of-stack pointer for GC
        sw $sp, 0($a0)

        li $a0, 47104           # initialize the heap for gc
        sub $sp, $sp, 4        # 41704 words = 188416 bytes = arg1
        sw $a0, 0($sp)
        
        li $a0, 268509184       # 0x1010000 + 8192 - start of the heap = arg0
        sub $sp, $sp, 4            
        sw $a0, 0($sp)
        
        la $a0, _mm_heap_add_area
        jal $a0
        
        la $a0, _sasm_entry     # jump to main
        jal $a0
        li $v0, 10
        syscall

        .data
_gc_root_stack_limit:   
        .word 0
        
        