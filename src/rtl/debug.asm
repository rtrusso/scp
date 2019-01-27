	bits 32
	section .text

        global _debug_corruption
        global _debug_corruption_id
        global _debug_corruption_pointer
        global _debug_corruption_watch
        extern _mj_system_out_println

_debug_corruption:
        push eax
        push ebx
        push ecx
        push edx
        push edi
        push esi
        mov dword [_debug_corruption_arg], eax
        inc dword [_debug_corruption_counter]

_debug_corruption_proceed:
        ;; jmp NEAR _debug_corruption_nonspecial

        ;; cmp [_debug_corruption_id], dword 1586
        ;; jnz NEAR _debug_corruption_nonspecial

        ;; cmp [_debug_corruption_counter], dword 4140
        ;; jnz NEAR _debug_corruption_nonspecial

        ;; mov eax, 73139125
        ;; push eax
        ;; call _mj_system_out_println
        ;; add esp, 4

        ;; mov eax, [_debug_corruption_watch]
        ;; push eax
        ;; call _mj_system_out_println
        ;; add esp, 4

        ;; mov eax, [_debug_corruption_watch]
        ;; mov eax, [eax]
        ;; push eax
        ;; call _mj_system_out_println
        ;; add esp, 4

        ;; mov eax, [_debug_corruption_arg]
        ;; push eax
        ;; call _mj_system_out_println
        ;; add esp, 4

        ;; mov eax, [_debug_corruption_arg]
        ;; mov eax, [eax+4]
        ;; push eax
        ;; call _mj_system_out_println
        ;; add esp, 4

        ;; mov eax, [_debug_corruption_arg]
        ;; mov eax, [eax]
        ;; push eax
        ;; call _mj_system_out_println
        ;; add esp, 4

        ;; mov eax, [_debug_corruption_watch]
        ;; mov [_debug_corruption_watch_internal], eax

        ;; mov eax, 7575123
        ;; push eax
        ;; call _mj_system_out_println
        ;; add esp, 4

;;         mov eax, [_debug_corruption_arg]
;;         mov [_debug_corruption_pointer], eax

;;         mov eax, [_debug_corruption_id]
;;         push eax
;;         call _mj_system_out_println
;;         add esp, 4

;;         mov eax, [_debug_corruption_counter]
;;         push eax
;;         call _mj_system_out_println
;;         add esp, 4

;;         mov eax, [_debug_corruption_watch_internal]
;;         push eax
;;         call _mj_system_out_println
;;         add esp, 4

;;         mov eax, [_debug_corruption_watch_internal]
;;         mov eax, [eax]
;;         push eax
;;         call _mj_system_out_println
;;         add esp, 4

;;         mov eax, [_debug_corruption_pointer]
;;         push eax
;;         call _mj_system_out_println
;;         add esp, 4

;;         mov eax, [eax]
;;         push eax
;;         call _mj_system_out_println
;;         add esp, 4

;;         mov eax, 1278127899
;;         push eax
;;         call _mj_system_out_println
;;         add esp, 4

;;         jmp NEAR _debug_corruption_done

_debug_corruption_nonspecial:

        ;;
        ;; mov eax, [_debug_corruption_id]
        ;; push eax
        ;; call _mj_system_out_println
        ;; add esp, 4
        ;;
        ;; mov eax, [_debug_corruption_counter]
        ;; push eax
        ;; call _mj_system_out_println
        ;; add esp, 4
        ;;
        ;;
        ;; mov eax, _debug_corruption_pointer
        ;; push eax
        ;; call _mj_system_out_println
        ;; add esp, 4

        ;; mov eax, [_debug_corruption_watch_internal]
        ;; cmp eax, 0
        ;; jz _debug_corruption_done

        ;; mov eax, [_debug_corruption_watch_internal]
        ;; push eax
        ;; call _mj_system_out_println
        ;; add esp, 4

        ;; mov eax, [_debug_corruption_watch_internal]
        ;; mov eax, [eax]
        ;; push eax
        ;; call _mj_system_out_println
        ;; add esp, 4

        ;; mov eax, [_debug_corruption_watch_internal]
        ;; mov eax, [eax+4]
        ;; push eax
        ;; call _mj_system_out_println
        ;; add esp, 4

        ;; mov eax, [_debug_corruption_watch_internal]
        ;; cmp [eax+8], dword 0xbaadfeed
        ;; jz _debug_corruption_done

        ;; mov eax, dword 234234234
        ;; push eax
        ;; call _mj_system_out_println
        ;; add esp, 4

        ;; mov eax, [_debug_corruption_id]
        ;; push eax
        ;; call _mj_system_out_println
        ;; add esp, 4

        ;; mov eax, [_debug_corruption_counter]
        ;; push eax
        ;; call _mj_system_out_println
        ;; add esp, 4

        ;; int 3

        ;;
        ;; cmp [_debug_corruption_pointer], dword 0
        ;; jz _debug_corruption_done
        ;; mov eax, [_debug_corruption_pointer]
        ;; push eax
        ;; call _mj_system_out_println
        ;; add esp, 4

        ;; mov eax, [_debug_corruption_pointer]
        ;; mov eax, [eax+4]
        ;; push eax
        ;; call _mj_system_out_println
        ;; add esp, 4
        ;;
        ;;         mov eax, [eax+4]
        ;;         cmp eax, 0xbaadfeed
        ;; mov ebx, [_debug_corruption_watch_internal]
        ;; cmp ebx, 0
        ;; jz _debug_corruption_done
        ;; mov eax, [_debug_corruption_pointer]
        ;; cmp eax, 0
        ;; jz _debug_corruption_done

        ;; mov eax, [_debug_corruption_watch_internal]
        ;; push eax
        ;; call _mj_system_out_println
        ;; add esp, 4

        ;; mov eax, [_debug_corruption_watch_internal]
        ;; mov eax, [eax]
        ;; push eax
        ;; call _mj_system_out_println
        ;; add esp, 4

        ;; mov eax, [_debug_corruption_pointer]
        ;; push eax
        ;; call _mj_system_out_println
        ;; add esp, 4

        ;; mov eax, [_debug_corruption_pointer]
        ;; mov eax, [eax]
        ;; push eax
        ;; call _mj_system_out_println
        ;; add esp, 4

        ;; mov ebx, [_debug_corruption_watch_internal]
        ;; mov eax, [_debug_corruption_pointer]
        ;; mov ebx, [ebx]
        ;; cmp eax, ebx
        ;; jnz _debug_corruption_done
        ;; mov eax, dword 234234234
        ;; push eax
        ;; call _mj_system_out_println
        ;; add esp, 4
        ;; int 3
_debug_corruption_done:
        ;; mov eax, 22222222
        ;; push eax
        ;; call _mj_system_out_println
        ;; add esp, 4
        ;; ;;
        ;; mov eax, 11111111
        ;; push eax
        ;; call _mj_system_out_println
        ;; add esp, 4
        ;;
        ;; mov eax, [_debug_corruption_pointer]
        ;; mov eax, [eax+4]
        ;; push eax
        ;; call _mj_system_out_println
        ;; add esp, 4
_debug_corruption_bail2:
_debug_corruption_bail:
        pop esi
        pop edi
        pop edx
        pop ecx
        pop ebx
        pop eax
        ret 0

        section .data

_debug_corruption_pointer:
        dd 0
        dd 0
        dd 0
        dd 0

_debug_corruption_watch:
        dd 0
        dd 0
        dd 0
        dd 0

_debug_corruption_watch_internal:
        dd 0
        dd 0
        dd 0
        dd 0

_debug_corruption_arg:
        dd 0
        dd 0
        dd 0
        dd 0

_debug_corruption_id:
        dd 0
        dd 0
        dd 0
        dd 0

_debug_corruption_counter:
        dd 0
        dd 0
        dd 0
        dd 0
