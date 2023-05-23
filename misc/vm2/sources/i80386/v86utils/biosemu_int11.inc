;------------------------------- read equipment list interrupt...
proc int11h
call interrupt_begin
sub ax,ax
mov ds,ax
mov ax,def:[410h]
mov cs:[stack_reg_eax],ax
jmp interrupt_finish
endp
;-------------------------------
