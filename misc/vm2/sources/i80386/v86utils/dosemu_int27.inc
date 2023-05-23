;------------------------------- terminate and stay resident...
proc int27h
call interrupt_begin
mov ax,cs:[stack_reg_edx]
shr ax,4
inc ax
mov cs:[stack_reg_edx],ax
sub ax,ax
mov cs:[int_saved_ip],ax
jmp int21h_fn31
endp
;-------------------------------
