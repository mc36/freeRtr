;------------------------------- report memory size interrupt...
proc int12h
call interrupt_begin
sub ax,ax
mov ds,ax
mov ax,def:[413h]
mov cs:[stack_reg_eax],ax
jmp interrupt_finish
endp
;-------------------------------
