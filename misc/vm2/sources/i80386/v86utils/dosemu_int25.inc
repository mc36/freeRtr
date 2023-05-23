;------------------------------- direct disk access...
proc int25h
call interrupt_begin
sub word cs:[int_saved_sp],2
les di,cs:[int_saved_sp]
mov ax,cs:[int_saved_flg]
mov es:[di],ax
mov ax,101h
mov cs:[stack_reg_eax],ax
mov cs:[dosfnct_errCod],ax
bts word cs:[int_saved_flg],flagVals_carry
jmp interrupt_finish
endp
;-------------------------------
