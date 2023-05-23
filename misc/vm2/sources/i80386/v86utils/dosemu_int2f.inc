;------------------------------- multiplexer...
proc int2fh
call interrupt_begin
mov si,offset int2fh_txt1
call interrupt_error
mov ax,1
mov cs:[stack_reg_eax],ax
mov cs:[dosfnct_errCod],ax
bts word cs:[int_saved_flg],flagVals_carry
jmp interrupt_finish
int2fh_txt1 db 'invalid int2Fh function requested!',0
endp
;-------------------------------
