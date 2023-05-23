;------------------------------- extended services...
proc int15h
call interrupt_begin
mov dx,cs:[stack_reg_eax]
mov si,offset int15h_fnTab
int15h_j1:
lodsb cs
mov dl,al
lodsw cs
or ax,ax
jz byte int15h_j2
cmp dl,dh
jne byte int15h_j1
jmp ax
int15h_j2:
mov si,offset int15h_txt1
call interrupt_error
bts word cs:[int_saved_flg],flagVals_carry
int15h_vege:
jmp interrupt_finish

int15h_txt1 db 'invalid int15h function requested!',0

int15h_fn88:              ;get extended memory size...
btr word cs:[int_saved_flg],flagVals_carry
sub ax,ax
mov cs:[stack_reg_eax],ax
jmp word int15h_vege

int15h_fnTab:
db 88h
dw offset int15h_fn88
db 0,0,0                        ;eot...
endp
;-------------------------------
