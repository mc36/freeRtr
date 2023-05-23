;------------------------------- keyboard interrupt handler...
proc int16h
call interrupt_begin
mov dx,cs:[stack_reg_eax]
mov si,offset int16h_fnTab
int16h_j1:
lodsb cs
mov dl,al
lodsw cs
or ax,ax
jz byte int16h_j2
cmp dl,dh
jne byte int16h_j1
jmp ax
int16h_j2:
mov si,offset int16h_txt1
call interrupt_error
int16h_vege:
jmp interrupt_finish

int16h_txt1 db 'invalid int16h function requested!',0

int16h_fn00:                ;keyboard read...
call keybrd_recvStrokes
call keybrd_readKey
jc byte int16h_fn00
mov cs:[stack_reg_eax],ax
call keybrd_removeKey
jmp word int16h_vege

int16h_fn01:                ;keyboard status...
bts word cs:[int_saved_flg],flagVals_zero
call keybrd_recvStrokes
call keybrd_readKey
jc word int16h_vege
mov cs:[stack_reg_eax],ax
btr word cs:[int_saved_flg],flagVals_zero
jmp word int16h_vege

int16h_fn02:                ;shift status...
call keybrd_readKey
mov cs:[stack_reg_eax],cl
jmp word int16h_vege

int16h_fn03:                ;set typematic rate...
jmp word int16h_vege

int16h_fn04:                ;adjust keyboard click...
jmp word int16h_vege

int16h_fn05:                ;keyboard write...
mov ax,cs:[stack_reg_ecx]
call keybrd_appendKey
mov byte cs:[stack_reg_eax],0
jmp word int16h_vege

int16h_fn12:                ;extended shift status...
call keybrd_readKey
mov cs:[stack_reg_eax],cx
jmp word int16h_vege

int16h_fnTab:
db 00h
dw offset int16h_fn00
db 01h
dw offset int16h_fn01
db 02h
dw offset int16h_fn02
db 03h
dw offset int16h_fn03
db 04h
dw offset int16h_fn04
db 05h
dw offset int16h_fn05
db 10h
dw offset int16h_fn00
db 11h
dw offset int16h_fn01
db 12h
dw offset int16h_fn12
db 0,0,0                        ;eot...
endp
;-------------------------------
