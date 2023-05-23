;------------------------------- time services...
proc int1ah
call interrupt_begin
mov dx,cs:[stack_reg_eax]
mov si,offset int1ah_fnTab
int1ah_j1:
lodsb cs
mov dl,al
lodsw cs
or ax,ax
jz byte int1ah_j2
cmp dl,dh
jne byte int1ah_j1
jmp ax
int1ah_j2:
mov si,offset int1ah_txt1
call interrupt_error
bts word cs:[int_saved_flg],flagVals_carry
int1ah_vege:
jmp interrupt_finish

int1ah_txt1 db 'invalid int1Ah function requested!',0

int1ah_fn00:              ;get system timer counter...
sub ax,ax
mov cs:[stack_reg_eax],al
mov ds,ax
mov eax,def:[46ch]
mov cs:[stack_reg_edx],ax
shr eax,16
mov cs:[stack_reg_ecx],ax
jmp word int1ah_vege

int1ah_fn02:              ;get real-time clock time...
call kernel_getTime
call conv2bcd
push ax
mov al,bl
call conv2bcd
pop bx
mov ah,bl
mov cs:[stack_reg_ecx],ax
mov al,cl
call conv2bcd
shl ax,8
mov cs:[stack_reg_edx],ax
btr word cs:[int_saved_flg],flagVals_carry
jmp word int1ah_vege

int1ah_fn04:              ;get real-time clock date...
call kernel_getDate
sub dx,dx
mov si,100
div si
call conv2bcd
push ax
mov al,dl
call conv2bcd
pop dx
mov ah,dl
mov cs:[stack_reg_ecx],ax
mov al,bl
call conv2bcd
push ax
mov al,cl
call conv2bcd
pop dx
mov ah,dl
mov cs:[stack_reg_edx],ax
btr word cs:[int_saved_flg],flagVals_carry
jmp word int1ah_vege

int1ah_fn07:              ;reset real-time clock alarm...
jmp word int1ah_vege

int1ah_fnTab:
db 00h
dw offset int1ah_fn00
db 02h
dw offset int1ah_fn02
db 04h
dw offset int1ah_fn04
db 07h
dw offset int1ah_fn07
db 0,0,0                        ;eot...
endp
;-------------------------------
