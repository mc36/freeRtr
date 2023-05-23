;------------------------------- serial io services...
proc int14h
call interrupt_begin
mov dx,cs:[stack_reg_eax]
mov si,offset int14h_fnTab
int14h_j1:
lodsb cs
mov dl,al
lodsw cs
or ax,ax
jz byte int14h_j2
cmp dl,dh
jne byte int14h_j1
jmp ax
int14h_j2:
mov si,offset int14h_txt1
call interrupt_error
int14h_err:
mov ax,8000h
mov cs:[stack_reg_eax],ax
int14h_vege:
jmp interrupt_finish

int14h_txt1 db 'invalid int14h function requested!',0

int14h_fnTab:
db 00h                          ;init serial port...
dw offset int14h_fn03
db 01h                          ;send character...
dw offset int14h_fn01
db 02h                          ;read character...
dw offset int14h_fn02
db 03h                          ;get status...
dw offset int14h_fn03
db 04h                          ;init serial port...
dw offset int14h_fn03
db 0,0,0                        ;eot...

int14h_selPort:
movzx byte ebp,cs:[stack_reg_edx]
and bp,3
shl bp,2
mov eax,cs:[serio_data+bp]
or eax,eax
jnz byte int14h_selPort_j1
pop ax
jmp word int14h_err
int14h_selPort_j1:
ret

int14h_fn01:                    ;send char...
call int14h_selPort
mov esi,offset stack_reg_eax
mov ax,cs
movzx eax,ax
shl eax,4
add esi,eax
mov eax,cs:[serio_data+bp]
mov ecx,1
call kernel_pipeSend
mov ax,cs:[stack_reg_eax]
mov ah,0
mov cs:[stack_reg_eax],ax
jmp word int14h_vege

int14h_fn02:                    ;read character...
call int14h_selPort
call serio_fillBuf
mov ax,cs:[serio_bufP+bp]
cmp al,ah
jae byte int14h_fn02_j1
movzx si,al
inc al
mov cs:[serio_bufP+bp],ax
mov al,cs:[serio_bufD+bp+si]
mov ah,0
jmp byte int14h_fn02_j2
int14h_fn02_j1:
mov ax,8000h
int14h_fn02_j2:
mov cs:[stack_reg_eax],ax
jmp word int14h_vege

int14h_fn03:                    ;get status...
call int14h_selPort
call serio_fillBuf
mov ax,cs:[serio_bufP+bp]
cmp al,ah
setb ah
or ah,60h
mov al,30h
mov cs:[stack_reg_eax],ax
jmp word int14h_vege

endp
;-------------------------------
