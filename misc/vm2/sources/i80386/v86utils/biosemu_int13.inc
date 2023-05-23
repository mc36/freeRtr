;------------------------------- disk io services...
proc int13h
call interrupt_begin
mov dx,cs:[stack_reg_eax]
mov si,offset int13h_fnTab
int13h_j1:
lodsb cs
mov dl,al
lodsw cs
or ax,ax
jz byte int13h_j2
cmp dl,dh
jne byte int13h_j1
jmp ax
int13h_j2:
mov si,offset int13h_txt1
call interrupt_error
jmp byte int13h_err
int13h_vege:
call video_send2console
jmp interrupt_finish
int13h_err:
bts word cs:[int_saved_flg],flagVals_carry
mov ax,cs:[stack_reg_eax]
mov ah,01h
mov cs:[stack_reg_eax],ax
jmp byte int13h_vege
int13h_ok:
btr word cs:[int_saved_flg],flagVals_carry
mov ax,cs:[stack_reg_eax]
mov ah,00h
mov cs:[stack_reg_eax],ax
jmp byte int13h_vege

int13h_fnTab:
db 00h                          ;reset disk system...
dw offset int13h_fn00
db 01h                          ;get disks system status...
dw offset int13h_fn00
db 02h                          ;read sectors...
dw offset int13h_fn02
db 03h                          ;write sectors...
dw offset int13h_fn03
;db 04h                          ;verify sectors...
;dw offset int13h_fn00
;db 05h                          ;format cylinder...
;dw offset int13h_fn00
db 08h                          ;get drive parameters...
dw offset int13h_fn08
db 15h                          ;get dasd type...
dw offset int13h_fn15
db 16h                          ;change of disk status...
dw offset int13h_fn00
db 41h                          ;install check...
dw offset int13h_fn41
db 42h                          ;read sectors...
dw offset int13h_fn42
db 43h                          ;write sectors...
dw offset int13h_fn43
;db 44h                          ;verify sectors...
;dw offset int13h_fn00
db 45h                          ;lock media...
dw offset int13h_fn00
db 46h                          ;eject media...
dw offset int13h_fn00
;db 47h                          ;seek...
;dw offset int13h_fn00
db 48h                          ;get params...
dw offset int13h_fn48
db 0,0,0                        ;eot...

int13h_txt1 db 'invalid int13h function requested!',0

int13h_selDrive:
mov al,cs:[stack_reg_edx]
mov cl,al
mov ch,al
and cx,7f03h
cmp cl,ch
jne byte int13h_selDrive_err
mov ah,al
and ax,8003h
shr ah,5
or al,ah
movzx ebp,al
shl ebp,2
mov eax,cs:[diskio_file+bp]
or eax,eax
jz byte int13h_selDrive_err
mov eax,cs:[diskio_size+bp]
or eax,eax
jz byte int13h_selDrive_err
ret
int13h_selDrive_err:
pop ax
jmp int13h_err

int13h_getCHS:
mov ax,cs:[stack_reg_ecx]
shr al,6
xchg al,ah
movzx eax,ax
mov ebx,cs:[diskio_hed+bp]
mul ebx
mov cx,cs:[stack_reg_edx]
movzx ecx,ch
add eax,ecx
mov ebx,cs:[diskio_sec+bp]
mul ebx
mov cx,cs:[stack_reg_ecx]
dec cl
and cl,3fh
movzx ecx,cl
add eax,ecx
ret

int13h_getLBA:
push ds
mov ds,cs:[stack_reg_ds]
mov si,cs:[stack_reg_esi]
mov eax,ds:[si+8]
pop ds
ret

int13h_seekFile:
cmp eax,cs:[diskio_size+bp]
jae byte int13h_seekFile_err
mov ecx,eax
shl ecx,9
mov eax,cs:[diskio_file+bp]
call kernel_fileSeek
or ebx,ebx
jnz byte int13h_seekFile_err
ret
int13h_seekFile_err:
pop ax
jmp int13h_err

int13h_getMemCHS:
mov ax,cs:[stack_reg_es]
mov si,cs:[stack_reg_ebx]
movzx eax,ax
movzx esi,si
shl eax,4
add esi,eax
mov cl,cs:[stack_reg_eax]
movzx ecx,cl
and cl,7fh
shl ecx,9
mov eax,cs:[diskio_file+bp]
mov edi,esi
ret

int13h_getMemLBA:
push ds
mov ds,cs:[stack_reg_ds]
mov si,cs:[stack_reg_esi]
mov cl,ds:[si+2]
movzx word eax,ds:[si+6]
movzx word esi,ds:[si+4]
shl eax,4
add esi,eax
movzx ecx,cl
and cl,7fh
shl ecx,9
pop ds
mov eax,cs:[diskio_file+bp]
mov edi,esi
ret

int13h_fn00:                    ;status...
call int13h_selDrive
jmp word int13h_ok

int13h_fn02:                    ;read...
call int13h_selDrive
call int13h_getCHS
call int13h_seekFile
call int13h_getMemCHS
call kernel_fileRead
or ebx,ebx
jnz word int13h_err
jmp word int13h_ok

int13h_fn03:                    ;write...
call int13h_selDrive
call int13h_getCHS
call int13h_seekFile
call int13h_getMemCHS
call kernel_fileWrite
or ebx,ebx
jnz word int13h_err
jmp word int13h_ok

int13h_fn08:                    ;get param...
call int13h_selDrive
mov eax,cs:[diskio_cyl+bp]
mov ebx,cs:[diskio_hed+bp]
mov ecx,cs:[diskio_sec+bp]
dec ax
dec bx
xchg al,ah
shl al,6
or al,cl
mov cs:[stack_reg_ecx],ax
mov ah,bl
mov al,4
mov cs:[stack_reg_edx],ax
jmp word int13h_ok

int13h_fn15:                    ;read dasd...
call int13h_selDrive
mov eax,cs:[diskio_cyl+bp]
mov ebx,cs:[diskio_hed+bp]
mov ecx,cs:[diskio_sec+bp]
imul eax,ebx
imul eax,ecx
mov cs:[stack_reg_edx],ax
shr eax,16
mov cs:[stack_reg_ecx],ax
btr word cs:[int_saved_flg],flagVals_carry
mov ax,cs:[stack_reg_eax]
mov ah,03h
mov cs:[stack_reg_eax],ax
jmp word int13h_vege

int13h_fn41:                    ;extended check...
call int13h_selDrive
mov ax,cs:[stack_reg_ebx]
cmp ax,55aah
jne word int13h_err
mov ax,0aa55h
mov cs:[stack_reg_ebx],ax
mov ax,0100h
mov cs:[stack_reg_eax],ax
mov ax,1h
mov cs:[stack_reg_ecx],ax
jmp word int13h_ok

int13h_fn42:                    ;read...
call int13h_selDrive
call int13h_getLBA
call int13h_seekFile
call int13h_getMemLBA
call kernel_fileRead
or ebx,ebx
jnz word int13h_err
jmp word int13h_ok

int13h_fn43:                    ;write...
call int13h_selDrive
call int13h_getLBA
call int13h_seekFile
call int13h_getMemLBA
call kernel_fileWrite
or ebx,ebx
jnz word int13h_err
jmp word int13h_ok

int13h_fn48:
call int13h_selDrive
mov es,cs:[stack_reg_ds]
mov di,cs:[stack_reg_esi]
mov ax,1ah
stosw
mov ax,3
stosw
mov eax,cs:[diskio_cyl+bp]
stosd
mov eax,cs:[diskio_hed+bp]
stosd
mov eax,cs:[diskio_sec+bp]
stosd
mov eax,cs:[diskio_cyl+bp]
mov ebx,cs:[diskio_hed+bp]
mov ecx,cs:[diskio_sec+bp]
imul eax,ebx
imul eax,ecx
stosd
sub eax,eax
stosd
mov ax,512
stosw
jmp word int13h_ok

endp
;-------------------------------
