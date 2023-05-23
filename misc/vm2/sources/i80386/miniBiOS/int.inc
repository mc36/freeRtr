;----------------------------------- interrupt ends
proc intend_carry
push bp
mov bp,sp
bts word ss:[bp+6],0
pop bp
iret
endp
;-----------------------------------

;----------------------------------- interrupt ends
proc intend_nocarr
push bp
mov bp,sp
btr word ss:[bp+6],0
pop bp
iret
endp
;-----------------------------------

;----------------------------------- interrupt ends
proc intend_zero
push bp
mov bp,sp
bts word ss:[bp+6],6
pop bp
iret
endp
;-----------------------------------

;----------------------------------- interrupt ends
proc intend_nozero
push bp
mov bp,sp
btr word ss:[bp+6],6
pop bp
iret
endp
;-----------------------------------

;----------------------------------- interrupt ends
proc intend_nop
iret
endp
;-----------------------------------


;-----------------------------------
proc setupInt
;in: al-int number...
;    dx-offset of handler...
push ds
push bx
sub bx,bx
mov ds,bx
movzx bx,al
shl bx,2
mov def:[bx],dx
mov def:[bx+2],cs
pop bx
pop dx
ret
endp
;-----------------------------------

;-----------------------------------
proc defaultInt
iret
endp
;-----------------------------------

;-----------------------------------
proc timerIrq
push ax
push ds
sub ax,ax
mov ds,ax
inc dword def:[46ch]
pop ds
mov al,20h
out 020h,al
out 0a0h,al
pop ax
iret
endp
;-----------------------------------

;-----------------------------------
proc defaultIrq
mov al,20h                      ;we have to tell to pic1 and
out 020h,al                     ; picZ that the irq servicer
out 0a0h,al                     ; had finished successfully...
iret
endp
;-----------------------------------



;----------------------------------- video functions...
proc int10
cmp ah,0eh
je int10_write
jmp intend_nop
;-----------------

;----------------- write
int10_write:
pushad
call sioWrite
popad
jmp intend_nop
endp
;-----------------------------------



;----------------------------------- keyboard functions...
proc int16
cmp ah,01h
je int16_check
cmp ah,11h
je int16_check
cmp ah,00h
je int16_read
cmp ah,10h
je int16_read
jmp intend_nop
;-----------------

;----------------- check
int16_check:
jmp intend_zero
;-----------------

;-----------------
int16_read:
mov ax,1c0dh
jmp intend_nop
endp
;-----------------------------------



;----------------------------------- reboot...
proc int19
jmp dword 0ffffh:0h
endp
;-----------------------------------



;----------------------------------- disk functions...
proc int13
cmp dl,80h  ;valid disk?
jne int13_err         ;no...

cmp ah,0 ;reset?
je int13_reset
cmp ah,1 ;status?
je int13_reset
cmp ah,2 ;read?
je int13_read
cmp ah,3 ;write?
je int13_err
cmp ah,8 ;get param?
je int13_param
cmp ah,15h ;read dasd?
je int13_dasd

jmp intend_nop
;-----------------

;----------------- reset
int13_reset:
mov ah,0
jmp int13_ok
;-----------------

;----------------- read
int13_read:
pushad
push ds
push bx
push ax
mov ax,dataSeg
mov ds,ax

call ConvertCLCHDH2RawPos

pop dx
pop di
int13_read_j1:
push ecx
push di
push dx
call drive_read
pop dx
pop di
pop ecx
inc ecx
add di,512
dec dl
or dl,dl
jnz byte int13_read_j1
pop ds
popad
jmp int13_ok
;-----------------

;----------------- parameters
int13_param:
push bp
push ds
mov ax,dataSeg
mov ds,ax
mov cx,def:[driveData_lcyl]
sub cx,2
xchg cl,ch
shl cl,6
mov al,def:[driveData_lsec]
or cl,al
mov dh,def:[driveData_lhed]
dec dh
pop ds
pop bp
mov ah,0
mov dl,1
jmp intend_nocarr
;-----------------

;----------------- parameters
int13_dasd:
push eax
push bp
push ecx
push edx
push ds
mov ax,dataSeg
mov ds,ax
movzx word eax,def:[driveData_lcyl]
dec eax
movzx byte ecx,def:[driveData_lhed]
mul ecx
movzx byte ecx,def:[driveData_lsec]
mul ecx
pop ds
pop edx
pop ecx
pop bp
mov dx,ax
shr eax,16
mov cx,ax
pop eax
mov ah,3h
mov al,dl
jmp intend_nocarr
;-----------------

;----------------- end.ok
int13_ok:
mov ah,0
jmp intend_nocarr
;-----------------

;----------------- end.err
int13_err:
mov al,1
jmp intend_carry
;-----------------

endp
;-----------------------------------

;----------------------------------- chs2lba
proc ConvertCLCHDH2RawPos
push ebx
push edx
mov ax,cx
shr al,6
xchg al,ah
movzx eax,ax
movzx byte ebx,def:[driveData_lhed]
mul ebx
pop edx
push edx
movzx ebx,dh
add eax,ebx
movzx byte ebx,def:[driveData_lsec]
mul ebx
mov bl,cl
and bl,3fh
movzx ebx,bl
add eax,ebx
dec eax
mov ecx,eax
pop edx
pop ebx
ret
endp
;-----------------------------------
