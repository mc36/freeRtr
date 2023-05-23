use16
org 100h
;-------------------------------

cld
mov eax,cs
mov es,ax
mov ds,ax
mov fs,ax
mov gs,ax

mov di,offset vesa_buffer
sub ax,ax
mov cx,1024
rep
  stosw
mov di,offset vesa_buffer
mov dword def:[di],32454256h
mov ax,4f00h                    ;GET SuperVGA INFORMATION...
int 10h
mov si,offset text01
cmp ax,004fh
jne word vege
mov bp,offset vesa_buffer
mov eax,cs:[bp]
cmp eax,41534556h
jne word vege
mov ax,cs:[bp+04h]
mov def:[vesa_version],ax
push ax
mov si,offset text03
call write
pop ax
push ax
movzx eax,ah
call conv2dec
call write
mov si,offset text04
call write
pop ax
movzx eax,al
call conv2dec
call write
mov si,offset textCRLF
call write
mov si,offset text05
call write
lds si,cs:[bp+06h]
call write
push cs
pop ds
mov si,offset textCRLF
call write
mov si,offset text06
call write
movzx word eax,cs:[bp+12h]
shl eax,6
call conv2dec
call write
mov si,offset text07
call write
mov si,offset textCRLF
call write
mov ax,def:[vesa_version]
cmp ax,0200h
jb byte main_j1
mov si,offset text08
call write
lds si,cs:[bp+16h]
call write
push cs
pop ds
mov si,offset textCRLF
call write
mov si,offset text09
call write
lds si,cs:[bp+1ah]
call write
push cs
pop ds
mov si,offset textCRLF
call write
mov si,offset text10
call write
lds si,cs:[bp+1eh]
call write
push cs
pop ds
mov si,offset textCRLF
call write
main_j1:

mov dword def:[vesa_mode],0000h
mov dword def:[vesa_max],01ffh
sub si,si
call string2num
jc byte main_j2
mov def:[vesa_mode],dx
call string2num
jc byte main_j2
mov def:[vesa_max],dx
main_j2:

mov si,offset text11
call write
mov eax,def:[vesa_mode]
mov cl,4
call conv2hex
call write
mov si,offset text13
call write
mov eax,def:[vesa_max]
mov cl,4
call conv2hex
call write
mov si,offset text14
call write
mov si,offset text12
call write

dec dword def:[vesa_mode]
read_j1:
inc dword def:[vesa_mode]
mov di,offset vesa_buffer
sub ax,ax
mov cx,1024
rep
  stosw
mov ax,4f01h                    ;GET SuperVGA MODE INFORMATION...
mov ecx,def:[vesa_mode]
cmp ecx,def:[vesa_max]
ja word read_j2
mov di,offset vesa_buffer
int 10h
cmp ax,004fh
jne byte read_j1
mov bp,offset vesa_buffer
mov ax,cs:[bp]
mov cx,11b
and ax,cx
cmp ax,cx
jne byte read_j1
mov di,offset lastbyte
mov eax,def:[vesa_mode]
mov cl,4
call conv2hex
call copy
movzx word eax,cs:[bp+12h]
call conv2dec
call copy
movzx word eax,cs:[bp+14h]
call conv2dec
call copy
movzx byte eax,cs:[bp+19h]
call conv2dec
call copy


mov si,offset textCRLF
call copy
mov si,offset lastbyte
inc si
call write
jmp word read_j1
read_j2:



mov si,offset text02
vege:
call write
sub eax,eax
clts                            ;terminate process...
dw 0
;-------------------------------



;-------------------------------
proc copy
;in: ds:si-source...
;    es:di-target...
push si
push ax
mov al,' '
stosb
copy_j1:
lodsb
stosb
or al,al
jnz byte copy_j1
dec di
pop ax
pop si
ret
endp
;-------------------------------



;-------------------------------
textCRLF db 13,10,0
text01 db 'error: vesa required!',13,10,0
text02 db 0
text03 db 'vesa version: v',0
text04 db '.',0
text05 db 'oem name: ',0
text06 db 'video ram: ',0
text07 db ' kb',0
text08 db 'vendor: ',0
text09 db 'product: ',0
text10 db 'revision: ',0
text11 db 'list of modes in range ',0
text12 db 'mode maxX maxY bits',13,10,0
text13 db '..',0
text14 db ':',13,10,0
;-------------------------------

;-------------------------------
include vesaUtils.inc
;-------------------------------

;-------------------------------
vesa_version dw ?
vesa_mode dd ?
vesa_max dd ?
vesa_buffer db 2048 dup (?)
lastbyte:
;-------------------------------
