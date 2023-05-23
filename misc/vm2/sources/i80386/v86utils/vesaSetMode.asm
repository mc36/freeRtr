use16
org 100h
;-------------------------------

cld
mov eax,cs
mov es,ax
mov ds,ax
mov fs,ax
mov gs,ax

sub si,si
call string2num
mov si,offset text03
jc word vege
mov def:[vesa_mode],edx

mov di,offset vesa_buffer
sub ax,ax
mov cx,1024
rep
  stosw
mov ax,4f02h                    ;SET SuperVGA VIDEO MODE
mov ebx,def:[vesa_mode]
mov di,offset vesa_buffer
int 10h
mov si,offset text02
cmp ax,004fh
jne word vege

mov si,offset text01
vege:
call write
sub eax,eax
clts                            ;terminate process...
dw 0
;-------------------------------



;-------------------------------
textCRLF db 13,10,0
text01 db 'successful!',13,10,0
text02 db 'failed to set video mode!',13,10,0
text03 db 'using: vesa_setmode.v86 <mode>',13,10,0
;-------------------------------

;-------------------------------
include vesaUtils.inc
;-------------------------------

;-------------------------------
vesa_version dw ?
vesa_mode dd ?
vesa_buffer db 2048 dup (?)
lastbyte:
;-------------------------------
