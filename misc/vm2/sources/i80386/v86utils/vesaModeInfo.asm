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
mov si,offset text11
jc word vege
mov def:[vesa_mode],edx

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

mov si,offset text13
call write
mov eax,def:[vesa_mode]
mov cl,4
call conv2hex
call write
mov si,offset textCRLF
call write

mov di,offset vesa_buffer
sub ax,ax
mov cx,1024
rep
  stosw
mov ax,4f01h                    ;GET SuperVGA MODE INFORMATION...
mov ecx,def:[vesa_mode]
mov di,offset vesa_buffer
int 10h
mov si,offset text12
cmp ax,004fh
jne word vege
mov bp,offset vesa_buffer

mov di,cs:[bp+0]
mov si,offset text14
call yesno
shr di,1
mov si,offset text18
call yesno
mov si,offset text19
call yesno
mov si,offset text20
call yesno
shr di,1
mov si,offset text21
xor di,1
call yesno
mov si,offset text22
call yesno
mov si,offset text23
call yesno
mov si,offset text24
call yesno
mov si,offset text25
call yesno
mov si,offset text26
call yesno
mov si,offset text27
call yesno

mov si,offset text28
call write
movzx word eax,cs:[bp+10h]
call conv2dec
call write
mov si,offset textCRLF
call write

mov si,offset text29
call write
movzx word eax,cs:[bp+12h]
call conv2dec
call write
mov si,offset text30
call write
movzx word eax,cs:[bp+14h]
call conv2dec
call write
mov si,offset text30
call write
movzx byte eax,cs:[bp+19h]
call conv2dec
call write
mov si,offset textCRLF
call write
mov si,offset text31
call write
movzx byte eax,cs:[bp+1ah]
call conv2dec
call write
mov si,offset textCRLF
call write
mov si,offset text32
call write
movzx byte eax,cs:[bp+18h]
call conv2dec
call write
mov si,offset textCRLF
call write

mov si,offset text33
call write
movzx byte eax,cs:[bp+26h]
call conv2dec
call write
mov si,offset text30
call write
movzx byte eax,cs:[bp+25h]
call conv2dec
call write
mov si,offset textCRLF
call write
mov si,offset text34
call write
movzx byte eax,cs:[bp+20h]
call conv2dec
call write
mov si,offset text30
call write
movzx byte eax,cs:[bp+1fh]
call conv2dec
call write
mov si,offset textCRLF
call write
mov si,offset text35
call write
movzx byte eax,cs:[bp+22h]
call conv2dec
call write
mov si,offset text30
call write
movzx byte eax,cs:[bp+21h]
call conv2dec
call write
mov si,offset textCRLF
call write
mov si,offset text36
call write
movzx byte eax,cs:[bp+24h]
call conv2dec
call write
mov si,offset text30
call write
movzx byte eax,cs:[bp+23h]
call conv2dec
call write
mov si,offset textCRLF
call write

mov si,offset text44
call write
movzx word eax,cs:[bp+06h]
call conv2dec
call write
mov si,offset text07
call write
mov si,offset textCRLF
call write
mov si,offset text45
call write
movzx word eax,cs:[bp+04h]
call conv2dec
call write
mov si,offset text07
call write
mov si,offset textCRLF
call write

mov si,offset text39
mov ax,cs:[bp+08h]
mov dl,cs:[bp+02h]
call window
mov si,offset text40
mov ax,cs:[bp+0ah]
mov dl,cs:[bp+03h]
call window
mov si,offset text37
call write
mov eax,cs:[bp+28h]
mov cl,8
call conv2hex
call write
mov si,offset textCRLF
call write

mov si,offset text02
vege:
call write
sub eax,eax
clts                            ;terminate process...
dw 0
;-------------------------------



;-------------------------------
proc window
push dx
push ax
call write
pop ax
movzx eax,ax
shl eax,4
mov cl,8
call conv2hex
call write
mov si,offset text30
call write
pop di
test di,1
jz byte window_j1
mov si,offset text41
call write
window_j1:
test di,2
jz byte window_j2
mov si,offset text42
call write
window_j2:
test di,3
jz byte window_j3
mov si,offset text43
call write
window_j3:
mov si,offset textCRLF
call write
ret
endp
;-------------------------------

;-------------------------------
proc yesno
test di,1
push edi
setnz al
movzx bx,al
push bx
call write
pop bx
add bx,bx
mov si,def:[text17+bx]
call write
mov si,offset textCRLF
call write
pop edi
shr edi,1
ret
endp
;-------------------------------



;-------------------------------
textCRLF db 13,10,0
text01 db 'error: vesa required!',13,10,0
text02 db 'successful!',13,10,0
text03 db 'vesa version: v',0
text04 db '.',0
text05 db 'oem name: ',0
text06 db 'video ram: ',0
text07 db ' kb',0
text08 db 'vendor: ',0
text09 db 'product: ',0
text10 db 'revision: ',0
text11 db 'using: vesa_modeinfo.v86 <mode>',13,10,0
text12 db 'error: mode not found!',13,10,0
text13 db 'mode: ',0
text14 db 'supported: ',0
text15 db 'no',0
text16 db 'yes',0
text17 dw offset text15,offset text16
text18 db 'bios output: ',0
text19 db 'color mode: ',0
text20 db 'graphic mode: ',0
text21 db 'bank mode: ',0
text22 db 'lfb mode: ',0
text23 db 'dualscan mode: ',0
text24 db 'interlaced: ',0
text25 db 'triple buffer: ',0
text26 db 'stereoscopic: ',0
text27 db 'dual display: ',0
text28 db 'bytes/scanline: ',0
text29 db 'resolution: ',0
text30 db ' ',0
text31 db 'banks: ',0
text32 db 'planes: ',0
text33 db 'reserved: ',0
text34 db 'red: ',0
text35 db 'green: ',0
text36 db 'blue: ',0
text37 db 'framebuffer: ',0
text39 db 'window 0: ',0
text40 db 'window 1: ',0
text41 db 'x',0
text42 db 'r',0
text43 db 'w',0
text44 db 'window size: ',0
text45 db 'window granularity: ',0
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
