;----------------------------------
vgaModeRec_attr equ 0   ;  Attribute:array[1..21] of byte;
vgaModeRec_crtc equ 21  ;  CRTC:array[1..24] of byte;
vgaModeRec_grfx equ 45  ;  Graphics:array[1..9] of byte;
vgaModeRec_misc equ 54  ;  MiscOutReg:byte;
vgaModeRec_sequ equ 55  ;  Sequencer:array[1..5] of byte;
vgaModeRec_siz  equ 60  ;  end;
;----------------------------------
vgaFontRec_siz equ 4096
vgaPalRec_siz equ 768
;----------------------------------

;----------------------------------
proc vga_SeekCursor
;cx-cursor position
mov dx,3d4h
mov al,0fh
out dx,al
inc dx
mov al,cl
out dx,al
dec dx
mov al,0eh
out dx,al
inc dx
mov al,ch
out dx,al
retnd
endp
;----------------------------------

;----------------------------------
proc vga_SizeCursor
;ch-cursor start scanline
;cl-cursor end scanline
mov dx,3d4h
mov al,0ah
out dx,al
inc dx
mov al,ch
out dx,al
dec dx
mov al,0bh
out dx,al
inc dx
mov al,cl
out dx,al
retnd
endp
;----------------------------------

;----------------------------------
proc vga_SetBlinking
;in: bl-0=intensity  1=blinking
mov cl,30h
call dword vga_ReadACTreg
or ch,8h
xor ch,8h
or ch,bl
call dword vga_WriteACTreg
retnd
endp
;----------------------------------

;----------------------------------
proc vga_BorderColor
;in: bh-border color
mov cl,31h
mov ch,bh
call dword vga_WriteACTreg
retnd
endp
;----------------------------------

;----------------------------------
proc vga_ReadACTreg
;in:  cl-reg num
;out: ch-value
mov dx,3dah
in al,dx
mov dx,3c0h
mov al,cl
out dx,al
inc dx
in al,dx
mov ch,al
retnd
endp
;----------------------------------

;----------------------------------
proc vga_WriteACTreg
;in: cl-reg num
;    ch-value
mov dx,3dah
in al,dx
mov dx,3c0h
mov al,cl
out dx,al
mov al,ch
out dx,al
retnd
endp
;----------------------------------

;----------------------------------
proc vga_ModeGet
;in: ds:edi-where mode data is...
mov dx,3cch
in al,dx
mov def:[edi+vgaModeRec_misc],al
mov dx,3dah
in al,dx
mov dx,3c0h
mov al,0
out dx,al
sub ebx,ebx
vga_ModeGet_j1:
mov al,bl
mov dx,3d4h
out dx,al
inc dx
in al,dx
mov def:[edi+ebx+vgaModeRec_crtc],al
inc bx
cmp bx,25
jb byte vga_ModeGet_j1
sub ebx,ebx
vga_ModeGet_j2:
mov dx,3dah
in al,dx
mov al,bl
mov dx,3c0h
out dx,al
inc dx
in al,dx
mov cs:[edi+ebx+vgaModeRec_attr],al
inc bx
cmp bx,21
jb byte vga_ModeGet_j2
sub ebx,ebx
vga_ModeGet_j3:
mov al,bl
mov dx,3ceh
out dx,al
inc dx
in al,dx
mov def:[edi+ebx+vgaModeRec_grfx],al
inc bx
cmp bx,9
jb byte vga_ModeGet_j3
sub ebx,ebx
vga_ModeGet_j4:
mov al,bl
mov dx,3c4h
out dx,al
inc dx
in al,dx
mov def:[edi+ebx+vgaModeRec_sequ],al
inc bx
cmp bx,5
jb byte vga_ModeGet_j4
mov dx,3dah
in al,dx
mov al,20h
mov dx,3c0h
out dx,al
retnd
endp
;----------------------------------

;----------------------------------
proc vga_ModeSet
;in: ds:edi-where mode data is...
mov dx,3dah
in al,dx
mov dx,3c0h
mov al,0
out dx,al
mov al,def:[edi+vgaModeRec_misc]
or al,1
mov dx,3c2h
out dx,al
sub ebx,ebx
vga_ModeSet_j1:
mov ah,def:[edi+ebx+vgaModeRec_sequ]
mov al,bl
mov dx,3c4h
out dx,ax
inc bx
cmp bx,5
jb byte vga_ModeSet_j1
mov ebx,17
mov ah,def:[edi+ebx+vgaModeRec_crtc]
and ah,7fh
mov al,bl
mov dx,3d4h
out dx,ax
sub ebx,ebx
vga_ModeSet_j2:
mov ah,def:[edi+ebx+vgaModeRec_crtc]
mov al,bl
mov dx,3d4h
out dx,ax
inc bx
cmp bx,25
jb byte vga_ModeSet_j2
sub ebx,ebx
vga_ModeSet_j3:
mov ah,def:[edi+ebx+vgaModeRec_grfx]
mov al,bl
mov dx,3ceh
out dx,ax
inc bx
cmp bx,9
jb byte vga_ModeSet_j3
sub ebx,ebx
vga_ModeSet_j4:
mov dx,3dah
in al,dx
mov dx,3c0h
mov al,bl
out dx,al
mov al,def:[edi+ebx+vgaModeRec_attr]
out dx,al
inc bx
cmp bx,21
jb byte vga_ModeSet_j4
mov dx,3dah
in al,dx
mov al,20h
mov dx,3c0h
out dx,al
retnd
endp
;----------------------------------

;----------------------------------
proc vga_FontSet
;in: es:esi-where font data is...
;    es:edi-writeable 0a000:0...
push esi
push edi
mov dx,3c4h
mov esi,offset vga_FontSet_d1
call dword vga_FontSet_j1
mov dx,3ceh
mov esi,offset vga_FontSet_d2
call dword vga_FontSet_j1
pop edi
pop esi
mov ecx,256
vga_FontSet_j3:
push ecx
mov ecx,8
rep
  movsw ptr32,es
sub ax,ax
mov ecx,8
rep
  stosw ptr32
pop ecx
loopd vga_FontSet_j3
mov dx,3c4h
mov esi,offset vga_FontSet_d3
call dword vga_FontSet_j1
mov dx,3ceh
mov esi,offset vga_FontSet_d4
call dword vga_FontSet_j1
retnd
vga_FontSet_d1:
db 00h,01h,02h,04h,04h,07h,00h,03h
dw 0ffffh
vga_FontSet_d2:
db 04h,02h,05h,00h,06h,00h
dw 0ffffh
vga_FontSet_d3:
db 00h,01h,02h,03h,04h,03h,00h,03h
dw 0ffffh
vga_FontSet_d4:
db 04h,00h,05h,10h,06h,0eh
dw 0ffffh
vga_FontSet_j1:
lodsw cs,ptr32
inc ax
jz byte vga_FontSet_j2
dec ax
out dx,al
mov al,ah
inc dx
out dx,al
dec dx
jmp byte vga_FontSet_j1
vga_FontSet_j2:
retnd
endp
;----------------------------------

;----------------------------------
proc vga_FontExp
;in: ds:esi-where font data is...
;    es:edi-where to expand...
mov ecx,256
vga_FontExp_j1:
push ecx
mov ecx,8
rep
  movsb ptr32
add edi,8
pop ecx
loopd vga_FontExp_j1
retnd
endp
;----------------------------------

;----------------------------------
proc vga_ColMono
;regs: ah-red
;      ch-green
;      cl-blue
push bx
push dx
and ax,3f00h
xchg al,ah
mul word cs:[vga_ColMono_d1]
push dx
push ax
mov al,ch
and al,3fh
xor ah,ah
mul word cs:[vga_ColMono_d2]
push dx
push ax
mov al,cl
and al,3fh
xor ah,ah
mul word cs:[vga_ColMono_d3]
pop bx
add ax,bx
pop bx
adc dx,bx
pop bx
add ax,bx
pop bx
adc dx,bx
add ax,ax
adc dx,dx
add ax,8000h
adc dx,0
mov ah,dl
mov cl,dl
mov ch,dl
pop dx
pop bx
retnd
vga_ColMono_d1 dw 2666h
vga_ColMono_d2 dw 4b85h
vga_ColMono_d3 dw 0e14h
endp
;----------------------------------

;----------------------------------
proc vga_ColUnpack
;in:  bl-value to convert; bitmap: 00rgbRGB
;out: ah-red
;     ch-green
;     cl-blue
push bx
call dword vga_ColUnpack_j1
mov cl,ah
call dword vga_ColUnpack_j1
mov ch,ah
call dword vga_ColUnpack_j1
pop bx
retnd
vga_ColUnpack_j1:
push bx
push ax
shl bx,5
shr bl,4
and bx,0102h
or bl,bh
mov bh,0
imul bx,21
pop ax
mov ah,bl
pop bx
shr bx,1
retnd
endp
;----------------------------------

;----------------------------------
proc vga_ColPack
;in:  ah-red
;     ch-green
;     cl-blue
;out: bl-converted value; bitmap: 00rgbRGB
push ax
mov bl,0
call dword vga_ColPack_j1
mov ah,ch
call dword vga_ColPack_j1
mov ah,cl
call dword vga_ColPack_j1
pop ax
retnd
vga_ColPack_j1:
push ax
push cx
movzx ax,ah
mov cl,21
div cl
and ax,3
shl ax,7
shr al,4
or al,ah
shl bl,1
or bl,al
pop cx
pop ax
retnd
endp
;----------------------------------

;----------------------------------
proc vga_PalPut
;in: ds:esi-where color data is...
mov dx,3c8h
sub ax,ax
out dx,al
inc dx
mov ecx,300h
vga_PalPut_j1:
lodsb ptr32
out dx,al
loopd vga_PalPut_j1
retnd
endp
;----------------------------------
