org 0f000h                      ;multiboot/failless disk...
firstbyte:
use16
cli
cld
jmp word start4
;-------------------------------


;------------------------------- error handler...
proc error
mov ax,0e21h
int 10h
int 19h
jmp byte error
endp
;-------------------------------


;------------------------------- multiboot header...
align 40h
header:
temp01 dd 01badb002h    ;magic...
temp02 dd 000010000h    ;flags...
temp03 dd 0e4514ffeh    ;chksum...
temp04 dd 200040h       ;header_addr
temp05 dd 200040h       ;load_addr
temp06 dd 240000h       ;load_end_addr
temp07 dd 0             ;bss_end_addr
temp08 dd 200080h       ;entry_addr
;-------------------------------



;------------------------------- constants...
drive_num equ 80h
;-------------------------------



;------------------------------- copy to final location...
align 80h
use32
proc start1
cli
cld
mov esi,200000h
mov edi,offset firstbyte
mov ecx,40000h
rep
  movsb ptr32
db 0ebh,00h                     ;jmp next...
lgdt dword def:[GlobDescrLoad]
db 0ebh,00h                     ;jmp next...
jmp 8:offset start2
endp
;-------------------------------



;------------------------------- gdt/idt tables...
align 4
GlobDescrLoad:
GlobDescrSize dw 18h
GlobDescrBase dd offset GlobDescrData

align 4
IntrDescrLoad:
IntrDescrSize dw 3ffh
IntrDescrBase dd 0

align 10
GlobDescrData:
db 0,0,0,0,0,0,0,0                              ;null seg...
db 0ffh,0ffh,0,0,0,10011011b,00010000b,0        ;code16,seg 0,lim 64kb...
db 0ffh,0ffh,0,0,0,10010011b,00010000b,0        ;data16,seg 0,lim 64kb...
;-------------------------------



;------------------------------- switch back to real mode...
align 4
use16
proc start2
mov eax,cr0
and al,0feh
mov cr0,eax
db 0ebh,00h                     ;jmp next...
jmp 0:offset start3
endp
;-------------------------------



;------------------------------- start the kernel itself...
align 100h
proc start3
db 0ebh,00h                     ;jmp next...
lidt dword cs:[IntrDescrLoad]
db 0ebh,00h                     ;jmp next...
start3_j1:
mov ax,offset lastbyte
mov cx,100h
sub ax,cx
shr ax,4
mov ss,ax
mov sp,cx
mov es,ax
mov ds,ax
mov fs,ax
mov gs,ax
push ax
push cx
retf
endp
;-------------------------------



;------------------------------- copy to final location...
proc start4
sub ax,ax
mov di,offset firstbyte
mov ss,ax
mov sp,di
mov es,ax
mov ds,ax
call start4_j1
start4_j1:
pop si
sub si,offset start4_j1
add si,di
mov cx,512
rep
  movsb cs
jmp 0:offset start5
endp
;-------------------------------



;------------------------------- load the kernel...
proc start5
mov dl,drive_num
mov ah,8
int 13h
jc word error
movzx eax,cl
and al,3fh
mov def:[temp03],eax
mov al,dh
inc ax
mov def:[temp02],eax
xchg cl,ch
shr ch,6
mov ax,cx
inc ax
inc ax
mov def:[temp01],eax
mov ax,1
mov def:[temp04],eax
mov ax,768
mov def:[temp05],eax
mov ax,offset lastbyte
shl eax,12
mov def:[temp06],eax

start5_j1:
mov eax,def:[temp04]
mov ebx,def:[temp03]
imul ebx,def:[temp02]
sub edx,edx
div ebx
xchg al,ah
shl al,6
mov cx,ax
mov eax,edx
sub edx,edx
div dword def:[temp03]
inc dx
or cl,dl
mov dh,al
mov dl,drive_num
mov ax,0201h
les bx,def:[temp06]
int 13h
jc word error
push cs
pop es
dec word def:[temp05]
js word start3_j1
mov eax,def:[temp06]
add eax,200000h
mov def:[temp06],eax
inc word def:[temp04]
jmp byte start5_j1
endp
;-------------------------------



;------------------------------- boot sector signature...
align 510
db 055h,0aah
lastbyte:
;-------------------------------
