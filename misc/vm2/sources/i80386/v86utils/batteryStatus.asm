use16
org 100h
;-------------------------------

mov si,offset text01
call write

mov ax,5301h
mov bx,0
int 15h

mov ax,530ah
mov bx,0001h
int 15h
mov si,offset text02
jc word vege

mov si,offset text03
call write
mov ah,bh
mov si,offset text04
call getFromTable
call write
mov si,offset textCRLF
call write

mov si,offset text09
call write
mov ah,bl
mov si,offset text10
call getFromTable
call write
mov si,offset textCRLF
call write

mov si,offset text15
call write
movzx eax,cl
call conv2dec
call write
mov si,offset text16
call write
mov si,offset textCRLF
call write



mov si,offset textNONE
vege:
call write
sub eax,eax
clts
dw 00h
;-------------------------------

;-------------------------------
proc getFromTable
;in:  ah-byte to find...
;     si-offset of list...
;out: si-offset of text...
getFromTable_j1:
mov di,def:[si]
inc si
inc si
or di,di
jz byte getFromTable_j2
cmp ah,def:[di]
jne byte getFromTable_j1
lea si,def:[di+1]
ret
getFromTable_j2:
mov si,offset text08
inc si
ret
endp
;-------------------------------

;-------------------------------
textCRLF db 13,10
textNONE db 0
text01 db 'battery information v1.0, done by Mc at ',%date,' ',%time,'.',13,10,0
text02 db 'failed to get power status!',13,10,0
text03 db 'power supply: ',0
text04 dw offset text05,offset text06,offset text07,offset text08,0
text05 db 00h,'battery',0
text06 db 01h,'ac power',0
text07 db 02h,'backup power',0
text08 db 0ffh,'unknown',0
text09 db 'battery status: ',0
text10 dw offset text11,offset text12,offset text13,offset text14,offset text08,0
text11 db 00h,'high',0
text12 db 01h,'low',0
text13 db 02h,'critical',0
text14 db 03h,'charging',0
text15 db 'battery load: ',0
text16 db '%',0
;-------------------------------

;-------------------------------
proc write
;in: ds:si-offset of data...
push eax
push ecx
push esi
sub ecx,ecx
write_j1:
inc ecx
lodsb
or al,al
jnz byte write_j1
dec ecx
and ecx,1ffh
pop esi
push esi
movzx eax,si
mov esi,ds
shl esi,4
add esi,eax
clts
dw 20h
pop esi
pop ecx
pop eax
ret
endp
;-------------------------------

;-------------------------------
proc conv2dec
;in:  eax-number to write...
;out: cs:si-text where asciiz is..;)
push ds
push eax
push ecx
push edx
push bx
push cs
pop ds
mov ecx,10
mov si,offset conv2hex_d2
mov byte def:[si],0
mov bh,3
conv2dec_j1:
sub edx,edx
div ecx
add dl,'0'
dec si
mov def:[si],dl
or eax,eax
jnz byte conv2dec_j1
conv2dec_j2:
push si
mov al,' '
conv2dec_j3:
dec si
cmp si,offset conv2hex_d1
jb byte conv2dec_j4
mov def:[si],al
jmp byte conv2dec_j3
conv2dec_j4:
pop si
pop bx
pop edx
pop ecx
pop eax
pop ds
ret
endp
;-------------------------------

;-------------------------------
proc conv2hex
;in:  eax-number to convert...
;     cl-align number...
;out: cs:si-text where asciiz is..;)
push dx
push bx
push cx
mov si,offset conv2hex_d1
mov ch,0
push cx
neg cl
add cl,8
shl cl,2
rol eax,cl
pop cx
mov bx,offset conv2hex_d3
conv2hex_j1:
rol eax,4
push eax
and al,0fh
xlat cs
mov cs:[si],al
inc si
pop eax
loopw conv2hex_j1
mov byte cs:[si],0
mov si,offset conv2hex_d0
pop cx
pop bx
pop dx
ret
conv2hex_d3 db '0123456789ABCDEF'
conv2hex_d0 db '$'
conv2hex_d1:
db 15 dup (?)
conv2hex_d2 db ?
endp
;-------------------------------
