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
proc lowerCase
cmp al,'A'
jb byte lowerCase_vege
cmp al,'Z'
ja byte lowerCase_vege
or al,20h
lowerCase_vege:
ret
endp
;-------------------------------

;-------------------------------
proc string2num
;in:  cs:si-text to convert..
;out: edx-number...
;     carry-clearer if success /jc error...;)
push eax
push cx
sub edx,edx
string2num_j1:
call string2num_Get
cmp al,' '
je byte string2num_j1
or al,al
jz byte string2num_err
cmp al,'$'
je byte string2num_hex
dec si
cmp al,'0'
jne byte string2num_dec
inc si
call string2num_Get
call lowerCase
cmp al,'x'
je byte string2num_hex
dec si
jmp byte string2num_dec
;---------------
string2num_hex:
call string2num_Get
cmp al,' '
je byte string2num_ok
or al,al
jz byte string2num_ok
mov ah,16
call string2num_Fnd
jz byte string2num_err
dec ax
shl edx,4
or dl,al
jmp byte string2num_hex
;---------------
string2num_err:
sub edx,edx
stc
jmp byte string2num_vege
string2num_ok:
clc
string2num_vege:
pop cx
pop eax
ret
;---------------
string2num_dec:
call string2num_Get
cmp al,' '
je byte string2num_ok
or al,al
jz byte string2num_ok
mov ah,10
call string2num_Fnd
jz byte string2num_err
dec ax
movzx eax,al
imul edx,10
add edx,eax
jmp byte string2num_dec
;---------------
string2num_Get:
mov al,cs:[si]
inc si
ret
;---------------
string2num_Fnd: ;ah-max, al-char /al-1..ah+1, 0=not found jz notfound!
push bx
call lowerCase
mov cx,ax
mov bx,offset conv2hex_d3
string2num_Fnd_j1:
dec ch
mov al,ch
xlat cs
call lowerCase
cmp al,cl
je byte string2num_Fnd_ok
or ch,ch
jnz byte string2num_Fnd_j1
string2num_Fnd_err:
sub ax,ax
jmp string2num_Fnd_vege
string2num_Fnd_ok:
mov al,ch
inc ax
string2num_Fnd_vege:
or al,al
pop bx
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
