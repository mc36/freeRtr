;----------------------------
proc writeMac
;in: ds:si-offset of address...
mov cx,nic_addrSize
writeMac_j1:
lodsb
push cx
push si
mov cl,2
call conv2hex
inc si
call writeCode
mov si,offset main_txt12
call writeCode
pop si
pop cx
loop writeMac_j1
mov si,offset main_txtCLR
call writeCode
ret
endp
;----------------------------

;----------------------------
proc showError
sti
mov si,offset main_txt02
call writeCode
mov bp,9
showError_j1:
mov si,offset main_txt04
call writeCode
mov ax,bp
mov cl,1
call conv2hex
inc si
call writeCode
mov si,offset main_txt05
call writeCode
mov ah,11h
int 16h
jnz byte showError_j2
mov esi,18
call dword timer_delay
dec bp
jns byte showError_j1
showError_j2:
jmp dword 0ffffh:0
endp
;----------------------------

;----------------------------
proc writeCode
;in: cs:si-asciiz text...
push ax
push si
writeCode_j1:
lodsb cs
or al,al
jz byte writeCode_j2
mov ah,0eh
int 10h
jmp byte writeCode_j1
writeCode_j2:
pop si
pop ax
ret
endp
;----------------------------

;----------------------------
proc writeData
;in: ds:si-asciiz text...
push ax
push si
writeData_j1:
lodsb
or al,al
jz byte writeData_j2
mov ah,0eh
int 10h
jmp byte writeData_j1
writeData_j2:
pop si
pop ax
ret
endp
;----------------------------

;----------------------------
proc UpperCase
cmp al,'a'
jb byte UpperCase_vege
cmp al,'z'
ja byte UpperCase_vege
and al,0dfh
UpperCase_vege:
ret
endp
;----------------------------

;----------------------------
proc LowerCase
cmp al,'A'
jb byte LowerCase_vege
cmp al,'Z'
ja byte LowerCase_vege
or al,20h
LowerCase_vege:
ret
endp
;----------------------------

;----------------------------
proc str2num
;in:  cs:si-text to convert...
;out: edx-number...
;     carry-clearer if success /jc error...;)
push eax
push cx
sub edx,edx
str2num_j1:
lodsb cs
cmp al,' '
je byte str2num_j1
or al,al
jz byte str2num_err
cmp al,'$'
je byte str2num_hex
dec si
cmp al,'0'
jne byte str2num_dec
inc si
call str2num_get
call LowerCase
cmp al,'x'
je byte str2num_hex
dec si
jmp byte str2num_dec
;---------------
str2num_hex:
call str2num_get
cmp al,' '
je byte str2num_ok
or al,al
jz byte str2num_ok
mov ah,16
call str2num_fnd
jz byte str2num_err
dec ax
shl edx,4
or dl,al
jmp byte str2num_hex
;---------------
str2num_err:
sub edx,edx
stc
jmp byte str2num_vege
str2num_ok:
clc
str2num_vege:
pop cx
pop eax
ret
;---------------
str2num_dec:
call str2num_get
cmp al,' '
je byte str2num_ok
or al,al
jz byte str2num_ok
mov ah,10
call str2num_fnd
jz byte str2num_err
dec ax
movzx eax,al
imul edx,10
add edx,eax
jmp byte str2num_dec
;---------------
str2num_get:
mov al,cs:[si]
inc si
ret
;---------------
str2num_fnd: ;ah-max, al-char /al-1..ah+1, 0=not found jz notfound!
push bx
call LowerCase
mov cx,ax
mov bx,offset conv2hex_d3
str2num_fnd_j1:
dec ch
mov al,ch
xlat cs
call LowerCase
cmp al,cl
je byte str2num_fnd_ok
or ch,ch
jnz byte str2num_fnd_j1
str2num_fnd_err:
sub ax,ax
jmp str2num_fnd_vege
str2num_fnd_ok:
mov al,ch
inc ax
str2num_fnd_vege:
or al,al
pop bx
ret
endp
;----------------------------

;----------------------------
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
mov byte cs:[si],0
mov bh,3
conv2dec_j1:
sub edx,edx
div ecx
add dl,'0'
dec si
mov cs:[si],dl
or eax,eax
jz byte conv2dec_j2
dec bh
jnz byte conv2dec_j1
mov bh,3
dec si
mov byte cs:[si],','
jmp byte conv2dec_j1
conv2dec_j2:
push si
mov al,' '
conv2dec_j3:
dec si
cmp si,offset conv2hex_d1
jb byte conv2dec_j4
mov cs:[si],al
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
;----------------------------

;----------------------------
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
loop conv2hex_j1
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
;----------------------------
