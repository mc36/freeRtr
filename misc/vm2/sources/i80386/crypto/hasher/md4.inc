MD4_size equ 16

;-----------------------------------------
proc MD4_init
;in: ds:bp-record...
cld
mov di,bp
mov eax,067452301h
stosd
mov eax,0efcdab89h
stosd
mov eax,098badcfeh
stosd
mov eax,010325476h
stosd
sub eax,eax
mov cx,64
rep
  stosb
stosd
stosb
retnd
endp
;-----------------------------------------


;-----------------------------------------
proc MD4_addChar
;in: ds:bp-record...
;    al-char
cld
mov si,bp
mov bl,ds:[si+54h]
mov bh,0
mov ds:[si+10h+bx],al
inc bx
mov ds:[si+54h],bl
sub eax,eax
mov al,8
add ds:[si+50h],eax
cmp bl,40h
jb dword MD4_addChar_j1
;-----------------------------------------
mov ds:[dataBlock_temp1],bp
mov si,bp
push dword ds:[si+00h]
push dword ds:[si+04h]
push dword ds:[si+08h]
push dword ds:[si+0ch]

;Round 1
;FF (a, b, c, d, x[ 0], 3 ); /* 1 */
mov cx,00003h
mov ax,0048Ch
call dword MD4_addChar_FF
;FF (d, a, b, c, x[ 1], 7 ); /* 2 */
mov cx,00107h
mov ax,0C048h
call dword MD4_addChar_FF
;FF (c, d, a, b, x[ 2], 11); /* 3 */
mov cx,0020Bh
mov ax,08C04h
call dword MD4_addChar_FF
;FF (b, c, d, a, x[ 3], 19); /* 4 */
mov cx,00313h
mov ax,048C0h
call dword MD4_addChar_FF
;FF (a, b, c, d, x[ 4], 3 ); /* 5 */
mov cx,00403h
mov ax,0048Ch
call dword MD4_addChar_FF
;FF (d, a, b, c, x[ 5], 7 ); /* 6 */
mov cx,00507h
mov ax,0C048h
call dword MD4_addChar_FF
;FF (c, d, a, b, x[ 6], 11); /* 7 */
mov cx,0060Bh
mov ax,08C04h
call dword MD4_addChar_FF
;FF (b, c, d, a, x[ 7], 19); /* 8 */
mov cx,00713h
mov ax,048C0h
call dword MD4_addChar_FF
;FF (a, b, c, d, x[ 8], 3 ); /* 9 */
mov cx,00803h
mov ax,0048Ch
call dword MD4_addChar_FF
;FF (d, a, b, c, x[ 9], 7 ); /* 10 */
mov cx,00907h
mov ax,0C048h
call dword MD4_addChar_FF
;FF (c, d, a, b, x[10], 11); /* 11 */
mov cx,00A0Bh
mov ax,08C04h
call dword MD4_addChar_FF
;FF (b, c, d, a, x[11], 19); /* 12 */
mov cx,00B13h
mov ax,048C0h
call dword MD4_addChar_FF
;FF (a, b, c, d, x[12], 3 ); /* 13 */
mov cx,00C03h
mov ax,0048Ch
call dword MD4_addChar_FF
;FF (d, a, b, c, x[13], 7 ); /* 14 */
mov cx,00D07h
mov ax,0C048h
call dword MD4_addChar_FF
;FF (c, d, a, b, x[14], 11); /* 15 */
mov cx,00E0Bh
mov ax,08C04h
call dword MD4_addChar_FF
;FF (b, c, d, a, x[15], 19); /* 16 */
mov cx,00F13h
mov ax,048C0h
call dword MD4_addChar_FF

;Round 2
;GG (a, b, c, d, x[ 0], 3 ); /* 17 */
mov cx,00003h
mov ax,0048Ch
call dword MD4_addChar_GG
;GG (d, a, b, c, x[ 4], 5 ); /* 18 */
mov cx,00405h
mov ax,0C048h
call dword MD4_addChar_GG
;GG (c, d, a, b, x[ 8], 9 ); /* 19 */
mov cx,00809h
mov ax,08C04h
call dword MD4_addChar_GG
;GG (b, c, d, a, x[12], 13); /* 20 */
mov cx,00C0Dh
mov ax,048C0h
call dword MD4_addChar_GG
;GG (a, b, c, d, x[ 1], 3 ); /* 21 */
mov cx,00103h
mov ax,0048Ch
call dword MD4_addChar_GG
;GG (d, a, b, c, x[ 5], 5 ); /* 22 */
mov cx,00505h
mov ax,0C048h
call dword MD4_addChar_GG
;GG (c, d, a, b, x[ 9], 9 ); /* 23 */
mov cx,00909h
mov ax,08C04h
call dword MD4_addChar_GG
;GG (b, c, d, a, x[13], 13); /* 24 */
mov cx,00D0Dh
mov ax,048C0h
call dword MD4_addChar_GG
;GG (a, b, c, d, x[ 2], 3 ); /* 25 */
mov cx,00203h
mov ax,0048Ch
call dword MD4_addChar_GG
;GG (d, a, b, c, x[ 6], 5 ); /* 26 */
mov cx,00605h
mov ax,0C048h
call dword MD4_addChar_GG
;GG (c, d, a, b, x[10], 9 ); /* 27 */
mov cx,00A09h
mov ax,08C04h
call dword MD4_addChar_GG
;GG (b, c, d, a, x[14], 13); /* 28 */
mov cx,00E0Dh
mov ax,048C0h
call dword MD4_addChar_GG
;GG (a, b, c, d, x[ 3], 3 ); /* 29 */
mov cx,00303h
mov ax,0048Ch
call dword MD4_addChar_GG
;GG (d, a, b, c, x[ 7], 5 ); /* 30 */
mov cx,00705h
mov ax,0C048h
call dword MD4_addChar_GG
;GG (c, d, a, b, x[11], 9 ); /* 31 */
mov cx,00B09h
mov ax,08C04h
call dword MD4_addChar_GG
;GG (b, c, d, a, x[15], 13); /* 32 */
mov cx,00F0Dh
mov ax,048C0h
call dword MD4_addChar_GG

;Round 3
;HH (a, b, c, d, x[ 0], 3 ); /* 33 */
mov cx,00003h
mov ax,0048Ch
call dword MD4_addChar_HH
;HH (d, a, b, c, x[ 8], 9 ); /* 34 */
mov cx,00809h
mov ax,0C048h
call dword MD4_addChar_HH
;HH (c, d, a, b, x[ 4], 11); /* 35 */
mov cx,0040Bh
mov ax,08C04h
call dword MD4_addChar_HH
;HH (b, c, d, a, x[12], 15); /* 36 */
mov cx,00C0Fh
mov ax,048C0h
call dword MD4_addChar_HH
;HH (a, b, c, d, x[ 2], 3 ); /* 37 */
mov cx,00203h
mov ax,0048Ch
call dword MD4_addChar_HH
;HH (d, a, b, c, x[10], 9 ); /* 38 */
mov cx,00A09h
mov ax,0C048h
call dword MD4_addChar_HH
;HH (c, d, a, b, x[ 6], 11); /* 39 */
mov cx,0060Bh
mov ax,08C04h
call dword MD4_addChar_HH
;HH (b, c, d, a, x[14], 15); /* 40 */
mov cx,00E0Fh
mov ax,048C0h
call dword MD4_addChar_HH
;HH (a, b, c, d, x[ 1], 3 ); /* 41 */
mov cx,00103h
mov ax,0048Ch
call dword MD4_addChar_HH
;HH (d, a, b, c, x[ 9], 9 ); /* 42 */
mov cx,00909h
mov ax,0C048h
call dword MD4_addChar_HH
;HH (c, d, a, b, x[ 5], 11); /* 43 */
mov cx,0050Bh
mov ax,08C04h
call dword MD4_addChar_HH
;HH (b, c, d, a, x[13], 15); /* 44 */
mov cx,00D0Fh
mov ax,048C0h
call dword MD4_addChar_HH
;HH (a, b, c, d, x[ 3], 3 ); /* 45 */
mov cx,00303h
mov ax,0048Ch
call dword MD4_addChar_HH
;HH (d, a, b, c, x[11], 9 ); /* 46 */
mov cx,00B09h
mov ax,0C048h
call dword MD4_addChar_HH
;HH (c, d, a, b, x[ 7], 11); /* 47 */
mov cx,0070Bh
mov ax,08C04h
call dword MD4_addChar_HH
;HH (b, c, d, a, x[15], 15); /* 48 */
mov cx,00F0Fh
mov ax,048C0h
call dword MD4_addChar_HH

mov si,ds:[dataBlock_temp1]
mov bp,si
pop edx
pop ecx
pop ebx
pop eax
add ds:[si+00h],eax
add ds:[si+04h],ebx
add ds:[si+08h],ecx
add ds:[si+0ch],edx
sub ax,ax
mov ds:[si+54h],al
jmp dword MD4_addChar_j1
;----------------------------
MD4_addChar_beg:
;in:  ax-encoded pointer record... 1234h
;     cl-rotate_left value
;     ch-bufpos
;out: si-1   1 + [bufpos]
;     di-2
;     bx-3
;     bp-4
movzx bp,al
movzx di,ah
shr ax,4
movzx bx,al
movzx si,ah
and si,0fh
and di,0fh
and bx,0fh
and bp,0fh
mov ax,ds:[dataBlock_temp1]
add si,ax
add di,ax
add bx,ax
add bp,ax
add ch,ch
add ch,ch
adc al,ch
adc ah,0
add ax,10h
xchg si,ax
mov edx,ds:[si]
xchg si,ax
add ds:[si],edx
retnd
;----------------------------
MD4_addChar_end:
add eax,ds:[si]
rol eax,cl
;add eax,ds:[di]
mov ds:[si],eax
retnd
;----------------------------
MD4_addChar_FF:
call dword MD4_addChar_beg
;((b and c) or (( (not b) and d))
mov eax,ds:[di]
mov edx,eax
and eax,ds:[bx]
not edx
and edx,ds:[bp]
or eax,edx
jmp dword MD4_addChar_end
;----------------------------
MD4_addChar_GG:
call dword MD4_addChar_beg
;((b and c) or (b and d) or (c and d)) + 0x5a827999
mov eax,ds:[di]
mov edx,eax
and eax,ds:[bx]
and edx,ds:[bp]
or eax,edx
mov edx,ds:[bx]
and edx,ds:[bp]
or eax,edx
add eax,5a827999h
jmp dword MD4_addChar_end
;----------------------------
MD4_addChar_HH:
call dword MD4_addChar_beg
;(b xor c xor d) + 0x6ed9eba1
mov eax,ds:[di]
xor eax,ds:[bx]
xor eax,ds:[bp]
add eax,6ed9eba1h
jmp dword MD4_addChar_end
;-----------------------------------------
MD4_addChar_j1:
retnd
endp
;-----------------------------------------

;-----------------------------------------
proc MD4_finish
;in: ds:bp-record...
cld
push dword ds:[bp+50h]
mov al,80h
call dword MD4_addChar
MD4_finish_j1:
mov al,ds:[bp+54h]
cmp al,56
je dword MD4_finish_j2
mov al,0
call dword MD4_addChar
jmp dword MD4_finish_j1
MD4_finish_j2:
pop eax
mov cx,8
MD4_finish_j3:
push cx
push eax
call dword MD4_addChar
pop eax
pop cx
shr eax,8
loopw MD4_finish_j3
retnd
endp
;-----------------------------------------


;rec:
;state dd 4 dup (?)
;bufdat db 64 dup (?)
;count dd ?
;buflen db ?
