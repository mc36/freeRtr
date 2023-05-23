MD5_size equ 16

;-----------------------------------------
proc MD5_init
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
proc MD5_addChar
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
jb dword MD5_addChar_j1
;-----------------------------------------
mov ds:[dataBlock_temp1],bp
mov si,bp
push dword ds:[si+00h]
push dword ds:[si+04h]
push dword ds:[si+08h]
push dword ds:[si+0ch]

;Round 1
;FF (a, b, c, d, x[ 0], S11, 0xd76aa478); /* 1 */
mov edx,0d76aa478h
mov cx,00007h
mov ax,0048Ch
call dword MD5_addChar_FF
;FF (d, a, b, c, x[ 1], S12, 0xe8c7b756); /* 2 */
mov edx,0e8c7b756h
mov cx,0010Ch
mov ax,0C048h
call dword MD5_addChar_FF
;FF (c, d, a, b, x[ 2], S13, 0x242070db); /* 3 */
mov edx,0242070dbh
mov cx,00211h
mov ax,08C04h
call dword MD5_addChar_FF
;FF (b, c, d, a, x[ 3], S14, 0xc1bdceee); /* 4 */
mov edx,0c1bdceeeh
mov cx,00316h
mov ax,048C0h
call dword MD5_addChar_FF
;FF (a, b, c, d, x[ 4], S11, 0xf57c0faf); /* 5 */
mov edx,0f57c0fafh
mov cx,00407h
mov ax,0048Ch
call dword MD5_addChar_FF
;FF (d, a, b, c, x[ 5], S12, 0x4787c62a); /* 6 */
mov edx,04787c62ah
mov cx,0050Ch
mov ax,0C048h
call dword MD5_addChar_FF
;FF (c, d, a, b, x[ 6], S13, 0xa8304613); /* 7 */
mov edx,0a8304613h
mov cx,00611h
mov ax,08C04h
call dword MD5_addChar_FF
;FF (b, c, d, a, x[ 7], S14, 0xfd469501); /* 8 */
mov edx,0fd469501h
mov cx,00716h
mov ax,048C0h
call dword MD5_addChar_FF
;FF (a, b, c, d, x[ 8], S11, 0x698098d8); /* 9 */
mov edx,0698098d8h
mov cx,00807h
mov ax,0048Ch
call dword MD5_addChar_FF
;FF (d, a, b, c, x[ 9], S12, 0x8b44f7af); /* 10 */
mov edx,08b44f7afh
mov cx,0090Ch
mov ax,0C048h
call dword MD5_addChar_FF
;FF (c, d, a, b, x[10], S13, 0xffff5bb1); /* 11 */
mov edx,0ffff5bb1h
mov cx,00A11h
mov ax,08C04h
call dword MD5_addChar_FF
;FF (b, c, d, a, x[11], S14, 0x895cd7be); /* 12 */
mov edx,0895cd7beh
mov cx,00B16h
mov ax,048C0h
call dword MD5_addChar_FF
;FF (a, b, c, d, x[12], S11, 0x6b901122); /* 13 */
mov edx,06b901122h
mov cx,00C07h
mov ax,0048Ch
call dword MD5_addChar_FF
;FF (d, a, b, c, x[13], S12, 0xfd987193); /* 14 */
mov edx,0fd987193h
mov cx,00D0Ch
mov ax,0C048h
call dword MD5_addChar_FF
;FF (c, d, a, b, x[14], S13, 0xa679438e); /* 15 */
mov edx,0a679438eh
mov cx,00E11h
mov ax,08C04h
call dword MD5_addChar_FF
;FF (b, c, d, a, x[15], S14, 0x49b40821); /* 16 */
mov edx,049b40821h
mov cx,00F16h
mov ax,048C0h
call dword MD5_addChar_FF

;Round 2
;GG (a, b, c, d, x[ 1], S21, 0xf61e2562); /* 17 */
mov edx,0f61e2562h
mov cx,00105h
mov ax,0048Ch
call dword MD5_addChar_GG
;GG (d, a, b, c, x[ 6], S22, 0xc040b340); /* 18 */
mov edx,0c040b340h
mov cx,00609h
mov ax,0C048h
call dword MD5_addChar_GG
;GG (c, d, a, b, x[11], S23, 0x265e5a51); /* 19 */
mov edx,0265e5a51h
mov cx,00B0Eh
mov ax,08C04h
call dword MD5_addChar_GG
;GG (b, c, d, a, x[ 0], S24, 0xe9b6c7aa); /* 20 */
mov edx,0e9b6c7aah
mov cx,00014h
mov ax,048C0h
call dword MD5_addChar_GG
;GG (a, b, c, d, x[ 5], S21, 0xd62f105d); /* 21 */
mov edx,0d62f105dh
mov cx,00505h
mov ax,0048Ch
call dword MD5_addChar_GG
;GG (d, a, b, c, x[10], S22,  0x2441453); /* 22 */
mov edx,02441453h
mov cx,00A09h
mov ax,0C048h
call dword MD5_addChar_GG
;GG (c, d, a, b, x[15], S23, 0xd8a1e681); /* 23 */
mov edx,0d8a1e681h
mov cx,00F0Eh
mov ax,08C04h
call dword MD5_addChar_GG
;GG (b, c, d, a, x[ 4], S24, 0xe7d3fbc8); /* 24 */
mov edx,0e7d3fbc8h
mov cx,00414h
mov ax,048C0h
call dword MD5_addChar_GG
;GG (a, b, c, d, x[ 9], S21, 0x21e1cde6); /* 25 */
mov edx,021e1cde6h
mov cx,00905h
mov ax,0048Ch
call dword MD5_addChar_GG
;GG (d, a, b, c, x[14], S22, 0xc33707d6); /* 26 */
mov edx,0c33707d6h
mov cx,00E09h
mov ax,0C048h
call dword MD5_addChar_GG
;GG (c, d, a, b, x[ 3], S23, 0xf4d50d87); /* 27 */
mov edx,0f4d50d87h
mov cx,0030Eh
mov ax,08C04h
call dword MD5_addChar_GG
;GG (b, c, d, a, x[ 8], S24, 0x455a14ed); /* 28 */
mov edx,0455a14edh
mov cx,00814h
mov ax,048C0h
call dword MD5_addChar_GG
;GG (a, b, c, d, x[13], S21, 0xa9e3e905); /* 29 */
mov edx,0a9e3e905h
mov cx,00D05h
mov ax,0048Ch
call dword MD5_addChar_GG
;GG (d, a, b, c, x[ 2], S22, 0xfcefa3f8); /* 30 */
mov edx,0fcefa3f8h
mov cx,00209h
mov ax,0C048h
call dword MD5_addChar_GG
;GG (c, d, a, b, x[ 7], S23, 0x676f02d9); /* 31 */
mov edx,0676f02d9h
mov cx,0070Eh
mov ax,08C04h
call dword MD5_addChar_GG
;GG (b, c, d, a, x[12], S24, 0x8d2a4c8a); /* 32 */
mov edx,08d2a4c8ah
mov cx,00C14h
mov ax,048C0h
call dword MD5_addChar_GG

;Round 3
;HH (a, b, c, d, x[ 5], S31, 0xfffa3942); /* 33 */
mov edx,0fffa3942h
mov cx,00504h
mov ax,0048Ch
call dword MD5_addChar_HH
;HH (d, a, b, c, x[ 8], S32, 0x8771f681); /* 34 */
mov edx,08771f681h
mov cx,0080Bh
mov ax,0C048h
call dword MD5_addChar_HH
;HH (c, d, a, b, x[11], S33, 0x6d9d6122); /* 35 */
mov edx,06d9d6122h
mov cx,00B10h
mov ax,08C04h
call dword MD5_addChar_HH
;HH (b, c, d, a, x[14], S34, 0xfde5380c); /* 36 */
mov edx,0fde5380ch
mov cx,00E17h
mov ax,048C0h
call dword MD5_addChar_HH
;HH (a, b, c, d, x[ 1], S31, 0xa4beea44); /* 37 */
mov edx,0a4beea44h
mov cx,00104h
mov ax,0048Ch
call dword MD5_addChar_HH
;HH (d, a, b, c, x[ 4], S32, 0x4bdecfa9); /* 38 */
mov edx,04bdecfa9h
mov cx,0040Bh
mov ax,0C048h
call dword MD5_addChar_HH
;HH (c, d, a, b, x[ 7], S33, 0xf6bb4b60); /* 39 */
mov edx,0f6bb4b60h
mov cx,00710h
mov ax,08C04h
call dword MD5_addChar_HH
;HH (b, c, d, a, x[10], S34, 0xbebfbc70); /* 40 */
mov edx,0bebfbc70h
mov cx,00A17h
mov ax,048C0h
call dword MD5_addChar_HH
;HH (a, b, c, d, x[13], S31, 0x289b7ec6); /* 41 */
mov edx,0289b7ec6h
mov cx,00D04h
mov ax,0048Ch
call dword MD5_addChar_HH
;HH (d, a, b, c, x[ 0], S32, 0xeaa127fa); /* 42 */
mov edx,0eaa127fah
mov cx,0000Bh
mov ax,0C048h
call dword MD5_addChar_HH
;HH (c, d, a, b, x[ 3], S33, 0xd4ef3085); /* 43 */
mov edx,0d4ef3085h
mov cx,00310h
mov ax,08C04h
call dword MD5_addChar_HH
;HH (b, c, d, a, x[ 6], S34,  0x4881d05); /* 44 */
mov edx,04881d05h
mov cx,00617h
mov ax,048C0h
call dword MD5_addChar_HH
;HH (a, b, c, d, x[ 9], S31, 0xd9d4d039); /* 45 */
mov edx,0d9d4d039h
mov cx,00904h
mov ax,0048Ch
call dword MD5_addChar_HH
;HH (d, a, b, c, x[12], S32, 0xe6db99e5); /* 46 */
mov edx,0e6db99e5h
mov cx,00C0Bh
mov ax,0C048h
call dword MD5_addChar_HH
;HH (c, d, a, b, x[15], S33, 0x1fa27cf8); /* 47 */
mov edx,01fa27cf8h
mov cx,00F10h
mov ax,08C04h
call dword MD5_addChar_HH
;HH (b, c, d, a, x[ 2], S34, 0xc4ac5665); /* 48 */
mov edx,0c4ac5665h
mov cx,00217h
mov ax,048C0h
call dword MD5_addChar_HH

;Round 4
;II (a, b, c, d, x[ 0], S41, 0xf4292244); /* 49 */
mov edx,0f4292244h
mov cx,00006h
mov ax,0048Ch
call dword MD5_addChar_II
;II (d, a, b, c, x[ 7], S42, 0x432aff97); /* 50 */
mov edx,0432aff97h
mov cx,0070Ah
mov ax,0C048h
call dword MD5_addChar_II
;II (c, d, a, b, x[14], S43, 0xab9423a7); /* 51 */
mov edx,0ab9423a7h
mov cx,00E0Fh
mov ax,08C04h
call dword MD5_addChar_II
;II (b, c, d, a, x[ 5], S44, 0xfc93a039); /* 52 */
mov edx,0fc93a039h
mov cx,00515h
mov ax,048C0h
call dword MD5_addChar_II
;II (a, b, c, d, x[12], S41, 0x655b59c3); /* 53 */
mov edx,0655b59c3h
mov cx,00C06h
mov ax,0048Ch
call dword MD5_addChar_II
;II (d, a, b, c, x[ 3], S42, 0x8f0ccc92); /* 54 */
mov edx,08f0ccc92h
mov cx,0030Ah
mov ax,0C048h
call dword MD5_addChar_II
;II (c, d, a, b, x[10], S43, 0xffeff47d); /* 55 */
mov edx,0ffeff47dh
mov cx,00A0Fh
mov ax,08C04h
call dword MD5_addChar_II
;II (b, c, d, a, x[ 1], S44, 0x85845dd1); /* 56 */
mov edx,085845dd1h
mov cx,00115h
mov ax,048C0h
call dword MD5_addChar_II
;II (a, b, c, d, x[ 8], S41, 0x6fa87e4f); /* 57 */
mov edx,06fa87e4fh
mov cx,00806h
mov ax,0048Ch
call dword MD5_addChar_II
;II (d, a, b, c, x[15], S42, 0xfe2ce6e0); /* 58 */
mov edx,0fe2ce6e0h
mov cx,00F0Ah
mov ax,0C048h
call dword MD5_addChar_II
;II (c, d, a, b, x[ 6], S43, 0xa3014314); /* 59 */
mov edx,0a3014314h
mov cx,0060Fh
mov ax,08C04h
call dword MD5_addChar_II
;II (b, c, d, a, x[13], S44, 0x4e0811a1); /* 60 */
mov edx,04e0811a1h
mov cx,00D15h
mov ax,048C0h
call dword MD5_addChar_II
;II (a, b, c, d, x[ 4], S41, 0xf7537e82); /* 61 */
mov edx,0f7537e82h
mov cx,00406h
mov ax,0048Ch
call dword MD5_addChar_II
;II (d, a, b, c, x[11], S42, 0xbd3af235); /* 62 */
mov edx,0bd3af235h
mov cx,00B0Ah
mov ax,0C048h
call dword MD5_addChar_II
;II (c, d, a, b, x[ 2], S43, 0x2ad7d2bb); /* 63 */
mov edx,02ad7d2bbh
mov cx,0020Fh
mov ax,08C04h
call dword MD5_addChar_II
;II (b, c, d, a, x[ 9], S44, 0xeb86d391); /* 64 */
mov edx,0eb86d391h
mov cx,00915h
mov ax,048C0h
call dword MD5_addChar_II

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
jmp dword MD5_addChar_j1
;----------------------------
MD5_addChar_beg:
;in:  ax-encoded pointer record... 1234h
;     cl-rotate_left value
;     ch-bufpos
;     edx-magic
;out: si-1   1 + magic+[bufpos]
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
add edx,ds:[si]
xchg si,ax
add ds:[si],edx
retnd
;----------------------------
MD5_addChar_end:
add eax,ds:[si]
rol eax,cl
add eax,ds:[di]
mov ds:[si],eax
retnd
;----------------------------
MD5_addChar_FF:
call dword MD5_addChar_beg
;(((b) and (c)) or ((not b) and (d)))
mov eax,ds:[di]
mov edx,eax
and eax,ds:[bx]
not edx
and edx,ds:[bp]
or eax,edx
jmp dword MD5_addChar_end
;----------------------------
MD5_addChar_GG:
call dword MD5_addChar_beg
;(((b) and (d)) or ((c) and (not d)))
mov eax,ds:[di]
mov edx,ds:[bp]
and eax,edx
not edx
and edx,ds:[bx]
or eax,edx
jmp dword MD5_addChar_end
;----------------------------
MD5_addChar_HH:
call dword MD5_addChar_beg
;((b) xor (c) xor (d))
mov eax,ds:[di]
xor eax,ds:[bx]
xor eax,ds:[bp]
jmp dword MD5_addChar_end
;----------------------------
MD5_addChar_II:
call dword MD5_addChar_beg
;((c) xor ((b) or (not d)))
mov eax,ds:[di]
mov edx,ds:[bp]
not edx
or eax,edx
xor eax,ds:[bx]
jmp dword MD5_addChar_end
;-----------------------------------------
MD5_addChar_j1:
retnd
endp
;-----------------------------------------

;-----------------------------------------
proc MD5_finish
;in: ds:bp-record...
cld
push dword ds:[bp+50h]
mov al,80h
call dword MD5_addChar
MD5_finish_j1:
mov al,ds:[bp+54h]
cmp al,56
je dword MD5_finish_j2
mov al,0
call dword MD5_addChar
jmp dword MD5_finish_j1
MD5_finish_j2:
pop eax
mov cx,8
MD5_finish_j3:
push cx
push eax
call dword MD5_addChar
pop eax
pop cx
shr eax,8
loopw MD5_finish_j3
retnd
endp
;-----------------------------------------


;rec:
;state dd 4 dup (?)
;bufdat db 64 dup (?)
;count dd ?
;buflen db ?
