RC6_size equ 16
RC6_param equ 0

;-----------------------------------------
proc RC6_init
;in: ds:bx-record...
;    es:si-key text...
;    cx-size of key...
mov ds:[DataBlock_temp30],bx
mov ds:[DataBlock_temp31],si
shr cx,2
dec cx
mov ds:[DataBlock_temp32],cx

;key->l_key[0] = 0xb7e15163;
mov eax,0b7e15163h;
mov ds:[bx],eax
;for (k = 1; k < 44; ++k)
mov cx,43
RC6_init_j1:
add bx,4
;key->l_key[k] = key->l_key[k - 1] + 0x9e3779b9;
add eax,09e3779b9h
mov ds:[bx],eax
loopw RC6_init_j1

;a = b = i = j = 0;
sub eax,eax
mov ds:[DataBlock_temp33],ax
mov ds:[DataBlock_temp36],ax
mov ds:[DataBlock_temp34],eax
mov ds:[DataBlock_temp35],eax

;for (k = 0; k < 132; ++k) {
mov cx,131
RC6_init_j2:
push cx
;  a = rotl32(key->l_key[i] + a + b, 3);
mov si,ds:[DataBlock_temp33]
shl si,2
add si,ds:[DataBlock_temp30]
mov eax,ds:[si]
add eax,ds:[DataBlock_temp34]
add eax,ds:[DataBlock_temp35]
rol eax,3
mov ds:[DataBlock_temp34],eax
;  key->l_key[i] = a;
mov ds:[si],eax
;  b += a;
add ds:[DataBlock_temp35],eax
;  b = rotl32(l[j] + b, b);
mov si,ds:[DataBlock_temp36]
shl si,2
add si,ds:[DataBlock_temp31]
mov eax,es:[si]
mov ecx,ds:[DataBlock_temp35]
add eax,ecx
and cl,1fh
rol eax,cl
mov ds:[DataBlock_temp35],eax
;  l[j] = b;
mov es:[si],eax

;  i = (i == 43 ? 0 : i + 1);
mov ax,ds:[DataBlock_temp33]
inc ax
cmp al,43
jbe dword RC6_init_j3
sub ax,ax
RC6_init_j3:
mov ds:[DataBlock_temp33],ax
;  j = (j == t ? 0 : j + 1);
mov ax,ds:[DataBlock_temp36]
inc ax
cmp ax,ds:[DataBlock_temp32]
jbe dword RC6_init_j4
sub ax,ax
RC6_init_j4:
mov ds:[DataBlock_temp36],ax

;  }
pop cx
dec cx
jns dword RC6_init_j2

retnd
endp
;-----------------------------------------

;-----------------------------------------
proc RC6_encrypt
;in: ds:si-record...
;    edi-a...
;    ebp-b...
;    ebx-c...
;    edx-d...
cld
;a = _blk[0];
;b = _blk[1] + key->l_key[0];
lodsd
add ebp,eax
;c = _blk[2];
;d = _blk[3] + key->l_key[1];
lodsd
add edx,eax

mov cx,20
RC6_encrypt_j1:
push cx

;t = rotl32(b * (b + b + 1), 5);
lea ecx,ds:[ebp*2+1]
imul ecx,ebp
rol ecx,5
;u = rotl32(d * (d + d + 1), 5);
lea eax,ds:[edx*2+1]
imul eax,edx
rol eax,5
;a = a ^ t;
xor edi,ecx
;c = c ^ u;
xor ebx,eax
;c = rotl32(c, t&31);
and cl,1fh
rol ebx,cl
;a = rotl32(a, u&31);
mov cl,al
and cl,1fh
rol edi,cl
;a += key->l_key[i];
lodsd
add edi,eax
;c += key->l_key[i + 1];
lodsd
add ebx,eax

xchg edi,ebp
xchg ebp,ebx
xchg ebx,edx

pop cx
loopw RC6_encrypt_j1

;_blk[0] = a + key->l_key[42];
;_blk[1] = b;
lodsd
add edi,eax
;_blk[2] = c + key->l_key[43];
;_blk[3] = d;
lodsd
add ebx,eax
retnd
endp
;-----------------------------------------

;-----------------------------------------
proc RC6_decrypt
;in: ds:si-record...
;    edi-a...
;    ebp-b...
;    ebx-c...
;    edx-d...
std
add si,0ach
;d = _blk[3];
;c = _blk[2] - key->l_key[43];
lodsd
sub ebx,eax
;b = _blk[1];
;a = _blk[0] - key->l_key[42];
lodsd
sub edi,eax

mov cx,20
RC6_decrypt_j1:
push cx

xchg edx,ebx
xchg ebx,ebp
xchg ebp,edi

;c = c - key->l_key[i + 1];
lodsd
sub ebx,eax
;a = a - key->l_key[i];
lodsd
sub edi,eax
;u = rotl32(d * (d + d + 1), 5);
lea eax,ds:[edx*2+1]
imul eax,edx
rol eax,5
;t = rotl32(b * (b + b + 1), 5);
lea ecx,ds:[ebp*2+1]
imul ecx,ebp
rol ecx,5
;c = rotr32(c, t&31) ^ u;
push cx
and cl,1fh
ror ebx,cl
pop cx
xor ebx,eax
;a = rotr32(a, u&31) ^ t;
xchg eax,ecx
and cl,1fh
ror edi,cl
xor edi,eax

pop cx
loopw RC6_decrypt_j1

;_blk[3] = d - key->l_key[1];
;_blk[2] = c;
lodsd
sub edx,eax
;_blk[1] = b - key->l_key[0];
;_blk[0] = a;
lodsd
sub ebp,eax

cld
retnd
endp
;-----------------------------------------


;rec:
;data dd 44 dup (?)         0000h
