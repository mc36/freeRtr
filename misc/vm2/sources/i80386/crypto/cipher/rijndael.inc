Rijndael_size equ 16
Rijndael_param equ 0

;-----------------------------------------
proc Rijndael_Init
;in: ds:bx-record...
;    ds:si-key text...
;    cx-size of key...
mov ds:[DataBlock_temp30],bx
mov ds:[DataBlock_temp31],cx
;i=0;
sub ax,ax
mov ds:[DataBlock_temp32],ax
;rk[0] = GETU32(cipherKey +  0);
;rk[1] = GETU32(cipherKey +  4);
;rk[2] = GETU32(cipherKey +  8);
;rk[3] = GETU32(cipherKey + 12);
shr cx,2
Rijndael_Init_j1:
mov eax,ds:[si]
xchg al,ah
rol eax,16
xchg al,ah
mov ds:[bx],eax
add si,4
add bx,4
loopw Rijndael_Init_j1

mov bp,ds:[DataBlock_temp30]
mov ax,ds:[DataBlock_temp31]
cmp ax,16
je dword Rijndael_Init_k128
cmp ax,24
je dword Rijndael_Init_k192
cmp ax,32
je dword Rijndael_Init_k256
Rijndael_Init_j3:
mov ah,0
mov ds:[DataBlock_temp31],ax
mov si,ds:[DataBlock_temp30]
mov ds:[si+1e0h],al
sub ax,ax
mov al,4
mov ds:[DataBlock_temp32],ax
mov si,ds:[DataBlock_temp30]
mov cx,60
Rijndael_Init_j4:
mov eax,ds:[si]
mov ds:[si+0f0h],eax
add si,4
loopw Rijndael_Init_j4
;for (i = 0, j = 4*Nr; i < j; i += 4, j -= 4) {
mov si,ds:[DataBlock_temp30]
mov cx,ds:[DataBlock_temp31]
add si,0f0h
imul di,cx,16
add di,si
Rijndael_Init_j5:
;  temp = rk[i    ]; rk[i    ] = rk[j    ]; rk[j    ] = temp;
mov eax,ds:[si+0]
xchg eax,ds:[di+0]
mov ds:[si+0],eax
;  temp = rk[i + 1]; rk[i + 1] = rk[j + 1]; rk[j + 1] = temp;
mov eax,ds:[si+4]
xchg eax,ds:[di+4]
mov ds:[si+4],eax
;  temp = rk[i + 2]; rk[i + 2] = rk[j + 2]; rk[j + 2] = temp;
mov eax,ds:[si+8]
xchg eax,ds:[di+8]
mov ds:[si+8],eax
;  temp = rk[i + 3]; rk[i + 3] = rk[j + 3]; rk[j + 3] = temp;
mov eax,ds:[si+12]
xchg eax,ds:[di+12]
mov ds:[si+12],eax
;  }
add si,16
sub di,16
cmp si,di
jb dword Rijndael_Init_j5

Rijndael_Init_j6:
mov si,ds:[DataBlock_temp32]
shl si,2
add si,ds:[DataBlock_temp30]
add si,0f0h
mov edx,ds:[si]
;rk[0] = Td0[Te4[(rk[0] >> 24) & 0xff] & 0xff] ^
rol edx,8
movzx ebx,dl
shl bx,2
mov ebx,cs:[Rijndael_Te4+ebx]
movzx ebx,bl
shl bx,2
mov eax,cs:[Rijndael_Td0+ebx]
;        Td1[Te4[(rk[0] >> 16) & 0xff] & 0xff] ^
rol edx,8
movzx ebx,dl
shl bx,2
mov ebx,cs:[Rijndael_Te4+ebx]
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Td1+ebx]
;        Td2[Te4[(rk[0] >>  8) & 0xff] & 0xff] ^
rol edx,8
movzx ebx,dl
shl bx,2
mov ebx,cs:[Rijndael_Te4+ebx]
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Td2+ebx]
;        Td3[Te4[(rk[0] >>  0) & 0xff] & 0xff];
rol edx,8
movzx ebx,dl
shl bx,2
mov ebx,cs:[Rijndael_Te4+ebx]
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Td3+ebx]
mov ds:[si],eax
mov ax,ds:[DataBlock_temp32]
inc ax
mov ds:[DataBlock_temp32],ax
shr ax,2
cmp ax,ds:[DataBlock_temp31]
jb dword Rijndael_Init_j6
retnd
;-----------------------------------------
Rijndael_Init_j2:
;in:  edx-temp value...
;out: eax-calculated value...
sub eax,eax
;(Te4[(temp >> 24) & 0xff] & 0x000000ff) ^
rol edx,8
movzx ebx,dl
shl bx,2
mov ecx,cs:[Rijndael_Te4+ebx]
mov al,cl
;(Te4[(temp >> 16) & 0xff] & 0xff000000) ^
rol edx,8
movzx ebx,dl
shl bx,2
mov ecx,cs:[Rijndael_Te4+ebx]
and ecx,0ff000000h
xor eax,ecx
;(Te4[(temp >>  8) & 0xff] & 0x00ff0000) ^
rol edx,8
movzx ebx,dl
shl bx,2
mov ecx,cs:[Rijndael_Te4+ebx]
and ecx,000ff0000h
xor eax,ecx
;(Te4[(temp >>  0) & 0xff] & 0x0000ff00) ^
rol edx,8
movzx ebx,dl
shl bx,2
mov ecx,cs:[Rijndael_Te4+ebx]
mov ah,ch
retnd
;-----------------------------------------
;for (;;) {
Rijndael_Init_k128:
;  temp  = rk[3];
;  rk[4] = (Te4[(temp >> 16) & 0xff] & 0xff000000) ^
;          (Te4[(temp >>  8) & 0xff] & 0x00ff0000) ^
;          (Te4[(temp      ) & 0xff] & 0x0000ff00) ^
;          (Te4[(temp >> 24)       ] & 0x000000ff) ^
;          rk[0] ^ rcon[i];
mov edx,ds:[bp+12]
call dword Rijndael_Init_j2
xor eax,ds:[bp+0]
mov bx,ds:[DataBlock_temp32]
movzx ebx,bx
shl bx,2
xor eax,cs:[Rijndael_Rcon+ebx]
mov ds:[bp+16],eax
;  rk[5] = rk[1] ^ rk[4];
mov eax,ds:[bp+4]
xor eax,ds:[bp+16]
mov ds:[bp+20],eax
;  rk[6] = rk[2] ^ rk[5];
mov eax,ds:[bp+8]
xor eax,ds:[bp+20]
mov ds:[bp+24],eax
;  rk[7] = rk[3] ^ rk[6];
mov eax,ds:[bp+12]
xor eax,ds:[bp+24]
mov ds:[bp+28],eax
;  rk += 4;
add bp,16
;  if (++i == 10) return 10;
mov ax,ds:[DataBlock_temp32]
inc ax
mov ds:[DataBlock_temp32],ax
;  }
cmp al,10
jb dword Rijndael_Init_k128
;return 10;
mov al,10
jmp dword Rijndael_Init_j3
;-----------------------------------------
;for (;;) {
Rijndael_Init_k192:
;  temp = rk[ 5];
;  rk[ 6] =(Te4[(temp >> 16) & 0xff] & 0xff000000) ^
;          (Te4[(temp >>  8) & 0xff] & 0x00ff0000) ^
;          (Te4[(temp      ) & 0xff] & 0x0000ff00) ^
;          (Te4[(temp >> 24)       ] & 0x000000ff) ^
;          rk[ 0] ^ rcon[i];
mov edx,ds:[bp+20]
call dword Rijndael_Init_j2
xor eax,ds:[bp+0]
mov bx,ds:[DataBlock_temp32]
movzx ebx,bx
shl bx,2
xor eax,cs:[Rijndael_Rcon+ebx]
mov ds:[bp+24],eax
;  rk[ 7] = rk[ 1] ^ rk[ 6];
mov eax,ds:[bp+4]
xor eax,ds:[bp+24]
mov ds:[bp+28],eax
;  rk[ 8] = rk[ 2] ^ rk[ 7];
mov eax,ds:[bp+8]
xor eax,ds:[bp+28]
mov ds:[bp+32],eax
;  rk[ 9] = rk[ 3] ^ rk[ 8];
mov eax,ds:[bp+12]
xor eax,ds:[bp+32]
mov ds:[bp+36],eax
;  if (++i == 8) return 12;
mov cx,ds:[DataBlock_temp32]
inc cx
mov ds:[DataBlock_temp32],cx
mov al,12
cmp cl,8
jae dword Rijndael_Init_j3
;  rk[10] = rk[ 4] ^ rk[ 9];
mov eax,ds:[bp+16]
xor eax,ds:[bp+36]
mov ds:[bp+40],eax
;  rk[11] = rk[ 5] ^ rk[10];
mov eax,ds:[bp+20]
xor eax,ds:[bp+40]
mov ds:[bp+44],eax
;  rk += 6;
add bp,24
;  }
jmp dword Rijndael_Init_k192
;-----------------------------------------
;for (;;) {
Rijndael_Init_k256:
;  temp = rk[ 7];
;  rk[ 8] =(Te4[(temp >> 16) & 0xff] & 0xff000000) ^
;          (Te4[(temp >>  8) & 0xff] & 0x00ff0000) ^
;          (Te4[(temp      ) & 0xff] & 0x0000ff00) ^
;          (Te4[(temp >> 24)       ] & 0x000000ff) ^
;          rk[ 0] ^ rcon[i];
mov edx,ds:[bp+28]
call dword Rijndael_Init_j2
xor eax,ds:[bp+0]
mov bx,ds:[DataBlock_temp32]
movzx ebx,bx
shl bx,2
xor eax,cs:[Rijndael_Rcon+ebx]
mov ds:[bp+32],eax
;  rk[ 9] = rk[ 1] ^ rk[ 8];
mov eax,ds:[bp+4]
xor eax,ds:[bp+32]
mov ds:[bp+36],eax
;  rk[10] = rk[ 2] ^ rk[ 9];
mov eax,ds:[bp+8]
xor eax,ds:[bp+36]
mov ds:[bp+40],eax
;  rk[11] = rk[ 3] ^ rk[10];
mov eax,ds:[bp+12]
xor eax,ds:[bp+40]
mov ds:[bp+44],eax
;  if (++i == 7) return 14;
mov cx,ds:[DataBlock_temp32]
inc cx
mov ds:[DataBlock_temp32],cx
mov al,14
cmp cl,7
jae dword Rijndael_Init_j3
;  temp = rk[11];
;  rk[12] =(Te4[(temp >> 24)       ] & 0xff000000) ^
;          (Te4[(temp >> 16) & 0xff] & 0x00ff0000) ^
;          (Te4[(temp >>  8) & 0xff] & 0x0000ff00) ^
;          (Te4[(temp      ) & 0xff] & 0x000000ff) ^
;          rk[ 4];
mov edx,ds:[bp+44]
ror edx,8
call dword Rijndael_Init_j2
xor eax,ds:[bp+16]
mov ds:[bp+48],eax
;  rk[13] = rk[ 5] ^ rk[12];
mov eax,ds:[bp+20]
xor eax,ds:[bp+48]
mov ds:[bp+52],eax
;  rk[14] = rk[ 6] ^ rk[13];
mov eax,ds:[bp+24]
xor eax,ds:[bp+52]
mov ds:[bp+56],eax
;  rk[15] = rk[ 7] ^ rk[14];
mov eax,ds:[bp+28]
xor eax,ds:[bp+56]
mov ds:[bp+60],eax
;  rk += 8;
add bp,32
;  }
jmp dword Rijndael_Init_k256
retnd
endp
;-----------------------------------------


;-----------------------------------------
proc Rijndael_Encrypt
;in: ds:si-record...
;    edi-x0...
;    ebp-x1...
;    ebx-x2...
;    edx-x3...
mov ds:[DataBlock_temp30],si
;s0 = GETU32(pt +  0) ^ rk[0];
lodsd
mov ecx,edi
xchg cl,ch
rol ecx,16
xchg cl,ch
xor eax,ecx
mov ds:[DataBlock_temp37],eax
;s1 = GETU32(pt +  4) ^ rk[1];
lodsd
mov ecx,ebp
xchg cl,ch
rol ecx,16
xchg cl,ch
xor eax,ecx
mov ds:[DataBlock_temp38],eax
;s2 = GETU32(pt +  8) ^ rk[2];
lodsd
mov ecx,ebx
xchg cl,ch
rol ecx,16
xchg cl,ch
xor eax,ecx
mov ds:[DataBlock_temp39],eax
;s3 = GETU32(pt + 12) ^ rk[3];
lodsd
mov ecx,edx
xchg cl,ch
rol ecx,16
xchg cl,ch
xor eax,ecx
mov ds:[DataBlock_temp40],eax

call dword Rijndael_Encrypt_j1 ;1
call dword Rijndael_Encrypt_j2 ;2
call dword Rijndael_Encrypt_j1 ;3
call dword Rijndael_Encrypt_j2 ;4
call dword Rijndael_Encrypt_j1 ;5
call dword Rijndael_Encrypt_j2 ;6
call dword Rijndael_Encrypt_j1 ;7
call dword Rijndael_Encrypt_j2 ;8
call dword Rijndael_Encrypt_j1 ;9
mov di,ds:[DataBlock_temp30]
mov al,ds:[di+1e0h]
cmp al,10
jbe dword Rijndael_Encrypt_j3
call dword Rijndael_Encrypt_j2 ;10
call dword Rijndael_Encrypt_j1 ;11
mov di,ds:[DataBlock_temp30]
mov al,ds:[di+1e0h]
cmp al,12
jbe dword Rijndael_Encrypt_j3
call dword Rijndael_Encrypt_j2 ;12
call dword Rijndael_Encrypt_j1 ;13
Rijndael_Encrypt_j3:

;s0= (Te4[(t0 >> 24)       ] & 0xff000000) ^
;    (Te4[(t1 >> 16) & 0xff] & 0x00ff0000) ^
;    (Te4[(t2 >>  8) & 0xff] & 0x0000ff00) ^
;    (Te4[(t3      ) & 0xff] & 0x000000ff) ^ rk[0];
lodsd
mov ebx,ds:[DataBlock_temp33]
shr ebx,24
shl bx,2
mov ecx,cs:[Rijndael_Te4+ebx]
and ecx,0ff000000h
xor eax,ecx
mov ebx,ds:[DataBlock_temp34]
shr ebx,16
movzx ebx,bl
shl bx,2
mov ecx,cs:[Rijndael_Te4+ebx]
and ecx,000ff0000h
xor eax,ecx
mov ebx,ds:[DataBlock_temp35]
shr ebx,8
movzx ebx,bl
shl bx,2
mov ecx,cs:[Rijndael_Te4+ebx]
xor ah,ch
mov ebx,ds:[DataBlock_temp36]
movzx ebx,bl
shl bx,2
mov ecx,cs:[Rijndael_Te4+ebx]
xor al,cl
mov ds:[DataBlock_temp37],eax
;s1= (Te4[(t1 >> 24)       ] & 0xff000000) ^
;    (Te4[(t2 >> 16) & 0xff] & 0x00ff0000) ^
;    (Te4[(t3 >>  8) & 0xff] & 0x0000ff00) ^
;    (Te4[(t0      ) & 0xff] & 0x000000ff) ^ rk[1];
lodsd
mov ebx,ds:[DataBlock_temp34]
shr ebx,24
shl bx,2
mov ecx,cs:[Rijndael_Te4+ebx]
and ecx,0ff000000h
xor eax,ecx
mov ebx,ds:[DataBlock_temp35]
shr ebx,16
movzx ebx,bl
shl bx,2
mov ecx,cs:[Rijndael_Te4+ebx]
and ecx,000ff0000h
xor eax,ecx
mov ebx,ds:[DataBlock_temp36]
shr ebx,8
movzx ebx,bl
shl bx,2
mov ecx,cs:[Rijndael_Te4+ebx]
xor ah,ch
mov ebx,ds:[DataBlock_temp33]
movzx ebx,bl
shl bx,2
mov ecx,cs:[Rijndael_Te4+ebx]
xor al,cl
mov ds:[DataBlock_temp38],eax
;s2= (Te4[(t2 >> 24)       ] & 0xff000000) ^
;    (Te4[(t3 >> 16) & 0xff] & 0x00ff0000) ^
;    (Te4[(t0 >>  8) & 0xff] & 0x0000ff00) ^
;    (Te4[(t1      ) & 0xff] & 0x000000ff) ^ rk[2];
lodsd
mov ebx,ds:[DataBlock_temp35]
shr ebx,24
shl bx,2
mov ecx,cs:[Rijndael_Te4+ebx]
and ecx,0ff000000h
xor eax,ecx
mov ebx,ds:[DataBlock_temp36]
shr ebx,16
movzx ebx,bl
shl bx,2
mov ecx,cs:[Rijndael_Te4+ebx]
and ecx,000ff0000h
xor eax,ecx
mov ebx,ds:[DataBlock_temp33]
shr ebx,8
movzx ebx,bl
shl bx,2
mov ecx,cs:[Rijndael_Te4+ebx]
xor ah,ch
mov ebx,ds:[DataBlock_temp34]
movzx ebx,bl
shl bx,2
mov ecx,cs:[Rijndael_Te4+ebx]
xor al,cl
mov ds:[DataBlock_temp39],eax
;s3= (Te4[(t3 >> 24)       ] & 0xff000000) ^
;    (Te4[(t0 >> 16) & 0xff] & 0x00ff0000) ^
;    (Te4[(t1 >>  8) & 0xff] & 0x0000ff00) ^
;    (Te4[(t2      ) & 0xff] & 0x000000ff) ^ rk[3];
lodsd
mov ebx,ds:[DataBlock_temp36]
shr ebx,24
shl bx,2
mov ecx,cs:[Rijndael_Te4+ebx]
and ecx,0ff000000h
xor eax,ecx
mov ebx,ds:[DataBlock_temp33]
shr ebx,16
movzx ebx,bl
shl bx,2
mov ecx,cs:[Rijndael_Te4+ebx]
and ecx,000ff0000h
xor eax,ecx
mov ebx,ds:[DataBlock_temp34]
shr ebx,8
movzx ebx,bl
shl bx,2
mov ecx,cs:[Rijndael_Te4+ebx]
xor ah,ch
mov ebx,ds:[DataBlock_temp35]
movzx ebx,bl
shl bx,2
mov ecx,cs:[Rijndael_Te4+ebx]
xor al,cl
mov ds:[DataBlock_temp40],eax

;PUTU32(ct +  0, s0);
mov eax,ds:[DataBlock_temp37]
xchg al,ah
rol eax,16
xchg al,ah
mov edi,eax
;PUTU32(ct +  4, s1);
mov eax,ds:[DataBlock_temp38]
xchg al,ah
rol eax,16
xchg al,ah
mov ebp,eax
;PUTU32(ct +  8, s2);
mov eax,ds:[DataBlock_temp39]
xchg al,ah
rol eax,16
xchg al,ah
mov ebx,eax
;PUTU32(ct + 12, s3);
mov eax,ds:[DataBlock_temp40]
xchg al,ah
rol eax,16
xchg al,ah
mov edx,eax
retnd
Rijndael_Encrypt_j1:
;t0= Te0[s0 >> 24] ^ Te1[(s1 >> 16) & 0xff] ^ Te2[(s2 >>  8) & 0xff] ^ Te3[s3 & 0xff] ^ rk[ 4];
lodsd
mov ebx,ds:[DataBlock_temp37]
shr ebx,24
shl bx,2
xor eax,cs:[Rijndael_Te0+ebx]
mov ebx,ds:[DataBlock_temp38]
shr ebx,16
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Te1+ebx]
mov ebx,ds:[DataBlock_temp39]
shr ebx,8
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Te2+ebx]
mov ebx,ds:[DataBlock_temp40]
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Te3+ebx]
mov ds:[DataBlock_temp33],eax
;t1= Te0[s1 >> 24] ^ Te1[(s2 >> 16) & 0xff] ^ Te2[(s3 >>  8) & 0xff] ^ Te3[s0 & 0xff] ^ rk[ 5];
lodsd
mov ebx,ds:[DataBlock_temp38]
shr ebx,24
shl bx,2
xor eax,cs:[Rijndael_Te0+ebx]
mov ebx,ds:[DataBlock_temp39]
shr ebx,16
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Te1+ebx]
mov ebx,ds:[DataBlock_temp40]
shr ebx,8
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Te2+ebx]
mov ebx,ds:[DataBlock_temp37]
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Te3+ebx]
mov ds:[DataBlock_temp34],eax
;t2= Te0[s2 >> 24] ^ Te1[(s3 >> 16) & 0xff] ^ Te2[(s0 >>  8) & 0xff] ^ Te3[s1 & 0xff] ^ rk[ 6];
lodsd
mov ebx,ds:[DataBlock_temp39]
shr ebx,24
shl bx,2
xor eax,cs:[Rijndael_Te0+ebx]
mov ebx,ds:[DataBlock_temp40]
shr ebx,16
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Te1+ebx]
mov ebx,ds:[DataBlock_temp37]
shr ebx,8
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Te2+ebx]
mov ebx,ds:[DataBlock_temp38]
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Te3+ebx]
mov ds:[DataBlock_temp35],eax
;t3= Te0[s3 >> 24] ^ Te1[(s0 >> 16) & 0xff] ^ Te2[(s1 >>  8) & 0xff] ^ Te3[s2 & 0xff] ^ rk[ 7];
lodsd
mov ebx,ds:[DataBlock_temp40]
shr ebx,24
shl bx,2
xor eax,cs:[Rijndael_Te0+ebx]
mov ebx,ds:[DataBlock_temp37]
shr ebx,16
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Te1+ebx]
mov ebx,ds:[DataBlock_temp38]
shr ebx,8
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Te2+ebx]
mov ebx,ds:[DataBlock_temp39]
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Te3+ebx]
mov ds:[DataBlock_temp36],eax
retnd
Rijndael_Encrypt_j2:
;s0= Te0[t0 >> 24] ^ Te1[(t1 >> 16) & 0xff] ^ Te2[(t2 >>  8) & 0xff] ^ Te3[t3 & 0xff] ^ rk[ 8];
lodsd
mov ebx,ds:[DataBlock_temp33]
shr ebx,24
shl bx,2
xor eax,cs:[Rijndael_Te0+ebx]
mov ebx,ds:[DataBlock_temp34]
shr ebx,16
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Te1+ebx]
mov ebx,ds:[DataBlock_temp35]
shr ebx,8
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Te2+ebx]
mov ebx,ds:[DataBlock_temp36]
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Te3+ebx]
mov ds:[DataBlock_temp37],eax
;s1= Te0[t1 >> 24] ^ Te1[(t2 >> 16) & 0xff] ^ Te2[(t3 >>  8) & 0xff] ^ Te3[t0 & 0xff] ^ rk[ 9];
lodsd
mov ebx,ds:[DataBlock_temp34]
shr ebx,24
shl bx,2
xor eax,cs:[Rijndael_Te0+ebx]
mov ebx,ds:[DataBlock_temp35]
shr ebx,16
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Te1+ebx]
mov ebx,ds:[DataBlock_temp36]
shr ebx,8
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Te2+ebx]
mov ebx,ds:[DataBlock_temp33]
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Te3+ebx]
mov ds:[DataBlock_temp38],eax
;s2= Te0[t2 >> 24] ^ Te1[(t3 >> 16) & 0xff] ^ Te2[(t0 >>  8) & 0xff] ^ Te3[t1 & 0xff] ^ rk[10];
lodsd
mov ebx,ds:[DataBlock_temp35]
shr ebx,24
shl bx,2
xor eax,cs:[Rijndael_Te0+ebx]
mov ebx,ds:[DataBlock_temp36]
shr ebx,16
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Te1+ebx]
mov ebx,ds:[DataBlock_temp33]
shr ebx,8
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Te2+ebx]
mov ebx,ds:[DataBlock_temp34]
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Te3+ebx]
mov ds:[DataBlock_temp39],eax
;s3= Te0[t3 >> 24] ^ Te1[(t0 >> 16) & 0xff] ^ Te2[(t1 >>  8) & 0xff] ^ Te3[t2 & 0xff] ^ rk[11];
lodsd
mov ebx,ds:[DataBlock_temp36]
shr ebx,24
shl bx,2
xor eax,cs:[Rijndael_Te0+ebx]
mov ebx,ds:[DataBlock_temp33]
shr ebx,16
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Te1+ebx]
mov ebx,ds:[DataBlock_temp34]
shr ebx,8
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Te2+ebx]
mov ebx,ds:[DataBlock_temp35]
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Te3+ebx]
mov ds:[DataBlock_temp40],eax
retnd
endp
;-----------------------------------------

;-----------------------------------------
proc Rijndael_Decrypt
;in: ds:si-record...
;    edi-x0...
;    ebp-x1...
;    ebx-x2...
;    edx-x3...
mov ds:[DataBlock_temp30],si
add si,0f0h

;s0 = GETU32(pt +  0) ^ rk[0];
lodsd
mov ecx,edi
xchg cl,ch
rol ecx,16
xchg cl,ch
xor eax,ecx
mov ds:[DataBlock_temp37],eax
;s1 = GETU32(pt +  4) ^ rk[1];
lodsd
mov ecx,ebp
xchg cl,ch
rol ecx,16
xchg cl,ch
xor eax,ecx
mov ds:[DataBlock_temp38],eax
;s2 = GETU32(pt +  8) ^ rk[2];
lodsd
mov ecx,ebx
xchg cl,ch
rol ecx,16
xchg cl,ch
xor eax,ecx
mov ds:[DataBlock_temp39],eax
;s3 = GETU32(pt + 12) ^ rk[3];
lodsd
mov ecx,edx
xchg cl,ch
rol ecx,16
xchg cl,ch
xor eax,ecx
mov ds:[DataBlock_temp40],eax

call dword Rijndael_Decrypt_j1 ;1
call dword Rijndael_Decrypt_j2 ;2
call dword Rijndael_Decrypt_j1 ;3
call dword Rijndael_Decrypt_j2 ;4
call dword Rijndael_Decrypt_j1 ;5
call dword Rijndael_Decrypt_j2 ;6
call dword Rijndael_Decrypt_j1 ;7
call dword Rijndael_Decrypt_j2 ;8
call dword Rijndael_Decrypt_j1 ;9
mov di,ds:[DataBlock_temp30]
mov al,ds:[di+1e0h]
cmp al,10
jbe dword Rijndael_Decrypt_j3
call dword Rijndael_Decrypt_j2 ;10
call dword Rijndael_Decrypt_j1 ;11
mov di,ds:[DataBlock_temp30]
mov al,ds:[di+1e0h]
cmp al,12
jbe dword Rijndael_Decrypt_j3
call dword Rijndael_Decrypt_j2 ;12
call dword Rijndael_Decrypt_j1 ;13
Rijndael_Decrypt_j3:

;s0= (Td4[(t0 >> 24)       ] & 0xff000000) ^
;    (Td4[(t3 >> 16) & 0xff] & 0x00ff0000) ^
;    (Td4[(t2 >>  8) & 0xff] & 0x0000ff00) ^
;    (Td4[(t1      ) & 0xff] & 0x000000ff) ^ rk[0];
lodsd
mov ebx,ds:[DataBlock_temp33]
shr ebx,24
shl bx,2
mov ecx,cs:[Rijndael_Td4+ebx]
and ecx,0ff000000h
xor eax,ecx
mov ebx,ds:[DataBlock_temp36]
shr ebx,16
movzx ebx,bl
shl bx,2
mov ecx,cs:[Rijndael_Td4+ebx]
and ecx,000ff0000h
xor eax,ecx
mov ebx,ds:[DataBlock_temp35]
shr ebx,8
movzx ebx,bl
shl bx,2
mov ecx,cs:[Rijndael_Td4+ebx]
xor ah,ch
mov ebx,ds:[DataBlock_temp34]
movzx ebx,bl
shl bx,2
mov ecx,cs:[Rijndael_Td4+ebx]
xor al,cl
mov ds:[DataBlock_temp37],eax
;s1= (Td4[(t1 >> 24)       ] & 0xff000000) ^
;    (Td4[(t0 >> 16) & 0xff] & 0x00ff0000) ^
;    (Td4[(t3 >>  8) & 0xff] & 0x0000ff00) ^
;    (Td4[(t2      ) & 0xff] & 0x000000ff) ^ rk[1];
lodsd
mov ebx,ds:[DataBlock_temp34]
shr ebx,24
shl bx,2
mov ecx,cs:[Rijndael_Td4+ebx]
and ecx,0ff000000h
xor eax,ecx
mov ebx,ds:[DataBlock_temp33]
shr ebx,16
movzx ebx,bl
shl bx,2
mov ecx,cs:[Rijndael_Td4+ebx]
and ecx,000ff0000h
xor eax,ecx
mov ebx,ds:[DataBlock_temp36]
shr ebx,8
movzx ebx,bl
shl bx,2
mov ecx,cs:[Rijndael_Td4+ebx]
xor ah,ch
mov ebx,ds:[DataBlock_temp35]
movzx ebx,bl
shl bx,2
mov ecx,cs:[Rijndael_Td4+ebx]
xor al,cl
mov ds:[DataBlock_temp38],eax
;s2= (Td4[(t2 >> 24)       ] & 0xff000000) ^
;    (Td4[(t1 >> 16) & 0xff] & 0x00ff0000) ^
;    (Td4[(t0 >>  8) & 0xff] & 0x0000ff00) ^
;    (Td4[(t3      ) & 0xff] & 0x000000ff) ^ rk[2];
lodsd
mov ebx,ds:[DataBlock_temp35]
shr ebx,24
shl bx,2
mov ecx,cs:[Rijndael_Td4+ebx]
and ecx,0ff000000h
xor eax,ecx
mov ebx,ds:[DataBlock_temp34]
shr ebx,16
movzx ebx,bl
shl bx,2
mov ecx,cs:[Rijndael_Td4+ebx]
and ecx,000ff0000h
xor eax,ecx
mov ebx,ds:[DataBlock_temp33]
shr ebx,8
movzx ebx,bl
shl bx,2
mov ecx,cs:[Rijndael_Td4+ebx]
xor ah,ch
mov ebx,ds:[DataBlock_temp36]
movzx ebx,bl
shl bx,2
mov ecx,cs:[Rijndael_Td4+ebx]
xor al,cl
mov ds:[DataBlock_temp39],eax
;s3= (Td4[(t3 >> 24)       ] & 0xff000000) ^
;    (Td4[(t2 >> 16) & 0xff] & 0x00ff0000) ^
;    (Td4[(t1 >>  8) & 0xff] & 0x0000ff00) ^
;    (Td4[(t0      ) & 0xff] & 0x000000ff) ^ rk[3];
lodsd
mov ebx,ds:[DataBlock_temp36]
shr ebx,24
shl bx,2
mov ecx,cs:[Rijndael_Td4+ebx]
and ecx,0ff000000h
xor eax,ecx
mov ebx,ds:[DataBlock_temp35]
shr ebx,16
movzx ebx,bl
shl bx,2
mov ecx,cs:[Rijndael_Td4+ebx]
and ecx,000ff0000h
xor eax,ecx
mov ebx,ds:[DataBlock_temp34]
shr ebx,8
movzx ebx,bl
shl bx,2
mov ecx,cs:[Rijndael_Td4+ebx]
xor ah,ch
mov ebx,ds:[DataBlock_temp33]
movzx ebx,bl
shl bx,2
mov ecx,cs:[Rijndael_Td4+ebx]
xor al,cl
mov ds:[DataBlock_temp40],eax

;PUTU32(ct +  0, s0);
mov eax,ds:[DataBlock_temp37]
xchg al,ah
rol eax,16
xchg al,ah
mov edi,eax
;PUTU32(ct +  4, s1);
mov eax,ds:[DataBlock_temp38]
xchg al,ah
rol eax,16
xchg al,ah
mov ebp,eax
;PUTU32(ct +  8, s2);
mov eax,ds:[DataBlock_temp39]
xchg al,ah
rol eax,16
xchg al,ah
mov ebx,eax
;PUTU32(ct + 12, s3);
mov eax,ds:[DataBlock_temp40]
xchg al,ah
rol eax,16
xchg al,ah
mov edx,eax
retnd
Rijndael_Decrypt_j1:
;t0= Td0[s0 >> 24] ^ Td1[(s3 >> 16) & 0xff] ^ Td2[(s2 >>  8) & 0xff] ^ Td3[s1 & 0xff] ^ rk[ 4];
lodsd
mov ebx,ds:[DataBlock_temp37]
shr ebx,24
shl bx,2
xor eax,cs:[Rijndael_Td0+ebx]
mov ebx,ds:[DataBlock_temp40]
shr ebx,16
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Td1+ebx]
mov ebx,ds:[DataBlock_temp39]
shr ebx,8
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Td2+ebx]
mov ebx,ds:[DataBlock_temp38]
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Td3+ebx]
mov ds:[DataBlock_temp33],eax
;t1= Td0[s1 >> 24] ^ Td1[(s0 >> 16) & 0xff] ^ Td2[(s3 >>  8) & 0xff] ^ Td3[s2 & 0xff] ^ rk[ 5];
lodsd
mov ebx,ds:[DataBlock_temp38]
shr ebx,24
shl bx,2
xor eax,cs:[Rijndael_Td0+ebx]
mov ebx,ds:[DataBlock_temp37]
shr ebx,16
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Td1+ebx]
mov ebx,ds:[DataBlock_temp40]
shr ebx,8
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Td2+ebx]
mov ebx,ds:[DataBlock_temp39]
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Td3+ebx]
mov ds:[DataBlock_temp34],eax
;t2= Td0[s2 >> 24] ^ Td1[(s1 >> 16) & 0xff] ^ Td2[(s0 >>  8) & 0xff] ^ Td3[s3 & 0xff] ^ rk[ 6];
lodsd
mov ebx,ds:[DataBlock_temp39]
shr ebx,24
shl bx,2
xor eax,cs:[Rijndael_Td0+ebx]
mov ebx,ds:[DataBlock_temp38]
shr ebx,16
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Td1+ebx]
mov ebx,ds:[DataBlock_temp37]
shr ebx,8
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Td2+ebx]
mov ebx,ds:[DataBlock_temp40]
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Td3+ebx]
mov ds:[DataBlock_temp35],eax
;t3= Td0[s3 >> 24] ^ Td1[(s2 >> 16) & 0xff] ^ Td2[(s1 >>  8) & 0xff] ^ Td3[s0 & 0xff] ^ rk[ 7];
lodsd
mov ebx,ds:[DataBlock_temp40]
shr ebx,24
shl bx,2
xor eax,cs:[Rijndael_Td0+ebx]
mov ebx,ds:[DataBlock_temp39]
shr ebx,16
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Td1+ebx]
mov ebx,ds:[DataBlock_temp38]
shr ebx,8
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Td2+ebx]
mov ebx,ds:[DataBlock_temp37]
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Td3+ebx]
mov ds:[DataBlock_temp36],eax
retnd
Rijndael_Decrypt_j2:
;s0= Td0[t0 >> 24] ^ Td1[(t3 >> 16) & 0xff] ^ Td2[(t2 >>  8) & 0xff] ^ Td3[t1 & 0xff] ^ rk[ 8];
lodsd
mov ebx,ds:[DataBlock_temp33]
shr ebx,24
shl bx,2
xor eax,cs:[Rijndael_Td0+ebx]
mov ebx,ds:[DataBlock_temp36]
shr ebx,16
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Td1+ebx]
mov ebx,ds:[DataBlock_temp35]
shr ebx,8
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Td2+ebx]
mov ebx,ds:[DataBlock_temp34]
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Td3+ebx]
mov ds:[DataBlock_temp37],eax
;s1= Td0[t1 >> 24] ^ Td1[(t0 >> 16) & 0xff] ^ Td2[(t3 >>  8) & 0xff] ^ Td3[t2 & 0xff] ^ rk[ 9];
lodsd
mov ebx,ds:[DataBlock_temp34]
shr ebx,24
shl bx,2
xor eax,cs:[Rijndael_Td0+ebx]
mov ebx,ds:[DataBlock_temp33]
shr ebx,16
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Td1+ebx]
mov ebx,ds:[DataBlock_temp36]
shr ebx,8
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Td2+ebx]
mov ebx,ds:[DataBlock_temp35]
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Td3+ebx]
mov ds:[DataBlock_temp38],eax
;s2= Td0[t2 >> 24] ^ Td1[(t1 >> 16) & 0xff] ^ Td2[(t0 >>  8) & 0xff] ^ Td3[t3 & 0xff] ^ rk[10];
lodsd
mov ebx,ds:[DataBlock_temp35]
shr ebx,24
shl bx,2
xor eax,cs:[Rijndael_Td0+ebx]
mov ebx,ds:[DataBlock_temp34]
shr ebx,16
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Td1+ebx]
mov ebx,ds:[DataBlock_temp33]
shr ebx,8
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Td2+ebx]
mov ebx,ds:[DataBlock_temp36]
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Td3+ebx]
mov ds:[DataBlock_temp39],eax
;s3= Td0[t3 >> 24] ^ Td1[(t2 >> 16) & 0xff] ^ Td2[(t1 >>  8) & 0xff] ^ Td3[t0 & 0xff] ^ rk[11];
lodsd
mov ebx,ds:[DataBlock_temp36]
shr ebx,24
shl bx,2
xor eax,cs:[Rijndael_Td0+ebx]
mov ebx,ds:[DataBlock_temp35]
shr ebx,16
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Td1+ebx]
mov ebx,ds:[DataBlock_temp34]
shr ebx,8
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Td2+ebx]
mov ebx,ds:[DataBlock_temp33]
movzx ebx,bl
shl bx,2
xor eax,cs:[Rijndael_Td3+ebx]
mov ds:[DataBlock_temp40],eax
retnd
endp
;-----------------------------------------




;-----------------------------------------
Rijndael_Te0 dd 0c66363a5h,0f87c7c84h,0ee777799h,0f67b7b8dh
dd 0fff2f20dh,0d66b6bbdh,0de6f6fb1h,091c5c554h
dd 060303050h,002010103h,0ce6767a9h,0562b2b7dh
dd 0e7fefe19h,0b5d7d762h,04dababe6h,0ec76769ah
dd 08fcaca45h,01f82829dh,089c9c940h,0fa7d7d87h
dd 0effafa15h,0b25959ebh,08e4747c9h,0fbf0f00bh
dd 041adadech,0b3d4d467h,05fa2a2fdh,045afafeah
dd 0239c9cbfh,053a4a4f7h,0e4727296h,09bc0c05bh
dd 075b7b7c2h,0e1fdfd1ch,03d9393aeh,04c26266ah
dd 06c36365ah,07e3f3f41h,0f5f7f702h,083cccc4fh
dd 06834345ch,051a5a5f4h,0d1e5e534h,0f9f1f108h
dd 0e2717193h,0abd8d873h,062313153h,02a15153fh
dd 00804040ch,095c7c752h,046232365h,09dc3c35eh
dd 030181828h,0379696a1h,00a05050fh,02f9a9ab5h
dd 00e070709h,024121236h,01b80809bh,0dfe2e23dh
dd 0cdebeb26h,04e272769h,07fb2b2cdh,0ea75759fh
dd 01209091bh,01d83839eh,0582c2c74h,0341a1a2eh
dd 0361b1b2dh,0dc6e6eb2h,0b45a5aeeh,05ba0a0fbh
dd 0a45252f6h,0763b3b4dh,0b7d6d661h,07db3b3ceh
dd 05229297bh,0dde3e33eh,05e2f2f71h,013848497h
dd 0a65353f5h,0b9d1d168h,000000000h,0c1eded2ch
dd 040202060h,0e3fcfc1fh,079b1b1c8h,0b65b5bedh
dd 0d46a6abeh,08dcbcb46h,067bebed9h,07239394bh
dd 0944a4adeh,0984c4cd4h,0b05858e8h,085cfcf4ah
dd 0bbd0d06bh,0c5efef2ah,04faaaae5h,0edfbfb16h
dd 0864343c5h,09a4d4dd7h,066333355h,011858594h
dd 08a4545cfh,0e9f9f910h,004020206h,0fe7f7f81h
dd 0a05050f0h,0783c3c44h,0259f9fbah,04ba8a8e3h
dd 0a25151f3h,05da3a3feh,0804040c0h,0058f8f8ah
dd 03f9292adh,0219d9dbch,070383848h,0f1f5f504h
dd 063bcbcdfh,077b6b6c1h,0afdada75h,042212163h
dd 020101030h,0e5ffff1ah,0fdf3f30eh,0bfd2d26dh
dd 081cdcd4ch,0180c0c14h,026131335h,0c3ecec2fh
dd 0be5f5fe1h,0359797a2h,0884444cch,02e171739h
dd 093c4c457h,055a7a7f2h,0fc7e7e82h,07a3d3d47h
dd 0c86464ach,0ba5d5de7h,03219192bh,0e6737395h
dd 0c06060a0h,019818198h,09e4f4fd1h,0a3dcdc7fh
dd 044222266h,0542a2a7eh,03b9090abh,00b888883h
dd 08c4646cah,0c7eeee29h,06bb8b8d3h,02814143ch
dd 0a7dede79h,0bc5e5ee2h,0160b0b1dh,0addbdb76h
dd 0dbe0e03bh,064323256h,0743a3a4eh,0140a0a1eh
dd 0924949dbh,00c06060ah,04824246ch,0b85c5ce4h
dd 09fc2c25dh,0bdd3d36eh,043acacefh,0c46262a6h
dd 0399191a8h,0319595a4h,0d3e4e437h,0f279798bh
dd 0d5e7e732h,08bc8c843h,06e373759h,0da6d6db7h
dd 0018d8d8ch,0b1d5d564h,09c4e4ed2h,049a9a9e0h
dd 0d86c6cb4h,0ac5656fah,0f3f4f407h,0cfeaea25h
dd 0ca6565afh,0f47a7a8eh,047aeaee9h,010080818h
dd 06fbabad5h,0f0787888h,04a25256fh,05c2e2e72h
dd 0381c1c24h,057a6a6f1h,073b4b4c7h,097c6c651h
dd 0cbe8e823h,0a1dddd7ch,0e874749ch,03e1f1f21h
dd 0964b4bddh,061bdbddch,00d8b8b86h,00f8a8a85h
dd 0e0707090h,07c3e3e42h,071b5b5c4h,0cc6666aah
dd 0904848d8h,006030305h,0f7f6f601h,01c0e0e12h
dd 0c26161a3h,06a35355fh,0ae5757f9h,069b9b9d0h
dd 017868691h,099c1c158h,03a1d1d27h,0279e9eb9h
dd 0d9e1e138h,0ebf8f813h,02b9898b3h,022111133h
dd 0d26969bbh,0a9d9d970h,0078e8e89h,0339494a7h
dd 02d9b9bb6h,03c1e1e22h,015878792h,0c9e9e920h
dd 087cece49h,0aa5555ffh,050282878h,0a5dfdf7ah
dd 0038c8c8fh,059a1a1f8h,009898980h,01a0d0d17h
dd 065bfbfdah,0d7e6e631h,0844242c6h,0d06868b8h
dd 0824141c3h,0299999b0h,05a2d2d77h,01e0f0f11h
dd 07bb0b0cbh,0a85454fch,06dbbbbd6h,02c16163ah
;-----------------------------------------

;-----------------------------------------
Rijndael_Te1 dd 0a5c66363h,084f87c7ch,099ee7777h,08df67b7bh
dd 00dfff2f2h,0bdd66b6bh,0b1de6f6fh,05491c5c5h
dd 050603030h,003020101h,0a9ce6767h,07d562b2bh
dd 019e7fefeh,062b5d7d7h,0e64dababh,09aec7676h
dd 0458fcacah,09d1f8282h,04089c9c9h,087fa7d7dh
dd 015effafah,0ebb25959h,0c98e4747h,00bfbf0f0h
dd 0ec41adadh,067b3d4d4h,0fd5fa2a2h,0ea45afafh
dd 0bf239c9ch,0f753a4a4h,096e47272h,05b9bc0c0h
dd 0c275b7b7h,01ce1fdfdh,0ae3d9393h,06a4c2626h
dd 05a6c3636h,0417e3f3fh,002f5f7f7h,04f83cccch
dd 05c683434h,0f451a5a5h,034d1e5e5h,008f9f1f1h
dd 093e27171h,073abd8d8h,053623131h,03f2a1515h
dd 00c080404h,05295c7c7h,065462323h,05e9dc3c3h
dd 028301818h,0a1379696h,00f0a0505h,0b52f9a9ah
dd 0090e0707h,036241212h,09b1b8080h,03ddfe2e2h
dd 026cdebebh,0694e2727h,0cd7fb2b2h,09fea7575h
dd 01b120909h,09e1d8383h,074582c2ch,02e341a1ah
dd 02d361b1bh,0b2dc6e6eh,0eeb45a5ah,0fb5ba0a0h
dd 0f6a45252h,04d763b3bh,061b7d6d6h,0ce7db3b3h
dd 07b522929h,03edde3e3h,0715e2f2fh,097138484h
dd 0f5a65353h,068b9d1d1h,000000000h,02cc1ededh
dd 060402020h,01fe3fcfch,0c879b1b1h,0edb65b5bh
dd 0bed46a6ah,0468dcbcbh,0d967bebeh,04b723939h
dd 0de944a4ah,0d4984c4ch,0e8b05858h,04a85cfcfh
dd 06bbbd0d0h,02ac5efefh,0e54faaaah,016edfbfbh
dd 0c5864343h,0d79a4d4dh,055663333h,094118585h
dd 0cf8a4545h,010e9f9f9h,006040202h,081fe7f7fh
dd 0f0a05050h,044783c3ch,0ba259f9fh,0e34ba8a8h
dd 0f3a25151h,0fe5da3a3h,0c0804040h,08a058f8fh
dd 0ad3f9292h,0bc219d9dh,048703838h,004f1f5f5h
dd 0df63bcbch,0c177b6b6h,075afdadah,063422121h
dd 030201010h,01ae5ffffh,00efdf3f3h,06dbfd2d2h
dd 04c81cdcdh,014180c0ch,035261313h,02fc3ecech
dd 0e1be5f5fh,0a2359797h,0cc884444h,0392e1717h
dd 05793c4c4h,0f255a7a7h,082fc7e7eh,0477a3d3dh
dd 0acc86464h,0e7ba5d5dh,02b321919h,095e67373h
dd 0a0c06060h,098198181h,0d19e4f4fh,07fa3dcdch
dd 066442222h,07e542a2ah,0ab3b9090h,0830b8888h
dd 0ca8c4646h,029c7eeeeh,0d36bb8b8h,03c281414h
dd 079a7dedeh,0e2bc5e5eh,01d160b0bh,076addbdbh
dd 03bdbe0e0h,056643232h,04e743a3ah,01e140a0ah
dd 0db924949h,00a0c0606h,06c482424h,0e4b85c5ch
dd 05d9fc2c2h,06ebdd3d3h,0ef43acach,0a6c46262h
dd 0a8399191h,0a4319595h,037d3e4e4h,08bf27979h
dd 032d5e7e7h,0438bc8c8h,0596e3737h,0b7da6d6dh
dd 08c018d8dh,064b1d5d5h,0d29c4e4eh,0e049a9a9h
dd 0b4d86c6ch,0faac5656h,007f3f4f4h,025cfeaeah
dd 0afca6565h,08ef47a7ah,0e947aeaeh,018100808h
dd 0d56fbabah,088f07878h,06f4a2525h,0725c2e2eh
dd 024381c1ch,0f157a6a6h,0c773b4b4h,05197c6c6h
dd 023cbe8e8h,07ca1ddddh,09ce87474h,0213e1f1fh
dd 0dd964b4bh,0dc61bdbdh,0860d8b8bh,0850f8a8ah
dd 090e07070h,0427c3e3eh,0c471b5b5h,0aacc6666h
dd 0d8904848h,005060303h,001f7f6f6h,0121c0e0eh
dd 0a3c26161h,05f6a3535h,0f9ae5757h,0d069b9b9h
dd 091178686h,05899c1c1h,0273a1d1dh,0b9279e9eh
dd 038d9e1e1h,013ebf8f8h,0b32b9898h,033221111h
dd 0bbd26969h,070a9d9d9h,089078e8eh,0a7339494h
dd 0b62d9b9bh,0223c1e1eh,092158787h,020c9e9e9h
dd 04987ceceh,0ffaa5555h,078502828h,07aa5dfdfh
dd 08f038c8ch,0f859a1a1h,080098989h,0171a0d0dh
dd 0da65bfbfh,031d7e6e6h,0c6844242h,0b8d06868h
dd 0c3824141h,0b0299999h,0775a2d2dh,0111e0f0fh
dd 0cb7bb0b0h,0fca85454h,0d66dbbbbh,03a2c1616h
;-----------------------------------------

;-----------------------------------------
Rijndael_Te2 dd 063a5c663h,07c84f87ch,07799ee77h,07b8df67bh
dd 0f20dfff2h,06bbdd66bh,06fb1de6fh,0c55491c5h
dd 030506030h,001030201h,067a9ce67h,02b7d562bh
dd 0fe19e7feh,0d762b5d7h,0abe64dabh,0769aec76h
dd 0ca458fcah,0829d1f82h,0c94089c9h,07d87fa7dh
dd 0fa15effah,059ebb259h,047c98e47h,0f00bfbf0h
dd 0adec41adh,0d467b3d4h,0a2fd5fa2h,0afea45afh
dd 09cbf239ch,0a4f753a4h,07296e472h,0c05b9bc0h
dd 0b7c275b7h,0fd1ce1fdh,093ae3d93h,0266a4c26h
dd 0365a6c36h,03f417e3fh,0f702f5f7h,0cc4f83cch
dd 0345c6834h,0a5f451a5h,0e534d1e5h,0f108f9f1h
dd 07193e271h,0d873abd8h,031536231h,0153f2a15h
dd 0040c0804h,0c75295c7h,023654623h,0c35e9dc3h
dd 018283018h,096a13796h,0050f0a05h,09ab52f9ah
dd 007090e07h,012362412h,0809b1b80h,0e23ddfe2h
dd 0eb26cdebh,027694e27h,0b2cd7fb2h,0759fea75h
dd 0091b1209h,0839e1d83h,02c74582ch,01a2e341ah
dd 01b2d361bh,06eb2dc6eh,05aeeb45ah,0a0fb5ba0h
dd 052f6a452h,03b4d763bh,0d661b7d6h,0b3ce7db3h
dd 0297b5229h,0e33edde3h,02f715e2fh,084971384h
dd 053f5a653h,0d168b9d1h,000000000h,0ed2cc1edh
dd 020604020h,0fc1fe3fch,0b1c879b1h,05bedb65bh
dd 06abed46ah,0cb468dcbh,0bed967beh,0394b7239h
dd 04ade944ah,04cd4984ch,058e8b058h,0cf4a85cfh
dd 0d06bbbd0h,0ef2ac5efh,0aae54faah,0fb16edfbh
dd 043c58643h,04dd79a4dh,033556633h,085941185h
dd 045cf8a45h,0f910e9f9h,002060402h,07f81fe7fh
dd 050f0a050h,03c44783ch,09fba259fh,0a8e34ba8h
dd 051f3a251h,0a3fe5da3h,040c08040h,08f8a058fh
dd 092ad3f92h,09dbc219dh,038487038h,0f504f1f5h
dd 0bcdf63bch,0b6c177b6h,0da75afdah,021634221h
dd 010302010h,0ff1ae5ffh,0f30efdf3h,0d26dbfd2h
dd 0cd4c81cdh,00c14180ch,013352613h,0ec2fc3ech
dd 05fe1be5fh,097a23597h,044cc8844h,017392e17h
dd 0c45793c4h,0a7f255a7h,07e82fc7eh,03d477a3dh
dd 064acc864h,05de7ba5dh,0192b3219h,07395e673h
dd 060a0c060h,081981981h,04fd19e4fh,0dc7fa3dch
dd 022664422h,02a7e542ah,090ab3b90h,088830b88h
dd 046ca8c46h,0ee29c7eeh,0b8d36bb8h,0143c2814h
dd 0de79a7deh,05ee2bc5eh,00b1d160bh,0db76addbh
dd 0e03bdbe0h,032566432h,03a4e743ah,00a1e140ah
dd 049db9249h,0060a0c06h,0246c4824h,05ce4b85ch
dd 0c25d9fc2h,0d36ebdd3h,0acef43ach,062a6c462h
dd 091a83991h,095a43195h,0e437d3e4h,0798bf279h
dd 0e732d5e7h,0c8438bc8h,037596e37h,06db7da6dh
dd 08d8c018dh,0d564b1d5h,04ed29c4eh,0a9e049a9h
dd 06cb4d86ch,056faac56h,0f407f3f4h,0ea25cfeah
dd 065afca65h,07a8ef47ah,0aee947aeh,008181008h
dd 0bad56fbah,07888f078h,0256f4a25h,02e725c2eh
dd 01c24381ch,0a6f157a6h,0b4c773b4h,0c65197c6h
dd 0e823cbe8h,0dd7ca1ddh,0749ce874h,01f213e1fh
dd 04bdd964bh,0bddc61bdh,08b860d8bh,08a850f8ah
dd 07090e070h,03e427c3eh,0b5c471b5h,066aacc66h
dd 048d89048h,003050603h,0f601f7f6h,00e121c0eh
dd 061a3c261h,0355f6a35h,057f9ae57h,0b9d069b9h
dd 086911786h,0c15899c1h,01d273a1dh,09eb9279eh
dd 0e138d9e1h,0f813ebf8h,098b32b98h,011332211h
dd 069bbd269h,0d970a9d9h,08e89078eh,094a73394h
dd 09bb62d9bh,01e223c1eh,087921587h,0e920c9e9h
dd 0ce4987ceh,055ffaa55h,028785028h,0df7aa5dfh
dd 08c8f038ch,0a1f859a1h,089800989h,00d171a0dh
dd 0bfda65bfh,0e631d7e6h,042c68442h,068b8d068h
dd 041c38241h,099b02999h,02d775a2dh,00f111e0fh
dd 0b0cb7bb0h,054fca854h,0bbd66dbbh,0163a2c16h
;-----------------------------------------

;-----------------------------------------
Rijndael_Te3 dd 06363a5c6h,07c7c84f8h,0777799eeh,07b7b8df6h
dd 0f2f20dffh,06b6bbdd6h,06f6fb1deh,0c5c55491h
dd 030305060h,001010302h,06767a9ceh,02b2b7d56h
dd 0fefe19e7h,0d7d762b5h,0ababe64dh,076769aech
dd 0caca458fh,082829d1fh,0c9c94089h,07d7d87fah
dd 0fafa15efh,05959ebb2h,04747c98eh,0f0f00bfbh
dd 0adadec41h,0d4d467b3h,0a2a2fd5fh,0afafea45h
dd 09c9cbf23h,0a4a4f753h,0727296e4h,0c0c05b9bh
dd 0b7b7c275h,0fdfd1ce1h,09393ae3dh,026266a4ch
dd 036365a6ch,03f3f417eh,0f7f702f5h,0cccc4f83h
dd 034345c68h,0a5a5f451h,0e5e534d1h,0f1f108f9h
dd 0717193e2h,0d8d873abh,031315362h,015153f2ah
dd 004040c08h,0c7c75295h,023236546h,0c3c35e9dh
dd 018182830h,09696a137h,005050f0ah,09a9ab52fh
dd 00707090eh,012123624h,080809b1bh,0e2e23ddfh
dd 0ebeb26cdh,02727694eh,0b2b2cd7fh,075759feah
dd 009091b12h,083839e1dh,02c2c7458h,01a1a2e34h
dd 01b1b2d36h,06e6eb2dch,05a5aeeb4h,0a0a0fb5bh
dd 05252f6a4h,03b3b4d76h,0d6d661b7h,0b3b3ce7dh
dd 029297b52h,0e3e33eddh,02f2f715eh,084849713h
dd 05353f5a6h,0d1d168b9h,000000000h,0eded2cc1h
dd 020206040h,0fcfc1fe3h,0b1b1c879h,05b5bedb6h
dd 06a6abed4h,0cbcb468dh,0bebed967h,039394b72h
dd 04a4ade94h,04c4cd498h,05858e8b0h,0cfcf4a85h
dd 0d0d06bbbh,0efef2ac5h,0aaaae54fh,0fbfb16edh
dd 04343c586h,04d4dd79ah,033335566h,085859411h
dd 04545cf8ah,0f9f910e9h,002020604h,07f7f81feh
dd 05050f0a0h,03c3c4478h,09f9fba25h,0a8a8e34bh
dd 05151f3a2h,0a3a3fe5dh,04040c080h,08f8f8a05h
dd 09292ad3fh,09d9dbc21h,038384870h,0f5f504f1h
dd 0bcbcdf63h,0b6b6c177h,0dada75afh,021216342h
dd 010103020h,0ffff1ae5h,0f3f30efdh,0d2d26dbfh
dd 0cdcd4c81h,00c0c1418h,013133526h,0ecec2fc3h
dd 05f5fe1beh,09797a235h,04444cc88h,01717392eh
dd 0c4c45793h,0a7a7f255h,07e7e82fch,03d3d477ah
dd 06464acc8h,05d5de7bah,019192b32h,0737395e6h
dd 06060a0c0h,081819819h,04f4fd19eh,0dcdc7fa3h
dd 022226644h,02a2a7e54h,09090ab3bh,08888830bh
dd 04646ca8ch,0eeee29c7h,0b8b8d36bh,014143c28h
dd 0dede79a7h,05e5ee2bch,00b0b1d16h,0dbdb76adh
dd 0e0e03bdbh,032325664h,03a3a4e74h,00a0a1e14h
dd 04949db92h,006060a0ch,024246c48h,05c5ce4b8h
dd 0c2c25d9fh,0d3d36ebdh,0acacef43h,06262a6c4h
dd 09191a839h,09595a431h,0e4e437d3h,079798bf2h
dd 0e7e732d5h,0c8c8438bh,03737596eh,06d6db7dah
dd 08d8d8c01h,0d5d564b1h,04e4ed29ch,0a9a9e049h
dd 06c6cb4d8h,05656faach,0f4f407f3h,0eaea25cfh
dd 06565afcah,07a7a8ef4h,0aeaee947h,008081810h
dd 0babad56fh,0787888f0h,025256f4ah,02e2e725ch
dd 01c1c2438h,0a6a6f157h,0b4b4c773h,0c6c65197h
dd 0e8e823cbh,0dddd7ca1h,074749ce8h,01f1f213eh
dd 04b4bdd96h,0bdbddc61h,08b8b860dh,08a8a850fh
dd 0707090e0h,03e3e427ch,0b5b5c471h,06666aacch
dd 04848d890h,003030506h,0f6f601f7h,00e0e121ch
dd 06161a3c2h,035355f6ah,05757f9aeh,0b9b9d069h
dd 086869117h,0c1c15899h,01d1d273ah,09e9eb927h
dd 0e1e138d9h,0f8f813ebh,09898b32bh,011113322h
dd 06969bbd2h,0d9d970a9h,08e8e8907h,09494a733h
dd 09b9bb62dh,01e1e223ch,087879215h,0e9e920c9h
dd 0cece4987h,05555ffaah,028287850h,0dfdf7aa5h
dd 08c8c8f03h,0a1a1f859h,089898009h,00d0d171ah
dd 0bfbfda65h,0e6e631d7h,04242c684h,06868b8d0h
dd 04141c382h,09999b029h,02d2d775ah,00f0f111eh
dd 0b0b0cb7bh,05454fca8h,0bbbbd66dh,016163a2ch
;-----------------------------------------

;-----------------------------------------
Rijndael_Te4 dd 063636363h,07c7c7c7ch,077777777h,07b7b7b7bh
dd 0f2f2f2f2h,06b6b6b6bh,06f6f6f6fh,0c5c5c5c5h
dd 030303030h,001010101h,067676767h,02b2b2b2bh
dd 0fefefefeh,0d7d7d7d7h,0ababababh,076767676h
dd 0cacacacah,082828282h,0c9c9c9c9h,07d7d7d7dh
dd 0fafafafah,059595959h,047474747h,0f0f0f0f0h
dd 0adadadadh,0d4d4d4d4h,0a2a2a2a2h,0afafafafh
dd 09c9c9c9ch,0a4a4a4a4h,072727272h,0c0c0c0c0h
dd 0b7b7b7b7h,0fdfdfdfdh,093939393h,026262626h
dd 036363636h,03f3f3f3fh,0f7f7f7f7h,0cccccccch
dd 034343434h,0a5a5a5a5h,0e5e5e5e5h,0f1f1f1f1h
dd 071717171h,0d8d8d8d8h,031313131h,015151515h
dd 004040404h,0c7c7c7c7h,023232323h,0c3c3c3c3h
dd 018181818h,096969696h,005050505h,09a9a9a9ah
dd 007070707h,012121212h,080808080h,0e2e2e2e2h
dd 0ebebebebh,027272727h,0b2b2b2b2h,075757575h
dd 009090909h,083838383h,02c2c2c2ch,01a1a1a1ah
dd 01b1b1b1bh,06e6e6e6eh,05a5a5a5ah,0a0a0a0a0h
dd 052525252h,03b3b3b3bh,0d6d6d6d6h,0b3b3b3b3h
dd 029292929h,0e3e3e3e3h,02f2f2f2fh,084848484h
dd 053535353h,0d1d1d1d1h,000000000h,0ededededh
dd 020202020h,0fcfcfcfch,0b1b1b1b1h,05b5b5b5bh
dd 06a6a6a6ah,0cbcbcbcbh,0bebebebeh,039393939h
dd 04a4a4a4ah,04c4c4c4ch,058585858h,0cfcfcfcfh
dd 0d0d0d0d0h,0efefefefh,0aaaaaaaah,0fbfbfbfbh
dd 043434343h,04d4d4d4dh,033333333h,085858585h
dd 045454545h,0f9f9f9f9h,002020202h,07f7f7f7fh
dd 050505050h,03c3c3c3ch,09f9f9f9fh,0a8a8a8a8h
dd 051515151h,0a3a3a3a3h,040404040h,08f8f8f8fh
dd 092929292h,09d9d9d9dh,038383838h,0f5f5f5f5h
dd 0bcbcbcbch,0b6b6b6b6h,0dadadadah,021212121h
dd 010101010h,0ffffffffh,0f3f3f3f3h,0d2d2d2d2h
dd 0cdcdcdcdh,00c0c0c0ch,013131313h,0ecececech
dd 05f5f5f5fh,097979797h,044444444h,017171717h
dd 0c4c4c4c4h,0a7a7a7a7h,07e7e7e7eh,03d3d3d3dh
dd 064646464h,05d5d5d5dh,019191919h,073737373h
dd 060606060h,081818181h,04f4f4f4fh,0dcdcdcdch
dd 022222222h,02a2a2a2ah,090909090h,088888888h
dd 046464646h,0eeeeeeeeh,0b8b8b8b8h,014141414h
dd 0dedededeh,05e5e5e5eh,00b0b0b0bh,0dbdbdbdbh
dd 0e0e0e0e0h,032323232h,03a3a3a3ah,00a0a0a0ah
dd 049494949h,006060606h,024242424h,05c5c5c5ch
dd 0c2c2c2c2h,0d3d3d3d3h,0acacacach,062626262h
dd 091919191h,095959595h,0e4e4e4e4h,079797979h
dd 0e7e7e7e7h,0c8c8c8c8h,037373737h,06d6d6d6dh
dd 08d8d8d8dh,0d5d5d5d5h,04e4e4e4eh,0a9a9a9a9h
dd 06c6c6c6ch,056565656h,0f4f4f4f4h,0eaeaeaeah
dd 065656565h,07a7a7a7ah,0aeaeaeaeh,008080808h
dd 0babababah,078787878h,025252525h,02e2e2e2eh
dd 01c1c1c1ch,0a6a6a6a6h,0b4b4b4b4h,0c6c6c6c6h
dd 0e8e8e8e8h,0ddddddddh,074747474h,01f1f1f1fh
dd 04b4b4b4bh,0bdbdbdbdh,08b8b8b8bh,08a8a8a8ah
dd 070707070h,03e3e3e3eh,0b5b5b5b5h,066666666h
dd 048484848h,003030303h,0f6f6f6f6h,00e0e0e0eh
dd 061616161h,035353535h,057575757h,0b9b9b9b9h
dd 086868686h,0c1c1c1c1h,01d1d1d1dh,09e9e9e9eh
dd 0e1e1e1e1h,0f8f8f8f8h,098989898h,011111111h
dd 069696969h,0d9d9d9d9h,08e8e8e8eh,094949494h
dd 09b9b9b9bh,01e1e1e1eh,087878787h,0e9e9e9e9h
dd 0cecececeh,055555555h,028282828h,0dfdfdfdfh
dd 08c8c8c8ch,0a1a1a1a1h,089898989h,00d0d0d0dh
dd 0bfbfbfbfh,0e6e6e6e6h,042424242h,068686868h
dd 041414141h,099999999h,02d2d2d2dh,00f0f0f0fh
dd 0b0b0b0b0h,054545454h,0bbbbbbbbh,016161616h
;-----------------------------------------

;-----------------------------------------
Rijndael_Td0 dd 051f4a750h,07e416553h,01a17a4c3h,03a275e96h
dd 03bab6bcbh,01f9d45f1h,0acfa58abh,04be30393h
dd 02030fa55h,0ad766df6h,088cc7691h,0f5024c25h
dd 04fe5d7fch,0c52acbd7h,026354480h,0b562a38fh
dd 0deb15a49h,025ba1b67h,045ea0e98h,05dfec0e1h
dd 0c32f7502h,0814cf012h,08d4697a3h,06bd3f9c6h
dd 0038f5fe7h,015929c95h,0bf6d7aebh,0955259dah
dd 0d4be832dh,0587421d3h,049e06929h,08ec9c844h
dd 075c2896ah,0f48e7978h,099583e6bh,027b971ddh
dd 0bee14fb6h,0f088ad17h,0c920ac66h,07dce3ab4h
dd 063df4a18h,0e51a3182h,097513360h,062537f45h
dd 0b16477e0h,0bb6bae84h,0fe81a01ch,0f9082b94h
dd 070486858h,08f45fd19h,094de6c87h,0527bf8b7h
dd 0ab73d323h,0724b02e2h,0e31f8f57h,06655ab2ah
dd 0b2eb2807h,02fb5c203h,086c57b9ah,0d33708a5h
dd 0302887f2h,023bfa5b2h,002036abah,0ed16825ch
dd 08acf1c2bh,0a779b492h,0f307f2f0h,04e69e2a1h
dd 065daf4cdh,00605bed5h,0d134621fh,0c4a6fe8ah
dd 0342e539dh,0a2f355a0h,0058ae132h,0a4f6eb75h
dd 00b83ec39h,04060efaah,05e719f06h,0bd6e1051h
dd 03e218af9h,096dd063dh,0dd3e05aeh,04de6bd46h
dd 091548db5h,071c45d05h,00406d46fh,0605015ffh
dd 01998fb24h,0d6bde997h,0894043cch,067d99e77h
dd 0b0e842bdh,007898b88h,0e7195b38h,079c8eedbh
dd 0a17c0a47h,07c420fe9h,0f8841ec9h,000000000h
dd 009808683h,0322bed48h,01e1170ach,06c5a724eh
dd 0fd0efffbh,00f853856h,03daed51eh,0362d3927h
dd 00a0fd964h,0685ca621h,09b5b54d1h,024362e3ah
dd 00c0a67b1h,09357e70fh,0b4ee96d2h,01b9b919eh
dd 080c0c54fh,061dc20a2h,05a774b69h,01c121a16h
dd 0e293ba0ah,0c0a02ae5h,03c22e043h,0121b171dh
dd 00e090d0bh,0f28bc7adh,02db6a8b9h,0141ea9c8h
dd 057f11985h,0af75074ch,0ee99ddbbh,0a37f60fdh
dd 0f701269fh,05c72f5bch,044663bc5h,05bfb7e34h
dd 08b432976h,0cb23c6dch,0b6edfc68h,0b8e4f163h
dd 0d731dccah,042638510h,013972240h,084c61120h
dd 0854a247dh,0d2bb3df8h,0aef93211h,0c729a16dh
dd 01d9e2f4bh,0dcb230f3h,00d8652ech,077c1e3d0h
dd 02bb3166ch,0a970b999h,0119448fah,047e96422h
dd 0a8fc8cc4h,0a0f03f1ah,0567d2cd8h,0223390efh
dd 087494ec7h,0d938d1c1h,08ccaa2feh,098d40b36h
dd 0a6f581cfh,0a57ade28h,0dab78e26h,03fadbfa4h
dd 02c3a9de4h,05078920dh,06a5fcc9bh,0547e4662h
dd 0f68d13c2h,090d8b8e8h,02e39f75eh,082c3aff5h
dd 09f5d80beh,069d0937ch,06fd52da9h,0cf2512b3h
dd 0c8ac993bh,010187da7h,0e89c636eh,0db3bbb7bh
dd 0cd267809h,06e5918f4h,0ec9ab701h,0834f9aa8h
dd 0e6956e65h,0aaffe67eh,021bccf08h,0ef15e8e6h
dd 0bae79bd9h,04a6f36ceh,0ea9f09d4h,029b07cd6h
dd 031a4b2afh,02a3f2331h,0c6a59430h,035a266c0h
dd 0744ebc37h,0fc82caa6h,0e090d0b0h,033a7d815h
dd 0f104984ah,041ecdaf7h,07fcd500eh,01791f62fh
dd 0764dd68dh,043efb04dh,0ccaa4d54h,0e49604dfh
dd 09ed1b5e3h,04c6a881bh,0c12c1fb8h,04665517fh
dd 09d5eea04h,0018c355dh,0fa877473h,0fb0b412eh
dd 0b3671d5ah,092dbd252h,0e9105633h,06dd64713h
dd 09ad7618ch,037a10c7ah,059f8148eh,0eb133c89h
dd 0cea927eeh,0b761c935h,0e11ce5edh,07a47b13ch
dd 09cd2df59h,055f2733fh,01814ce79h,073c737bfh
dd 053f7cdeah,05ffdaa5bh,0df3d6f14h,07844db86h
dd 0caaff381h,0b968c43eh,03824342ch,0c2a3405fh
dd 0161dc372h,0bce2250ch,0283c498bh,0ff0d9541h
dd 039a80171h,0080cb3deh,0d8b4e49ch,06456c190h
dd 07bcb8461h,0d532b670h,0486c5c74h,0d0b85742h
;-----------------------------------------

;-----------------------------------------
Rijndael_Td1 dd 05051f4a7h,0537e4165h,0c31a17a4h,0963a275eh
dd 0cb3bab6bh,0f11f9d45h,0abacfa58h,0934be303h
dd 0552030fah,0f6ad766dh,09188cc76h,025f5024ch
dd 0fc4fe5d7h,0d7c52acbh,080263544h,08fb562a3h
dd 049deb15ah,06725ba1bh,09845ea0eh,0e15dfec0h
dd 002c32f75h,012814cf0h,0a38d4697h,0c66bd3f9h
dd 0e7038f5fh,09515929ch,0ebbf6d7ah,0da955259h
dd 02dd4be83h,0d3587421h,02949e069h,0448ec9c8h
dd 06a75c289h,078f48e79h,06b99583eh,0dd27b971h
dd 0b6bee14fh,017f088adh,066c920ach,0b47dce3ah
dd 01863df4ah,082e51a31h,060975133h,04562537fh
dd 0e0b16477h,084bb6baeh,01cfe81a0h,094f9082bh
dd 058704868h,0198f45fdh,08794de6ch,0b7527bf8h
dd 023ab73d3h,0e2724b02h,057e31f8fh,02a6655abh
dd 007b2eb28h,0032fb5c2h,09a86c57bh,0a5d33708h
dd 0f2302887h,0b223bfa5h,0ba02036ah,05ced1682h
dd 02b8acf1ch,092a779b4h,0f0f307f2h,0a14e69e2h
dd 0cd65daf4h,0d50605beh,01fd13462h,08ac4a6feh
dd 09d342e53h,0a0a2f355h,032058ae1h,075a4f6ebh
dd 0390b83ech,0aa4060efh,0065e719fh,051bd6e10h
dd 0f93e218ah,03d96dd06h,0aedd3e05h,0464de6bdh
dd 0b591548dh,00571c45dh,06f0406d4h,0ff605015h
dd 0241998fbh,097d6bde9h,0cc894043h,07767d99eh
dd 0bdb0e842h,08807898bh,038e7195bh,0db79c8eeh
dd 047a17c0ah,0e97c420fh,0c9f8841eh,000000000h
dd 083098086h,048322bedh,0ac1e1170h,04e6c5a72h
dd 0fbfd0effh,0560f8538h,01e3daed5h,027362d39h
dd 0640a0fd9h,021685ca6h,0d19b5b54h,03a24362eh
dd 0b10c0a67h,00f9357e7h,0d2b4ee96h,09e1b9b91h
dd 04f80c0c5h,0a261dc20h,0695a774bh,0161c121ah
dd 00ae293bah,0e5c0a02ah,0433c22e0h,01d121b17h
dd 00b0e090dh,0adf28bc7h,0b92db6a8h,0c8141ea9h
dd 08557f119h,04caf7507h,0bbee99ddh,0fda37f60h
dd 09ff70126h,0bc5c72f5h,0c544663bh,0345bfb7eh
dd 0768b4329h,0dccb23c6h,068b6edfch,063b8e4f1h
dd 0cad731dch,010426385h,040139722h,02084c611h
dd 07d854a24h,0f8d2bb3dh,011aef932h,06dc729a1h
dd 04b1d9e2fh,0f3dcb230h,0ec0d8652h,0d077c1e3h
dd 06c2bb316h,099a970b9h,0fa119448h,02247e964h
dd 0c4a8fc8ch,01aa0f03fh,0d8567d2ch,0ef223390h
dd 0c787494eh,0c1d938d1h,0fe8ccaa2h,03698d40bh
dd 0cfa6f581h,028a57adeh,026dab78eh,0a43fadbfh
dd 0e42c3a9dh,00d507892h,09b6a5fcch,062547e46h
dd 0c2f68d13h,0e890d8b8h,05e2e39f7h,0f582c3afh
dd 0be9f5d80h,07c69d093h,0a96fd52dh,0b3cf2512h
dd 03bc8ac99h,0a710187dh,06ee89c63h,07bdb3bbbh
dd 009cd2678h,0f46e5918h,001ec9ab7h,0a8834f9ah
dd 065e6956eh,07eaaffe6h,00821bccfh,0e6ef15e8h
dd 0d9bae79bh,0ce4a6f36h,0d4ea9f09h,0d629b07ch
dd 0af31a4b2h,0312a3f23h,030c6a594h,0c035a266h
dd 037744ebch,0a6fc82cah,0b0e090d0h,01533a7d8h
dd 04af10498h,0f741ecdah,00e7fcd50h,02f1791f6h
dd 08d764dd6h,04d43efb0h,054ccaa4dh,0dfe49604h
dd 0e39ed1b5h,01b4c6a88h,0b8c12c1fh,07f466551h
dd 0049d5eeah,05d018c35h,073fa8774h,02efb0b41h
dd 05ab3671dh,05292dbd2h,033e91056h,0136dd647h
dd 08c9ad761h,07a37a10ch,08e59f814h,089eb133ch
dd 0eecea927h,035b761c9h,0ede11ce5h,03c7a47b1h
dd 0599cd2dfh,03f55f273h,0791814ceh,0bf73c737h
dd 0ea53f7cdh,05b5ffdaah,014df3d6fh,0867844dbh
dd 081caaff3h,03eb968c4h,02c382434h,05fc2a340h
dd 072161dc3h,00cbce225h,08b283c49h,041ff0d95h
dd 07139a801h,0de080cb3h,09cd8b4e4h,0906456c1h
dd 0617bcb84h,070d532b6h,074486c5ch,042d0b857h
;-----------------------------------------

;-----------------------------------------
Rijndael_Td2 dd 0a75051f4h,065537e41h,0a4c31a17h,05e963a27h
dd 06bcb3babh,045f11f9dh,058abacfah,003934be3h
dd 0fa552030h,06df6ad76h,0769188cch,04c25f502h
dd 0d7fc4fe5h,0cbd7c52ah,044802635h,0a38fb562h
dd 05a49deb1h,01b6725bah,00e9845eah,0c0e15dfeh
dd 07502c32fh,0f012814ch,097a38d46h,0f9c66bd3h
dd 05fe7038fh,09c951592h,07aebbf6dh,059da9552h
dd 0832dd4beh,021d35874h,0692949e0h,0c8448ec9h
dd 0896a75c2h,07978f48eh,03e6b9958h,071dd27b9h
dd 04fb6bee1h,0ad17f088h,0ac66c920h,03ab47dceh
dd 04a1863dfh,03182e51ah,033609751h,07f456253h
dd 077e0b164h,0ae84bb6bh,0a01cfe81h,02b94f908h
dd 068587048h,0fd198f45h,06c8794deh,0f8b7527bh
dd 0d323ab73h,002e2724bh,08f57e31fh,0ab2a6655h
dd 02807b2ebh,0c2032fb5h,07b9a86c5h,008a5d337h
dd 087f23028h,0a5b223bfh,06aba0203h,0825ced16h
dd 01c2b8acfh,0b492a779h,0f2f0f307h,0e2a14e69h
dd 0f4cd65dah,0bed50605h,0621fd134h,0fe8ac4a6h
dd 0539d342eh,055a0a2f3h,0e132058ah,0eb75a4f6h
dd 0ec390b83h,0efaa4060h,09f065e71h,01051bd6eh

dd 08af93e21h,0063d96ddh,005aedd3eh,0bd464de6h
dd 08db59154h,05d0571c4h,0d46f0406h,015ff6050h
dd 0fb241998h,0e997d6bdh,043cc8940h,09e7767d9h
dd 042bdb0e8h,08b880789h,05b38e719h,0eedb79c8h
dd 00a47a17ch,00fe97c42h,01ec9f884h,000000000h
dd 086830980h,0ed48322bh,070ac1e11h,0724e6c5ah
dd 0fffbfd0eh,038560f85h,0d51e3daeh,03927362dh
dd 0d9640a0fh,0a621685ch,054d19b5bh,02e3a2436h
dd 067b10c0ah,0e70f9357h,096d2b4eeh,0919e1b9bh
dd 0c54f80c0h,020a261dch,04b695a77h,01a161c12h
dd 0ba0ae293h,02ae5c0a0h,0e0433c22h,0171d121bh
dd 00d0b0e09h,0c7adf28bh,0a8b92db6h,0a9c8141eh
dd 0198557f1h,0074caf75h,0ddbbee99h,060fda37fh
dd 0269ff701h,0f5bc5c72h,03bc54466h,07e345bfbh
dd 029768b43h,0c6dccb23h,0fc68b6edh,0f163b8e4h
dd 0dccad731h,085104263h,022401397h,0112084c6h
dd 0247d854ah,03df8d2bbh,03211aef9h,0a16dc729h
dd 02f4b1d9eh,030f3dcb2h,052ec0d86h,0e3d077c1h
dd 0166c2bb3h,0b999a970h,048fa1194h,0642247e9h
dd 08cc4a8fch,03f1aa0f0h,02cd8567dh,090ef2233h
dd 04ec78749h,0d1c1d938h,0a2fe8ccah,00b3698d4h
dd 081cfa6f5h,0de28a57ah,08e26dab7h,0bfa43fadh
dd 09de42c3ah,0920d5078h,0cc9b6a5fh,04662547eh
dd 013c2f68dh,0b8e890d8h,0f75e2e39h,0aff582c3h
dd 080be9f5dh,0937c69d0h,02da96fd5h,012b3cf25h
dd 0993bc8ach,07da71018h,0636ee89ch,0bb7bdb3bh
dd 07809cd26h,018f46e59h,0b701ec9ah,09aa8834fh
dd 06e65e695h,0e67eaaffh,0cf0821bch,0e8e6ef15h
dd 09bd9bae7h,036ce4a6fh,009d4ea9fh,07cd629b0h
dd 0b2af31a4h,023312a3fh,09430c6a5h,066c035a2h
dd 0bc37744eh,0caa6fc82h,0d0b0e090h,0d81533a7h
dd 0984af104h,0daf741ech,0500e7fcdh,0f62f1791h
dd 0d68d764dh,0b04d43efh,04d54ccaah,004dfe496h
dd 0b5e39ed1h,0881b4c6ah,01fb8c12ch,0517f4665h
dd 0ea049d5eh,0355d018ch,07473fa87h,0412efb0bh
dd 01d5ab367h,0d25292dbh,05633e910h,047136dd6h
dd 0618c9ad7h,00c7a37a1h,0148e59f8h,03c89eb13h
dd 027eecea9h,0c935b761h,0e5ede11ch,0b13c7a47h
dd 0df599cd2h,0733f55f2h,0ce791814h,037bf73c7h
dd 0cdea53f7h,0aa5b5ffdh,06f14df3dh,0db867844h
dd 0f381caafh,0c43eb968h,0342c3824h,0405fc2a3h
dd 0c372161dh,0250cbce2h,0498b283ch,09541ff0dh
dd 0017139a8h,0b3de080ch,0e49cd8b4h,0c1906456h
dd 084617bcbh,0b670d532h,05c74486ch,05742d0b8h
;-----------------------------------------

;-----------------------------------------
Rijndael_Td3 dd 0f4a75051h,04165537eh,017a4c31ah,0275e963ah
dd 0ab6bcb3bh,09d45f11fh,0fa58abach,0e303934bh
dd 030fa5520h,0766df6adh,0cc769188h,0024c25f5h
dd 0e5d7fc4fh,02acbd7c5h,035448026h,062a38fb5h
dd 0b15a49deh,0ba1b6725h,0ea0e9845h,0fec0e15dh
dd 02f7502c3h,04cf01281h,04697a38dh,0d3f9c66bh
dd 08f5fe703h,0929c9515h,06d7aebbfh,05259da95h
dd 0be832dd4h,07421d358h,0e0692949h,0c9c8448eh
dd 0c2896a75h,08e7978f4h,0583e6b99h,0b971dd27h
dd 0e14fb6beh,088ad17f0h,020ac66c9h,0ce3ab47dh
dd 0df4a1863h,01a3182e5h,051336097h,0537f4562h
dd 06477e0b1h,06bae84bbh,081a01cfeh,0082b94f9h
dd 048685870h,045fd198fh,0de6c8794h,07bf8b752h
dd 073d323abh,04b02e272h,01f8f57e3h,055ab2a66h
dd 0eb2807b2h,0b5c2032fh,0c57b9a86h,03708a5d3h
dd 02887f230h,0bfa5b223h,0036aba02h,016825cedh
dd 0cf1c2b8ah,079b492a7h,007f2f0f3h,069e2a14eh
dd 0daf4cd65h,005bed506h,034621fd1h,0a6fe8ac4h
dd 02e539d34h,0f355a0a2h,08ae13205h,0f6eb75a4h
dd 083ec390bh,060efaa40h,0719f065eh,06e1051bdh
dd 0218af93eh,0dd063d96h,03e05aeddh,0e6bd464dh
dd 0548db591h,0c45d0571h,006d46f04h,05015ff60h
dd 098fb2419h,0bde997d6h,04043cc89h,0d99e7767h
dd 0e842bdb0h,0898b8807h,0195b38e7h,0c8eedb79h
dd 07c0a47a1h,0420fe97ch,0841ec9f8h,000000000h
dd 080868309h,02bed4832h,01170ac1eh,05a724e6ch
dd 00efffbfdh,08538560fh,0aed51e3dh,02d392736h
dd 00fd9640ah,05ca62168h,05b54d19bh,0362e3a24h
dd 00a67b10ch,057e70f93h,0ee96d2b4h,09b919e1bh
dd 0c0c54f80h,0dc20a261h,0774b695ah,0121a161ch
dd 093ba0ae2h,0a02ae5c0h,022e0433ch,01b171d12h
dd 0090d0b0eh,08bc7adf2h,0b6a8b92dh,01ea9c814h
dd 0f1198557h,075074cafh,099ddbbeeh,07f60fda3h
dd 001269ff7h,072f5bc5ch,0663bc544h,0fb7e345bh
dd 04329768bh,023c6dccbh,0edfc68b6h,0e4f163b8h
dd 031dccad7h,063851042h,097224013h,0c6112084h
dd 04a247d85h,0bb3df8d2h,0f93211aeh,029a16dc7h
dd 09e2f4b1dh,0b230f3dch,08652ec0dh,0c1e3d077h
dd 0b3166c2bh,070b999a9h,09448fa11h,0e9642247h
dd 0fc8cc4a8h,0f03f1aa0h,07d2cd856h,03390ef22h
dd 0494ec787h,038d1c1d9h,0caa2fe8ch,0d40b3698h
dd 0f581cfa6h,07ade28a5h,0b78e26dah,0adbfa43fh
dd 03a9de42ch,078920d50h,05fcc9b6ah,07e466254h
dd 08d13c2f6h,0d8b8e890h,039f75e2eh,0c3aff582h
dd 05d80be9fh,0d0937c69h,0d52da96fh,02512b3cfh
dd 0ac993bc8h,0187da710h,09c636ee8h,03bbb7bdbh
dd 0267809cdh,05918f46eh,09ab701ech,04f9aa883h
dd 0956e65e6h,0ffe67eaah,0bccf0821h,015e8e6efh
dd 0e79bd9bah,06f36ce4ah,09f09d4eah,0b07cd629h
dd 0a4b2af31h,03f23312ah,0a59430c6h,0a266c035h
dd 04ebc3774h,082caa6fch,090d0b0e0h,0a7d81533h
dd 004984af1h,0ecdaf741h,0cd500e7fh,091f62f17h
dd 04dd68d76h,0efb04d43h,0aa4d54cch,09604dfe4h
dd 0d1b5e39eh,06a881b4ch,02c1fb8c1h,065517f46h
dd 05eea049dh,08c355d01h,0877473fah,00b412efbh
dd 0671d5ab3h,0dbd25292h,0105633e9h,0d647136dh
dd 0d7618c9ah,0a10c7a37h,0f8148e59h,0133c89ebh
dd 0a927eeceh,061c935b7h,01ce5ede1h,047b13c7ah
dd 0d2df599ch,0f2733f55h,014ce7918h,0c737bf73h
dd 0f7cdea53h,0fdaa5b5fh,03d6f14dfh,044db8678h
dd 0aff381cah,068c43eb9h,024342c38h,0a3405fc2h
dd 01dc37216h,0e2250cbch,03c498b28h,00d9541ffh
dd 0a8017139h,00cb3de08h,0b4e49cd8h,056c19064h
dd 0cb84617bh,032b670d5h,06c5c7448h,0b85742d0h
;-----------------------------------------

;-----------------------------------------
Rijndael_Td4 dd 052525252h,009090909h,06a6a6a6ah,0d5d5d5d5h
dd 030303030h,036363636h,0a5a5a5a5h,038383838h
dd 0bfbfbfbfh,040404040h,0a3a3a3a3h,09e9e9e9eh
dd 081818181h,0f3f3f3f3h,0d7d7d7d7h,0fbfbfbfbh
dd 07c7c7c7ch,0e3e3e3e3h,039393939h,082828282h
dd 09b9b9b9bh,02f2f2f2fh,0ffffffffh,087878787h
dd 034343434h,08e8e8e8eh,043434343h,044444444h
dd 0c4c4c4c4h,0dedededeh,0e9e9e9e9h,0cbcbcbcbh
dd 054545454h,07b7b7b7bh,094949494h,032323232h
dd 0a6a6a6a6h,0c2c2c2c2h,023232323h,03d3d3d3dh
dd 0eeeeeeeeh,04c4c4c4ch,095959595h,00b0b0b0bh
dd 042424242h,0fafafafah,0c3c3c3c3h,04e4e4e4eh
dd 008080808h,02e2e2e2eh,0a1a1a1a1h,066666666h
dd 028282828h,0d9d9d9d9h,024242424h,0b2b2b2b2h
dd 076767676h,05b5b5b5bh,0a2a2a2a2h,049494949h
dd 06d6d6d6dh,08b8b8b8bh,0d1d1d1d1h,025252525h
dd 072727272h,0f8f8f8f8h,0f6f6f6f6h,064646464h
dd 086868686h,068686868h,098989898h,016161616h
dd 0d4d4d4d4h,0a4a4a4a4h,05c5c5c5ch,0cccccccch
dd 05d5d5d5dh,065656565h,0b6b6b6b6h,092929292h
dd 06c6c6c6ch,070707070h,048484848h,050505050h
dd 0fdfdfdfdh,0ededededh,0b9b9b9b9h,0dadadadah
dd 05e5e5e5eh,015151515h,046464646h,057575757h
dd 0a7a7a7a7h,08d8d8d8dh,09d9d9d9dh,084848484h
dd 090909090h,0d8d8d8d8h,0ababababh,000000000h
dd 08c8c8c8ch,0bcbcbcbch,0d3d3d3d3h,00a0a0a0ah
dd 0f7f7f7f7h,0e4e4e4e4h,058585858h,005050505h
dd 0b8b8b8b8h,0b3b3b3b3h,045454545h,006060606h
dd 0d0d0d0d0h,02c2c2c2ch,01e1e1e1eh,08f8f8f8fh
dd 0cacacacah,03f3f3f3fh,00f0f0f0fh,002020202h
dd 0c1c1c1c1h,0afafafafh,0bdbdbdbdh,003030303h
dd 001010101h,013131313h,08a8a8a8ah,06b6b6b6bh
dd 03a3a3a3ah,091919191h,011111111h,041414141h
dd 04f4f4f4fh,067676767h,0dcdcdcdch,0eaeaeaeah
dd 097979797h,0f2f2f2f2h,0cfcfcfcfh,0cecececeh
dd 0f0f0f0f0h,0b4b4b4b4h,0e6e6e6e6h,073737373h
dd 096969696h,0acacacach,074747474h,022222222h
dd 0e7e7e7e7h,0adadadadh,035353535h,085858585h
dd 0e2e2e2e2h,0f9f9f9f9h,037373737h,0e8e8e8e8h
dd 01c1c1c1ch,075757575h,0dfdfdfdfh,06e6e6e6eh
dd 047474747h,0f1f1f1f1h,01a1a1a1ah,071717171h
dd 01d1d1d1dh,029292929h,0c5c5c5c5h,089898989h
dd 06f6f6f6fh,0b7b7b7b7h,062626262h,00e0e0e0eh
dd 0aaaaaaaah,018181818h,0bebebebeh,01b1b1b1bh
dd 0fcfcfcfch,056565656h,03e3e3e3eh,04b4b4b4bh
dd 0c6c6c6c6h,0d2d2d2d2h,079797979h,020202020h
dd 09a9a9a9ah,0dbdbdbdbh,0c0c0c0c0h,0fefefefeh
dd 078787878h,0cdcdcdcdh,05a5a5a5ah,0f4f4f4f4h
dd 01f1f1f1fh,0ddddddddh,0a8a8a8a8h,033333333h
dd 088888888h,007070707h,0c7c7c7c7h,031313131h
dd 0b1b1b1b1h,012121212h,010101010h,059595959h
dd 027272727h,080808080h,0ecececech,05f5f5f5fh
dd 060606060h,051515151h,07f7f7f7fh,0a9a9a9a9h
dd 019191919h,0b5b5b5b5h,04a4a4a4ah,00d0d0d0dh
dd 02d2d2d2dh,0e5e5e5e5h,07a7a7a7ah,09f9f9f9fh
dd 093939393h,0c9c9c9c9h,09c9c9c9ch,0efefefefh
dd 0a0a0a0a0h,0e0e0e0e0h,03b3b3b3bh,04d4d4d4dh
dd 0aeaeaeaeh,02a2a2a2ah,0f5f5f5f5h,0b0b0b0b0h
dd 0c8c8c8c8h,0ebebebebh,0bbbbbbbbh,03c3c3c3ch
dd 083838383h,053535353h,099999999h,061616161h
dd 017171717h,02b2b2b2bh,004040404h,07e7e7e7eh
dd 0babababah,077777777h,0d6d6d6d6h,026262626h
dd 0e1e1e1e1h,069696969h,014141414h,063636363h
dd 055555555h,021212121h,00c0c0c0ch,07d7d7d7dh
;-----------------------------------------

;-----------------------------------------
Rijndael_Rcon dd 001000000h,002000000h,004000000h,008000000h
dd 010000000h,020000000h,040000000h,080000000h
dd 01B000000h,036000000h
;-----------------------------------------


;rec:
;enckey dd 60 dup (?)    000h
;deckey dd 60 dup (?)    0f0h
;rounds db ?             1e0h
