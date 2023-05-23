camellia_size equ 16
camellia_param equ 0

;-----------------------------------------
proc camellia_Init
;in: ds:bx-record...
;    ds:si-key text...
;    cx-size of key...
mov bp,bx
mov ds:[bp+200h],cx
cmp cx,16
mov di,DataBlock_temp1
jne dword camellia_Init_j1
mov cx,16
rep
  movsb
sub eax,eax
mov cx,16
rep
  stosb
jmp dword camellia_Init_j4
camellia_Init_j1:
cmp cx,24
jne dword camellia_Init_j3
mov cx,24
rep
  movsb
mov cx,8
camellia_Init_j2:
mov al,ds:[di-8]
not al
stosb
loopw camellia_Init_j2
jmp dword camellia_Init_j4
camellia_Init_j3:
cmp cx,32
jne dword camellia_Init_vege
mov cx,32
rep
  movsb
camellia_Init_j4:

;XorBlock( t+0, t+16, t+32 );
mov si,DataBlock_temp1
lea ebx,ds:[esi+16]
lea edi,ds:[esi+32]
call dword camellia_xor

mov esi,offset camellia_sigma
mov edi,DataBlock_temp4
mov ecx,48
rep
  movsb cs,ptr32

;Camellia_Feistel( t+32, SIGMA+0, t+40 );
mov esi,DataBlock_temp1
mov ebx,DataBlock_temp4
lea edi,ds:[esi+40]
add esi,32
call dword camellia_Feistel

;Camellia_Feistel( t+40, SIGMA+8, t+32 );
mov esi,DataBlock_temp1
mov ebx,DataBlock_temp4
add ebx,8
lea edi,ds:[esi+32]
add esi,40
call dword camellia_Feistel

;XorBlock( t+32, t+0, t+32 );
mov ebx,DataBlock_temp1
lea edi,ds:[ebx+32]
mov esi,edi
call dword camellia_xor

;Camellia_Feistel( t+32, SIGMA+16, t+40 );
mov esi,DataBlock_temp1
mov ebx,DataBlock_temp4
add ebx,16
lea edi,ds:[esi+40]
add esi,32
call dword camellia_Feistel

;Camellia_Feistel( t+40, SIGMA+24, t+32 );
mov esi,DataBlock_temp1
mov ebx,DataBlock_temp4
add ebx,24
lea edi,ds:[esi+32]
add esi,40
call dword camellia_Feistel

;ByteWord( t+0,  u+0 );
mov esi,DataBlock_temp1
mov edi,DataBlock_temp2
call dword camellia_msb

;ByteWord( t+32, u+4 );
mov esi,DataBlock_temp1
mov edi,DataBlock_temp2
add edi,16
add esi,32
call dword camellia_msb

mov ax,ds:[bp+200h]
cmp ax,16
jne dword camellia_Init_j6

;for( i=0; i<26; i+=2 ){
sub eax,eax
mov ds:[DataBlock_temp40],eax
camellia_Init_j5:

;  RotBlock( u+KIDX1[i+0], KSFT1[i+0], u+16 );
mov eax,ds:[DataBlock_temp40]
movzx byte esi,cs:[camellia_kidx1+eax]
movzx byte ecx,cs:[camellia_ksft1+eax]
shl si,2
add si,DataBlock_temp2
mov di,bp
call dword camellia_rot

;  RotBlock( u+KIDX1[i+1], KSFT1[i+1], u+18 );
mov eax,ds:[DataBlock_temp40]
inc eax
movzx byte esi,cs:[camellia_kidx1+eax]
movzx byte ecx,cs:[camellia_ksft1+eax]
shl si,2
add si,DataBlock_temp2
lea di,ds:[bp+8]
call dword camellia_rot

;  WordByte( u+16, e+i*8 );                      }
mov si,bp
mov di,bp
call dword camellia_msb

add bp,16
mov eax,ds:[DataBlock_temp40]
add eax,2
mov ds:[DataBlock_temp40],eax
cmp al,26
jb dword camellia_Init_j5

retnd

camellia_Init_j6:

;XorBlock( t+32, t+16, t+48 );
mov si,DataBlock_temp1
lea ebx,ds:[esi+16]
lea edi,ds:[esi+48]
add si,32
call dword camellia_xor

;Camellia_Feistel( t+48, SIGMA+32, t+56 );
mov esi,DataBlock_temp1
mov ebx,DataBlock_temp4
add ebx,32
lea edi,ds:[esi+56]
add esi,48
call dword camellia_Feistel

;Camellia_Feistel( t+56, SIGMA+40, t+48 );
mov esi,DataBlock_temp1
mov ebx,DataBlock_temp4
add ebx,40
lea edi,ds:[esi+48]
add esi,56
call dword camellia_Feistel

;ByteWord( t+16, u+8  );
mov esi,DataBlock_temp1
mov edi,DataBlock_temp2
add edi,32
add esi,16
call dword camellia_msb

;ByteWord( t+48, u+12 );
mov esi,DataBlock_temp1
mov edi,DataBlock_temp2
add edi,48
add esi,48
call dword camellia_msb

;for( i=0; i<34; i+=2 ){
sub eax,eax
mov ds:[DataBlock_temp40],eax
camellia_Init_j7:

;  RotBlock( u+KIDX2[i+0], KSFT2[i+0], u+16 );
mov eax,ds:[DataBlock_temp40]
movzx byte esi,cs:[camellia_kidx2+eax]
movzx byte ecx,cs:[camellia_ksft2+eax]
shl si,2
add si,DataBlock_temp2
mov di,bp
call dword camellia_rot

;  RotBlock( u+KIDX2[i+1], KSFT2[i+1], u+18 );
mov eax,ds:[DataBlock_temp40]
inc eax
movzx byte esi,cs:[camellia_kidx2+eax]
movzx byte ecx,cs:[camellia_ksft2+eax]
shl si,2
add si,DataBlock_temp2
lea di,ds:[bp+8]
call dword camellia_rot

;  WordByte( u+16, e+(i<<3) );
mov si,bp
mov di,bp
call dword camellia_msb

add bp,16
mov eax,ds:[DataBlock_temp40]
add eax,2
mov ds:[DataBlock_temp40],eax
cmp al,34
jb dword camellia_Init_j7

camellia_Init_vege:
retnd
endp
;-----------------------------------------



;-----------------------------------------
proc camellia_Encrypt
;in: ds:si-record...
;    edi-x0...
;    ebp-x1...
;    ebx-x2...
;    edx-x3...
mov eax,DataBlock_temp1
mov ds:[eax+0],edi
mov ds:[eax+4],ebp
mov ds:[eax+8],ebx
mov ds:[eax+12],edx
mov bp,si

;XorBlock( p, e+0, c );
mov si,DataBlock_temp1
mov bx,bp
mov di,DataBlock_temp2
call dword camellia_xor

;for( i=0; i<3; i++ ){
sub eax,eax
mov ds:[DataBlock_temp41],eax
camellia_Encrypt_j1:

;  Camellia_Feistel( c+0, e+16+(i<<4), c+8 );
mov si,DataBlock_temp2
mov di,si
add di,8
mov bx,ds:[DataBlock_temp41]
shl bx,4
add bx,16
add bx,bp
call dword camellia_Feistel

;  Camellia_Feistel( c+8, e+24+(i<<4), c+0 );      }
mov si,DataBlock_temp2
mov di,si
add si,8
mov bx,ds:[DataBlock_temp41]
shl bx,4
add bx,24
add bx,bp
call dword camellia_Feistel

mov eax,ds:[DataBlock_temp41]
inc eax
mov ds:[DataBlock_temp41],eax
cmp al,3
jb dword camellia_Encrypt_j1

;Camellia_FLlayer( c, e+64, e+72 );
mov di,DataBlock_temp2
lea si,ds:[bp+64]
lea bx,ds:[bp+72]
call dword camellia_FLlayer

;for( i=0; i<3; i++ ){
sub eax,eax
mov ds:[DataBlock_temp41],eax
camellia_Encrypt_j2:

;  Camellia_Feistel( c+0, e+80+(i<<4), c+8 );
mov si,DataBlock_temp2
mov di,si
add di,8
mov bx,ds:[DataBlock_temp41]
shl bx,4
add bx,80
add bx,bp
call dword camellia_Feistel

;  Camellia_Feistel( c+8, e+88+(i<<4), c+0 );      }
mov si,DataBlock_temp2
mov di,si
add si,8
mov bx,ds:[DataBlock_temp41]
shl bx,4
add bx,88
add bx,bp
call dword camellia_Feistel

mov eax,ds:[DataBlock_temp41]
inc eax
mov ds:[DataBlock_temp41],eax
cmp al,3
jb dword camellia_Encrypt_j2

;Camellia_FLlayer( c, e+128, e+136 );
mov di,DataBlock_temp2
lea si,ds:[bp+128]
lea bx,ds:[bp+136]
call dword camellia_FLlayer

;for( i=0; i<3; i++ ){
sub eax,eax
mov ds:[DataBlock_temp41],eax
camellia_Encrypt_j3:

;  Camellia_Feistel( c+0, e+144+(i<<4), c+8 );
mov si,DataBlock_temp2
mov di,si
add di,8
mov bx,ds:[DataBlock_temp41]
shl bx,4
add bx,144
add bx,bp
call dword camellia_Feistel

;  Camellia_Feistel( c+8, e+152+(i<<4), c+0 );     }
mov si,DataBlock_temp2
mov di,si
add si,8
mov bx,ds:[DataBlock_temp41]
shl bx,4
add bx,152
add bx,bp
call dword camellia_Feistel

mov eax,ds:[DataBlock_temp41]
inc eax
mov ds:[DataBlock_temp41],eax
cmp al,3
jb dword camellia_Encrypt_j3

mov ax,ds:[bp+200h]
cmp ax,16
jne dword camellia_Encrypt_j4

;SwapHalf( c );
mov si,DataBlock_temp2
call dword camellia_half

;XorBlock( c, e+192, c );
mov si,DataBlock_temp2
lea bx,ds:[bp+192]
mov di,si
call dword camellia_xor

jmp dword camellia_Encrypt_vege
camellia_Encrypt_j4:

;Camellia_FLlayer( c, e+192, e+200 );
mov di,DataBlock_temp2
lea si,ds:[bp+192]
lea bx,ds:[bp+200]
call dword camellia_FLlayer

;for( i=0; i<3; i++ ){
sub eax,eax
mov ds:[DataBlock_temp41],eax
camellia_Encrypt_j5:

;  Camellia_Feistel( c+0, e+208+(i<<4), c+8 );
mov si,DataBlock_temp2
mov di,si
add di,8
mov bx,ds:[DataBlock_temp41]
shl bx,4
add bx,208
add bx,bp
call dword camellia_Feistel

;  Camellia_Feistel( c+8, e+216+(i<<4), c+0 );
mov si,DataBlock_temp2
mov di,si
add si,8
mov bx,ds:[DataBlock_temp41]
shl bx,4
add bx,216
add bx,bp
call dword camellia_Feistel

mov eax,ds:[DataBlock_temp41]
inc eax
mov ds:[DataBlock_temp41],eax
cmp al,3
jb dword camellia_Encrypt_j5

;SwapHalf( c );
mov si,DataBlock_temp2
call dword camellia_half

;XorBlock( c, e+256, c );
mov si,DataBlock_temp2
lea bx,ds:[bp+256]
mov di,si
call dword camellia_xor

camellia_Encrypt_vege:
mov eax,DataBlock_temp2
mov edi,ds:[eax+0]
mov ebp,ds:[eax+4]
mov ebx,ds:[eax+8]
mov edx,ds:[eax+12]
retnd
endp
;-----------------------------------------



;-----------------------------------------
proc camellia_Decrypt
;in: ds:si-record...
;    edi-x0...
;    ebp-x1...
;    ebx-x2...
;    edx-x3...
mov eax,DataBlock_temp1
mov ds:[eax+0],edi
mov ds:[eax+4],ebp
mov ds:[eax+8],ebx
mov ds:[eax+12],edx
mov bp,si

mov ax,ds:[bp+200h]
cmp ax,16
jne dword camellia_Decrypt_j1

;XorBlock( c, e+192, p );
mov si,DataBlock_temp1
lea bx,ds:[bp+192]
mov di,DataBlock_temp2
call dword camellia_xor

jmp dword camellia_Decrypt_j3

camellia_Decrypt_j1:

;XorBlock( c, e+256, p );
mov si,DataBlock_temp1
lea bx,ds:[bp+256]
mov di,DataBlock_temp2
call dword camellia_xor

;for( i=2; i>=0; i-- ){
mov eax,2
mov ds:[DataBlock_temp41],eax
camellia_Decrypt_j2:

;  Camellia_Feistel( p+0, e+216+(i<<4), p+8 );
mov si,DataBlock_temp2
mov di,si
add di,8
mov bx,ds:[DataBlock_temp41]
shl bx,4
add bx,216
add bx,bp
call dword camellia_Feistel

;  Camellia_Feistel( p+8, e+208+(i<<4), p+0 );
mov si,DataBlock_temp2
mov di,si
add si,8
mov bx,ds:[DataBlock_temp41]
shl bx,4
add bx,208
add bx,bp
call dword camellia_Feistel

mov eax,ds:[DataBlock_temp41]
dec eax
mov ds:[DataBlock_temp41],eax
or eax,eax
jns dword camellia_Decrypt_j2

;Camellia_FLlayer( p, e+200, e+192 );
mov di,DataBlock_temp2
lea si,ds:[bp+200]
lea bx,ds:[bp+192]
call dword camellia_FLlayer

camellia_Decrypt_j3:

;for( i=2; i>=0; i-- ){
mov eax,2
mov ds:[DataBlock_temp41],eax
camellia_Decrypt_j4:

;  Camellia_Feistel( p+0, e+152+(i<<4), p+8 );
mov si,DataBlock_temp2
mov di,si
add di,8
mov bx,ds:[DataBlock_temp41]
shl bx,4
add bx,152
add bx,bp
call dword camellia_Feistel

;  Camellia_Feistel( p+8, e+144+(i<<4), p+0 );
mov si,DataBlock_temp2
mov di,si
add si,8
mov bx,ds:[DataBlock_temp41]
shl bx,4
add bx,144
add bx,bp
call dword camellia_Feistel

mov eax,ds:[DataBlock_temp41]
dec eax
mov ds:[DataBlock_temp41],eax
or eax,eax
jns dword camellia_Decrypt_j4

;Camellia_FLlayer( p, e+136, e+128 );
mov di,DataBlock_temp2
lea si,ds:[bp+136]
lea bx,ds:[bp+128]
call dword camellia_FLlayer

;for( i=2; i>=0; i-- ){
mov eax,2
mov ds:[DataBlock_temp41],eax
camellia_Decrypt_j5:

;  Camellia_Feistel( p+0, e+88+(i<<4), p+8 );
mov si,DataBlock_temp2
mov di,si
add di,8
mov bx,ds:[DataBlock_temp41]
shl bx,4
add bx,88
add bx,bp
call dword camellia_Feistel

;  Camellia_Feistel( p+8, e+80+(i<<4), p+0 );
mov si,DataBlock_temp2
mov di,si
add si,8
mov bx,ds:[DataBlock_temp41]
shl bx,4
add bx,80
add bx,bp
call dword camellia_Feistel

mov eax,ds:[DataBlock_temp41]
dec eax
mov ds:[DataBlock_temp41],eax
or eax,eax
jns dword camellia_Decrypt_j5

;Camellia_FLlayer( p, e+72, e+64 );
mov di,DataBlock_temp2
lea si,ds:[bp+72]
lea bx,ds:[bp+64]
call dword camellia_FLlayer

;for( i=2; i>=0; i-- ){
mov eax,2
mov ds:[DataBlock_temp41],eax
camellia_Decrypt_j6:

;  Camellia_Feistel( p+0, e+24+(i<<4), p+8 );
mov si,DataBlock_temp2
mov di,si
add di,8
mov bx,ds:[DataBlock_temp41]
shl bx,4
add bx,24
add bx,bp
call dword camellia_Feistel

;  Camellia_Feistel( p+8, e+16+(i<<4), p+0 );
mov si,DataBlock_temp2
mov di,si
add si,8
mov bx,ds:[DataBlock_temp41]
shl bx,4
add bx,16
add bx,bp
call dword camellia_Feistel

mov eax,ds:[DataBlock_temp41]
dec eax
mov ds:[DataBlock_temp41],eax
or eax,eax
jns dword camellia_Decrypt_j6

;SwapHalf( p );
mov si,DataBlock_temp2
call dword camellia_half

;XorBlock( p, e+0, p );
mov si,DataBlock_temp2
mov bx,bp
mov di,si
call dword camellia_xor

mov eax,DataBlock_temp2
mov edi,ds:[eax+0]
mov ebp,ds:[eax+4]
mov ebx,ds:[eax+8]
mov edx,ds:[eax+12]
retnd
endp
;-----------------------------------------


;-----------------------------------------
proc camellia_Feistel
;in: ds:si-src1
;    ds:bx-src2
;    ds:di-trg
mov ecx,DataBlock_temp30

;t[0] = SBOX1(x[0]^k[0]);
movzx byte eax,ds:[si+0]
xor al,ds:[bx+0]
mov al,cs:[camellia_sbox1+eax]
mov ds:[ecx+0],al

;t[1] = SBOX2(x[1]^k[1]);
movzx byte eax,ds:[si+1]
xor al,ds:[bx+1]
mov al,cs:[camellia_sbox2+eax]
mov ds:[ecx+1],al

;t[2] = SBOX3(x[2]^k[2]);
movzx byte eax,ds:[si+2]
xor al,ds:[bx+2]
mov al,cs:[camellia_sbox3+eax]
mov ds:[ecx+2],al

;t[3] = SBOX4(x[3]^k[3]);
movzx byte eax,ds:[si+3]
xor al,ds:[bx+3]
mov al,cs:[camellia_sbox4+eax]
mov ds:[ecx+3],al

;t[4] = SBOX2(x[4]^k[4]);
movzx byte eax,ds:[si+4]
xor al,ds:[bx+4]
mov al,cs:[camellia_sbox2+eax]
mov ds:[ecx+4],al

;t[5] = SBOX3(x[5]^k[5]);
movzx byte eax,ds:[si+5]
xor al,ds:[bx+5]
mov al,cs:[camellia_sbox3+eax]
mov ds:[ecx+5],al

;t[6] = SBOX4(x[6]^k[6]);
movzx byte eax,ds:[si+6]
xor al,ds:[bx+6]
mov al,cs:[camellia_sbox4+eax]
mov ds:[ecx+6],al

;t[7] = SBOX1(x[7]^k[7]);
movzx byte eax,ds:[si+7]
xor al,ds:[bx+7]
mov al,cs:[camellia_sbox1+eax]
mov ds:[ecx+7],al

;y[0] ^= t[0]^t[2]^t[3]^t[5]^t[6]^t[7];
mov al,ds:[ecx+0]
xor al,ds:[ecx+2]
xor al,ds:[ecx+3]
xor al,ds:[ecx+5]
xor al,ds:[ecx+6]
xor al,ds:[ecx+7]
xor ds:[di+0],al

;y[1] ^= t[0]^t[1]^t[3]^t[4]^t[6]^t[7];
mov al,ds:[ecx+0]
xor al,ds:[ecx+1]
xor al,ds:[ecx+3]
xor al,ds:[ecx+4]
xor al,ds:[ecx+6]
xor al,ds:[ecx+7]
xor ds:[di+1],al

;y[2] ^= t[0]^t[1]^t[2]^t[4]^t[5]^t[7];
mov al,ds:[ecx+0]
xor al,ds:[ecx+1]
xor al,ds:[ecx+2]
xor al,ds:[ecx+4]
xor al,ds:[ecx+5]
xor al,ds:[ecx+7]
xor ds:[di+2],al

;y[3] ^= t[1]^t[2]^t[3]^t[4]^t[5]^t[6];
mov al,ds:[ecx+1]
xor al,ds:[ecx+2]
xor al,ds:[ecx+3]
xor al,ds:[ecx+4]
xor al,ds:[ecx+5]
xor al,ds:[ecx+6]
xor ds:[di+3],al

;y[4] ^= t[0]^t[1]^t[5]^t[6]^t[7];
mov al,ds:[ecx+0]
xor al,ds:[ecx+1]
xor al,ds:[ecx+5]
xor al,ds:[ecx+6]
xor al,ds:[ecx+7]
xor ds:[di+4],al

;y[5] ^= t[1]^t[2]^t[4]^t[6]^t[7];
mov al,ds:[ecx+1]
xor al,ds:[ecx+2]
xor al,ds:[ecx+4]
xor al,ds:[ecx+6]
xor al,ds:[ecx+7]
xor ds:[di+5],al

;y[6] ^= t[2]^t[3]^t[4]^t[5]^t[7];
mov al,ds:[ecx+2]
xor al,ds:[ecx+3]
xor al,ds:[ecx+4]
xor al,ds:[ecx+5]
xor al,ds:[ecx+7]
xor ds:[di+6],al

;y[7] ^= t[0]^t[3]^t[4]^t[5]^t[6];
mov al,ds:[ecx+0]
xor al,ds:[ecx+3]
xor al,ds:[ecx+4]
xor al,ds:[ecx+5]
xor al,ds:[ecx+6]
xor ds:[di+7],al

retnd
endp
;-----------------------------------------

;-----------------------------------------
proc camellia_FLlayer
;in: ds:di-trg
;    ds:si-src1
;    ds:bx-src2

;t[1] ^= (t[0]&u[0])<<1^(t[0]&u[0])>>31;
mov eax,ds:[di+0]
and eax,ds:[si+0]
xchg al,ah
rol eax,16
xchg al,ah
rol eax,1
xchg al,ah
rol eax,16
xchg al,ah
xor ds:[di+4],eax

;t[0] ^= t[1]|u[1];
mov eax,ds:[di+4]
or eax,ds:[si+4]
xor ds:[di+0],eax

;t[2] ^= t[3]|v[1];
mov eax,ds:[di+12]
or eax,ds:[bx+4]
xor ds:[di+8],eax

;t[3] ^= (t[2]&v[0])<<1^(t[2]&v[0])>>31;
mov eax,ds:[di+8]
and eax,ds:[bx+0]
xchg al,ah
rol eax,16
xchg al,ah
rol eax,1
xchg al,ah
rol eax,16
xchg al,ah
xor ds:[di+12],eax

retnd
endp
;-----------------------------------------

;-----------------------------------------
proc camellia_xor
;in: ds:si-src1
;    ds:bx-src2
;    es:di-trg
mov cx,16
;for( i=0; i<16; i++ ) z[i] = x[i] ^ y[i];
camellia_xor_j1:
lodsb
xor al,ds:[bx]
inc bx
stosb
loopw camellia_xor_j1
retnd
endp
;-----------------------------------------

;-----------------------------------------
proc camellia_half
;in: ds:si-block
mov cx,8
camellia_half_j1:
mov al,ds:[si]
xchg al,ds:[si+8]
mov ds:[si],al
inc si
loopw camellia_half_j1
retnd
endp
;-----------------------------------------

;-----------------------------------------
proc camellia_msb
;in: ds:si-src
;    es:di-trg
mov cx,4
camellia_msb_j1:
lodsd
xchg al,ah
rol eax,16
xchg al,ah
stosd
loopw camellia_msb_j1
retnd
endp
;-----------------------------------------

;-----------------------------------------
proc camellia_rot
;in: ds:si-src
;    es:di-trg
;    cx-bits to shift
mov ax,cx
shr ax,5
and cl,31
mov ch,al
call dword camellia_rot_j1
stosd
add ch,1
call dword camellia_rot_j1
stosd
retnd
camellia_rot_j1:
;y[0] = x[((n>>5)+0)&3];
;y[0] = x[((n>>5)+0)&3]<<r^x[((n>>5)+1)&3]>>(32-r);
movzx ebx,ch
and bl,3
shl bx,2
add bx,si
mov eax,ds:[bx]
or cl,cl
jz dword camellia_rot_j2
shl eax,cl
sub bx,si
add bl,4
and bl,0ch
add bx,si
mov edx,ds:[bx]
mov bl,cl
mov cl,32
sub cl,bl
shr edx,cl
or eax,edx
mov cl,bl
camellia_rot_j2:
retnd
endp
;-----------------------------------------




;-----------------------------------------
camellia_sbox1 db 070h,082h,02ch,0ech,0b3h,027h,0c0h,0e5h,0e4h,085h,057h
db 035h,0eah,00ch,0aeh,041h,023h,0efh,06bh,093h,045h,019h,0a5h,021h,0edh
db 00eh,04fh,04eh,01dh,065h,092h,0bdh,086h,0b8h,0afh,08fh,07ch,0ebh,01fh
db 0ceh,03eh,030h,0dch,05fh,05eh,0c5h,00bh,01ah,0a6h,0e1h,039h,0cah,0d5h
db 047h,05dh,03dh,0d9h,001h,05ah,0d6h,051h,056h,06ch,04dh,08bh,00dh,09ah
db 066h,0fbh,0cch,0b0h,02dh,074h,012h,02bh,020h,0f0h,0b1h,084h,099h,0dfh
db 04ch,0cbh,0c2h,034h,07eh,076h,005h,06dh,0b7h,0a9h,031h,0d1h,017h,004h
db 0d7h,014h,058h,03ah,061h,0deh,01bh,011h,01ch,032h,00fh,09ch,016h,053h
db 018h,0f2h,022h,0feh,044h,0cfh,0b2h,0c3h,0b5h,07ah,091h,024h,008h,0e8h
db 0a8h,060h,0fch,069h,050h,0aah,0d0h,0a0h,07dh,0a1h,089h,062h,097h,054h
db 05bh,01eh,095h,0e0h,0ffh,064h,0d2h,010h,0c4h,000h,048h,0a3h,0f7h,075h
db 0dbh,08ah,003h,0e6h,0dah,009h,03fh,0ddh,094h,087h,05ch,083h,002h,0cdh
db 04ah,090h,033h,073h,067h,0f6h,0f3h,09dh,07fh,0bfh,0e2h,052h,09bh,0d8h
db 026h,0c8h,037h,0c6h,03bh,081h,096h,06fh,04bh,013h,0beh,063h,02eh,0e9h
db 079h,0a7h,08ch,09fh,06eh,0bch,08eh,029h,0f5h,0f9h,0b6h,02fh,0fdh,0b4h
db 059h,078h,098h,006h,06ah,0e7h,046h,071h,0bah,0d4h,025h,0abh,042h,088h
db 0a2h,08dh,0fah,072h,007h,0b9h,055h,0f8h,0eeh,0ach,00ah,036h,049h,02ah
db 068h,03ch,038h,0f1h,0a4h,040h,028h,0d3h,07bh,0bbh,0c9h,043h,0c1h,015h
db 0e3h,0adh,0f4h,077h,0c7h,080h,09eh
;-----------------------------------------

;-----------------------------------------
camellia_sbox2 db 0e0h,005h,058h,0d9h,067h,04eh,081h,0cbh,0c9h,00bh,0aeh
db 06ah,0d5h,018h,05dh,082h,046h,0dfh,0d6h,027h,08ah,032h,04bh,042h,0dbh
db 01ch,09eh,09ch,03ah,0cah,025h,07bh,00dh,071h,05fh,01fh,0f8h,0d7h,03eh
db 09dh,07ch,060h,0b9h,0beh,0bch,08bh,016h,034h,04dh,0c3h,072h,095h,0abh
db 08eh,0bah,07ah,0b3h,002h,0b4h,0adh,0a2h,0ach,0d8h,09ah,017h,01ah,035h
db 0cch,0f7h,099h,061h,05ah,0e8h,024h,056h,040h,0e1h,063h,009h,033h,0bfh
db 098h,097h,085h,068h,0fch,0ech,00ah,0dah,06fh,053h,062h,0a3h,02eh,008h
db 0afh,028h,0b0h,074h,0c2h,0bdh,036h,022h,038h,064h,01eh,039h,02ch,0a6h
db 030h,0e5h,044h,0fdh,088h,09fh,065h,087h,06bh,0f4h,023h,048h,010h,0d1h
db 051h,0c0h,0f9h,0d2h,0a0h,055h,0a1h,041h,0fah,043h,013h,0c4h,02fh,0a8h
db 0b6h,03ch,02bh,0c1h,0ffh,0c8h,0a5h,020h,089h,000h,090h,047h,0efh,0eah
db 0b7h,015h,006h,0cdh,0b5h,012h,07eh,0bbh,029h,00fh,0b8h,007h,004h,09bh
db 094h,021h,066h,0e6h,0ceh,0edh,0e7h,03bh,0feh,07fh,0c5h,0a4h,037h,0b1h
db 04ch,091h,06eh,08dh,076h,003h,02dh,0deh,096h,026h,07dh,0c6h,05ch,0d3h
db 0f2h,04fh,019h,03fh,0dch,079h,01dh,052h,0ebh,0f3h,06dh,05eh,0fbh,069h
db 0b2h,0f0h,031h,00ch,0d4h,0cfh,08ch,0e2h,075h,0a9h,04ah,057h,084h,011h
db 045h,01bh,0f5h,0e4h,00eh,073h,0aah,0f1h,0ddh,059h,014h,06ch,092h,054h
db 0d0h,078h,070h,0e3h,049h,080h,050h,0a7h,0f6h,077h,093h,086h,083h,02ah
db 0c7h,05bh,0e9h,0eeh,08fh,001h,03dh
;-----------------------------------------

;-----------------------------------------
camellia_sbox3 db 038h,041h,016h,076h,0d9h,093h,060h,0f2h,072h,0c2h,0abh
db 09ah,075h,006h,057h,0a0h,091h,0f7h,0b5h,0c9h,0a2h,08ch,0d2h,090h,0f6h
db 007h,0a7h,027h,08eh,0b2h,049h,0deh,043h,05ch,0d7h,0c7h,03eh,0f5h,08fh
db 067h,01fh,018h,06eh,0afh,02fh,0e2h,085h,00dh,053h,0f0h,09ch,065h,0eah
db 0a3h,0aeh,09eh,0ech,080h,02dh,06bh,0a8h,02bh,036h,0a6h,0c5h,086h,04dh
db 033h,0fdh,066h,058h,096h,03ah,009h,095h,010h,078h,0d8h,042h,0cch,0efh
db 026h,0e5h,061h,01ah,03fh,03bh,082h,0b6h,0dbh,0d4h,098h,0e8h,08bh,002h
db 0ebh,00ah,02ch,01dh,0b0h,06fh,08dh,088h,00eh,019h,087h,04eh,00bh,0a9h
db 00ch,079h,011h,07fh,022h,0e7h,059h,0e1h,0dah,03dh,0c8h,012h,004h,074h
db 054h,030h,07eh,0b4h,028h,055h,068h,050h,0beh,0d0h,0c4h,031h,0cbh,02ah
db 0adh,00fh,0cah,070h,0ffh,032h,069h,008h,062h,000h,024h,0d1h,0fbh,0bah
db 0edh,045h,081h,073h,06dh,084h,09fh,0eeh,04ah,0c3h,02eh,0c1h,001h,0e6h
db 025h,048h,099h,0b9h,0b3h,07bh,0f9h,0ceh,0bfh,0dfh,071h,029h,0cdh,06ch
db 013h,064h,09bh,063h,09dh,0c0h,04bh,0b7h,0a5h,089h,05fh,0b1h,017h,0f4h
db 0bch,0d3h,046h,0cfh,037h,05eh,047h,094h,0fah,0fch,05bh,097h,0feh,05ah
db 0ach,03ch,04ch,003h,035h,0f3h,023h,0b8h,05dh,06ah,092h,0d5h,021h,044h
db 051h,0c6h,07dh,039h,083h,0dch,0aah,07ch,077h,056h,005h,01bh,0a4h,015h
db 034h,01eh,01ch,0f8h,052h,020h,014h,0e9h,0bdh,0ddh,0e4h,0a1h,0e0h,08ah
db 0f1h,0d6h,07ah,0bbh,0e3h,040h,04fh
;-----------------------------------------

;-----------------------------------------
camellia_sbox4 db 070h,02ch,0b3h,0c0h,0e4h,057h,0eah,0aeh,023h,06bh,045h
db 0a5h,0edh,04fh,01dh,092h,086h,0afh,07ch,01fh,03eh,0dch,05eh,00bh,0a6h
db 039h,0d5h,05dh,0d9h,05ah,051h,06ch,08bh,09ah,0fbh,0b0h,074h,02bh,0f0h
db 084h,0dfh,0cbh,034h,076h,06dh,0a9h,0d1h,004h,014h,03ah,0deh,011h,032h
db 09ch,053h,0f2h,0feh,0cfh,0c3h,07ah,024h,0e8h,060h,069h,0aah,0a0h,0a1h
db 062h,054h,01eh,0e0h,064h,010h,000h,0a3h,075h,08ah,0e6h,009h,0ddh,087h
db 083h,0cdh,090h,073h,0f6h,09dh,0bfh,052h,0d8h,0c8h,0c6h,081h,06fh,013h
db 063h,0e9h,0a7h,09fh,0bch,029h,0f9h,02fh,0b4h,078h,006h,0e7h,071h,0d4h
db 0abh,088h,08dh,072h,0b9h,0f8h,0ach,036h,02ah,03ch,0f1h,040h,0d3h,0bbh
db 043h,015h,0adh,077h,080h,082h,0ech,027h,0e5h,085h,035h,00ch,041h,0efh
db 093h,019h,021h,00eh,04eh,065h,0bdh,0b8h,08fh,0ebh,0ceh,030h,05fh,0c5h
db 01ah,0e1h,0cah,047h,03dh,001h,0d6h,056h,04dh,00dh,066h,0cch,02dh,012h
db 020h,0b1h,099h,04ch,0c2h,07eh,005h,0b7h,031h,017h,0d7h,058h,061h,01bh
db 01ch,00fh,016h,018h,022h,044h,0b2h,0b5h,091h,008h,0a8h,0fch,050h,0d0h
db 07dh,089h,097h,05bh,095h,0ffh,0d2h,0c4h,048h,0f7h,0dbh,003h,0dah,03fh
db 094h,05ch,002h,04ah,033h,067h,0f3h,07fh,0e2h,09bh,026h,037h,03bh,096h
db 04bh,0beh,02eh,079h,08ch,06eh,08eh,0f5h,0b6h,0fdh,059h,098h,06ah,046h
db 0bah,025h,042h,0a2h,0fah,007h,055h,0eeh,00ah,049h,068h,038h,0a4h,028h
db 07bh,0c9h,0c1h,0e3h,0f4h,0c7h,09eh
;-----------------------------------------

;-----------------------------------------
camellia_sigma db 0a0h,09eh,066h,07fh,03bh,0cch,090h,08bh,0b6h,07ah,0e8h
db 058h,04ch,0aah,073h,0b2h,0c6h,0efh,037h,02fh,0e9h,04fh,082h,0beh,054h
db 0ffh,053h,0a5h,0f1h,0d3h,06fh,01ch,010h,0e5h,027h,0fah,0deh,068h,02dh
db 01dh,0b0h,056h,088h,0c2h,0b3h,0e6h,0c1h,0fdh
;-----------------------------------------

;-----------------------------------------
camellia_ksft1 db 0,64,0,64,15,79,15,79,30,94,45,109,45,124,60,124,77,13
db 94,30,94,30,111,47,111,47
;-----------------------------------------

;-----------------------------------------
camellia_kidx1 db 0,0,4,4,0,0,4,4,4,4,0,0,4,0,4,4,0,0,0,0,4,4,0,0,4,4
;-----------------------------------------

;-----------------------------------------
camellia_ksft2 db 0,64,0,64,15,79,15,79,30,94,30,94,45,109,45,109,60,124
db 60,124,60,124,77,13,77,13,94,30,94,30,111,47,111,47
;-----------------------------------------

;-----------------------------------------
camellia_kidx2 db 0,0,12,12,8,8,4,4,8,8,12,12,0,0,4,4,0,0,8,8,12,12,0,0
db 4,4,8,8,4,4,0,0,12,12
;-----------------------------------------

;rec:
;key dd 128 dup (?)      0000h
;len dd ?                0200h
