idea_size equ 8
idea_param equ 0



;-----------------------------------------
proc idea_Init
;in: ds:bx-record...
;    ds:si-key text...
;    cx-size of key...
;    dx-number of rounds...
movzx ebp,bx
add ebp,100h
mov edi,DataBlock_temp1
rep
  movsb ptr16
sub eax,eax
mov cx,16
rep
  stosb ptr16

;for i:= 0 to 7 do EK[i]:= (EK[i] shl 8) or (EK[i] shr 8);
mov esi,DataBlock_temp1
mov edi,ebp
mov cx,8
idea_Init_j1:
lodsw
xchg al,ah
stosw
loopw idea_Init_j1

;for i:= 1 to 5 do begin
sub edi,edi
sub esi,esi
inc edi
idea_Init_j2:

;(EK[((i-1)*8)+1] shl 9)
movzx ebx,di
dec ebx
shl ebx,3
mov ecx,esi
inc ecx
and cl,7
add ebx,ecx
add ebx,ebx
add ebx,ebp
mov ax,ds:[bx]
shl eax,9

;(EK[((i-1)*8)+2] shr 7);
movzx ebx,di
dec ebx
shl ebx,3
mov ecx,esi
inc ecx
inc ecx
and cl,7
add ebx,ecx
add ebx,ebx
add ebx,ebp
mov cx,ds:[bx]
shr ecx,7
or ax,cx

;EK[(i*8)+0]:= 
movzx ebx,di
shl ebx,3
add ebx,esi
add ebx,ebx
add ebx,ebp
mov ds:[bx],ax

inc esi
shl esi,13
rol esi,16
add di,si
shr esi,29
cmp di,6
jbe dword idea_Init_j2

;for i:= 0 to 7 do begin
mov esi,ebp
mov edi,154h
add edi,ebp
mov ecx,8
idea_Init_j3:

;  DK[42-i*6]:= MulInv(EK[i*6+6]);
mov ax,ds:[si+12]
call dword idea_invMul
mov ds:[di+0],ax

;  DK[43-i*6]:= -EK[i*6+8];
mov ax,ds:[si+16]
neg ax
mov ds:[di+2],ax

;  DK[44-i*6]:= -EK[i*6+7];
mov ax,ds:[si+14]
neg ax
mov ds:[di+4],ax

;  DK[45-i*6]:= MulInv(EK[i*6+9]);
mov ax,ds:[si+18]
call dword idea_invMul
mov ds:[di+6],ax

;  DK[46-i*6]:= EK[i*6+4];
mov ax,ds:[si+8]
mov ds:[di+8],ax

;  DK[47-i*6]:= EK[i*6+5];
mov ax,ds:[si+10]
mov ds:[di+10],ax

add si,12
sub di,12
loopw idea_Init_j3

mov esi,ebp
mov edi,100h
add edi,ebp

;DK[48]:= MulInv(EK[0]);
mov ax,ds:[si+0]
call dword idea_invMul
mov ds:[di+96],ax

;DK[49]:= -EK[1];
mov ax,ds:[si+2]
neg ax
mov ds:[di+98],ax

;DK[50]:= -EK[2];
mov ax,ds:[si+4]
neg ax
mov ds:[di+100],ax

;DK[51]:= MulInv(EK[3]);
mov ax,ds:[si+6]
call dword idea_invMul
mov ds:[di+102],ax

;DK[0]:= MulInv(EK[48]);
mov ax,ds:[si+96]
call dword idea_invMul
mov ds:[di+0],ax

;DK[1]:= -EK[49];
mov ax,ds:[si+98]
neg ax
mov ds:[di+2],ax

;DK[2]:= -EK[50];
mov ax,ds:[si+100]
neg ax
mov ds:[di+4],ax

;DK[3]:= MulInv(EK[51]);
mov ax,ds:[si+102]
call dword idea_invMul
mov ds:[di+6],ax

;DK[4]:= EK[46];
mov ax,ds:[si+92]
mov ds:[di+8],ax

;DK[5]:= EK[47];
mov ax,ds:[si+94]
mov ds:[di+10],ax

retnd
endp
;-----------------------------------------



;-----------------------------------------
proc idea_Encrypt
;in: ds:bx-record...
;    edi-xl...
;    ebp-xr...

;for i:= 1 to 4 do x[i]:= (x[i] shl 8) or (x[i] shr 8);
mov eax,edi
xchg al,ah
rol eax,16
xchg al,ah
rol eax,16
mov esi,eax
mov eax,ebp
xchg al,ah
rol eax,16
xchg al,ah
rol eax,16
mov edi,eax
movzx ebp,bx

;for i:= 0 to 7 do begin;
sub ebx,ebx
add ebp,100h
idea_Encrypt_j1:

;  Mul(x[1],EK[(i*6)+0]);
mov ax,si
mov dx,ds:[bp+0]
call dword idea_mul
mov si,ax

;  Inc(x[2],EK[(i*6)+1]);
rol esi,16
add si,ds:[bp+2]
rol esi,16

;  Inc(x[3],EK[(i*6)+2]);
add di,ds:[bp+4]

;  Mul(x[4],EK[(i*6)+3]);
rol edi,16
mov ax,di
mov dx,ds:[bp+6]
call dword idea_mul
mov di,ax
rol edi,16

;  s3:= x[3];
push di

;  x[3]:= x[3] xor x[1];
xor di,si

;  Mul(x[3],EK[(i*6)+4]);
mov ax,di
mov dx,ds:[bp+8]
call dword idea_mul
mov di,ax

;  s2:= x[2];
rol esi,16
push si

;  x[2]:= x[2] xor x[4];
rol edi,16
xor si,di
rol edi,16

;  Inc(x[2],x[3]);
add si,di

;  Mul(x[2],EK[(i*6)+5]);
mov ax,si
mov dx,ds:[bp+10]
call dword idea_mul
mov si,ax

;  Inc(x[3],x[2]);
add di,si

;  x[1]:= x[1] xor x[2];
mov ax,si
rol esi,16
xor si,ax

;  x[4]:= x[4] xor x[3];
mov ax,di
shl eax,16
xor edi,eax

;  x[3]:= x[3] xor s2;
pop ax
xor di,ax

;  x[2]:= x[2] xor s3;
pop ax
shl eax,16
xor esi,eax

inc ebx
add bp,12
cmp bx,8
jb dword idea_Encrypt_j1

;Mul(x[1],EK[48]);
mov ax,si
mov dx,ds:[bp+0]
call dword idea_mul
mov si,ax

;Inc(x[3],EK[49]);
add di,ds:[bp+2]

;Inc(x[2],EK[50]);
rol esi,16
add si,ds:[bp+4]
rol esi,16

;Mul(x[4],EK[51]);
rol edi,16
mov ax,di
mov dx,ds:[bp+6]
call dword idea_mul
mov di,ax
rol edi,16

;x[2]:= (x[3] shl 8) or (x[3] shr 8);
mov ax,di
xchg al,ah
shl eax,16

;x[1]:= (x[1] shl 8) or (x[1] shr 8);
mov ax,si
xchg al,ah

;x[4]:= (x[4] shl 8) or (x[4] shr 8);
mov ecx,edi
shr ecx,16
xchg cl,ch
shl ecx,16

;x[3]:= (x[2] shl 8) or (x[2] shr 8);
shr esi,16
mov cx,si
xchg cl,ch

mov edi,eax
mov ebp,ecx

retnd
endp
;-----------------------------------------



;-----------------------------------------
proc idea_Decrypt
;in: ds:bx-record...
;    edi-xl...
;    ebp-xr...

;for i:= 1 to 4 do x[i]:= (x[i] shl 8) or (x[i] shr 8);
mov eax,edi
xchg al,ah
rol eax,16
xchg al,ah
rol eax,16
mov esi,eax
mov eax,ebp
xchg al,ah
rol eax,16
xchg al,ah
rol eax,16
mov edi,eax
movzx ebp,bx

;for i:= 0 to 7 do begin
sub ebx,ebx
add ebp,200h
idea_Decrypt_j1:

;  Mul(x[1],DK[(i*6)+0]);
mov ax,si
mov dx,ds:[bp+0]
call dword idea_mul
mov si,ax

;  Inc(x[2],DK[(i*6)+1]);
rol esi,16
add si,ds:[bp+2]
rol esi,16

;  Inc(x[3],DK[(i*6)+2]);
add di,ds:[bp+4]

;  Mul(x[4],DK[(i*6)+3]);
rol edi,16
mov ax,di
mov dx,ds:[bp+6]
call dword idea_mul
mov di,ax
rol edi,16

;  s3:= x[3];
push di

;  x[3]:= x[3] xor x[1];
xor di,si

;  Mul(x[3],DK[(i*6)+4]);
mov ax,di
mov dx,ds:[bp+8]
call dword idea_mul
mov di,ax

;  s2:= x[2];
rol esi,16
push si

;  x[2]:= x[2] xor x[4];
rol edi,16
xor si,di
rol edi,16

;  Inc(x[2],x[3]);
add si,di

;  Mul(x[2],DK[(i*6)+5]);
mov ax,si
mov dx,ds:[bp+10]
call dword idea_mul
mov si,ax

;  Inc(x[3],x[2]);
add di,si

;  x[1]:= x[1] xor x[2];
mov ax,si
rol esi,16
xor si,ax

;  x[4]:= x[4] xor x[3];
mov ax,di
rol edi,16
xor di,ax
rol edi,16

;  x[3]:= x[3] xor s2;
pop ax
xor di,ax

;  x[2]:= x[2] xor s3;
pop ax
shl eax,16
xor esi,eax

inc ebx
add bp,12
cmp bx,8
jb dword idea_Decrypt_j1

;Mul(x[1],DK[48]);
mov ax,si
mov dx,ds:[bp+0]
call dword idea_mul
mov si,ax

;Inc(x[3],DK[49]);
add di,ds:[bp+2]

;Inc(x[2],DK[50]);
rol esi,16
add si,ds:[bp+4]
rol esi,16

;Mul(x[4],DK[51]);
rol edi,16
mov ax,di
mov dx,ds:[bp+6]
call dword idea_mul
mov di,ax
rol edi,16

;x[2]:= (x[3] shl 8) or (x[3] shr 8);
mov ax,di
xchg al,ah
shl eax,16

;x[1]:= (x[1] shl 8) or (x[1] shr 8);
mov ax,si
xchg al,ah

;x[4]:= (x[4] shl 8) or (x[4] shr 8);
mov ecx,edi
shr ecx,16
xchg cl,ch
shl ecx,16

;x[3]:= (x[2] shl 8) or (x[2] shr 8);
shr esi,16
mov cx,si
xchg cl,ch

mov edi,eax
mov ebp,ecx

retnd
endp
;-----------------------------------------



;-----------------------------------------
proc idea_invMul
;in:  ax-x...
;out: eax-inv(x)...
push ecx
push edx
push ebx
push esi
push edi
push ebp
movzx ebx,ax
;if x<= 1 then begin; MulInv:=x; Exit; end;
cmp ax,1
jbe dword idea_invMul_vege

;t1:= DWord($10001) div x;
;y:= DWord($10001) mod x;
mov eax,10001h
sub edx,edx
div ebx
movzx edi,ax
movzx ecx,dx

;if y= 1 then begin;
cmp cx,1
jne dword  idea_invMul_j1
;  MulInv:= (1 - t1) and $FFFF;exit; end;
mov bx,1
sub bx,di
jmp dword idea_invMul_vege
idea_invMul_j1:

;t0:= 1;
mov esi,1

;repeat
idea_invMul_j2:
;  q:= x div y;
;  x:= x mod y;
movzx eax,bx
sub edx,edx
div ecx
movzx ebp,ax
movzx ebx,dx

;  inc(t0 , q*t1);
imul ax,di
add si,ax

;  if x= 1 then begin
cmp bx,1
jne dword idea_invMul_j3
;    MulInv:= t0;Exit; end;
mov bx,si
jmp dword idea_invMul_vege
idea_invMul_j3:

;  q:= y div x;
;  y:= y mod x;
movzx eax,cx
sub edx,edx
div ebx
movzx ebp,ax
movzx ecx,dx

;  inc(t1 , q*t0);
imul ax,si
add di,ax

;  until y= 1;
cmp cx,1
jne dword idea_invMul_j2

;MulInv:= (1-t1) and $FFFF;
mov bx,1
sub bx,di

idea_invMul_vege:
movzx eax,bx
pop ebp
pop edi
pop esi
pop ebx
pop edx
pop ecx
retnd
endp
;-----------------------------------------



;-----------------------------------------
proc idea_mul
;in:  ax-x...
;     dx-y...
;out: eax-x...
push edx
push ecx
movzx eax,ax
movzx edx,dx
;p:= DWord(x)*y;
mov ecx,eax
imul ecx,edx
;if p= 0 then
or ecx,ecx
jnz dword idea_mul_j1
;x:= 1 - x - y
mov ecx,1
sub ecx,eax
sub ecx,edx
movzx eax,cx
jmp dword idea_mul_vege
idea_mul_j1:
;x:= p shr 16;
mov edx,ecx
shr edx,16
;t16:= p and $FFFF;
movzx ecx,cx
;x:= t16 - x;
movzx eax,cx
sub ax,dx
;if (t16 < x) then Inc(x);
cmp cx,ax
jnb dword idea_mul_vege
inc ax
idea_mul_vege:
pop ecx
pop edx
movzx eax,ax
retnd
endp
;-----------------------------------------



;rec:
;ke dw 64 dup (?)        0100h
;kd dw 64 dup (?)        0100h
