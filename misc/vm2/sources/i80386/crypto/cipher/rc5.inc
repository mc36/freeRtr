RC5_size equ 8
RC5_param equ 16

;-----------------------------------------
proc RC5_init
;in: ds:bx-record...
;    ds:si-key text...
;    cx-size of key...
;    dx-rounds...
mov ds:[DataBlock_temp30],bx
mov ds:[DataBlock_temp31],si
;key->rounds=rounds;
mov ds:[bx+88h],dx
;c=(len+3)/4;
;t=(rounds+1)*2;
inc dx
add dx,dx
mov ds:[DataBlock_temp33],dx
shr cx,2
mov ds:[DataBlock_temp32],cx
mov di,bx
;S[0]=RC5_32_P;
mov eax,0b7e15163h
mov ds:[di],eax
;for (i=1; i<t; i++) S[i]=(S[i-1]+RC5_32_Q)&RC5_32_MASK;
lea cx,ds:[edx-1]
RC5_init_j1:
add di,4
add eax,09e3779b9h
mov ds:[di],eax
loopw RC5_init_j1

;j=(t>c)?t:c;
mov ax,ds:[DataBlock_temp33]
mov cx,ds:[DataBlock_temp32]
cmp ax,cx
ja dword RC5_init_j2
mov ax,cx
RC5_init_j2:
;j*=3;
imul ax,3
mov ds:[DataBlock_temp39],ax
;ii=jj=0;
sub eax,eax
mov ds:[DataBlock_temp36],ax
mov ds:[DataBlock_temp37],ax
;A=B=0;
mov ds:[DataBlock_temp34],eax
mov ds:[DataBlock_temp35],eax
;for (i=0; i<j; i++)  {
mov ds:[DataBlock_temp38],ax
RC5_init_j3:
;  k=S[ii]+A+B;
mov si,ds:[DataBlock_temp36]
shl si,2
add si,ds:[DataBlock_temp30]
mov eax,ds:[si]
add eax,ds:[DataBlock_temp34]
add eax,ds:[DataBlock_temp35]
;  A=S[ii]=ROTATE_l32(k,3);
rol eax,3
mov ds:[si],eax
mov ds:[DataBlock_temp34],eax
;  m=(int)(A+B);
add eax,ds:[DataBlock_temp35]
mov cl,al
and cl,1fh
;  k=L[jj]+A+B;
mov si,ds:[DataBlock_temp37]
shl si,2
add si,ds:[DataBlock_temp31]
mov eax,ds:[si]
add eax,ds:[DataBlock_temp34]
add eax,ds:[DataBlock_temp35]
;  B=L[jj]=ROTATE_l32(k,m);
rol eax,cl
mov ds:[si],eax
mov ds:[DataBlock_temp35],eax
;  if (++ii >= t) ii=0;
mov ax,ds:[DataBlock_temp36]
inc ax
cmp ax,ds:[DataBlock_temp33]
jb dword RC5_init_j4
sub ax,ax
RC5_init_j4:
mov ds:[DataBlock_temp36],ax
;  if (++jj >= c) jj=0;
mov ax,ds:[DataBlock_temp37]
inc ax
cmp ax,ds:[DataBlock_temp32]
jb dword RC5_init_j5
sub ax,ax
RC5_init_j5:
mov ds:[DataBlock_temp37],ax
;  }
mov ax,ds:[DataBlock_temp38]
inc ax
mov ds:[DataBlock_temp38],ax
cmp ax,ds:[DataBlock_temp39]
jb dword RC5_init_j3
retnd
endp
;-----------------------------------------

;-----------------------------------------
proc RC5_encrypt
;in: ds:bx-record...
;    edi-a...
;    ebp-b...
cld
mov si,bx
;a=d[0]+s[0];
lodsd
add edi,eax
;b=d[1]+s[1];
lodsd
add ebp,eax
mov dx,ds:[bx+88h]
dec dx
RC5_encrypt_j1:
;a^=b;
xor edi,ebp
;a=ROTATE_l32(a,b);
mov cx,bp
and cl,1fh
rol edi,cl
;a+=s[n];
lodsd
add edi,eax
;a&=RC5_32_MASK;
;b^=a;
xor ebp,edi
;b=ROTATE_l32(b,a);
mov cx,di
and cl,1fh
rol ebp,cl
;b+=s[n+1];
lodsd
add ebp,eax
;b&=RC5_32_MASK;
dec dx
jns dword RC5_encrypt_j1
retnd
endp
;-----------------------------------------

;-----------------------------------------
proc RC5_decrypt
;in: ds:bx-record...
;    edi-a...
;    ebp-b...
std
mov dx,ds:[bx+88h]
dec dx
lea si,ds:[ebx+edx*8+0ch]
RC5_decrypt_j1:
;b-=s[n+1];
lodsd
sub ebp,eax
;b&=RC5_32_MASK;
;b=ROTATE_r32(b,a);
mov cx,di
and cl,1fh
ror ebp,cl
;b^=a;
xor ebp,edi
;a-=s[n];
lodsd
sub edi,eax
;a&=RC5_32_MASK;
;a=ROTATE_r32(a,b);
mov cx,bp
and cl,1fh
ror edi,cl
;a^=b;
xor edi,ebp
dec dx
jns dword RC5_decrypt_j1
;d[1]=b-s[1];
lodsd
sub ebp,eax
;d[0]=a-s[0];
lodsd
sub edi,eax
cld
retnd
endp
;-----------------------------------------


;rec:
;data dd 34 dup (?)         0000h
;rounds dw ?                0088h
