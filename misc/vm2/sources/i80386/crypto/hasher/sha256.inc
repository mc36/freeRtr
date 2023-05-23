SHA256_size equ 32

;-----------------------------------------
proc SHA256_init
;in: ds:bp-record...
mov di,bp
mov eax,06a09e667h
stosd
mov eax,0bb67ae85h
stosd
mov eax,03c6ef372h
stosd
mov eax,0a54ff53ah
stosd
mov eax,0510e527fh
stosd
mov eax,09b05688ch
stosd
mov eax,01f83d9abh
stosd
mov eax,05be0cd19h
stosd
sub eax,eax
mov cx,32
rep
  stosw
stosd
stosb
retnd
endp
;-----------------------------------------

;-----------------------------------------
proc SHA256_addChar
;in: ds:bp-record...
;    al-char
mov si,bp
mov bl,ds:[si+84h]
mov bh,0
mov ds:[si+40h+bx],al
inc bx
mov ds:[si+84h],bl
sub eax,eax
mov al,8
add ds:[si+80h],eax
cmp bl,40h
jb dword SHA256_addChar_j1

;a:=CurrentHash[0]; b:=CurrentHash[1]; c:=CurrentHash[2]; d:=CurrentHash[3];
;e:=CurrentHash[4]; f:=CurrentHash[5]; g:=CurrentHash[6]; h:=CurrentHash[7];
mov si,bp
mov di,DataBlock_temp1
mov cx,32
rep
  movsb
;for i:=0 to 15 do W[i]:=SwapDWord(W[i]);
mov cx,16
lea si,ds:[bp+40h]
mov di,DataBlock_temp2
SHA256_addChar_j2:
lodsd
xchg al,ah
rol eax,16
xchg al,ah
stosd
loopw SHA256_addChar_j2

;for i:=16 to 63 do
mov cx,48
SHA256_addChar_j3:
; W[i]:=(((W[i-2] shr 17) or (W[i-2] shl 15)) xor
;       ((W[i-2] shr 19) or (W[i-2] shl 13)) xor
;       (W[i-2] shr 10))
mov eax,ds:[di-8]
mov edx,eax
mov esi,eax
rol eax,15
rol edx,13
shr esi,10
xor eax,edx
xor esi,eax
;     + (((W[i-15] shr 7) or (W[i-15] shl 25)) xor
;       ((W[i-15] shr 18) or (W[i-15] shl 14)) xor
;       (W[i-15] shr 3))
mov eax,ds:[di-60]
mov edx,eax
mov ebx,eax
rol eax,25
rol edx,14
shr ebx,3
xor eax,edx
xor eax,ebx
add esi,eax
;     + W[i-7]
;     + W[i-16];
add esi,ds:[di-28]
add esi,ds:[di-64]
mov eax,esi
stosd
loopw SHA256_addChar_j3

;for i:=0 to 63 do begin
sub eax,eax
mov ds:[DataBlock_temp30],eax
SHA256_addChar_j4:
; t1:= (((e shr 6) or (e shl 26)) xor
;      ((e shr 11) or (e shl 21)) xor
;      ((e shr 25) or (e shl 7)))
;      + ((e and f) xor (not e and g)) + h + K[i] + W[i] ;
mov di,DataBlock_temp1
mov eax,ds:[di+16]
mov esi,eax
mov edx,eax
rol eax,26
rol esi,21
rol edx,7
xor eax,edx
xor esi,eax
mov eax,ds:[di+16]
and eax,ds:[di+20]
mov edx,ds:[di+16]
not edx
and edx,ds:[di+24]
xor eax,edx
add esi,eax
add esi,ds:[di+28]
mov edi,ds:[DataBlock_temp30]
shl edi,2
add esi,cs:[SHA256_ks+edi]
add esi,ds:[DataBlock_temp2+di]

; t2:= (((a shr 2) or (a shl 30)) xor
;      ((a shr 13) or (a shl 19)) xor
;      ((a shr 22) xor (a shl 10))) +
;      + ((a and b) xor (a and c) xor (b and c));
mov di,DataBlock_temp1
mov eax,ds:[di+0]
mov ebx,eax
mov edx,eax
rol eax,30
rol ebx,19
rol edx,10
xor ebx,eax
xor ebx,edx
mov ecx,ds:[di+0]
and ecx,ds:[di+4]
mov eax,ds:[di+0]
and eax,ds:[di+8]
xor ecx,eax
mov eax,ds:[di+4]
and eax,ds:[di+8]
xor ecx,eax
add ebx,ecx

mov di,DataBlock_temp1
;  h:=g;
mov eax,ds:[di+24]
mov ds:[di+28],eax
;  g:=f;
mov eax,ds:[di+20]
mov ds:[di+24],eax
;  f:=e;
mov eax,ds:[di+16]
mov ds:[di+20],eax
;  e:=d + t1;
mov eax,ds:[di+12]
add eax,esi
mov ds:[di+16],eax
;  d:=c;
mov eax,ds:[di+8]
mov ds:[di+12],eax
;  c:=b;
mov eax,ds:[di+4]
mov ds:[di+8],eax
;  b:=a;
mov eax,ds:[di+0]
mov ds:[di+4],eax
;  a:=t1 + t2;
add ebx,esi
mov ds:[di+0],ebx

mov eax,ds:[DataBlock_temp30]
inc eax
mov ds:[DataBlock_temp30],eax
cmp eax,64
jb dword SHA256_addChar_j4

; CurrentHash[0]:=CurrentHash[0] + a; CurrentHash[1]:=CurrentHash[1] + b;
; CurrentHash[2]:=CurrentHash[2] + c; CurrentHash[3]:=CurrentHash[3] + d;
; CurrentHash[4]:=CurrentHash[4] + e; CurrentHash[5]:=CurrentHash[5] + f;
; CurrentHash[6]:=CurrentHash[6] + g; CurrentHash[7]:=CurrentHash[7] + h;

mov si,bp
mov di,DataBlock_temp1
mov cx,8
SHA256_addChar_j5:
mov eax,ds:[si]
add eax,ds:[di]
mov ds:[si],eax
add si,4
add di,4
loopw SHA256_addChar_j5

sub ax,ax
mov ds:[bp+84h],al
SHA256_addChar_j1:
retnd
endp
;-----------------------------------------

;-----------------------------------------
proc SHA256_finish
;in: ds:bp-record...
push dword ds:[bp+80h]
mov al,80h
call dword SHA256_addChar
SHA256_finish_j1:
mov al,ds:[bp+84h]
cmp al,56
je dword SHA256_finish_j2
mov al,0
call dword SHA256_addChar
jmp dword SHA256_finish_j1
SHA256_finish_j2:
mov cx,4
SHA256_finish_j3:
push cx
sub ax,ax
call dword SHA256_addChar
pop cx
loopw SHA256_finish_j3
pop eax
mov cx,4
SHA256_finish_j4:
rol eax,8
push cx
push eax
call dword SHA256_addChar
pop eax
pop cx
loopw SHA256_finish_j4
mov si,bp
mov di,bp
mov cx,8
SHA256_finish_j5:
lodsd
xchg al,ah
rol eax,16
xchg al,ah
stosd
loopw SHA256_finish_j5
retnd
endp
;-----------------------------------------

;-----------------------------------------
SHA256_ks:
dd 0428a2f98h,071374491h,0b5c0fbcfh,0e9b5dba5h
dd 03956c25bh,059f111f1h,0923f82a4h,0ab1c5ed5h
dd 0d807aa98h,012835b01h,0243185beh,0550c7dc3h
dd 072be5d74h,080deb1feh,09bdc06a7h,0c19bf174h
dd 0e49b69c1h,0efbe4786h,00fc19dc6h,0240ca1cch
dd 02de92c6fh,04a7484aah,05cb0a9dch,076f988dah
dd 0983e5152h,0a831c66dh,0b00327c8h,0bf597fc7h
dd 0c6e00bf3h,0d5a79147h,006ca6351h,014292967h
dd 027b70a85h,02e1b2138h,04d2c6dfch,053380d13h
dd 0650a7354h,0766a0abbh,081c2c92eh,092722c85h
dd 0a2bfe8a1h,0a81a664bh,0c24b8b70h,0c76c51a3h
dd 0d192e819h,0d6990624h,0f40e3585h,0106aa070h
dd 019a4c116h,01e376c08h,02748774ch,034b0bcb5h
dd 0391c0cb3h,04ed8aa4ah,05b9cca4fh,0682e6ff3h
dd 0748f82eeh,078a5636fh,084c87814h,08cc70208h
dd 090befffah,0a4506cebh,0bef9a3f7h,0c67178f2h
;-----------------------------------------

;rec:
;state dd 64 dup (?)         00h
;bufdat db 64 dup (?)        40h
;count dd ?                  80h
;buflen db ?                 84h
