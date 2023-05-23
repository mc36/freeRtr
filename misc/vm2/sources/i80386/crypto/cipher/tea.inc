tea_size equ 8
tea_param equ 32
tea_delta equ 9e3779b9h

;-----------------------------------------
proc tea_Init
;in: ds:bx-record...
;    ds:si-key text...
;    cx-size of key...
;    dx-number of rounds...
movzx edx,dx
mov ds:[bx+80h],edx
mov edi,DataBlock_temp1
rep
  movsb ptr16
sub eax,eax
mov cx,16
rep
  stosb ptr16
mov esi,DataBlock_temp1
mov cx,4
tea_Init_j1:
mov eax,ds:[si]
xchg al,ah
rol eax,16
xchg al,ah
mov ds:[bx],eax
add si,4
add bx,4
loopw tea_Init_j1
retnd
endp
;-----------------------------------------



;-----------------------------------------
proc tea_Encrypt
;in: ds:bx-record...
;    edi-xl...
;    ebp-xr...

;x:= SwapDWord(pdword(@InData)^);
;y:= SwapDWord(pdword(longword(@InData)+4)^);
mov eax,edi
xchg al,ah
rol eax,16
xchg al,ah
mov esi,eax
mov eax,ebp
xchg al,ah
rol eax,16
xchg al,ah
mov edi,eax
movzx ebp,bx

;for n:= 1 to Rounds do begin
mov cx,ds:[bp+80h]
;sum:= 0; 
sub edx,edx
tea_Encrypt_j1:

;Inc(sum,Delta);
add edx,tea_delta

;inc(x,y shl 4);
mov eax,edi
shl eax,4
add esi,eax

;inc(x,a xor y);
mov eax,ds:[bp+0]
xor eax,edi
add esi,eax

;inc(x,(y shr 5) xor sum);
mov eax,edi
shr eax,5
xor eax,edx
add esi,eax

;inc(x,b);
add esi,ds:[bp+4]

;inc(y,x shl 4);
mov eax,esi
shl eax,4
add edi,eax

;inc(y,c xor x);
mov eax,ds:[bp+8]
xor eax,esi
add edi,eax

;inc(y,(x shr 5) xor sum);
mov eax,esi
shr eax,5
xor eax,edx
add edi,eax

;inc(y,d);
add edi,ds:[bp+12]

loopw tea_Encrypt_j1

;pdword(@OutData)^:= SwapDWord(x);
;pdword(longword(@OutData)+4)^:= SwapDWord(y);
mov eax,edi
xchg al,ah
rol eax,16
xchg al,ah
mov ebp,eax
mov eax,esi
xchg al,ah
rol eax,16
xchg al,ah
mov edi,eax

retnd
endp
;-----------------------------------------



;-----------------------------------------
proc tea_Decrypt
;in: ds:bx-record...
;    edi-xl...
;    ebp-xr...

;x:= SwapDWord(pdword(@InData)^);
;y:= SwapDWord(pdword(longword(@InData)+4)^);
mov eax,edi
xchg al,ah
rol eax,16
xchg al,ah
mov esi,eax
mov eax,ebp
xchg al,ah
rol eax,16
xchg al,ah
mov edi,eax
movzx ebp,bx

;for n:= 1 to Rounds do begin
mov ecx,ds:[bp+80h]
;sum:= Delta shl 5; 
mov edx,tea_delta
imul edx,ecx
tea_Decrypt_j1:

;Dec(y,x shl 4);
mov eax,esi
shl eax,4
sub edi,eax

;Dec(y,c xor x);
mov eax,ds:[bp+8]
xor eax,esi
sub edi,eax

;Dec(y,(x shr 5) xor sum);
mov eax,esi
shr eax,5
xor eax,edx
sub edi,eax

;Dec(y,d);
sub edi,ds:[bp+12]


;Dec(x,y shl 4);
mov eax,edi
shl eax,4
sub esi,eax

;Dec(x,a xor y);
mov eax,ds:[bp+0]
xor eax,edi
sub esi,eax

;Dec(x,y shr 5) xor sum)
mov eax,edi
shr eax,5
xor eax,edx
sub esi,eax

;Dec(x,b);
sub esi,ds:[bp+4]

;Dec(sum,Delta);
sub edx,tea_delta

loopw tea_Decrypt_j1

;pdword(@OutData)^:= SwapDWord(x);
;pdword(longword(@OutData)+4)^:= SwapDWord(y);
mov eax,edi
xchg al,ah
rol eax,16
xchg al,ah
mov ebp,eax
mov eax,esi
xchg al,ah
rol eax,16
xchg al,ah
mov edi,eax

retnd
endp
;-----------------------------------------





;rec:
;key dd 16 dup (?)       0000h
;rounds dd ?             0080h
