SHA1_size equ 20

;-----------------------------------------
proc SHA1_init
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
mov eax,0c3d2e1f0h
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
proc SHA1_addChar
;in: ds:bp-record...
;    al-char
cld
mov si,bp
mov bl,ds:[si+58h]
mov bh,0
mov ds:[si+14h+bx],al
inc bx
mov ds:[si+58h],bl
sub eax,eax
mov al,8
add ds:[si+54h],eax
cmp bl,40h
jb dword SHA1_addChar_j1
;-----------------------------------------
;for(t = 0; t < 16; t++)
mov ds:[dataBlock_temp1],bp
lea si,ds:[bp+14h]
mov di,dataBlock_temp2
mov cx,16
SHA1_addChar_j2:
;W[t] =  context->Message_Block[t * 4 + 0] << 24;
;W[t] |= context->Message_Block[t * 4 + 1] << 16;
;W[t] |= context->Message_Block[t * 4 + 2] << 8;
;W[t] |= context->Message_Block[t * 4 + 3] << 0;
lodsd
xchg al,ah
rol eax,16
xchg al,ah
stosd
loopw SHA1_addChar_j2
;for(t = 16; t < 80; t++)
mov cx,64
SHA1_addChar_j3:
;W[t] = SHA1CircularShift(1,W[t-3] ^ W[t-8] ^ W[t-14] ^ W[t-16]);
mov eax,ds:[di-12]
xor eax,ds:[di-32]
xor eax,ds:[di-56]
xor eax,ds:[di-64]
rol eax,1
stosd
loopw SHA1_addChar_j3
;A = context->Intermediate_Hash[0];
;B = context->Intermediate_Hash[1];
;C = context->Intermediate_Hash[2];
;D = context->Intermediate_Hash[3];
;E = context->Intermediate_Hash[4];
mov ecx,ds:[bp+16]     ;e
mov edx,ds:[bp+12]     ;d
mov edi,ds:[bp+8]      ;c
mov esi,ds:[bp+4]      ;b
mov ebp,ds:[bp+0]      ;a
sub bx,bx              ;t
;for(t = 0; t < 20; t++)
SHA1_addChar_j4:
;temp=((B & C) | ((~B) & D)) + SHA1CircularShift(5,A) + E +W[t]+K[0];
push bx
mov eax,esi
and eax,edi
mov ebx,esi
not ebx
and ebx,edx
or eax,ebx
mov ebx,ebp
rol ebx,5
add eax,ebx
add eax,ecx
pop bx
add eax,ds:[dataBlock_temp2+bx]
add eax,05a827999h
;E = D;
mov ecx,edx
;D = C;
mov edx,edi
;C = SHA1CircularShift(30,B);
mov edi,esi
rol edi,30
;B = A;
mov esi,ebp
;A = temp;
mov ebp,eax
add bx,4
cmp bx,80
jb dword SHA1_addChar_j4
;for(t = 20; t < 40; t++)
SHA1_addChar_j5:
;temp=(B ^ C ^ D) + SHA1CircularShift(5,A) + E + W[t]+K[1];
push bx
mov eax,esi
xor eax,edi
xor eax,edx
mov ebx,ebp
rol ebx,5
add eax,ebx
add eax,ecx
pop bx
add eax,ds:[dataBlock_temp2+bx]
add eax,06ed9eba1h
;E = D;
mov ecx,edx
;D = C;
mov edx,edi
;C = SHA1CircularShift(30,B);
mov edi,esi
rol edi,30
;B = A;
mov esi,ebp
;A = temp;
mov ebp,eax
add bx,4
cmp bx,160
jb dword SHA1_addChar_j5
;for(t = 40; t < 60; t++)
SHA1_addChar_j6:
;temp=((B & C) | (B & D) | (C & D)) + SHA1CircularShift(5,A) + E + W[t]+K[2];
push bx
mov eax,esi
and eax,edi
mov ebx,esi
and ebx,edx
or eax,ebx
mov ebx,edi
and ebx,edx
or eax,ebx
mov ebx,ebp
rol ebx,5
add eax,ebx
add eax,ecx
pop bx
add eax,ds:[dataBlock_temp2+bx]
add eax,08f1bbcdch
;E = D;
mov ecx,edx
;D = C;
mov edx,edi
;C = SHA1CircularShift(30,B);
mov edi,esi
rol edi,30
;B = A;
mov esi,ebp
;A = temp;
mov ebp,eax
add bx,4
cmp bx,240
jb dword SHA1_addChar_j6
;for(t = 60; t < 80; t++)
SHA1_addChar_j7:
;temp = (B ^ C ^ D) + SHA1CircularShift(5,A) + E + W[t] + K[3];
push bx
mov eax,esi
xor eax,edi
xor eax,edx
mov ebx,ebp
rol ebx,5
add eax,ebx
add eax,ecx
pop bx
add eax,ds:[dataBlock_temp2+bx]
add eax,0ca62c1d6h
;E = D;
mov ecx,edx
;D = C;
mov edx,edi
;C = SHA1CircularShift(30,B);
mov edi,esi
rol edi,30
;B = A;
mov esi,ebp
;A = temp;
mov ebp,eax
add bx,4
cmp bx,320
jb dword SHA1_addChar_j7
mov bx,ds:[dataBlock_temp1]
;context->Intermediate_Hash[0] += A;
;context->Intermediate_Hash[1] += B;
;context->Intermediate_Hash[2] += C;
;context->Intermediate_Hash[3] += D;
;context->Intermediate_Hash[4] += E;
add ds:[bx+16],ecx     ;e
add ds:[bx+12],edx     ;d
add ds:[bx+8],edi      ;c
add ds:[bx+4],esi      ;b
add ds:[bx+0],ebp      ;a
sub ax,ax
mov ds:[bx+58h],al
mov bp,bx
SHA1_addChar_j1:
retnd
endp
;-----------------------------------------

;-----------------------------------------
proc SHA1_finish
;in: ds:bp-record...
cld
push dword ds:[bp+54h]
mov al,80h
call dword SHA1_addChar
SHA1_finish_j1:
mov al,ds:[bp+58h]
cmp al,56
je dword SHA1_finish_j2
mov al,0
call dword SHA1_addChar
jmp dword SHA1_finish_j1
SHA1_finish_j2:
mov cx,4
SHA1_finish_j3:
push cx
sub ax,ax
call dword SHA1_addChar
pop cx
loopw SHA1_finish_j3
pop eax
mov cx,4
SHA1_finish_j4:
rol eax,8
push cx
push eax
call dword SHA1_addChar
pop eax
pop cx
loopw SHA1_finish_j4
mov si,bp
mov di,bp
mov cx,5
SHA1_finish_j5:
lodsd
xchg al,ah
rol eax,16
xchg al,ah
stosd
loopw SHA1_finish_j5
retnd
endp
;-----------------------------------------


;rec:
;state dd 5 dup (?)          00h
;bufdat db 64 dup (?)        14h
;count dd ?                  54h
;buflen db ?                 58h
