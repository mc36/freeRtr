;------------------------------- cipher starter...
proc doCipher_init
;in: eax-offset of handler...
;    edx-parameter to give...
;    ebp-iv initer...
mov def:[DataBlock_Handle],eax
push edx
call ebp
pop edx
mov ebx,DataBlock_contx
mov esi,DataBlock_bufDat
mov ecx,def:[DataBlock_bufSiz]
mov eax,def:[DataBlock_Handle]
call eax
retnd
endp
;-------------------------------

;------------------------------- coder for 64 bit ciphers...
proc doCipher_code1
;in: eax-offset of handler...
mov def:[DataBlock_Handle],eax
sub eax,eax
mov def:[DataBlock_bufPos],eax
doCipher_code1_j1:
mov eax,def:[DataBlock_bufPos]
cmp eax,def:[DataBlock_bufSiz]
jae byte doCipher_code1_j2
add eax,DataBlock_bufDat
mov edi,def:[eax]
mov ebp,def:[eax+4]
mov ebx,DataBlock_contx
mov eax,def:[DataBlock_Handle]
call eax
mov eax,def:[DataBlock_bufPos]
add eax,DataBlock_bufDat
mov def:[eax],edi
mov def:[eax+4],ebp
add dword def:[DataBlock_bufPos],8
jmp byte doCipher_code1_j1
doCipher_code1_j2:
retnd
endp
;-------------------------------

;------------------------------- coder for 128 bit ciphers...
proc doCipher_code2
;in: eax-offset of handler...
mov def:[DataBlock_Handle],eax
sub eax,eax
mov def:[DataBlock_bufPos],eax
doCipher_code2_j1:
mov eax,def:[DataBlock_bufPos]
cmp eax,def:[DataBlock_bufSiz]
jae dword doCipher_code2_j2
add eax,DataBlock_bufDat
mov edi,def:[eax]
mov ebp,def:[eax+4]
mov ebx,def:[eax+8]
mov edx,def:[eax+12]
mov esi,DataBlock_contx
mov eax,def:[DataBlock_Handle]
call eax
mov eax,def:[DataBlock_bufPos]
add eax,DataBlock_bufDat
mov def:[eax],edi
mov def:[eax+4],ebp
mov def:[eax+8],ebx
mov def:[eax+12],edx
add dword def:[DataBlock_bufPos],16
jmp dword doCipher_code2_j1
doCipher_code2_j2:
retnd
endp
;-------------------------------

;------------------------------- coder for 8 bit ciphers...
proc doCipher_code3
;in: eax-offset of handler...
mov def:[DataBlock_Handle],eax
mov eax,def:[DataBlock_bufSiz]
or eax,eax
jz byte doCipher_code3_j1
mov edi,DataBlock_bufDat
mov esi,edi
mov ecx,def:[DataBlock_bufSiz]
mov ebx,DataBlock_contx
mov eax,def:[DataBlock_Handle]
call eax
doCipher_code3_j1:
retnd
endp
;-------------------------------

;------------------------------- dummy iv initer...
proc doCipher_ivInit1
retnd
endp
;-------------------------------

;------------------------------- iv initer for 64 bit ciphers...
proc doCipher_ivInit2
mov esi,DataBlock_contx
lodsd ptr32
xchg al,ah
rol eax,16
xchg al,ah
mov def:[DataBlock_ivec0],eax
lodsd ptr32
xchg al,ah
rol eax,16
xchg al,ah
mov def:[DataBlock_ivec1],eax
retnd
endp
;-------------------------------

;------------------------------- iv initer for 128 bit ciphers...
proc doCipher_ivInit3
mov esi,DataBlock_contx
lodsd ptr32
mov def:[DataBlock_ivec0],eax
lodsd ptr32
mov def:[DataBlock_ivec1],eax
lodsd ptr32
mov def:[DataBlock_ivec2],eax
lodsd ptr32
mov def:[DataBlock_ivec3],eax
retnd
endp
;-------------------------------

;------------------------------- cbc encrypter for 64 bit ciphers...
proc doCipher_code4
;in: eax-offset of handler...
mov def:[DataBlock_Handle],eax
sub eax,eax
mov def:[DataBlock_bufPos],eax
doCipher_code4_j1:
mov esi,def:[DataBlock_bufPos]
cmp esi,def:[DataBlock_bufSiz]
jae byte doCipher_code4_j2
add esi,DataBlock_bufDat
lodsd ptr32
xchg al,ah
rol eax,16
xchg al,ah
mov edi,eax
lodsd ptr32
xchg al,ah
rol eax,16
xchg al,ah
mov ebp,eax
xor edi,def:[DataBlock_ivec0]
xor ebp,def:[DataBlock_ivec1]
mov ebx,DataBlock_contx
mov eax,def:[DataBlock_Handle]
call eax
mov def:[DataBlock_ivec0],edi
mov def:[DataBlock_ivec1],ebp
mov eax,edi
mov edi,def:[DataBlock_bufPos]
add edi,DataBlock_bufDat
xchg al,ah
rol eax,16
xchg al,ah
stosd ptr32
mov eax,ebp
xchg al,ah
rol eax,16
xchg al,ah
stosd ptr32
add dword def:[DataBlock_bufPos],8
jmp byte doCipher_code4_j1
doCipher_code4_j2:
retnd
endp
;-------------------------------

;------------------------------- cbc decrypter for 64 bit ciphers...
proc doCipher_code5
;in: eax-offset of handler...
mov def:[DataBlock_Handle],eax
sub eax,eax
mov def:[DataBlock_bufPos],eax
doCipher_code5_j1:
mov esi,def:[DataBlock_bufPos]
cmp esi,def:[DataBlock_bufSiz]
jae byte doCipher_code5_j2
add esi,DataBlock_bufDat
lodsd ptr32
xchg al,ah
rol eax,16
xchg al,ah
mov edi,eax
lodsd ptr32
xchg al,ah
rol eax,16
xchg al,ah
mov ebp,eax
push edi
push ebp
mov ebx,DataBlock_contx
mov eax,def:[DataBlock_Handle]
call eax
xor edi,def:[DataBlock_ivec0]
xor ebp,def:[DataBlock_ivec1]
mov eax,edi
mov edi,def:[DataBlock_bufPos]
add edi,DataBlock_bufDat
xchg al,ah
rol eax,16
xchg al,ah
stosd ptr32
mov eax,ebp
xchg al,ah
rol eax,16
xchg al,ah
stosd ptr32
pop dword def:[DataBlock_ivec1]
pop dword def:[DataBlock_ivec0]
add dword def:[DataBlock_bufPos],8
jmp byte doCipher_code5_j1
doCipher_code5_j2:
retnd
endp
;-------------------------------

;------------------------------- cbc encrypter for 128 bit ciphers...
proc doCipher_code6
;in: eax-offset of handler...
mov def:[DataBlock_Handle],eax
sub eax,eax
mov def:[DataBlock_bufPos],eax
doCipher_code6_j1:
mov eax,def:[DataBlock_bufPos]
cmp eax,def:[DataBlock_bufSiz]
jae dword doCipher_code6_j2
add eax,DataBlock_bufDat
mov edi,def:[eax]
mov ebp,def:[eax+4]
mov ebx,def:[eax+8]
mov edx,def:[eax+12]
xor edi,def:[DataBlock_ivec0]
xor ebp,def:[DataBlock_ivec1]
xor ebx,def:[DataBlock_ivec2]
xor edx,def:[DataBlock_ivec3]
mov esi,DataBlock_contx
mov eax,def:[DataBlock_Handle]
call eax
mov def:[DataBlock_ivec0],edi
mov def:[DataBlock_ivec1],ebp
mov def:[DataBlock_ivec2],ebx
mov def:[DataBlock_ivec3],edx
mov eax,def:[DataBlock_bufPos]
add eax,DataBlock_bufDat
mov def:[eax],edi
mov def:[eax+4],ebp
mov def:[eax+8],ebx
mov def:[eax+12],edx
add dword def:[DataBlock_bufPos],16
jmp dword doCipher_code6_j1
doCipher_code6_j2:
retnd
endp
;-------------------------------

;------------------------------- cbc decrypter for 128 bit ciphers...
proc doCipher_code7
;in: eax-offset of handler...
mov def:[DataBlock_Handle],eax
sub eax,eax
mov def:[DataBlock_bufPos],eax
doCipher_code7_j1:
mov eax,def:[DataBlock_bufPos]
cmp eax,def:[DataBlock_bufSiz]
jae dword doCipher_code7_j2
add eax,DataBlock_bufDat
mov edi,def:[eax]
mov ebp,def:[eax+4]
mov ebx,def:[eax+8]
mov edx,def:[eax+12]
push edi
push ebp
push ebx
push edx
mov esi,DataBlock_contx
mov eax,def:[DataBlock_Handle]
call eax
mov eax,def:[DataBlock_bufPos]
add eax,DataBlock_bufDat
xor edi,def:[DataBlock_ivec0]
xor ebp,def:[DataBlock_ivec1]
xor ebx,def:[DataBlock_ivec2]
xor edx,def:[DataBlock_ivec3]
mov def:[eax],edi
mov def:[eax+4],ebp
mov def:[eax+8],ebx
mov def:[eax+12],edx
pop dword def:[DataBlock_ivec3]
pop dword def:[DataBlock_ivec2]
pop dword def:[DataBlock_ivec1]
pop dword def:[DataBlock_ivec0]
add dword def:[DataBlock_bufPos],16
jmp dword doCipher_code7_j1
doCipher_code7_j2:
retnd
endp
;-------------------------------
