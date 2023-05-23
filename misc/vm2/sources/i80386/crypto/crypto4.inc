;-------------------------------
CommandList_beg:
dd offset command00,offset command01,offset command02,offset command03
dd offset command04,offset command05,offset command06,offset command07
dd offset command08,offset command09,offset command10,offset command11
CommandList_end:
;-------------------------------

;------------------------------- list of ciphers...
proc command00
mov edx,offset cipherSizes
mov ebx,offset cipherNames
mov ecx,cipherMax
command00_j1:
mov eax,context_size
stosd ptr32
command00_j2:
mov eax,cs:[edx]
stosd ptr32
mov esi,cs:[ebx]
add edx,4
add ebx,4
command00_j3:
lodsb cs,ptr32
stosb ptr32
or al,al
jnz byte command00_j3
loopd command00_j2
retnd
endp
;-------------------------------

;------------------------------- list of hashers...
proc command01
mov edx,offset hasherSizes
mov ebx,offset hasherNames
mov ecx,hasherMax
jmp byte command00_j1
endp
;-------------------------------

;------------------------------- immediate hash data...
proc command02
lodsd ptr32
cmp eax,hasherMax
jb byte command02_j1
command02_j2:
mov edi,dataBlock_freMem
retnd
command02_j1:
shl eax,2
mov def:[DataBlock_algOfs],eax
lodsb ptr32
movzx ecx,al
mov def:[DataBlock_bufSiz],ecx
mov edi,DataBlock_bufDat
rep
  movsb ptr32
lodsb ptr32
movzx ecx,al
mov edi,DataBlock_contx
rep
  movsb ptr32
sub ebp,esi
js byte command02_j2
lea ecx,def:[ebp+3]
mov def:[DataBlock_bufSiz],ebp
mov edi,DataBlock_bufDat
shr ecx,2
rep
  movsd ptr32
mov eax,def:[DataBlock_algOfs]
mov eax,cs:[hasherInits+eax]
call dword doHasher_init
mov eax,def:[DataBlock_algOfs]
mov eax,cs:[hasherAdds+eax]
call dword doHasher_add
mov eax,def:[DataBlock_algOfs]
mov eax,cs:[hasherFinish+eax]
call dword doHasher_finish
mov eax,def:[DataBlock_algOfs]
mov ecx,cs:[hasherSizes+eax]
mov esi,DataBlock_contx
mov edi,dataBlock_freMem
rep
  movsb ptr32
retnd
endp
;-------------------------------

;------------------------------- immediate encrypt data...
proc command03
mov ebx,offset cipherEncrypts
mov edx,offset cipherEncoders
command03_j1:
lodsd ptr32
cmp eax,cipherMax
jb byte command03_j2
command03_j3:
mov edi,dataBlock_freMem
retnd
command03_j2:
shl eax,2
mov def:[DataBlock_algOfs],eax
lodsb ptr32
movzx ecx,al
mov def:[DataBlock_bufSiz],ecx
mov edi,DataBlock_bufDat
rep
  movsb ptr32
lodsb ptr32
movzx ecx,al
mov edi,DataBlock_contx
rep
  movsb ptr32
push edx
push esi
push ebx
push ebp
mov ecx,def:[DataBlock_algOfs]
mov edx,cs:[cipherParams+ecx]
mov eax,cs:[cipherInits+ecx]
mov ebp,cs:[cipherIvPut+ecx]
call dword doCipher_init
pop ebp
pop ebx
pop esi
pop edx
sub ebp,esi
js byte command03_j3
lea ecx,def:[ebp+3]
mov def:[DataBlock_bufSiz],ebp
mov edi,DataBlock_bufDat
shr ecx,2
rep
  movsd ptr32
mov ecx,def:[DataBlock_algOfs]
mov eax,cs:[ebx+ecx]
mov ecx,cs:[edx+ecx]
call ecx
mov ecx,def:[DataBlock_bufSiz]
mov esi,DataBlock_bufDat
mov edi,dataBlock_freMem
rep
  movsb ptr32
retnd
endp
;-------------------------------

;------------------------------- immediate decrypt data...
proc command04
mov ebx,offset cipherDecrypts
mov edx,offset cipherDecoders
jmp dword command03_j1
endp
;-------------------------------

;------------------------------- start hash context...
proc command05
lodsd ptr32
cmp eax,hasherMax
jb byte command05_j1
mov edi,dataBlock_freMem
retnd
command05_j1:
shl eax,2
mov def:[DataBlock_algOfs],eax
lodsb ptr32
movzx ecx,al
mov def:[DataBlock_bufSiz],ecx
mov edi,DataBlock_bufDat
rep
  movsb ptr32
lodsb ptr32
movzx ecx,al
mov edi,DataBlock_contx
rep
  movsb ptr32
mov eax,def:[DataBlock_algOfs]
mov eax,cs:[hasherInits+eax]
call dword doHasher_init
mov eax,def:[DataBlock_algOfs]
mov def:[DataBlock_algor],eax
mov edi,dataBlock_freMem
mov esi,DataBlock_contx
mov ecx,context_size
shr ecx,2
rep
  movsd ptr32
retnd
endp
;-------------------------------

;------------------------------- add hash data...
proc command06
mov edi,DataBlock_contx
mov ecx,context_size
shr ecx,2
rep
  movsd ptr32
mov eax,def:[DataBlock_algor]
mov def:[DataBlock_algOfs],eax
sub ebp,esi
jns byte command06_j1
mov edi,dataBlock_freMem
retnd
command06_j1:
lea ecx,def:[ebp+3]
mov def:[DataBlock_bufSiz],ebp
mov edi,DataBlock_bufDat
shr ecx,2
rep
  movsd ptr32
mov eax,def:[DataBlock_algOfs]
mov eax,cs:[hasherAdds+eax]
call dword doHasher_add
mov edi,dataBlock_freMem
mov esi,DataBlock_contx
mov ecx,context_size
shr ecx,2
rep
  movsd ptr32
retnd
endp
;-------------------------------

;------------------------------- finish hash context...
proc command07
mov edi,DataBlock_contx
mov ecx,context_size
shr ecx,2
rep
  movsd ptr32
mov eax,def:[DataBlock_algor]
mov def:[DataBlock_algOfs],eax
mov eax,def:[DataBlock_algOfs]
mov eax,cs:[hasherFinish+eax]
call dword doHasher_finish
mov eax,def:[DataBlock_algOfs]
mov ecx,cs:[hasherSizes+eax]
mov esi,DataBlock_contx
mov edi,dataBlock_freMem
rep
  movsb ptr32
retnd
endp
;-------------------------------

;------------------------------- start cipher context...
proc command08
lodsd ptr32
cmp eax,cipherMax
jb byte command08_j1
mov edi,dataBlock_freMem
retnd
command08_j1:
shl eax,2
mov def:[DataBlock_algOfs],eax
lodsb ptr32
movzx ecx,al
mov def:[DataBlock_bufSiz],ecx
mov edi,DataBlock_bufDat
rep
  movsb ptr32
lodsb ptr32
movzx ecx,al
mov edi,DataBlock_contx
rep
  movsb ptr32
mov ecx,def:[DataBlock_algOfs]
mov edx,cs:[cipherParams+ecx]
mov eax,cs:[cipherInits+ecx]
mov ebp,cs:[cipherIvPut+ecx]
call dword doCipher_init
mov eax,def:[DataBlock_algOfs]
mov def:[DataBlock_algor],eax
mov edi,dataBlock_freMem
mov esi,DataBlock_contx
mov ecx,context_size
shr ecx,2
rep
  movsd ptr32
retnd
endp
;-------------------------------

;------------------------------- encrypt cipher data...
proc command09
mov ebx,offset cipherEncrypts
mov edx,offset cipherEncoders
command09_j1:
mov edi,DataBlock_contx
mov ecx,context_size
shr ecx,2
rep
  movsd ptr32
mov eax,def:[DataBlock_algor]
mov def:[DataBlock_algOfs],eax
sub ebp,esi
jns byte command09_j2
mov edi,dataBlock_freMem
retnd
command09_j2:
lea ecx,def:[ebp+3]
mov def:[DataBlock_bufSiz],ebp
mov edi,DataBlock_bufDat
shr ecx,2
rep
  movsd ptr32
mov ecx,def:[DataBlock_algOfs]
mov eax,cs:[ebx+ecx]
mov ecx,cs:[edx+ecx]
call ecx
mov edi,dataBlock_freMem
mov esi,DataBlock_contx
mov ecx,context_size
shr ecx,2
rep
  movsd ptr32
mov ecx,def:[DataBlock_bufSiz]
mov esi,DataBlock_bufDat
rep
  movsb ptr32
retnd
endp
;-------------------------------

;------------------------------- decrypt cipher data...
proc command10
mov ebx,offset cipherDecrypts
mov edx,offset cipherDecoders
jmp dword command09_j1
endp
;-------------------------------

;------------------------------- update cipher context...
proc command11
mov edi,DataBlock_contx
mov ecx,context_size
shr ecx,2
rep
  movsd ptr32
inc esi
push esi
mov edi,DataBlock_temp1
mov esi,DataBlock_contx
mov ecx,32
rep
  movsd ptr32
pop esi
mov edi,DataBlock_contx
mov ecx,32
rep
  movsd ptr32
mov eax,def:[DataBlock_algor]
mov eax,cs:[cipherIvPut+eax]
call eax
mov esi,DataBlock_temp1
mov edi,DataBlock_contx
mov ecx,32
rep
  movsd ptr32
mov edi,dataBlock_freMem
mov esi,DataBlock_contx
mov ecx,context_size
shr ecx,2
rep
  movsd ptr32
retnd
endp
;-------------------------------
