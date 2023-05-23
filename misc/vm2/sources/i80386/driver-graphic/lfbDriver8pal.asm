;-------------------------------
include lfbDriver_beg.inc
;-------------------------------

;-------------------------------
textMN db 'linear, 8 bits (custom palette)',0
textPR db '<palette>',0
resolMuler equ 1
resolAdder equ 1024
;-------------------------------

;-------------------------------
proc processParam
mov edi,def:[dataSeg_buffer]
add edi,def:[dataSeg_maxP]
sub eax,eax
mov ecx,300h
rep
  stosb ptr32
mov eax,1
clts                            ;open file...
dd 3fh
or ebx,ebx
jnz dword init_err
mov ebp,eax
clts                            ;get file size...
dd 43h
or ebx,ebx
jnz dword init_err
cmp ecx,300h
jb byte processParam_j1
mov ecx,300h
processParam_j1:
mov eax,ebp
mov edi,def:[dataSeg_buffer]
add edi,def:[dataSeg_maxP]
clts                            ;read from file...
dd 40h
or ebx,ebx
jnz dword init_err
mov eax,ebp
clts                            ;close file...
dd 46h
or ebx,ebx
jnz dword init_err
retnd
endp
;-------------------------------

;-------------------------------
proc clearScreen
sub eax,eax
call dword getColor
mov edi,def:[dataSeg_mapped]
mov ecx,def:[dataSeg_maxP]
rep
  stosb ptr32
mov edi,def:[dataSeg_buffer]
mov ecx,def:[dataSeg_maxP]
rep
  stosb ptr32
retnd
endp
;-------------------------------

;-------------------------------
proc processData
lodsd ptr32
mov edi,eax
lodsd ptr32
mul dword def:[dataSeg_maxX]
add edi,eax
cmp edi,def:[dataSeg_maxP]
ja dword processData_vege
lea eax,def:[edi+ecx]
cmp eax,def:[dataSeg_maxP]
ja dword processData_vege
mov ebx,edi
add ebx,def:[dataSeg_buffer]
add edi,def:[dataSeg_mapped]
processData_j1:
mov eax,def:[esi]
add esi,3
call dword getColor
cmp al,def:[ebx]
je byte processData_j2
mov def:[edi],al
mov def:[ebx],al
processData_j2:
inc edi
inc ebx
loopd processData_j1
processData_vege:
retnd
endp
;-------------------------------

;-------------------------------
proc getColor
;in:  eax-rgb color value...
;out: al-pal color number...
push esi
push edx
push ecx
push ebx
shr eax,2
and eax,3f3f3fh
mov ebx,eax
mov esi,def:[dataSeg_buffer]
add esi,def:[dataSeg_maxP]
mov edx,0ff00h
sub ecx,ecx
getColor_j1:
lodsb ptr32
sub al,bl
jns byte getColor_j2
neg al
getColor_j2:
mov ah,al
lodsb ptr32
sub al,bh
jns byte getColor_j3
neg al
getColor_j3:
add ah,al
rol ebx,16
lodsb ptr32
sub al,bl
jns byte getColor_j4
neg al
getColor_j4:
add ah,al
rol ebx,16
cmp dh,ah
jb byte getColor_j5
mov dh,ah
mov dl,cl
getColor_j5:
inc ecx
or ch,ch
jz byte getColor_j1
mov al,dl
pop ebx
pop ecx
pop edx
pop esi
retnd
endp
;-------------------------------

;-------------------------------
include lfbDriver_end.inc
;-------------------------------
