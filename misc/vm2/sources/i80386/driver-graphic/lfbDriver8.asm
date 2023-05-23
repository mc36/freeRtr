;-------------------------------
include lfbDriver_beg.inc
;-------------------------------

;-------------------------------
textMN db 'linear, 8 bits (default palette)',0
textPR db 0
resolMuler equ 1
resolAdder equ 1024
;-------------------------------

;-------------------------------
proc processParam
retnd
endp
;-------------------------------

;-------------------------------
proc clearScreen
mov al,1
clts                            ;switch io accessible mode...
dd 04h
clts                            ;give away the control...
dd 01h
mov dx,3c8h
sub ax,ax
out dx,al
inc dx
mov ecx,64
sub ebp,ebp
clearScreen_j1:
mov eax,ebp
shr al,4
and eax,3
mov al,cs:[clearScreen_d1+eax]
out dx,al
mov eax,ebp
shr al,2
and eax,3
mov al,cs:[clearScreen_d1+eax]
out dx,al
mov eax,ebp
and eax,3
mov al,cs:[clearScreen_d1+eax]
out dx,al
inc ebp
loopd clearScreen_j1
mov ecx,192
clearScreen_j2:
sub eax,eax
out dx,al
loopd clearScreen_j2
mov al,0
clts                            ;switch io accessible mode...
dd 04h
sub eax,eax
mov edi,def:[dataSeg_mapped]
mov ecx,def:[dataSeg_maxP]
rep
  stosb ptr32
mov edi,def:[dataSeg_buffer]
mov ecx,def:[dataSeg_maxP]
rep
  stosb ptr32
retnd
clearScreen_d1 db 0,21,42,63
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
and eax,0c0c0c0h
shr al,2
shr ah,4
or ah,al
shr eax,8
shr ah,6
or al,ah
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
include lfbDriver_end.inc
;-------------------------------
