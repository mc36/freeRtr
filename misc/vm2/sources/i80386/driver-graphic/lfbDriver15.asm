;-------------------------------
include lfbDriver_beg.inc
;-------------------------------

;-------------------------------
textMN db 'linear, 15 bits (5b 5g 5r 1u)',0
textPR db 0
resolMuler equ 2
resolAdder equ 0
;-------------------------------

;-------------------------------
proc processParam
retnd
endp
;-------------------------------

;-------------------------------
proc clearScreen
mov edi,def:[dataSeg_mapped]
mov ecx,def:[dataSeg_maxP]
sub eax,eax
rep
  stosw ptr32
mov edi,def:[dataSeg_buffer]
mov ecx,def:[dataSeg_maxP]
rep
  stosw ptr32
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
add edi,edi
mov ebx,edi
add ebx,def:[dataSeg_buffer]
add edi,def:[dataSeg_mapped]
processData_j1:
mov eax,def:[esi]
add esi,3
mov edx,eax
shr edx,19
and dl,1fh
xchg al,ah
shr ah,3
shl eax,2
and al,0e0h
or al,dl
cmp ax,def:[ebx]
je byte processData_j2
mov def:[edi],ax
mov def:[ebx],ax
processData_j2:
add edi,2
add ebx,2
loopd processData_j1
processData_vege:
retnd
endp
;-------------------------------

;-------------------------------
include lfbDriver_end.inc
;-------------------------------
