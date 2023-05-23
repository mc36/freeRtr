;-------------------------------
include lfbDriver_beg.inc
;-------------------------------

;-------------------------------
textMN db 'linear, 24 bits (8b 8g 8r 0u)',0
textPR db 0
resolMuler equ 3
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
mov ecx,def:[dataSeg_maxB]
sub eax,eax
rep
  stosb ptr32
mov edi,def:[dataSeg_buffer]
mov ecx,def:[dataSeg_maxB]
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
imul edi,resolMuler
mov ebx,edi
add ebx,def:[dataSeg_buffer]
add edi,def:[dataSeg_mapped]
processData_j1:
mov eax,def:[esi]
add esi,3
xchg al,ah
rol eax,16
xchg al,ah
mov al,def:[ebx+3]
ror eax,8
cmp eax,def:[ebx]
je byte processData_j2
mov def:[edi],eax
mov def:[ebx],eax
processData_j2:
add edi,3
add ebx,3
loopd processData_j1
processData_vege:
retnd
endp
;-------------------------------

;-------------------------------
include lfbDriver_end.inc
;-------------------------------
