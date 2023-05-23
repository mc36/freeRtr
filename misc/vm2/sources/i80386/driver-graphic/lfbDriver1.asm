;-------------------------------
include lfbDriver_beg.inc
;-------------------------------

;-------------------------------
textMN db 'linear, 1 bit (mono)',0
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
sub eax,eax
mov edi,def:[dataSeg_mapped]
mov ecx,def:[dataSeg_maxP]
shr ecx,3
rep
  stosb ptr32
mov edi,def:[dataSeg_buffer]
mov ecx,def:[dataSeg_maxP]
shr ecx,3
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
mov ebp,ecx
mov ecx,edi
shr edi,3
and ecx,7
mov dl,80h
shr dl,cl
mov ecx,ebp
mov ebx,edi
add ebx,def:[dataSeg_buffer]
add edi,def:[dataSeg_mapped]
mov dh,def:[ebx]
processData_j1:
movzx byte eax,def:[esi]
inc esi
add al,def:[esi]
adc ah,0
inc esi
add al,def:[esi]
adc ah,0
inc esi
or ah,ah
jz byte processData_j2
or dh,dl
jmp byte processData_j3
processData_j2:
or dh,dl
xor dh,dl
processData_j3:
shr dl,1
or dl,dl
jnz byte processData_j5
cmp dh,def:[ebx]
je byte processData_j4
mov def:[ebx],dh
mov def:[edi],dh
processData_j4:
inc ebx
inc edi
mov dl,80h
mov dh,def:[ebx]
processData_j5:
loopd processData_j1
cmp dh,def:[ebx]
je byte processData_vege
mov def:[ebx],dh
mov def:[edi],dh
processData_vege:
retnd
endp
;-------------------------------

;-------------------------------
include lfbDriver_end.inc
;-------------------------------
