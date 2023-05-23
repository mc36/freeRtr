org 0h
use32
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 4096                         ;data
dd 4096                         ;stack
;-------------------------------

;-------------------------------
mov edi,dataSeg_wrtBuf
clts                            ;get process parameters...
dd 13h
mov esi,offset text01
mov al,def:[dataSeg_wrtBuf]
or al,al
jz dword err

mov esi,dataSeg_wrtBuf
mov eax,1
clts                            ;open file...
dd 3fh
mov esi,offset text03
or ebx,ebx
jnz dword err
mov def:[dataSeg_temp01],eax

mov eax,def:[dataSeg_temp01]
clts                            ;get file size...
dd 43h
mov esi,offset text03
or ebx,ebx
jnz dword err
cmp ecx,vgaPalRec_siz
jb byte main_j1
mov ecx,vgaPalRec_siz
main_j1:
mov def:[dataSeg_temp02],ecx

mov edi,dataSeg_buffer
sub eax,eax
mov ecx,vgaPalRec_siz
rep
  stosb ptr32

mov eax,def:[dataSeg_temp01]
mov edi,dataSeg_buffer
mov ecx,def:[dataSeg_temp02]
clts                            ;read from file...
dd 40h
mov esi,offset text03
or ebx,ebx
jnz dword err

mov eax,def:[dataSeg_temp01]
clts                            ;close file...
dd 46h
mov esi,offset text03
or ebx,ebx
jnz dword err

mov edx,def:[dataSeg_temp02]
call dword conv2dec
call dword writeDataStr
mov esi,offset text02
call dword writeCodeStr

mov al,1
clts                            ;switch io accessible mode...
dd 04h

mov esi,dataSeg_buffer
call dword vga_PalPut

mov esi,offset text04
err:
call dword writeCodeStr
vege:
sub eax,eax                     ;terminate process...
clts
dd 00h
jmp byte vege
;-------------------------------



;-------------------------------
include vgaCore.inc
include vgaUtils.inc
;-------------------------------



;-------------------------------
dataSeg_wrtBuf equ 0000h        ;256: write buffer...
dataSeg_temp01 equ 0100h        ;dd: temporary dword...
dataSeg_temp02 equ 0104h        ;dd: temporary dword...
dataSeg_temp03 equ 0108h        ;dd: temporary dword...
dataSeg_temp04 equ 010ch        ;dd: temporary dword...
dataSeg_buffer equ 0200h        ;data buffer...
;-------------------------------

;-------------------------------
text01 db 'using: vga.code <filename>',13,10,0
text02 db ' bytes readed!',13,10,0
text03 db 'error reading file!',13,10,0
text04 db 'done!',13,10,0
;-------------------------------

lastbyte:
