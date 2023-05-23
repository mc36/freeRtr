org 0h
use32
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 12000h                       ;data
dd 1024                         ;stack
;-------------------------------

;-------------------------------
mov esi,offset text01
call dword writeCodeStr

mov al,1
clts                            ;switch io accessible mode...
dd 04h

mov eax,comx_mem
mov ecx,10000h
clts                            ;map system memory...
dd 03h
or ebx,ebx
jnz dword err
mov def:[dataSeg_memory],edi

mov edi,dataSeg_filDat
clts                            ;get process parameters...
dd 13h

mov esi,offset text04
call dword writeCodeStr

mov esi,dataSeg_filDat
mov eax,1
clts                            ;open file...
dd 3fh
or ebx,ebx
jnz dword err
mov def:[dataSeg_filHdr],eax

mov eax,def:[dataSeg_filHdr]
clts                            ;get file size...
dd 43h
or ebx,ebx
jnz dword err
mov def:[dataSeg_filSiz],ecx

mov esi,offset text05
call dword writeCodeStr

mov eax,def:[dataSeg_filHdr]
mov ecx,def:[dataSeg_filSiz]
mov edi,dataSeg_filDat
cmp ecx,0fff0h
jae dword err
or ecx,ecx
jz dword err
clts                            ;read from file...
dd 40h
or ebx,ebx
jnz dword err

mov eax,def:[dataSeg_filHdr]
clts                            ;close file...
dd 46h
or ebx,ebx
jnz dword err






mov esi,offset text06
call dword writeCodeStr

mov dx,comx_prt
mov eax,comx_mem
shr eax,16
or al,20h
out dx,al
call dword wait4abit
mov dx,comx_prt
mov eax,comx_mem
shr eax,16
out dx,al
call dword wait4abit

mov esi,offset text07
call dword writeCodeStr

mov dx,comx_prt
mov eax,comx_mem
shr eax,16
or al,10h
out dx,al
call dword wait4abit

mov esi,offset text08
call dword writeCodeStr

mov ebp,64
init_j1:
dec ebp
js dword err
call dword wait4abit
mov esi,def:[dataSeg_memory]
add esi,4000h
mov ecx,0a000h
init_j2:
lodsb ptr32
or al,al
jnz byte init_j1
loopd init_j2
mov esi,def:[dataSeg_memory]
mov ax,def:[esi+0fffeh]
cmp ax,0fe18h
jne byte init_j1

mov esi,offset text09
call dword writeCodeStr

mov ebp,64
init_j3:
dec ebp
js dword err
call dword wait4abit
mov edi,def:[dataSeg_memory]
mov esi,dataSeg_filDat
mov ecx,def:[dataSeg_filSiz]
rep
  movsb ptr32
mov esi,def:[dataSeg_memory]
mov edi,dataSeg_filDat
mov ecx,def:[dataSeg_filSiz]
init_j4:
lodsb ptr32
cmp al,def:[edi]
jne byte init_j3
inc edi
loopd init_j4

mov esi,offset text10
call dword writeCodeStr

call dword wait4abit
mov esi,def:[dataSeg_memory]
mov byte def:[esi+3ch],80h
mov byte def:[esi+0ffffh],38h
call dword wait4abit






mov esi,offset text03
call dword writeCodeStr
sub eax,eax
clts                            ;terminate process...
dd 00h
err:
mov esi,offset text02
call dword writeCodeStr
sub eax,eax
dec eax
clts                            ;terminate process...
dd 00h
;-------------------------------


;-------------------------------
proc wait4abit
push eax
push ecx
push edx
push ebp
clts                            ;get uptime info...
dd 2ah
mov ebp,ecx
wait4abit_j1:
clts                            ;give away the control...
dd 01h
clts                            ;get uptime info...
dd 2ah
shr edx,2
sub ecx,edx
sub ecx,ebp
js byte wait4abit_j1
pop ebp
pop edx
pop ecx
pop eax
retnd
endp
;-------------------------------

;-------------------------------
proc writeCodeStr
;in: cs:esi-offset of text
push esi
push edi
push ecx
push eax
sub ecx,ecx
mov edi,dataSeg_wrtBuf
writeCodeStr_j1:
inc ecx
lodsb cs,ptr32
stosb ptr32
or al,al
jnz byte writeCodeStr_j1
dec ecx
mov esi,dataSeg_wrtBuf
clts                            ;write to console...
dd 20h
pop eax
pop ecx
pop edi
pop esi
retnd
endp
;-------------------------------

;-------------------------------
text01 db 'comx initer v1.0, done by Mc at ',%date,' ',%time,'.',13,10,0
text02 db 'error!',13,10,0
text03 db 'successful!',13,10,0
text04 db 'opening file...',13,10,0
text05 db 'reading file...',13,10,0
text06 db 'resetting board...',13,10,0
text07 db 'enabling memory...',13,10,0
text08 db 'waiting for zeros...',13,10,0
text09 db 'copying code...',13,10,0
text10 db 'starting code...',13,10,0
;-------------------------------

;-------------------------------
dataSeg_wrtBuf equ 000h         ;write part...
dataSeg_memory equ 100h         ;memory offset...
dataSeg_filSiz equ 104h         ;size of file...
dataSeg_filHdr equ 108h         ;file handler...
dataSeg_filDat equ 10ch         ;file data...
;-------------------------------

;-------------------------------
include comx-info.inc
;-------------------------------

;-------------------------------
lastbyte:
