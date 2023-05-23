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
mov def:[dataSeg_memory],edi

mov esi,offset text02
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


mov esi,offset text03
call dword writeCodeStr
sub eax,eax
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
text01 db 'comx stopper v1.0, done by Mc at ',%date,' ',%time,'.',13,10,0
text02 db 'resetting board...',13,10,0
text03 db 'successful!',13,10,0
;-------------------------------

;-------------------------------
dataSeg_wrtBuf equ 000h         ;write part...
dataSeg_memory equ 100h         ;memory offset...
;-------------------------------

;-------------------------------
include comx-info.inc
;-------------------------------

;-------------------------------
lastbyte:
