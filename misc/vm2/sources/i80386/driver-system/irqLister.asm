org 0h
use32
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 4096                         ;data
dd 4096                         ;stack
;-------------------------------

mov esi,offset text01
call dword writeCodeStr



sub eax,eax
mov def:[dataSeg_curNum],eax
mov def:[dataSeg_outpt1],eax
mov def:[dataSeg_outpt2],eax

mov ecx,16
main_j1:
push ecx

mov al,def:[dataSeg_curNum]
mov esi,offset newIrq
mov edi,1024
clts                            ;hook irq line...
dd 05h
mov def:[dataSeg_resCod],ebx

mov al,def:[dataSeg_curNum]
clts                            ;unhook irq line...
dd 06h

mov eax,def:[dataSeg_resCod]
mov edi,dataSeg_outpt1
or eax,eax
jz byte main_j2
mov edi,dataSeg_outpt2
main_j2:
push edi
mov edx,def:[dataSeg_curNum]
call dword conv2dec
pop edi
main_j3:
mov al,def:[edi]
inc edi
or al,al
jnz byte main_j3
dec edi
mov ax,202ch
stosw ptr32
main_j4:
lodsb ptr32
stosb ptr32
or al,al
jnz byte main_j4
dec edi

pop ecx
inc dword def:[dataSeg_curNum]
loopd main_j1

mov esi,offset text02
call dword writeCodeStr
mov esi,dataSeg_outpt2
inc esi
inc esi
call dword writeDataStr
mov esi,offset text04
call dword writeCodeStr

mov esi,offset text03
call dword writeCodeStr
mov esi,dataSeg_outpt1
inc esi
inc esi
call dword writeDataStr
mov esi,offset text04
call dword writeCodeStr




sub eax,eax
clts                            ;terminate process...
dd 00h
;-------------------------------


;-------------------------------
proc newIrq
clts                            ;terminate irq handler...
dd 07h
jmp byte newIrq
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
proc writeDataStr
;in: cs:esi-offset of text
push ecx
push eax
push esi
sub ecx,ecx
writeDataStr_j1:
inc ecx
lodsb ptr32
or al,al
jnz byte writeDataStr_j1
dec ecx
pop esi
clts                            ;write to console...
dd 20h
pop eax
pop ecx
retnd
endp
;-------------------------------

;-------------------------------
proc conv2dec
;in:  edx-value to write...
;out: esi-where converted...
mov esi,offset conv2dec_d1
mov edi,dataSeg_wrtBuf
conv2dec_j3:
cmp esi,offset conv2dec_d2
jae byte conv2dec_j4
lodsd cs,ptr32
or eax,eax
jz byte conv2dec_j3
cmp edx,eax
jb byte conv2dec_j3
conv2dec_j4:
sub esi,4
conv2dec_j1:
lodsd cs,ptr32
or eax,eax
jnz byte conv2dec_j2
mov al,' '
stosb ptr32
jmp byte conv2dec_j1
conv2dec_j2:
mov ecx,eax
mov eax,edx
sub edx,edx
div ecx
add al,'0'
stosb ptr32
cmp esi,offset conv2dec_d2
jb byte conv2dec_j1
sub eax,eax
stosb ptr32
dec edi
mov esi,dataSeg_wrtBuf
retnd
conv2dec_d1:
dd 1000000000,0,100000000,10000000,1000000,0,100000,10000,1000,0,100,10,1
conv2dec_d2:
endp
;-------------------------------

;-------------------------------
text01 db 'free irq lister v1.0, done by Mc at ',%date,' ',%time,'.',13,10,0
text02 db 'hooked irqs: ',0
text03 db 'free irqs: ',0
text04 db 13,10,0
;-------------------------------

;-------------------------------
dataSeg_wrtBuf equ 0000h      ;256: write buffer...
dataSeg_outpt1 equ 0100h      ;256: output buffer...
dataSeg_outpt2 equ 0200h      ;256: output buffer...
dataSeg_curNum equ 0300h      ;dd: current number...
dataSeg_resCod equ 0304h      ;dd: result code...
;-------------------------------

lastbyte:
