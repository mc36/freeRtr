org 0h
use32
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 4000h                        ;data
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

mov edi,dataSeg_wrtBuf
clts                            ;get process parameters...
dd 13h

mov esi,dataSeg_wrtBuf
lodsb ptr32
jne dword err
lodsb ptr32
sub al,'0'
cmp al,8
jae dword err
movzx eax,al
mov def:[dataSeg_padNum],eax

mov esi,offset text03
call dword writeCodeStr

mov eax,def:[dataSeg_padNum]
shl eax,3
add eax,def:[dataSeg_memory]
add eax,0fec0h
mov def:[dataSeg_padCtr],eax
mov eax,def:[dataSeg_padNum]
shl eax,9
add eax,def:[dataSeg_memory]
add eax,0e900h
mov def:[dataSeg_padTxB],eax
mov eax,def:[dataSeg_padNum]
shl eax,9
add eax,def:[dataSeg_memory]
add eax,0ea00h
mov def:[dataSeg_padRxB],eax


call dword pad_close
call dword wait4abit
call dword pad_open

mov esi,offset text04
call dword writeCodeStr




main_j1:
clts                            ;give away the control...
dd 01h
clts                            ;get console status...
dd 1fh
mov ebp,edx
or ecx,ecx
jz dword main_j2

call dword pad_carrier
mov ah,al
xchg al,def:[dataSeg_padCar]
cmp al,ah
je dword main_j3
or ah,ah
jz dword main_j8

sub esi,esi
mov def:[esi],esi
mov ecx,4
clts                            ;write to console...
dd 20h

main_j3:
call dword pad_read
or ah,ah
jnz dword main_j2
mov esi,dataSeg_wrtBuf
mov def:[esi],al
mov ecx,1
clts                            ;write to console...
dd 20h
jmp dword main_j3

main_j2:
cmp ebp,2
jb dword main_j1
call dword pad_ready
or al,al
jz dword main_j1

mov edi,dataSeg_wrtBuf
mov ecx,2
clts                            ;read from console...
dd 21h
cmp ecx,2
jne dword main_j1
mov ax,def:[dataSeg_wrtBuf]
test ah,80h
jnz dword main_j4
main_j7:
call dword pad_send
jmp dword main_j2

main_j4:
mov ecx,eax
mov esi,offset keyTable
main_j5:
lodsd cs,ptr32
or eax,eax
jz dword main_j6
cmp ax,cx
jne dword main_j5
shr eax,16
jmp dword main_j7
main_j6:

main_j8:
call dword pad_read
or ah,ah
jnz dword main_j9
mov esi,dataSeg_wrtBuf
mov def:[esi],al
mov ecx,1
clts                            ;write to console...
dd 20h
jmp byte main_j8
main_j9:






vege:
call dword pad_close
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
keyTable:
dw 8002h,09h,8003h,08h,8004h,0dh,8204h,0ah,8005h,1bh
dw 0,0
;-------------------------------

;-------------------------------
proc pad_carrier
;out: al-status: 1-connected...
mov esi,def:[dataSeg_padCtr]
mov al,def:[esi+2]
shr al,3
and al,1
retnd
endp
;-------------------------------

;-------------------------------
proc pad_ready
;out: al-status: 1-ready...
mov esi,def:[dataSeg_padCtr]
mov al,def:[esi+5]
mov ah,def:[esi+4]
dec ah
cmp al,ah
setne al
retnd
endp
;-------------------------------

;-------------------------------
proc pad_send
;in: al-char to send...
mov esi,def:[dataSeg_padCtr]
mov edi,def:[dataSeg_padTxB]
movzx byte ecx,def:[esi+5]
mov def:[edi+ecx],al
inc byte def:[esi+5]
retnd
endp
;-------------------------------

;-------------------------------
proc pad_read
;out: al-char readed...
;     ah-status: 0=ok...
mov esi,def:[dataSeg_padCtr]
movzx byte eax,def:[esi+6]
cmp al,def:[esi+7]
jne byte pad_read_j1
sub eax,eax
dec eax
retnd
pad_read_j1:
mov edi,def:[dataSeg_padRxB]
movzx byte eax,def:[edi+eax]
inc byte def:[esi+6]
retnd
endp
;-------------------------------

;-------------------------------
proc pad_open
mov esi,def:[dataSeg_padCtr]
mov al,def:[esi+5]
mov def:[esi+4],al
mov al,def:[esi+7]
mov def:[esi+6],al
mov byte def:[esi+1],00h
mov byte def:[esi+0],80h
retnd
endp
;-------------------------------

;-------------------------------
proc pad_close
mov esi,def:[dataSeg_padCtr]
mov al,def:[esi+5]
mov def:[esi+4],al
mov al,def:[esi+7]
mov def:[esi+6],al
mov byte def:[esi+1],00h
mov byte def:[esi+0],00h
sub eax,eax
mov def:[dataSeg_padCar],eax
retnd
endp
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
text01 db 'comx terminal v1.0, done by Mc at ',%date,' ',%time,'.',13,10,0
text02 db 'using: comx-term.code <pad#>',13,10,0
text03 db 'opening pad...',13,10,0
text04 db 'done...',13,10,0
;-------------------------------

;-------------------------------
dataSeg_wrtBuf equ 000h         ;write part...
dataSeg_padNum equ 100h         ;operating pad number...
dataSeg_padCtr equ 104h         ;control registers...
dataSeg_padTxB equ 108h         ;transmit buffer...
dataSeg_padRxB equ 10ch         ;receive buffer...
dataSeg_padCar equ 110h         ;carrier status...
dataSeg_memory equ 114h         ;memory offset...
;-------------------------------

;-------------------------------
include comx-info.inc
;-------------------------------

;-------------------------------
lastbyte:
