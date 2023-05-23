org 0h
use32
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 8192                         ;data
dd 4096                         ;stack
;-------------------------------

;-------------------------------
mov esi,offset text01
call dword writeCodeStr

mov al,1
clts                            ;switch io accessible mode...
dd 04h

clts                            ;start listening...
dd 14h
or ebx,ebx
jnz dword vege

sub esi,esi
mov def:[esi],esi
mov ecx,4
clts                            ;write to console...
dd 20h

sub eax,eax
mov def:[dataBlock_pipLin],eax
call dword printer_reset

main_j1:
mov eax,def:[dataBlock_pipLin]
clts                            ;close pipeline side...
dd 18h
main_j2:
clts                            ;give away the control...
dd 01h
clts                            ;get next incoming pipeline...
dd 16h
or ebx,ebx
jnz byte main_j2
mov def:[dataBlock_pipLin],eax
clts                            ;get pipeline info...
dd 19h
mov def:[dataBlock_prcNum],eax

call dword printer_reset
main_j3:
clts                            ;give away the control...
dd 01h
mov edi,dataBlock_freMem
mov ecx,1024
mov eax,def:[dataBlock_pipLin]
clts
dd 1bh
or ebx,ebx
jnz byte main_j4
or ecx,ecx
jz byte main_j4
mov def:[dataBlock_datSiz],ecx
mov dword def:[dataBlock_datPos],dataBlock_freMem
main_j5:
dec dword def:[dataBlock_datSiz]
js byte main_j3
mov esi,def:[dataBlock_datPos]
lodsb ptr32
call dword printer_send
inc dword def:[dataBlock_datPos]
or al,al
jz byte main_j5
mov esi,dataBlock_freMem
mov def:[esi],al
mov ecx,1
mov eax,def:[dataBlock_pipLin]
clts                            ;nonblocking send through pipeline...
dd 1ah
jmp byte main_j3
main_j4:
mov eax,def:[dataBlock_pipLin]
clts                            ;get pipeline info...
dd 19h
or eax,eax
jz dword main_j1
jmp dword main_j3

vege:
mov esi,offset text02
call dword writeCodeStr
sub eax,eax                     ;terminate process...
clts
dd 00h
;-------------------------------


;-------------------------------
proc printer_send
;in:  al-char
;out: al-result, 0-ok...
movzx esi,al
clts                            ;get uptime info...
dd 2ah
mov ebp,ecx
imul edi,edx,10
printer_send_j1:
mov edx,1                       ;printer status...
add edx,printerPort
in al,dx
mov ah,al
mov al,'o'
test ah,10h
jz dword printer_send_vege
mov al,'p'
test ah,20h
jnz dword printer_send_vege
mov al,'e'
test ah,08h
jz dword printer_send_vege
test ah,80h
jnz byte printer_send_j2
clts                            ;get uptime info...
dd 2bh
mov ecx,eax
mov al,'t'
sub ecx,ebp
js dword printer_send_vege
sub ecx,edi
jns dword printer_send_vege
clts                            ;give away the control...
dd 01h
jmp byte printer_send_j1
printer_send_j2:
mov edx,printerPort             ;data port...
mov eax,esi
out dx,al
mov edx,2                       ;printer control...
add edx,printerPort
mov al,1101b                    ;select+stroble...
out dx,al
printer_send_j3:
mov edx,1                       ;printer status...
add edx,printerPort
in al,dx
test al,40h
jz byte printer_send_j4
clts                            ;give away the control...
dd 01h
clts                            ;get uptime info...
dd 2bh
sub eax,ebp
js byte printer_send_j4
sub eax,edi
jns byte printer_send_j3
printer_send_j4:
mov edx,2                       ;printer control...
add edx,printerPort
mov al,1100b                    ;select...
out dx,al
printer_send_j5:
mov edx,1                       ;printer status...
add edx,printerPort
in al,dx
test al,40h
jnz byte printer_send_j6
clts                            ;give away the control...
dd 01h
clts                            ;get uptime info...
dd 2bh
sub eax,ebp
js byte printer_send_j6
sub eax,edi
jns byte printer_send_j5
printer_send_j6:
sub ax,ax
printer_send_vege:
retnd
endp
;-------------------------------

;-------------------------------
proc printer_reset
mov edx,2                       ;printer control...
add edx,printerPort
mov al,0000b                    ;reset...
out dx,al
call dword printer_reset_j1
mov edx,2                       ;printer control...
add edx,printerPort
mov al,1000b                    ;reset+select...
out dx,al
call dword printer_reset_j1
mov edx,2                       ;printer control...
add edx,printerPort
mov al,1100b                    ;select...
out dx,al
call dword printer_reset_j1
retnd
printer_reset_j1:
clts                            ;get uptime info...
dd 2ah
printer_reset_j2:
clts                            ;give away the control...
dd 01h
clts                            ;get uptime info...
dd 2bh
sub eax,ecx
js byte printer_reset_j3
sub eax,edx
js byte printer_reset_j2
printer_reset_j3:
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
mov edi,dataBlock_wrtBuf
writeCodeStr_j1:
inc ecx
lodsb cs,ptr32
stosb ptr32
or al,al
jnz byte writeCodeStr_j1
dec ecx
mov esi,dataBlock_wrtBuf
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
printerPort equ 378h
;-------------------------------

;-------------------------------
dataBlock_wrtBuf equ 0000h      ;128: write buffer...
dataBlock_pipLin equ 0100h      ;dd: pipeline number...
dataBlock_prcNum equ 0104h      ;dd: process number
dataBlock_datSiz equ 0108h      ;dd: bytes left...
dataBlock_datPos equ 010ch      ;dd: byte position...
dataBlock_freMem equ 0110h      ;free memory...
;-------------------------------

;-------------------------------
text01 db 'printer driver v1.0, done by Mc at ',%date,' ',%time,'.',13,10,0
text02 db 'error!',0
;-------------------------------

lastbyte:
