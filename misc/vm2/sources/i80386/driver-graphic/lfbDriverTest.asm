org 0h
use32
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 16384                        ;data
dd 4096                         ;stack
;-------------------------------

mov edi,dataSeg_free
clts                            ;get process parameters...
dd 13h

mov esi,dataSeg_free
clts                            ;find process by name...
dd 0dh
mov def:[dataSeg_proc],eax
or eax,eax
jz dword vege

mov eax,def:[dataSeg_proc]
mov ecx,10000h
mov bl,1
clts                            ;create new pipeline...
dd 17h
mov def:[dataSeg_pipe],eax
or ebx,ebx
jnz dword vege

mov ecx,16
j1:
clts                            ;give away the control...
dd 01h
loopd j1

mov eax,def:[dataSeg_pipe]
mov edi,dataSeg_free
mov ecx,1000h
clts                            ;nonblocking receive through pipeline...
dd 1bh
or ebx,ebx
jnz dword vege
mov esi,dataSeg_free
lodsd ptr32
mov def:[dataSeg_scrX],eax
lodsd ptr32
mov def:[dataSeg_scrY],eax



;construct the X-------------X line...
mov edi,dataSeg_free
mov al,0ffh
stosb ptr32
stosb ptr32
stosb ptr32
mov ecx,def:[dataSeg_scrX]
sub ecx,2
imul ecx,3
mov al,0
rep
  stosb ptr32
mov al,0ffh
stosb ptr32
stosb ptr32
stosb ptr32

;send it...
sub eax,eax
mov def:[dataSeg_posX],eax
mov def:[dataSeg_posY],eax
mov ecx,def:[dataSeg_scrX]
j2:
push ecx
mov ecx,def:[dataSeg_scrX]
call dword send
inc dword def:[dataSeg_posY]
pop ecx
loopd j2

;construct the XXXXXXXXXXXXXXX line...
sub eax,eax
dec eax
mov edi,dataSeg_free
mov ecx,def:[dataSeg_scrX]
rep
  stosd ptr32
sub eax,eax

;send it...
mov def:[dataSeg_posX],eax
mov def:[dataSeg_posY],eax
mov ecx,def:[dataSeg_scrX]
call dword send
mov eax,def:[dataSeg_scrY]
dec eax
mov def:[dataSeg_posY],eax
mov ecx,def:[dataSeg_scrX]
call dword send

;calculate line position...
mov eax,def:[dataSeg_scrX]
sub eax,320
shr eax,1
mov def:[dataSeg_posX],eax
mov eax,def:[dataSeg_scrY]
sub eax,256
shr eax,1
mov def:[dataSeg_posY],eax
sub eax,eax
mov def:[dataSeg_free],al

mov ecx,256
j3:
push ecx
mov edi,dataSeg_free
movzx byte eax,def:[dataSeg_free]
mov ecx,100
j4:
stosd ptr32
dec edi
loopd j4
mov al,0
mov ecx,30
rep
  stosb ptr32
movzx byte eax,def:[dataSeg_free]
shl eax,8
mov ecx,100
j5:
stosd ptr32
dec edi
loopd j5
mov al,0
mov ecx,30
rep
  stosb ptr32
movzx byte eax,def:[dataSeg_free]
shl eax,16
mov ecx,100
j6:
stosd ptr32
dec edi
loopd j6
mov al,0
mov ecx,30
rep
  stosb ptr32
mov ecx,320
call dword send
inc dword def:[dataSeg_free]
inc dword def:[dataSeg_posY]
pop ecx
loopd j3

wait:
clts                            ;give away the control...
dd 01h
jmp byte wait

vege:
sub eax,eax
inc eax
clts
dd 00h
;-------------------------------

;-------------------------------
proc send
mov eax,def:[dataSeg_pipe]
mov esi,dataSeg_posX
imul ecx,3
add ecx,8
send_j1:
clts                            ;nonblocking send through pipeline...
dd 1ah
or ebx,ebx
jz byte send_j2
clts                            ;give away the control...
dd 01h
jmp byte send_j1
send_j2:
retnd
endp
;-------------------------------

;-------------------------------
dataSeg_proc equ 000h
dataSeg_pipe equ 004h
dataSeg_maxX equ 008h
dataSeg_maxY equ 00ch
dataSeg_scrX equ 010h
dataSeg_scrY equ 014h
dataSeg_posX equ 018h
dataSeg_posY equ 01ch
dataSeg_free equ 020h
;-------------------------------
lastbyte:
