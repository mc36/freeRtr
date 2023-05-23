org 0h
use32
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 512                          ;data
dd 256                          ;stack
;-------------------------------

;-------------------------------
;generate path...
mov esi,offset txt2
sub edi,edi
call dword copy

;wait for drive to get ready...
j1:
clts
dd 01h
sub esi,esi
clts
dd 32h
or ebx,ebx
jnz byte j1

;wait a bit...
mov ecx,32
j2:
clts
dd 01h
loopd j2

;generate names...
mov esi,offset txt3
sub edi,edi
call dword copy
mov esi,offset txt4
mov edi,256
call dword copy

;execute in background...
sub esi,esi
mov edi,256
clts
dd 48h

;terminate process...
j3:
clts
dd 00h
jmp byte j3
;-------------------------------

;-------------------------------
proc copy
push edi
inc edi
push esi
mov esi,offset txt1
call dword copy_j1
pop esi
call dword copy_j1
mov eax,edi
pop edi
sub eax,edi
dec eax
stosb ptr32
retnd
copy_j1:
lodsb cs,ptr32
stosb ptr32
or al,al
jnz byte copy_j1
dec edi
retnd
endp
;-------------------------------

;-------------------------------
txt1 db '@:\'
txt2 db 0
txt3 db 'sysload.cod',0
txt4 db 'sysload.cfg',0
;-------------------------------

;-------------------------------
lastbyte:
