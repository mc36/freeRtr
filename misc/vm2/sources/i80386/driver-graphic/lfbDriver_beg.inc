org 0h
use32
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 32768                        ;data
dd 4096                         ;stack
;-------------------------------

;-------------------------------
mov esi,offset text01
call dword writeCodeStr
mov esi,offset text02
call dword writeCodeStr
mov esi,offset textMN
call dword writeCodeStr
mov esi,offset textCRLF
call dword writeCodeStr

mov edi,dataSeg_free
clts                            ;get process parameters...
dd 13h
mov esi,dataSeg_free
movzx byte eax,def:[esi]
inc esi
mov def:[esi+eax],ah
call dword str2num
mov def:[dataSeg_proc],edx
jc byte init_err
call dword str2num
mov def:[dataSeg_maxX],edx
jc byte init_err
call dword str2num
mov def:[dataSeg_maxY],edx
jc byte init_j1
jmp byte init_j1
init_err:
mov esi,offset text03
call dword writeCodeStr
mov esi,offset textPR
call dword writeCodeStr
jmp dword vege
init_j1:
mov eax,def:[dataSeg_maxX]
or eax,eax
jz byte init_err
mov eax,def:[dataSeg_maxY]
or eax,eax
jz byte init_err
mov def:[dataSeg_free],esi

mov ecx,def:[dataSeg_maxX]
imul ecx,def:[dataSeg_maxY]
mov def:[dataSeg_maxP],ecx
imul ecx,resolMuler
mov def:[dataSeg_maxB],ecx
add ecx,16
mov def:[dataSeg_pipe],ecx
mov eax,def:[dataSeg_proc]
clts                            ;map system memory...
dd 03h
or ebx,ebx
jnz dword vege
sub eax,def:[dataSeg_proc]
sub edi,eax
mov def:[dataSeg_mapped],edi

mov ecx,resolAdder
add ecx,def:[dataSeg_pipe]
clts                            ;resize extended memory...
dd 24h
cmp ecx,def:[dataSeg_pipe]
jb dword vege
mov def:[dataSeg_buffer],edi

mov esi,def:[dataSeg_free]
init_j2:
lodsb ptr32
cmp al,' '
je byte init_j2
dec esi
dec esi
mov edi,esi
init_j3:
lodsb ptr32
or al,al
jnz byte init_j3
lea eax,def:[esi-2]
sub eax,edi
mov esi,edi
mov def:[esi],al
call dword processParam

mov esi,offset text04
call dword writeCodeStr
mov edx,def:[dataSeg_proc]
mov cl,8
call dword conv2hex
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr
mov esi,offset text05
call dword writeCodeStr
mov edx,def:[dataSeg_maxX]
call dword conv2dec
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr
mov esi,offset text06
call dword writeCodeStr
mov edx,def:[dataSeg_maxY]
call dword conv2dec
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr

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
mov def:[dataSeg_pipe],eax
mov def:[dataSeg_proc],eax
call dword clearScreen

main_j1:
mov eax,def:[dataSeg_pipe]
clts                            ;close pipeline side...
dd 18h
main_j2:
clts                            ;give away the control...
dd 01h
clts                            ;get next incoming pipeline...
dd 16h
or ebx,ebx
jnz byte main_j2
mov def:[dataSeg_pipe],eax
clts                            ;get pipeline info...
dd 19h
mov def:[dataSeg_proc],eax
mov edi,dataSeg_free
clts                            ;get other process name...
dd 0bh
test dl,40h
jz byte main_j1

mov edi,dataSeg_free
mov eax,def:[dataSeg_maxX]
stosd ptr32
mov eax,def:[dataSeg_maxY]
stosd ptr32
mov esi,dataSeg_free
mov ecx,edi
sub ecx,esi
mov eax,def:[dataSeg_pipe]
clts                            ;nonblocking send through pipeline...
dd 1ah
call dword clearScreen

main_j3:
mov eax,def:[dataSeg_pipe]
mov edi,dataSeg_free
mov ecx,10000h
clts                            ;nonblocking receive through pipeline...
dd 1bh
or ebx,ebx
jnz byte main_j4
lea eax,def:[ecx-8]
or eax,eax
js byte main_j3
jz byte main_j3
sub edx,edx
mov ebx,3
div ebx
mov ecx,eax
mov esi,dataSeg_free
call dword processData
jmp byte main_j3
main_j4:
mov eax,def:[dataSeg_pipe]
clts                            ;get pipeline info...
dd 19h
or ebx,ebx
jnz dword main_j1
cmp eax,def:[dataSeg_proc]
jne dword main_j1
clts                            ;give away the control...
dd 01h
jmp byte main_j3

vege:
sub eax,eax
inc eax
clts
dd 00h
;-------------------------------
