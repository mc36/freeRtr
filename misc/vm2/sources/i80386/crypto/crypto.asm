org 0h
use32
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 14000h                       ;data
dd 4096                         ;stack
;-------------------------------
mov esi,offset text01
call dword writeCodeStr

sub eax,eax
mov def:[DataBlock_datNum],eax

clts                            ;start listening...
dd 14h
or ebx,ebx
jnz dword vege

sub esi,esi
mov def:[esi],esi
mov ecx,4
clts                            ;write to console...
dd 20h

main_j1:
clts                            ;give away the control...
dd 01h
clts                            ;get next incoming pipeline...
dd 16h
or ebx,ebx
jnz dword main_j2
mov def:[DataBlock_contx],eax
mov esi,offset text03
call dword writeCodeStr
mov edx,def:[DataBlock_contx]
mov cl,8
call dword conv2hex
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr
mov ecx,def:[DataBlock_datNum]
inc ecx
shl ecx,2
clts                            ;resize extended memory...
dd 24h
mov def:[DataBlock_datOfs],edi
mov eax,def:[DataBlock_datNum]
shl eax,2
add edi,eax
mov eax,def:[DataBlock_contx]
stosd ptr32
inc dword def:[DataBlock_datNum]
main_j2:

mov ecx,def:[DataBlock_datNum]
mov esi,def:[DataBlock_datOfs]
main_j3:
dec ecx
js dword main_j1
lodsd ptr32
mov def:[DataBlock_curPip],eax
push esi
push ecx
mov edi,DataBlock_freMem
mov ecx,8000h
clts                            ;nonblocking receive through pipeline...
dd 1bh
or ebx,ebx
jnz dword main_j4
or ecx,ecx
jz dword main_j4
lea ebp,def:[DataBlock_freMem+ecx]
mov esi,DataBlock_freMem
lodsd ptr32
shl eax,2
add eax,offset CommandList_beg
cmp eax,offset CommandList_end
jae byte main_j5
mov eax,cs:[eax]
mov edi,DataBlock_freMem
call eax
mov ecx,edi
mov esi,DataBlock_freMem
sub ecx,esi
mov eax,def:[DataBlock_curPip]
clts                            ;nonblocking send through pipeline...
dd 1ah
main_j6:
pop ecx
pop esi
jmp dword main_j3
main_j4:
mov eax,def:[DataBlock_curPip]
clts                            ;get pipeline info...
dd 19h
or ebx,ebx
jnz byte main_j5
or eax,eax
jz byte main_j5
jmp byte main_j6
main_j5:
mov eax,def:[DataBlock_curPip]
clts                            ;close pipeline side...
dd 18h
dec dword def:[DataBlock_datNum]
pop ecx
pop esi
mov ecx,def:[DataBlock_datNum]
shl ecx,2
mov edx,def:[DataBlock_datOfs]
add edx,ecx
mov eax,def:[edx]
mov def:[esi-4],eax
clts                            ;resize extended memory...
dd 24h
mov def:[DataBlock_datOfs],edi
jmp dword main_j2


vege:
mov esi,offset text02
call dword writeCodeStr
sub eax,eax                     ;terminate process...
clts
dd 00h
;-------------------------------

;-------------------------------
proc conv2dec
;in:  edx-value to write...
;out: esi-where converted...
mov esi,offset conv2dec_d1
mov edi,DataBlock_freMem
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
mov esi,DataBlock_freMem
retnd
conv2dec_d1:
dd 1000000000,0,100000000,10000000,1000000,0,100000,10000,1000,0,100,10,1
conv2dec_d2:
endp
;-------------------------------

;-------------------------------
proc conv2hex
;in:  edx-value to write...
;     cl-digits to convert...
;out: esi-where converted...
mov edi,DataBlock_freMem
mov al,'$'
stosb ptr32
dec ecx
and ecx,7
inc ecx
push ecx
neg cl
add cl,8
shl cl,2
rol edx,cl
pop ecx
conv2hex_j1:
rol edx,4
movzx eax,dl
and al,0fh
mov ah,'0'
cmp al,10
jb byte conv2hex_j2
mov ah,'A'
sub al,10
conv2hex_j2:
add al,ah
stosb ptr32
loopd conv2hex_j1
mov eax,' '
stosd ptr32
mov esi,DataBlock_freMem
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
proc writeCodeStr
;in: cs:esi-offset of text
push esi
push edi
push ecx
push eax
sub ecx,ecx
mov edi,DataBlock_freMem
writeCodeStr_j1:
inc ecx
lodsb cs,ptr32
stosb ptr32
or al,al
jnz byte writeCodeStr_j1
dec ecx
mov esi,DataBlock_freMem
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
DataBlock_datNum equ 8000h      ;dd: number of pipelines...
DataBlock_datOfs equ 8004h      ;dd: offset of pipelines...
DataBlock_curPip equ 8008h      ;dd: current pipeline...
DataBlock_freMem equ 800ch      ;free memory...
;-------------------------------

;-------------------------------
text01 db 'crypto v1.0, done by Mc at ',%date,' ',%time,'.',13,10,0
text02 db 'error!',13,10,0
text03 db 'incoming pipeline: ',0
textCRLF db 13,10,0
;-------------------------------

;-------------------------------
include crypto1.inc             ;algorithm headers...
include crypto2.inc             ;hash upper level...
include crypto3.inc             ;cipher upper level...
include crypto4.inc             ;user routines...
;-------------------------------




;------------------------------- cipher algorithms...
include cipher\blowfish.inc
include cipher\cast.inc
include cipher\des.inc
include cipher\des3.inc
include cipher\rc2.inc
include cipher\rc4.inc
include cipher\rc5.inc
include cipher\rc6.inc
include cipher\rijndael.inc
include cipher\twofish.inc
include cipher\idea.inc
include cipher\tea.inc
include cipher\camellia.inc
;-------------------------------

;------------------------------- hasher algorithms...
include hasher\crc16.inc
include hasher\crc32.inc
include hasher\md2.inc
include hasher\md4.inc
include hasher\md5.inc
include hasher\rmd160.inc
include hasher\sha1.inc
include hasher\sha256.inc
;-------------------------------

lastbyte:
