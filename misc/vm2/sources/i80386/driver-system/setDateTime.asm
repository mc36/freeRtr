org 0h
use32
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 8192                         ;data
dd 512                          ;stack
;-------------------------------


;-------------------------------
mov esi,offset text01
call dword writeCodeStr


sub edi,edi
clts                            ;get process parameters...
dd 13h

sub esi,esi
lodsb ptr32
movzx ecx,al
sub edi,edi
j1:
dec ecx
js byte j3
lodsb ptr32
cmp al,'-'
je byte j2
cmp al,':'
je byte j2
cmp al,'.'
je byte j2
stosb ptr32
jmp byte j1
j2:
mov al,20h
stosb ptr32
jmp byte j1
j3:
mov eax,20202020h
stosd ptr32
sub eax,eax
stosd ptr32

sub esi,esi
mov edi,404h
mov ecx,6
j4:
dec ecx
js byte j5
push ecx
push edi
call dword str2num
pop edi
pop ecx
stosd ptr32
jnc byte j4
err:
mov esi,offset text02
call dword writeCodeStr
jmp dword vege
j5:
mov eax,def:[404h]
or eax,eax
jz byte err
mov ecx,100
sub edx,edx
div ecx
mov def:[400h],eax

mov al,1
clts                            ;switch io accessible mode...
dd 04h

clts                            ;get system time...
dd 2dh
mov ebp,ecx
j6:
clts                            ;get system time...
dd 2dh
cmp ebp,ecx
jne byte j7
clts                            ;give away the control...
dd 01h
jmp byte j6
j7:

mov esi,400h
mov ebp,offset data01
mov ecx,7
j8:
lodsd ptr32
push ecx
push esi
mov ecx,100
sub edx,edx
div ecx
mov al,cs:[ebp]
out 70h,al
mov al,dl
aam 10
aad 10h
out 71h,al
pop esi
pop ecx
inc ebp
loopd j8



mov esi,offset text03
call dword writeCodeStr
vege:
sub eax,eax
clts                            ;terminate process...
dd 00h
;-------------------------------

;-------------------------------
proc writeCodeStr
;in: cs:esi-offset of text
push esi
push edi
push ecx
push eax
sub ecx,ecx
mov edi,800h
push edi
writeCodeStr_j1:
inc ecx
lodsb cs,ptr32
stosb ptr32
or al,al
jnz byte writeCodeStr_j1
dec ecx
pop esi
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
proc str2num
push edx
sub edx,edx
str2num_j1:
lodsb ptr32
or al,al
jz byte str2num_j2
cmp al,' '
je byte str2num_j2
movzx eax,al
sub al,'0'
cmp al,9
ja byte str2num_err
imul edx,10
add edx,eax
jmp byte str2num_j1
str2num_j2:
mov eax,edx
clc
str2num_vege:
pop edx
retnd
str2num_err:
sub eax,eax
stc
jmp byte str2num_vege
endp
;-------------------------------

;-------------------------------
data01 db 50,9,8,7,4,2,0
text01 db 'date setter v1.0, done by Mc at ',%date,' ',%time,'.',13,10,0
text02 db 'using: setdate.code YYYY-MM-DD HH:MM:SS',13,10,0
text03 db 'date set!',13,10,0
;-------------------------------
lastbyte:
