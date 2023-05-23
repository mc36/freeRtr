org 0h
use32
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 1000h                        ;data
dd 4096                         ;stack
;-------------------------------

;-------------------------------
mov esi,offset text01
call dword writeCodeStr

sub edi,edi
clts                            ;get process parameters...
dd 13h

sub eax,eax
mov al,def:[eax]
or al,al
jnz byte main_j2
main_j1:
mov esi,offset text04
call dword writeCodeStr
jmp dword vege
main_j2:
mov def:[eax+1],ah

sub esi,esi
inc esi
call dword str2num
jc byte main_j1
mov def:[210h],edx
call dword str2num
jc byte main_j1
mov def:[220h],edx

sub edi,edi
inc edi
main_j3:
lodsb ptr32
stosb ptr32
or al,al
jnz byte main_j3
lea eax,def:[edi-2]
mov def:[0],al


mov eax,def:[210h]
mov ecx,def:[220h]
clts                            ;map system memory...
dd 03h
or ebx,ebx
jnz dword err
neg eax
add eax,def:[210h]
add edi,eax
mov def:[230h],edi

sub esi,esi
clts                            ;create file...
dd 35h
or ebx,ebx
jnz dword err

sub esi,esi
mov eax,3
clts                            ;open file...
dd 3fh
mov ebp,eax
or ebx,ebx
jnz dword err

main_j4:
mov eax,def:[220h]
or eax,eax
jz main_j6
js main_j6
cmp eax,1024
jb byte main_j5
mov eax,1024
main_j5:
mov def:[240h],eax
mov ecx,eax
mov esi,def:[230h]
mov edi,300h
rep
  movsb ptr32
mov eax,ebp
mov esi,300h
mov ecx,def:[240h]
clts                            ;write to file...
dd 41h
or ebx,ebx
jnz byte err
mov eax,def:[240h]
sub def:[220h],eax
add def:[230h],eax
jmp byte main_j4

main_j6:
mov eax,ebp
clts                            ;close file...
dd 46h


mov esi,offset text03
call dword writeCodeStr
jmp byte vege

err:
mov esi,offset text02
call dword writeCodeStr
sub eax,eax                     ;terminate process...
clts
dd 00h

vege:
sub eax,eax                     ;terminate process...
clts
dd 00h
;-------------------------------


;-------------------------------
proc str2num
;in:  esi-where data is...
;out: carry-cleared if successful...
;     edx-number...
push eax
push ebx
mov ebx,10
sub edx,edx
mov al,def:[esi]
cmp al,'$'
jne byte str2num_j1
inc esi
mov ebx,16
str2num_j1:
lodsb ptr32
cmp al,'A'
jb byte str2num_j3
cmp al,'Z'
ja byte str2num_j3
or al,20h
str2num_j3:
or al,al
jz byte str2num_j2
cmp al,' '
je byte str2num_j2
call dword convDigit
cmp eax,ebx
jae byte str2num_err
imul edx,ebx
add edx,eax
jmp byte str2num_j1
str2num_j2:
dec esi
str2num_j4:
mov al,def:[esi]
cmp al,' '
jne byte str2num_j5
inc esi
jmp byte str2num_j4
str2num_j5:
clc
str2num_vege:
pop ebx
pop eax
retnd
str2num_err:
sub edx,edx
stc
jmp byte str2num_vege
endp
;-------------------------------

;-------------------------------
proc convDigit
;in:  al-byte...
;out: eax-value...
mov ah,al
sub al,'0'
cmp al,10
jb byte convDigit_j1
mov al,ah
sub al,'a'
add al,10
convDigit_j1:
movzx eax,al
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
sub edi,edi
writeCodeStr_j1:
inc ecx
lodsb cs,ptr32
stosb ptr32
or al,al
jnz byte writeCodeStr_j1
dec ecx
sub esi,esi
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
text01 db 'memory saver v1.0, done by Mc at ',%date,' ',%time,'.',13,10,0
text02 db 'error!',0
text03 db 'successful!',0
text04 db 'using: memorysaver.code <begin> <length> <file>',13,10,0
;-------------------------------

lastbyte:
