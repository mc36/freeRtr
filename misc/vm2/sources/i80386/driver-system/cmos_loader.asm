org 0h
use32
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 4096                         ;data
dd 4096                         ;stack
;-------------------------------

;-------------------------------
mov al,1
clts                            ;switch io accessible mode...
dd 04h

sub edi,edi
clts                            ;get process parameters...
dd 13h

sub esi,esi
mov eax,7
clts                            ;open file...
dd 3fh
or ebx,ebx
jnz byte vege
mov ebp,eax

mov eax,ebp
sub edi,edi
mov ecx,128
clts                            ;read from file...
dd 40h
or ebx,ebx
jnz byte vege

mov eax,ebp
clts                            ;close file...
dd 46h
or ebx,ebx
jnz byte vege

mov esi,10h
mov ecx,70h
mov ebx,esi
j1:
mov al,bl
out 70h,al
lodsb ptr32
out 71h,al
inc ebx
loopd j1

sub esi,esi
mov dword def:[esi],656e6f64h
mov ecx,4
clts                            ;write to console...
dd 20h

vege:
sub eax,eax                     ;terminate process...
clts
dd 00h
;-------------------------------
lastbyte:
