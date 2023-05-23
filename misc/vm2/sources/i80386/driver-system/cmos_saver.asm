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

mov edi,400h
mov ecx,256
sub ebx,ebx
j1:
mov al,bl
out 70h,al
in al,71h
stosb ptr32
inc ebx
loopd j1

sub edi,edi
clts                            ;get process parameters...
dd 13h

sub esi,esi
clts                            ;create file...
dd 35h
or ebx,ebx
jnz byte vege

sub esi,esi
mov eax,7
clts                            ;open file...
dd 3fh
or ebx,ebx
jnz byte vege
mov ebp,eax

mov eax,ebp
mov esi,400h
mov ecx,256
clts                            ;write to file...
dd 41h
or ebx,ebx
jnz byte vege

mov eax,ebp
clts                            ;close file...
dd 46h

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
