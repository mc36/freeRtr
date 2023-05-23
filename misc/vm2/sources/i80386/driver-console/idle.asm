org 0h
use32
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 512                          ;data
dd 512                          ;stack
;-------------------------------

sub edi,edi
clts                            ;get process parameters...
dd 13h

mov al,def:[0]                  ;get terminator...
or al,al
setz al

clts                            ;switch cpu idle mode...
dd 50h

sub eax,eax                     ;terminate process...
clts
dd 00h

lastbyte:
