org 0h
use32
db 'exec'
dd offset lastbyte
dd 1024
dd 1024

sub edi,edi
mov esi,offset fn
mov ecx,256
rep
  movsb cs,ptr32

mov edi,200h
clts                            ;get process parameters...
dd 13h

sub esi,esi
clts                            ;execute and wait...
dd 47h

clts
dd 00h

fn db 19,'\utils\bc-main.code'
lastbyte: