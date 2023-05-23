org 0h
use32
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 8192                         ;data
dd 512                          ;stack
;-------------------------------

mov al,1
clts                            ;switch io accessible mode...
dd 04h


mov edx,176h
call dword doer
mov edx,1f6h
call dword doer

sub eax,eax
clts                            ;terminate process...
dd 00h
;-------------------------------


;-------------------------------
proc doer
mov al,0b0h
out dx,al
inc edx
mov al,0e0h
out dx,al
dec edx
mov al,0a0h
out dx,al
inc edx
mov al,0e0h
out dx,al
retnd
endp
;-------------------------------

lastbyte:
