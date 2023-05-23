org 0h
use32
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 1024                         ;data
dd 1024                         ;stack
;-------------------------------
sub eax,eax
div eax
lastbyte:
