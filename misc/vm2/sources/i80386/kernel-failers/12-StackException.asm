org 0h
use32
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 1024                         ;data
dd 1024                         ;stack
;-------------------------------
mov ecx,4096
j1:
push eax
loopd j1
lastbyte:
