org 0h
use32
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 1024                         ;data
dd 1024                         ;stack
;-------------------------------
mov eax,def:[10000h]
lastbyte:
