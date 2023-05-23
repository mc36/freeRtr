org 0h
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 1024                         ;data
dd 1024                         ;stack
;-------------------------------

lui t0,9000h
lb t0,0,t0

lastbyte:
