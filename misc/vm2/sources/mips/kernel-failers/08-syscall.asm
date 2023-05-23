org 0h
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 1024                         ;data
dd 1024                         ;stack
;-------------------------------

ori t0,r0,0ffh
syscall 0

lastbyte:
