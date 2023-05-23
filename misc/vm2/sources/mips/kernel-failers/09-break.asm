org 0h
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 1024                         ;data
dd 1024                         ;stack
;-------------------------------

break 0

lastbyte:
