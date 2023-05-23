;------------------------------- bootstrap loader service...
proc int19h
mov si,offset textMyName
call write
mov si,offset int19h_txt1
call write
mov si,offset textCRLF
call write
jmp word terminate
int19h_txt1 db 'restart interrupt requested!',0
endp
;-------------------------------
