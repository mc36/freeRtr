;------------------------------- terminate process..
proc int20h
call interrupt_begin
jmp int21h_fn00
endp
;-------------------------------
