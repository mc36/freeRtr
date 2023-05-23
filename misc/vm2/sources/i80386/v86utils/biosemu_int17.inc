;------------------------------- serial io services...
proc int17h
call interrupt_begin
jmp interrupt_finish
endp
;-------------------------------
