;------------------------------- variables...
align 4
timer_ticks dd ?                ;ticks per second...
timer_later dd ?                ;timer late in ticks...
timer_dayTK dd ?                ;ticks per day...
timer_day18 dd ?                ;ticks per day...
;-------------------------------

;------------------------------- initialize structures...
proc timer_initialize
mov ax,cs
mov bx,offset timer_handler
clts                            ;initialize timer handler...
dw 4eh
call kernel_getUptime1
mov cs:[timer_ticks],edx
neg ecx
mov cs:[timer_later],ecx
imul eax,edx,86400
mov cs:[timer_dayTK],eax
mov dword cs:[timer_day18],1573040      ;24*60*60*18.20648193...
call kernel_getTime
imul eax,60
add eax,ebx
imul eax,60
add eax,ecx
imul eax,cs:[timer_ticks]
add cs:[timer_later],eax
call timer_doRound
ret
endp
;-------------------------------

;------------------------------- timer handler...
proc timer_handler
push es
push ds
push fs
push gs
pushad
call timer_doRound
call video_send2console_flush
popad
pop gs
pop fs
pop ds
pop es
iret
endp
;-------------------------------

;------------------------------- do one timer round...
proc timer_doRound
sub ax,ax
mov ds,ax
call kernel_getUptime2
add eax,cs:[timer_later]
sub edx,edx
div dword cs:[timer_dayTK]
mov eax,edx
mul dword cs:[timer_day18]
div dword cs:[timer_dayTK]
mov def:[46ch],eax
ret
endp
;-------------------------------
