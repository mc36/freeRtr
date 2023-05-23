;------------------------------- variables...
portio_data3da dd ?             ;3da port data...
;-------------------------------

;------------------------------- initialize structures...
proc portio_initialize
mov ax,cs
mov bx,offset portio_handler
clts                            ;initialize io handler...
dw 4dh
mov al,4
mov cs:[portio_data3da],al
ret
endp
;-------------------------------

;------------------------------- port io handler...
proc portio_handler
mov cs:[portio_handler_d1],dx   ;port number...
mov cs:[portio_handler_d2],bl   ;1=byte, 2=word, 4=dword...
mov cs:[portio_handler_d3],bh   ;0=in, 1=out, 2=ins, 3=outs, 4=repins, 5=repouts...
pop bx
pop dx
call interrupt_begin
mov bp,cs:[portio_handler_d1]
mov al,cs:[portio_handler_d3]
cmp al,0
je byte portio_handler_in
cmp al,1
je byte portio_handler_out
mov si,offset portio_handler_txt3
call interrupt_error
jmp interrupt_finish
portio_handler_in:
mov si,offset portNumbers_in
call portio_handler_j1
mov si,offset portio_handler_txt1
call interrupt_error
jmp interrupt_finish
portio_handler_out:
mov si,offset portNumbers_out
call portio_handler_j1
mov si,offset portio_handler_txt2
call interrupt_error
jmp interrupt_finish
portio_handler_j1:
lodsd cs
or eax,eax
jz byte portio_handler_j2
cmp ax,bp
jne byte portio_handler_j1
pop ax
shr eax,16
jmp ax
portio_handler_j2:
ret
portio_handler_txt1 db 'invalid port input requested!',0
portio_handler_txt2 db 'invalid port output requested!',0
portio_handler_txt3 db 'invalid port access requested!',0
portio_handler_d1 dd ?
portio_handler_d2 dd ?
portio_handler_d3 dd ?
endp
;-------------------------------

;------------------------------- input ports to skip...
portNumbers_in:
dw 03b5h,offset portHandle_nop  ;video...
dw 03c5h,offset portHandle_nop
dw 03d5h,offset portHandle_nop
dw 0020h,offset portHandle_nop  ;pic1...
dw 0021h,offset portHandle_nop
dw 00a0h,offset portHandle_nop  ;pic2...
dw 00a1h,offset portHandle_nop
dw 0040h,offset portHandle_nop  ;pit...
dw 0041h,offset portHandle_nop
dw 0042h,offset portHandle_nop
dw 0043h,offset portHandle_nop
dw 0061h,offset portHandle_nop  ;keyboard...
dw 0070h,offset portHandle_nop  ;cmos...
dw 0071h,offset portHandle_nop
dw 00f0h,offset portHandle_nop  ;coprocessor...
dw 00f1h,offset portHandle_nop
;executables...
dw 0060h,offset portHandle_in0060
dw 0064h,offset portHandle_in0064
dw 03bah,offset portHandle_in03da
dw 03c9h,offset portHandle_in03c9
dw 03dah,offset portHandle_in03da
dw 0,0                          ;eol...
;-------------------------------

;------------------------------- output ports to skip...
portNumbers_out:
dw 03b4h,offset portHandle_nop  ;video...
dw 03b5h,offset portHandle_nop
dw 03b8h,offset portHandle_nop
dw 03c4h,offset portHandle_nop
dw 03c5h,offset portHandle_nop
dw 03ceh,offset portHandle_nop
dw 03d4h,offset portHandle_nop
dw 03d5h,offset portHandle_nop
dw 03d8h,offset portHandle_nop
dw 03c7h,offset portHandle_nop
dw 03c8h,offset portHandle_nop
dw 03c9h,offset portHandle_nop
dw 0040h,offset portHandle_nop  ;pit...
dw 0041h,offset portHandle_nop
dw 0042h,offset portHandle_nop
dw 0043h,offset portHandle_nop
dw 0020h,offset portHandle_nop  ;pic1...
dw 0021h,offset portHandle_nop
dw 00a0h,offset portHandle_nop  ;pic2...
dw 00a1h,offset portHandle_nop
dw 0060h,offset portHandle_nop  ;keyboard...
dw 0061h,offset portHandle_nop
dw 0070h,offset portHandle_nop  ;cmos...
dw 0071h,offset portHandle_nop
dw 00f0h,offset portHandle_nop  ;coprocessor...
dw 00f1h,offset portHandle_nop
;executables...
dw 0,0                          ;eol...
;-------------------------------

;------------------------------- no operation io handlers...
proc portHandle_nop
jmp interrupt_finish
endp
;-------------------------------

;------------------------------- input port...
proc portHandle_in0060
mov byte cs:[stack_reg_eax],1
jmp interrupt_finish
endp
;-------------------------------

;------------------------------- input port...
proc portHandle_in0064
mov byte cs:[stack_reg_eax],1ch
jmp interrupt_finish
endp
;-------------------------------

;------------------------------- input port...
proc portHandle_in03da
xor byte cs:[portio_data3da],1
mov al,cs:[portio_data3da]
mov cs:[stack_reg_eax],al
jmp interrupt_finish
endp
;-------------------------------

;------------------------------- input port...
proc portHandle_in03c9
mov byte cs:[stack_reg_eax],0
jmp interrupt_finish
endp
;-------------------------------
