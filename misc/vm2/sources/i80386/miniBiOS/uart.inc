;-----------------------------------
proc writeHex
;in:  eax-number to convert...
;     cl-align number...
push dx
push bx
push cx
mov ch,0
push cx
neg cl
add cl,8
shl cl,2
rol eax,cl
pop cx
push ax
mov al,'0'
call sioWrite
mov al,'x'
call sioWrite
pop ax
mov bx,offset writeHex_data
writeHex_j1:
rol eax,4
push eax
and al,0fh
xlat cs
call sioWrite
pop eax
loop writeHex_j1
pop cx
pop bx
pop dx
ret
writeHex_data db '0123456789ABCDEF'
endp
;-----------------------------------

;-----------------------------------
proc writeData
;in: ds:si-offset of asciiz...
push si
push ax
writeData_j1:
lodsb
or al,al
jz byte writeData_j2
call sioWrite
jmp byte writeData_j1
writeData_j2:
pop ax
pop si
ret
endp
;-----------------------------------

;-----------------------------------
proc writeCode
;in: ds:si-offset of asciiz...
push si
push ax
writeCode_j1:
lodsb cs
or al,al
jz byte writeCode_j2
call sioWrite
jmp byte writeCode_j1
writeCode_j2:
pop ax
pop si
ret
endp
;-----------------------------------

;-----------------------------------
proc sioWrite
;in: al-char to write...
push dx
push ax
sioWrite_j1:
mov dl,5                                ;lsr...
call sioGetPort
and al,20h
jz byte sioWrite_j1
pop ax
mov dl,0                                ;thr...
call sioPutPort
pop dx
ret
endp
;-----------------------------------

;-----------------------------------
proc sioInit
mov dl,4                                ;mcr...
mov al,08h                              ;out2, no loop...
call sioPutPort
mov dl,3                                ;lcr...
mov al,83h                              ;speed, 8n1...
call sioPutPort
mov dl,0                                ;dll...
mov al,12
call sioPutPort
mov dl,1                                ;dlm...
mov al,0
call sioPutPort
mov dl,3                                ;lcr...
mov al,03h                              ;8n1...
call sioPutPort
mov dl,1                                ;ier...
mov al,00h                              ;no ints...
call sioPutPort
mov dl,2                                ;msr...
call sioGetPort
mov dl,4                                ;lsr...
call sioGetPort
mov dl,2                                ;iir...
call sioGetPort
ret
endp
;-----------------------------------

;-----------------------------------
proc sioPutPort
;in: dl-port number...
;    al-data value...
movzx dx,dl
add dx,sioPortBase
out dx,al
ret
endp
;-----------------------------------

;-----------------------------------
proc sioGetPort
;in:  dl-port number...
;out: al-data value...
movzx dx,dl
add dx,sioPortBase
in al,dx
ret
endp
;-----------------------------------

sioPortBase equ 3f8h                     ;3f8, 2f8, 3e8, 2e8...
