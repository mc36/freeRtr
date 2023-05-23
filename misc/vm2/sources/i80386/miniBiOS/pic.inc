;-----------------------------------
proc SetUpPIC   ;cl-master begin  ch-slave begin
mov al,11h      ;initialization sequence
call pic_out20  ;send it to 8259A-1
call pic_outA0  ;and to 8259A-2
mov al,cl       ;start of hardware ints 1
call pic_out21
mov al,ch       ;start of hardware ints 2
call pic_outA1
mov al,04h      ;8259-1 is master
call pic_out21
mov al,02h      ;8259-2 is slave
call pic_outA1
mov al,01h      ;8086 mode for both
call pic_out21
call pic_outA1
mov al,0h       ;there are no disabled interrupts
call pic_outA1
call pic_out21
mov al,20h      ;sign end of pending irq!
call pic_out20
call pic_outA0
ret
;---------------------
pic_outend:
db 0ebh,00       ;'jmp short $+2' in tasm..
ret
;---------------------
pic_out20:
mov dx,20h
out dx,al
jmp byte pic_outend
;---------------------
pic_out21:
mov dx,21h
out dx,al
jmp byte pic_outend
;---------------------
pic_outA0:
mov dx,0a0h
out dx,al
jmp byte pic_outend
;---------------------
pic_outA1:
mov dx,0a1h
out dx,al
jmp byte pic_outend
endp
;-----------------------------------

;-----------------------------------
proc SetUpRTC
;in: ecx-timer value
push eax
push ecx
push edx
or ecx,ecx
setz al
or cl,al
mov eax,1193280
sub edx,edx
div ecx
cmp eax,1
jg byte SetUpRTC_j1
sub eax,eax
inc ax
SetUpRTC_j1:
cmp eax,0ffffh
jl byte SetUpRTC_j2
sub eax,eax
dec ax
SetUpRTC_j2:
mov dx,ax
mov al,36h
out 43h,al
mov al,dl
out 40h,al
mov al,dh
out 40h,al
pop edx
pop ecx
pop eax
ret
endp
;-----------------------------------

;-----------------------------------
proc SetUpCMOS
mov ax,0a26h       ;control register a
call SetUpCMOS_j1
mov ax,0b02h       ;control register b
call SetUpCMOS_j1
ret
SetUpCMOS_j1:
mov dx,70h
xchg al,ah
out dx,al
mov al,ah
inc dx
out dx,al
ret
endp
;-----------------------------------
