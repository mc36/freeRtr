;------------------------------- system stack...
align 4
badcall_report dd 1             ;what to do on bad calls... (0=none, 1=report, 2=halt)
stack_begin:
db 512 dup (?)                  ;the stack...
stack_reg_begin:
stack_reg_edi dd ?              ;edi register...
stack_reg_esi dd ?              ;esi register...
stack_reg_ebp dd ?              ;ebp register...
dd ?                            ;esp register before pushad...
stack_reg_ebx dd ?              ;ebx register...
stack_reg_edx dd ?              ;edx register...
stack_reg_ecx dd ?              ;ecx register...
stack_reg_eax dd ?              ;eax register...
stack_reg_gs dw ?               ;gs register...
stack_reg_fs dw ?               ;fs register...
stack_reg_es dw ?               ;es register...
stack_reg_ds dw ?               ;ds register...
stack_ending:
stack_pointer dd ?              ;pointer to stack...
int_saved_sp dw ?               ;stack offset...
int_saved_ss dw ?               ;stack segment...
int_saved_ip dw ?               ;return offset...
int_saved_cs dw ?               ;return segment...
int_saved_flg dw ?              ;return flags...
;-------------------------------

;------------------------------- general interrupt...
proc intXXh
iret
endp
;-------------------------------

;------------------------------- hook all interrupts...
proc interrupts_clear
sub di,di
mov cx,256
mov es,di
mov ax,cs
shl eax,16
mov ax,offset intXXh
rep
  stosd
ret
endp
;-------------------------------

;------------------------------- hook all interrupts...
proc interrupts_hook
;in: si-hook table...
sub di,di
mov es,di
interrupts_hook_j1:
lodsd cs
inc ax
jz byte interrupts_hook_j2
lea di,def:[eax*4-4]
mov ax,cs
rol eax,16
mov es:[di],eax
jmp byte interrupts_hook_j1
interrupts_hook_j2:
ret
endp
;-------------------------------

;-------------------------------
bios_interrupts:
dw 10h,offset int10h
dw 11h,offset int11h
dw 12h,offset int12h
dw 13h,offset int13h
dw 14h,offset int14h
dw 15h,offset int15h
dw 16h,offset int16h
dw 17h,offset int17h
dw 18h,offset int19h
dw 19h,offset int19h
dw 1ah,offset int1ah
dw 20h,offset int19h
dw 0ffffh
;-------------------------------


;------------------------------- begin interrupt handler...
proc interrupt_begin
cld
pop word cs:[interrupt_begin_d1]
pop word cs:[int_saved_ip]
pop word cs:[int_saved_cs]
pop word cs:[int_saved_flg]
mov cs:[int_saved_ss],ss
mov cs:[int_saved_sp],sp
lss sp,cs:[stack_pointer]
push ds
push es
push fs
push gs
pushad
jmp word cs:[interrupt_begin_d1]
interrupt_begin_d1 dd ?
endp
;-------------------------------

;------------------------------- finish interrupt handler...
proc interrupt_finish
mov sp,offset stack_reg_begin
popad
pop gs
pop fs
pop es
pop ds
lss sp,cs:[int_saved_sp]
push word cs:[int_saved_flg]
push word cs:[int_saved_cs]
push word cs:[int_saved_ip]
iret
endp
;-------------------------------

;-------------------------------
proc interrupt_error
;in: si-offset of error text...
or byte cs:[badcall_report],0
jnz byte interrupt_error_j1
ret
interrupt_error_j1:
push si
call video_flushAll
mov si,offset textMyName
call write
pop si
call write
mov si,offset textCRLF
call write
mov si,offset interrupt_error_txt02
call write
mov si,offset interrupt_error_txt03
call write
mov eax,cs:[stack_reg_eax]
mov cl,8
call conv2hex
call write
mov si,offset interrupt_error_txt04
call write
mov eax,cs:[stack_reg_ebx]
mov cl,8
call conv2hex
call write
mov si,offset interrupt_error_txt05
call write
mov eax,cs:[stack_reg_ecx]
mov cl,8
call conv2hex
call write
mov si,offset interrupt_error_txt06
call write
mov eax,cs:[stack_reg_edx]
mov cl,8
call conv2hex
call write
mov si,offset interrupt_error_txt07
call write
mov eax,cs:[stack_reg_esi]
mov cl,8
call conv2hex
call write
mov si,offset interrupt_error_txt08
call write
mov eax,cs:[stack_reg_edi]
mov cl,8
call conv2hex
call write
mov si,offset interrupt_error_txt09
call write
mov eax,cs:[stack_reg_ebp]
mov cl,8
call conv2hex
call write
mov si,offset interrupt_error_txt10
call write
mov ax,cs:[int_saved_sp]
mov cl,4
call conv2hex
call write
mov si,offset interrupt_error_txt11
call write
mov ax,cs:[int_saved_ip]
mov cl,4
call conv2hex
call write
mov si,offset interrupt_error_txt12
call write
mov ax,cs:[int_saved_flg]
mov cl,4
call conv2hex
call write
mov si,offset interrupt_error_txt13
call write
mov ax,cs:[int_saved_cs]
mov cl,4
call conv2hex
call write
mov si,offset interrupt_error_txt14
call write
mov ax,cs:[int_saved_ss]
mov cl,4
call conv2hex
call write
mov si,offset interrupt_error_txt15
call write
mov ax,cs:[stack_reg_es]
mov cl,4
call conv2hex
call write
mov si,offset interrupt_error_txt16
call write
mov ax,cs:[stack_reg_ds]
mov cl,4
call conv2hex
call write
mov si,offset interrupt_error_txt17
call write
mov ax,cs:[stack_reg_fs]
mov cl,4
call conv2hex
call write
mov si,offset interrupt_error_txt18
call write
mov ax,cs:[stack_reg_gs]
mov cl,4
call conv2hex
call write
mov si,offset interrupt_error_txt19
call write
mov ds,cs:[int_saved_cs]
mov si,cs:[int_saved_ip]
mov cx,15
interrupt_error_j2:
lodsb
push ds
push si
push cx
mov cl,2
call conv2hex
call write
mov si,offset interrupt_error_txt21
call write
pop cx
pop si
pop ds
loopw interrupt_error_j2
mov si,offset interrupt_error_txt20
call write
mov ds,cs:[int_saved_ss]
mov si,cs:[int_saved_sp]
mov cx,9
interrupt_error_j3:
lodsw
push ds
push si
push cx
mov cl,4
call conv2hex
call write
mov si,offset interrupt_error_txt21
call write
pop cx
pop si
pop ds
loopw interrupt_error_j3
mov si,offset textCRLF
call write
call video_flushAll
cmp byte cs:[badcall_report],1
jne word terminate
ret
interrupt_error_txt02 db 'register dump follows:',13,10,0
interrupt_error_txt03 db 'eax=',0
interrupt_error_txt04 db '  ebx=',0
interrupt_error_txt05 db '  ecx=',0
interrupt_error_txt06 db '  edx=',0
interrupt_error_txt07 db 13,10,'esi=',0
interrupt_error_txt08 db '  edi=',0
interrupt_error_txt09 db '  ebp=',0
interrupt_error_txt10 db 13,10,'sp=',0
interrupt_error_txt11 db '  ip=',0
interrupt_error_txt12 db '  flags=',0
interrupt_error_txt13 db 13,10,'cs=',0
interrupt_error_txt14 db '  ss=',0
interrupt_error_txt15 db '  es=',0
interrupt_error_txt16 db '  ds=',0
interrupt_error_txt17 db '  fs=',0
interrupt_error_txt18 db '  gs=',0
interrupt_error_txt19 db 13,10,'cs:ip=',0
interrupt_error_txt20 db 13,10,'ss:sp=',0
interrupt_error_txt21 db ' ',0
endp
;-------------------------------
