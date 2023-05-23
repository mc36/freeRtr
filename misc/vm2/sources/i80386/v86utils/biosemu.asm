org 100h
use16
firstbyte:
;-------------------------------
call copyCode2final
call general_initialize
call video_initialize
call setup_zero_page
call interrupts_clear
mov si,offset bios_interrupts
call interrupts_hook
call portio_initialize
call timer_initialize

mov si,offset main_txt01
call write
mov si,offset main_txt02
call write
mov si,offset textCRLF
call write

call parseParams

mov si,offset main_txt05
call write

mov ax,offset main_txt04
mov cs:[data_temp9],ax
main_j1:
mov si,cs:[data_temp9]
lodsb cs
mov cs:[data_temp9],si
movzx eax,al
cmp al,0ffh
jne byte main_j2
mov si,offset main_txt06
call write
jmp terminate
main_j2:
mov dl,al               ;drive...
mov dh,0                ;head...
mov cx,1                ;cyl/sec...
mov ax,0201h            ;function...
sub bx,bx               ;segment...
mov es,bx
mov bx,7c00h            ;offset...
int 13h
or ah,ah
jnz byte main_j1

mov si,offset main_txt07
call write

sub eax,eax
mov ebx,7c00h
mov ss,ax
mov esp,ebx
mov ds,ax
mov es,ax
mov fs,ax
mov gs,ax
push ax
push bx
sub ecx,ecx
sub edx,edx
sub ebx,ebx
sub esi,esi
sub edi,edi
sub ebp,ebp
retf
;-------------------------------

;-------------------------------
textMyName db 13,10,'biosemu: ',0
main_txt01 db 'biosEmu v1.0',0
main_txt02 db ', done by Mc at ',%date,' ',%time,'.',0
main_txt03 db 'using: biosemu.v86 <params>',13,10
           db '-f<a..d> <image>       - floppy drive...',13,10
           db '-h<a..d> <image>       - hard drive...',13,10
           db '-s<a..d> <proc> <port> - serial port...',13,10
           db '-b<a..d>               - boot source...',13,10
           db 0
main_txt04 db 80h,80h,81h,00h,01h,0ffh
main_txt05 db 'loading boot sector...',0
main_txt06 db ' error, ',0
main_txt07 db ' starting it...',13,10,0
;-------------------------------

;------------------------------- general purpose...
align 4
data_temp0 db 512 dup (?)       ;temporary data...
data_temp1 db 512 dup (?)       ;temporary data...
data_temp2 dd ?                 ;temporary data...
data_temp3 dd ?                 ;temporary data...
data_temp4 dd ?                 ;temporary data...
data_temp5 dd ?                 ;temporary data...
data_temp6 dd ?                 ;temporary data...
data_temp7 dd ?                 ;temporary data...
data_temp8 dd ?                 ;temporary data...
data_temp9 dd ?                 ;temporary data...
;-------------------------------

;-------------------------------
proc copyCode2final
pop gs
mov ax,0c000h
call copyCode2final_j1
mov ax,0d000h
call copyCode2final_j1
mov ax,0e000h
call copyCode2final_j1
mov ax,0f000h
call copyCode2final_j1
mov ax,0f000h
mov es,ax
mov ss,ax
mov sp,offset firstbyte
mov ax,cs
mov ds,ax
sub si,si
sub di,di
mov cx,offset lastbyte
rep
  movsb
push es
push word offset copyCode2final_j2
retf
copyCode2final_j1:
mov es,ax
sub di,di
mov ax,0ffffh
mov cx,8000h
rep
  stosw
ret
copyCode2final_j2:
push gs
mov ax,cs
mov es,ax
mov ds,ax
mov fs,ax
mov gs,ax
ret
endp
;-------------------------------

;-------------------------------
proc parseParams
mov ax,cs
mov ds,ax
mov es,ax
sub si,si
mov def:[data_temp9],si
mov al,def:[si]
or al,al
jnz byte parseParams_j1
mov si,offset main_txt03
call write
mov si,offset textCRLF
call write
jmp terminate
parseParams_j2:                 ;get param...
mov di,offset data_temp0
inc di
mov si,def:[data_temp9]
parseParams_j10:
lodsb
cmp al,20h
je byte parseParams_j10
dec si
parseParams_j3:
lodsb
cmp al,20h
je byte parseParams_j4
or al,al
jz byte parseParams_j4
stosb
jmp byte parseParams_j3
parseParams_j4:
dec si
mov def:[data_temp9],si
mov dx,di
sub ax,ax
stosw
mov si,offset data_temp0
inc si
sub dx,si
mov def:[si-1],dl
ret
parseParams_j6:                 ;get letter...
lodsb
call lowerCase
sub al,'a'
and al,3
movzx eax,al
imul ebp,eax,4
ret
parseParams_j1:                 ;parse next param...
call parseParams_j2
lodsb
cmp al,'-'
je byte parseParams_j5
dec si
parseParams_j5:
lodsb
call lowerCase
cmp al,'f'
je parseParams_j7
cmp al,'h'
je parseParams_j8
cmp al,'b'
je parseParams_j9
cmp al,'s'
je parseParams_j11
or al,al
jnz parseParams_j1
ret
parseParams_j7:                 ;floppy...
call parseParams_j6
call parseParams_j2
dec si
call diskio_open
call diskio_floppy1440
jmp parseParams_j1
parseParams_j8:                 ;hard...
call parseParams_j6
call parseParams_j2
dec si
add bp,16
call diskio_open
jmp parseParams_j1
parseParams_j9:                 ;boot...
call parseParams_j6
cmp al,2
setae ah
shl ah,7
and al,1
or al,ah
mov def:[main_txt04],al
jmp parseParams_j1
parseParams_j11:                ;serial port...
call parseParams_j6
call parseParams_j2
dec si
mov di,offset data_temp1
mov cx,256
rep
  movsb
call parseParams_j2
lodsb
sub al,'0'
movzx dx,al
mov si,offset data_temp1
call serio_open
jmp parseParams_j1
endp
;-------------------------------



;------------------------------- includes...
include biosemu_int10.inc       ;interrupt handler...
include biosemu_int11.inc       ;interrupt handler...
include biosemu_int12.inc       ;interrupt handler...
include biosemu_int13.inc       ;interrupt handler...
include biosemu_int14.inc       ;interrupt handler...
include biosemu_int15.inc       ;interrupt handler...
include biosemu_int16.inc       ;interrupt handler...
include biosemu_int17.inc       ;interrupt handler...
include biosemu_int19.inc       ;interrupt handler...
include biosemu_int1a.inc       ;interrupt handler...
include biosemu_intXX.inc       ;interrupt handler...
include biosemu_disk.inc        ;disk io...
include biosemu_keyb.inc        ;keyboard io...
include biosemu_port.inc        ;port io...
include biosemu_serial.inc      ;serial io...
include biosemu_syscall.inc     ;kernel calls...
include biosemu_timer.inc       ;timer io...
include biosemu_utils.inc       ;general functions...
include biosemu_video.inc       ;video io...
;-------------------------------
lastbyte:
