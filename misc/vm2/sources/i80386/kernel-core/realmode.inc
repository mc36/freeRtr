use16                           ;these codes are all in real mode!
;-------------------------------



;------------------------------- write to console...
proc realmode_write
realmode_write_j1:
lodsb cs,ptr16
or al,al
jz byte realmode_write_j2
mov ah,0eh
int 10h
jmp byte realmode_write_j1
realmode_write_j2:
retnw
endp
;-------------------------------



;------------------------------- test for 386+ cpu...
proc realmode_test386
;out: ax=0 if no 386+ cpu found...
pushfw
pushfw
pop ax
or ax,realmode_test386_d1
push ax
popfw
pushfw
pop ax
popfw
and ax,realmode_test386_d1
retnw
endp
realmode_test386_d1 equ 4000h
;-------------------------------

;------------------------------- test for protected mode...
proc realmode_testPM
;out: al=1 if already in protected mode...
smsw ax
and al,1
retnw
endp
;-------------------------------



;------------------------------- skip 4 bytes from cs...
proc realmode_skipInst
pop word cs:[realmode_skipInst_d1]   ;get back ip...
add sp,4                             ;skip cs and flags...
add word cs:[realmode_skipInst_d1],4 ;skip 4 bytes...
stc                                  ;set error flag...
jmp word cs:[realmode_skipInst_d1]   ;execute code...
realmode_skipInst_d1 dw ?
endp
;-------------------------------

;------------------------------- install interrupt skipper..
proc realmode_instSkip
sub ax,ax                       ;hook int6...
mov gs,ax
mov ax,cs
shl eax,16
mov ax,offset realmode_skipInst
mov gs:[18h],eax
retnw
endp
;-------------------------------



;------------------------------- get type of cpu...
proc realmode_cpuType
call word realmode_instSkip
mov di,offset cpuManufacturer   ;target offset...
sub eax,eax                     ;try cpuid's 1st page...
cpuid
clc
clc
jnc byte realmode_cpuType_j3
mov eax,303869h                 ;no cpuid instruction...
stosd ptr16
dec di
pushfd
pop eax
push eax
mov ecx,eax
xor eax,realmode_cpuType_d1
push eax
popfd
pushfd
pop eax
and eax,realmode_cpuType_d1
and ecx,realmode_cpuType_d1
cmp eax,ecx
setne dl
popfd
add dl,3
mov cs:[cpuFamilyCode],dl
add dl,'0'
mov al,dl
stosb ptr16
mov ax,3638h
stosw ptr16
retnw
realmode_cpuType_j3:            ;has cpuid instruction...
mov eax,ebx
stosd ptr16
mov eax,edx
stosd ptr16
mov eax,ecx
stosd ptr16
mov eax,303820h
stosd ptr16
dec di
sub eax,eax                     ;read 2nd page...
inc ax
cpuid
clc
clc
ror eax,8
mov cl,al
and cl,0fh
mov cs:[cpuFamilyCode],cl
call word realmode_cpuType_j1
push eax
mov ax,3638h
stosw ptr16
mov al,'.'
stosb ptr16
pop eax
rol eax,4
call word realmode_cpuType_j1
push eax
mov al,'.'
stosb ptr16
pop eax
rol eax,4
call word realmode_cpuType_j1
retnw
realmode_cpuType_j1:
push ax
and al,0fh
cmp al,10
jb byte realmode_cpuType_j2
push ax
mov al,'1'
stosb ptr16
pop ax
sub al,10
realmode_cpuType_j2:
add al,'0'
stosb ptr16
pop ax
retnw
realmode_cpuType_d1 equ 40000h
endp
;-------------------------------



;------------------------------- enable a20 line...
proc realmode_setA20
call word realmode_setA20_j1
mov al,0d1h
mov dx,64h
out dx,al
call word realmode_setA20_j1
mov al,0dfh
mov dx,60h
out dx,al
call word realmode_setA20_j1
retnw
realmode_setA20_j1:
call word realmode_setA20_j3
mov dx,64h
in al,dx
test al,1
jz byte realmode_setA20_j2
call word realmode_setA20_j3
mov dx,60h
in al,dx
jmp byte realmode_setA20_j1
realmode_setA20_j2:
test al,2
jnz byte realmode_setA20_j1
retnw
realmode_setA20_j3:
jmp byte realmode_setA20_j4
realmode_setA20_j4:
retnw
endp
;-------------------------------

;------------------------------- test a20 line enabled...
proc realmode_testA20
;out: al-0 if enabled...
push cx
push si
push di
xor ax,ax
mov fs,ax
dec ax
mov gs,ax
mov si,400h
mov di,si
add di,10h
mov al,1
mov ah,2
mov cl,fs:[si]                  ;get byte from 0:0
mov ch,gs:[di]                  ;get byte from 0ffffh:10h
mov fs:[si],al                  ;put al to 0:0
mov gs:[di],ah                  ;put ah to 0ffffh:10h
cmp fs:[si],al                  ;is al at 0:0?
setnz al                        ;0-yes 1-none
mov fs:[si],cl                  ;put byte to 0:0
mov gs:[di],ch                  ;put byte to 0ffffh:10h
pop di
pop si
pop cx
retnw
endp
;-------------------------------

;------------------------------- try to enable a20 line...
proc realmode_doA20work
;out: al=0 if successful...
mov cx,4096
realmode_doA20work_j1:
call word realmode_setA20
call word realmode_testA20
or al,al
jz byte realmode_doA20work_j2
loopw byte realmode_doA20work_j1
realmode_doA20work_j2:
retnw
endp
;-------------------------------



;------------------------------- get size of memory from cmos...
proc realmode_getMemSize
;out: eax-size in pages
mov dx,70h
mov al,31h
out dx,al
inc dx
in al,dx
mov ah,al
dec dx
mov al,30h
out dx,al
inc dx
in al,dx
movzx eax,ax
add eax,400h
shr eax,2
mov ebx,255
not ebx
and eax,ebx
retnw
endp
;-------------------------------



;------------------------------- setup programmable interrupt controller...
proc realmode_setupPIC
;cl-master begin  ch-slave begin
mov al,11h                         ;initialization sequence...
call word realmode_setupPIC_out20  ;send it to 8259A-1...
call word realmode_setupPIC_outA0  ;and to 8259A-2...
mov al,cl                          ;start of hardware ints 1...
call word realmode_setupPIC_out21
mov al,ch                          ;start of hardware ints 2...
call word realmode_setupPIC_outA1
mov al,04h                         ;8259-1 is master...
call word realmode_setupPIC_out21
mov al,02h                         ;8259-2 is slave...
call word realmode_setupPIC_outA1
mov al,01h                         ;8086 mode for both...
call word realmode_setupPIC_out21
call word realmode_setupPIC_outA1
mov al,0h                          ;there are no disabled interrupts
call word realmode_setupPIC_out21
call word realmode_setupPIC_outA1
mov al,20h                         ;sign end of pending irq!
call word realmode_setupPIC_out20
call word realmode_setupPIC_outA0
retnw
realmode_setupPIC_end:
jmp byte realmode_setupPIC_end_j1
realmode_setupPIC_end_j1:
retnw
realmode_setupPIC_out20:
mov dx,20h
out dx,al
jmp byte realmode_setupPIC_end
realmode_setupPIC_out21:
mov dx,21h
out dx,al
jmp byte realmode_setupPIC_end
realmode_setupPIC_outA0:
mov dx,0a0h
out dx,al
jmp byte realmode_setupPIC_end
realmode_setupPIC_outA1:
mov dx,0a1h
out dx,al
jmp byte realmode_setupPIC_end
endp
;-------------------------------

;------------------------------- setup real time clock...
proc realmode_setupRTC
;in: ecx-timer value
or ecx,ecx
setz al
add cl,al
mov eax,1193280
sub edx,edx
div ecx
cmp eax,1
jg byte realmode_setupRTC_j1
sub eax,eax
inc ax
realmode_setupRTC_j1:
cmp eax,0ffffh
jl byte realmode_setupRTC_j2
sub eax,eax
dec ax
realmode_setupRTC_j2:
mov dx,ax
mov al,36h
out 43h,al
mov al,dl
out 40h,al
mov al,dh
out 40h,al
retnw
endp
;-------------------------------

;------------------------------- setup timer values...
proc realmode_setupTicks
mov eax,15180h
imul eax,TimerClicksPerSec
mov cs:[TicksPerOneDay],eax
retnw
endp
;-------------------------------

;------------------------------- setup cmos...
proc realmode_setupCMOS
mov ax,0a26h                       ;control register a
call word realmode_setupCMOS_j1
mov ax,0b02h                       ;control register b
call word realmode_setupCMOS_j1
mov ax,0c00h                       ;control register c
call word realmode_setupCMOS_j1
retnw
realmode_setupCMOS_j1:
mov dx,70h
xchg al,ah
out dx,al
mov al,ah
inc dx
out dx,al
retnw
endp
;-------------------------------



;------------------------------- write one idt entry...
proc realmode_writeIDT
;in: al-int number...
;    bx-selector...
;    edx-offset...
movzx di,al                     ;each entry is 8 bytes long....
shl di,3
add di,offset IntrDescrTable
mov ax,dx                       ;offset 0-15...
stosw ptr16
mov ax,bx                       ;selector...
stosw ptr16
mov ax,8e00h                    ;type, etc fields...
stosw ptr16
mov eax,edx                     ;offset 16-31..
shr eax,16
stosw ptr16
retnw
endp
;-------------------------------

;------------------------------- write one gdt entry...
proc realmode_writeGDT
;in: al-selector...
;    ebx-base...
;    edx-limit...
and al,0f8h                     ;each entry is 8 bytes long....
movzx di,al
add di,offset GlobalDescrTab
mov al,def:[di+6]               ;test G bit...
and al,80h
jz byte realmode_writeGDT_j1
shr edx,12
realmode_writeGDT_j1:
mov ax,dx                       ;limit 0-15...
stosw ptr16
mov ax,bx                       ;base 0-15...
stosw ptr16
mov eax,ebx                     ;base 16-23...
shr eax,16
stosb ptr16
inc di                          ;type...
mov eax,edx                     ;limit 16-19...
shr eax,16
and al,0fh
mov ah,def:[di]
and ah,0f0h
or al,ah
stosb ptr16
mov eax,ebx                     ;base 24-31...
shr eax,24
stosb ptr16
retnw
endp
;-------------------------------
