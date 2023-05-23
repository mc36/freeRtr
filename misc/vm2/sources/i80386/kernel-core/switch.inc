use16                           ;begins in real, finishes in prot...
;-------------------------------



;-------------------------------
proc switch2prot
;who knows...
use16
cli
cld

;copy (and execute) the kernel to it's final position...
mov eax,KernelPhyOfs
shr eax,4
mov es,ax
sub di,di
sub si,si
mov bp,cs
mov cx,offset lastbyte
rep
  movsb cs,ptr16
mov ecx,offset firstbyte
mov ss,ax
mov esp,ecx
mov ds,ax
push ax
push word offset switch2prot_j0
retfw
switch2prot_j0:
call word realmode_instSkip

;copy the first process just after the kernel...
mov fs,bp
mov di,offset lastbyte
mov ecx,fs:[di+execHdr_codSiz]
add ecx,0fffh
and ecx,7f000h
mov fs:[di+execHdr_codSiz],ecx
shr ecx,10
mov ebx,offset lastbyte
add ebx,0fffh
and bx,0f000h
add ebx,KernelPhyOfs
mov cs:[0],ebx
shr ebx,4
switch2prot_j1:
push cx
mov ds,bp
mov es,bx
mov si,offset lastbyte
sub di,di
mov cx,200h
rep
  movsw ptr16
pop cx
add bp,40h
add bx,40h
loopw switch2prot_j1
mov ax,cs
mov ds,ax
mov es,ax
sub ax,ax
mov fs,ax
mov gs,ax

;get memory size in pages...
call word realmode_getMemSize
mov def:[MemorySizePages],eax

;setup descriptor tables...
mov eax,offset GlobalDescrTab
mov cx,offset GlobDescrLoad
sub cx,ax
add eax,KernelPhyOfs
mov def:[GlobDescrSize],cx
mov def:[GlobDescrBase],eax
mov eax,offset IntrDescrTable
mov cx,offset IntrDescrLoad
sub cx,ax
add eax,KernelPhyOfs
mov def:[IntrDescrSize],cx
mov def:[IntrDescrBase],eax

;setup gdt...
mov si,offset protmode_gdtData1
switch2prot_j2:
lodsb cs,ptr16
push ax
lodsd cs,ptr16
mov ebx,eax
lodsd cs,ptr16
mov edx,eax
pop ax
call word realmode_writeGDT
cmp si,offset protmode_gdtData2
jb byte switch2prot_j2

;setup idt...
mov si,offset protmode_idtData1
switch2prot_j3:
lodsb cs,ptr16
push ax
lodsw cs,ptr16
movzx edx,ax
pop ax
mov bx,sel_KernelHI
call word realmode_writeIDT
cmp si,offset protmode_idtData2
jb byte switch2prot_j3

;initialize pic... master to 40h, slave to 48h...
mov cl,IRQbeginNumber
mov ch,8
add ch,cl
call word realmode_setupPIC

;initialize rtc...
mov ecx,TimerClicksPerSec
call word realmode_setupRTC
call word realmode_setupTicks

;initialize cmos...
call word realmode_setupCMOS

;setup eflags...
pushfd
pop eax
btr eax,0      ;CF: carry flag
bts eax,1      ;reserved, 1
btr eax,2      ;PF: parity flag
btr eax,3      ;reserved, 0
btr eax,4      ;AF: auxilliary carry
btr eax,5      ;reserved, 0
btr eax,6      ;ZF: zero flag
btr eax,7      ;SF: sign flag
btr eax,8      ;TF: trap flag
btr eax,9      ;IF: interrupt flag
btr eax,10     ;DF: direction flag
btr eax,11     ;OF: overflow flag
btr eax,12     ;IOPL: i/o privilege level /low/
btr eax,13     ;IOPL: i/o privilege level /high/
btr eax,14     ;NT: nested task
btr eax,15     ;reserved, 0
btr eax,16     ;RF: resume flag
btr eax,17     ;VM: virtual mode flag
btr eax,18     ;AC: alignment check /486+/
btr eax,19     ;VIF: virtual interrupt flag
btr eax,20     ;VIP: virtual interrupt pending
bts eax,21     ;ID: id flag
push eax
popfd
bts eax,9      ;IF: interrupt flag
mov cs:[DefaultFlagsReg],eax

;setup machine status word (cr0)...
mov eax,cr0
clc
btr eax,0      ;PE: protection enable
btr eax,1      ;MP: math present
bts eax,2      ;EM: emulation
bts eax,3      ;TS: task switched
bt  eax,4      ;ET: extension type
btr eax,5      ;NE: numeric error
btr eax,16     ;WP: write protect
btr eax,18     ;AM: alignment mask
btr eax,29     ;NW: not write-through
btr eax,30     ;CD: cache disabled
btr eax,31     ;PG: paging
mov cr0,eax
clc

;setup paging control word (cr3)...
sub eax,eax
mov cr3,eax
clc

;setup extended machine control (cr4)...
mov eax,cr4
clc
btr eax,0      ;VME: virtual mode extension
btr eax,1      ;PVI: protected mode virtual interrupts
btr eax,2      ;TSD: time stamp instruction disable
btr eax,3      ;DE: debugging extension
btr eax,4      ;PSE: page size extension
btr eax,5      ;PAE: physical address extension
btr eax,6      ;MCE: machine check enable
btr eax,7      ;PGE: page global enable
btr eax,8      ;PCE: performance counters enable
btr eax,9      ;FSR: fast fp context save/restore enable
mov cr4,eax
clc

;clear last page fault linear address (cr2)...
sub eax,eax
mov cr2,eax
clc

;setup breakpoints (dr7)...
mov eax,dr7
clc
btr eax,0      ;L0: local enable breakpoint # 0
btr eax,1      ;G0: global enable breakpoint # 0
btr eax,2      ;L1: local enable breakpoint # 1
btr eax,3      ;G1: global enable breakpoint # 1
btr eax,4      ;L2: local enable breakpoint # 2
btr eax,5      ;G2: global enable breakpoint # 2
btr eax,6      ;L3: local enable breakpoint # 3
btr eax,7      ;G3: global enable breakpoint # 3
btr eax,8      ;LE: local exact
btr eax,9      ;GE: global exact
mov dr7,eax
clc

;load gdt and idt...
lgdt dword cs:[GlobDescrLoad]
lidt dword cs:[IntrDescrLoad]

;switch to protected mode...
mov eax,cr0
or al,1
mov cr0,eax

;load cs:eip with selector...
jmp dword sel_KernelLO:offset switch2prot_j4
switch2prot_j4:
use32

;setup stack and segment registers...
mov eax,sel_raw4gbMem
mov ecx,offset firstbyte
add ecx,KernelPhyOfs
mov ss,ax
mov esp,ecx
mov es,ax
mov ds,ax
mov fs,ax
mov gs,ax

;build memory tables...
call dword protmode_getMemSize
call dword protmode_buildMemUsage
call dword protmode_buildPageTab
call dword switch2prot_j6

;enable paging...
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,cr0
bts eax,31
mov cr0,eax

;load cs:eip with selector...
jmp dword sel_KernelHI:offset switch2prot_j5
switch2prot_j5:

;load task register...
mov eax,sel_defaultTSS
ltr ax

;setup stack and segment registers...
mov eax,sel_KernelStk
mov ecx,KernelStkSiz
mov ss,ax
mov esp,ecx
mov eax,sel_raw4gbMem
mov es,ax
mov ds,ax
mov fs,ax
mov gs,ax

;update descriptor tables...
mov eax,KernelLogOfs
mov edi,KernelPhyOfs
sub eax,edi
add def:[edi+IntrDescrBase],eax
add def:[edi+GlobDescrBase],eax

;load gdt and idt...
lgdt dword cs:[GlobDescrLoad]
lidt dword cs:[IntrDescrLoad]

;create starter tasks...
call dword protmode_buildInitTasks
call dword switch2prot_j6

;setup hardware...
call dword irqMaskUnneeded

;start current process...
jmp dword process_startCurr

switch2prot_j6:                 ;test for out of memory...
mov eax,cs:[LinearPagingTab]
mov cr3,eax
sub ebx,ebx
call dword memmap_count
cmp edx,8
jb dword switch2reboot
retnd
endp
;-------------------------------




;-------------------------------
proc switch2reboot
;who knows...
use32
cli
cld
mov ebp,4096                    ;number of times to try...
switch2reboot_j1:
mov ecx,10000h
switch2reboot_j2:
in al,64h                       ;read keyboard status port...
and al,2                        ;test for it's busy...
jz byte switch2reboot_j3
loopd switch2reboot_j2
switch2reboot_j3:
mov al,0feh                     ;pulse reset pin...
out 64h,al                      ;send it to the keyboard controller...
dec ebp
jns byte switch2reboot_j1
jmp dword switch2real           ;go back to real mode...
endp
;-------------------------------




;-------------------------------
proc switch2real
;who knows...
use32
cli
cld

;load default paging table...
mov eax,cs:[LinearPagingTab]
mov cr3,eax

;execute kernel at low...
jmp dword sel_code16:offset switch2real_j1
switch2real_j1:
use16

;setup stack and segment registers...
mov eax,sel_data16
mov ecx,offset firstbyte
mov ss,ax
mov esp,ecx
mov es,ax
mov ds,ax
mov fs,ax
mov gs,ax

;setup eflags...
pushfd
pop eax
btr eax,9      ;IF: interrupt flag
btr eax,12     ;IOPL: i/o privilege level /low/
btr eax,13     ;IOPL: i/o privilege level /high/
btr eax,14     ;NT: nested task
btr eax,16     ;RF: resume flag
btr eax,17     ;VM: virtual mode flag
push eax
popfd

;setup machine status word (cr0)...
mov eax,cr0
btr eax,0      ;PE: protection enable
btr eax,2      ;EM: emulation
btr eax,3      ;TS: task switched
btr eax,31     ;PG: paging
mov cr0,eax

;setup paging control word (cr3)...
sub eax,eax
mov cr3,eax

;setup stack, segments and cs:ip...
mov eax,KernelPhyOfs
shr eax,4
mov ecx,offset firstbyte
mov ss,ax
mov esp,ecx
mov es,ax
mov ds,ax
mov fs,ax
mov gs,ax
push ax
push word offset switch2real_j2
retfw
switch2real_j2:

;setup descriptor tables...
sub eax,eax
mov def:[GlobDescrBase],eax
mov def:[IntrDescrBase],eax
mov def:[GlobDescrSize],ax
mov ax,3ffh
mov def:[IntrDescrSize],ax

;load gdt and idt...
lidt dword def:[IntrDescrLoad]
lgdt dword def:[GlobDescrLoad]

;initialize RTC...
sub ecx,ecx
call word realmode_setupRTC

;initialize CMOS...
call word realmode_setupCMOS

;initialize PICs...
mov cl,8
mov ch,70h
call word realmode_setupPIC

;write bye message...
mov si,offset text006
call word realmode_write

jmp word 0ffffh:00000h          ;restart the computer...
endp
;-------------------------------
