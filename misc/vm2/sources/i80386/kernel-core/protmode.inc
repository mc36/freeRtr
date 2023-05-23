use32                           ;not a question...
;-------------------------------

;------------------------------- global descriptor table entries...
protmode_gdtData1:
db sel_raw4gbMem                ;raw 4gb memory read/write...
dd 0,0ffffffffh
db sel_code16                   ;kernel 16bit physical executable...
dd KernelPhyOfs,0ffffh
db sel_data16                   ;kernel 16bit physical read/write...
dd KernelPhyOfs,0ffffh
db sel_KernelLO                 ;kernel physical executable...
dd KernelPhyOfs,offset lastbyte
db sel_KernelHI                 ;kernel mapped executable...
dd KernelLogOfs,offset lastbyte
db sel_KernelRW                 ;kernel mapped read/write...
dd KernelLogOfs,offset lastbyte
db sel_KernelStk                ;kernel stack read/write...
dd KernelStkLog,KernelStkSiz
db sel_KernelBuf                ;kernel buffer read/write...
dd KernelBufOfs,KernelBufSiz
db sel_ProcCode                 ;process code executable...
dd ProcessCodeOfs,ProcessCodeSiz
db sel_ProcData                 ;process data read/write...
dd ProcessDataOfs,ProcessDataSiz
db sel_ProcStak                 ;process stack read/write...
dd ProcessStakOfs,ProcessStakSiz
db sel_defaultTSS               ;default tss read/write...
dd DefaultTSSlog,DefaultTSSsiz
protmode_gdtData2:
;-------------------------------

;------------------------------- interrupt descriptor table entries...
protmode_idtData1:
db 40h                          ;interrupt #0...
dw offset irqHandler_irq00
db 41h                          ;interrupt #1...
dw offset irqHandler_irq01
db 42h                          ;interrupt #2...
dw offset irqHandler_irq02
db 43h                          ;interrupt #3...
dw offset irqHandler_irq03
db 44h                          ;interrupt #4...
dw offset irqHandler_irq04
db 45h                          ;interrupt #5...
dw offset irqHandler_irq05
db 46h                          ;interrupt #6...
dw offset irqHandler_irq06
db 47h                          ;interrupt #7...
dw offset irqHandler_irq07
db 48h                          ;interrupt #8...
dw offset irqHandler_irq08
db 49h                          ;interrupt #9...
dw offset irqHandler_irq09
db 4ah                          ;interrupt #10...
dw offset irqHandler_irq10
db 4bh                          ;interrupt #11...
dw offset irqHandler_irq11
db 4ch                          ;interrupt #12...
dw offset irqHandler_irq12
db 4dh                          ;interrupt #13...
dw offset irqHandler_irq13
db 4eh                          ;interrupt #14...
dw offset irqHandler_irq14
db 4fh                          ;interrupt #15...
dw offset irqHandler_irq15
db 00h                          ;exception #0...
dw offset excHandler_exc00
db 01h                          ;exception #1...
dw offset excHandler_exc01
db 02h                          ;exception #2...
dw offset excHandler_exc02
db 03h                          ;exception #3...
dw offset excHandler_exc03
db 04h                          ;exception #4...
dw offset excHandler_exc04
db 05h                          ;exception #5...
dw offset excHandler_exc05
db 06h                          ;exception #6...
dw offset excHandler_exc06
db 07h                          ;exception #7...
dw offset excHandler_exc07
db 08h                          ;exception #8...
dw offset excHandler_exc08
db 09h                          ;exception #9...
dw offset excHandler_exc09
db 0ah                          ;exception #10...
dw offset excHandler_exc10
db 0bh                          ;exception #11...
dw offset excHandler_exc11
db 0ch                          ;exception #12...
dw offset excHandler_exc12
db 0dh                          ;exception #13...
dw offset excHandler_exc13
db 0eh                          ;exception #14...
dw offset excHandler_exc14
db 0fh                          ;exception #15...
dw offset excHandler_exc15
db 10h                          ;exception #16...
dw offset excHandler_exc16
db 11h                          ;exception #17...
dw offset excHandler_exc17
db 12h                          ;exception #18...
dw offset excHandler_exc18
db 13h                          ;exception #19...
dw offset excHandler_exc19
db 14h                          ;exception #20...
dw offset excHandler_exc20
db 15h                          ;exception #21...
dw offset excHandler_exc21
db 16h                          ;exception #22...
dw offset excHandler_exc22
db 17h                          ;exception #23...
dw offset excHandler_exc23
db 18h                          ;exception #24...
dw offset excHandler_exc24
db 19h                          ;exception #25...
dw offset excHandler_exc25
db 1ah                          ;exception #26...
dw offset excHandler_exc26
db 1bh                          ;exception #27...
dw offset excHandler_exc27
db 1ch                          ;exception #28...
dw offset excHandler_exc28
db 1dh                          ;exception #29...
dw offset excHandler_exc29
db 1eh                          ;exception #30...
dw offset excHandler_exc30
db 1fh                          ;exception #31...
dw offset excHandler_exc31
protmode_idtData2:
;-------------------------------



;------------------------------- convert to hex...
proc protmode_conv2hex
;in: edx-number to write...
;    es:edi-where to write...
push eax
push ecx
mov ecx,8
protmode_conv2hex_j1:
rol edx,4
movzx eax,dl
and al,0fh
mov al,cs:[text034+eax]
stosb ptr32
loopd protmode_conv2hex_j1
pop ecx
pop eax
retnd
endp
;-------------------------------

;------------------------------- convert hex to number...
proc protmode_conv2num
;in:  ds:esi-offset of text...
;out: eax-number converted...
;     carry-cleared if succeeded...
push ebp
push esi
push edi
sub ebp,ebp
protmode_conv2num_j1:
lodsb ptr32
call dword protmode_lowCase
mov ah,al
or al,al
jz byte protmode_conv2num_ok
mov edi,offset text034
protmode_conv2num_j2:
mov al,cs:[edi]
call dword protmode_lowCase
inc edi
or al,al
jz byte protmode_conv2num_err
cmp al,ah
jne byte protmode_conv2num_j2
sub edi,offset text034
shl ebp,4
dec edi
or ebp,edi
jmp byte protmode_conv2num_j1
protmode_conv2num_ok:
mov eax,ebp
clc
protmode_conv2num_vege:
pop edi
pop esi
pop ebp
retnd
protmode_conv2num_err:
sub eax,eax
stc
jmp byte protmode_conv2num_vege
endp
;-------------------------------

;------------------------------- copy asciiz string...
proc protmode_copyAsciiZ
;in: ds:esi-source offset...
;    es:edi-target offset...
push esi
push eax
protmode_copyAsciiZ_j1:
lodsb ptr32
stosb ptr32
or al,al
jnz byte protmode_copyAsciiZ_j1
dec edi
pop eax
pop esi
retnd
endp
;-------------------------------

;------------------------------- lower case a character...
proc protmode_lowCase
;in:  al-char to lower case...
;out: al-char changed to lower case...
cmp al,'A'
jb byte protmode_lowCase_j1
cmp al,'Z'
ja byte protmode_lowCase_j1
add al,20h
protmode_lowCase_j1:
retnd
endp
;-------------------------------

;-------------------------------
proc protmode_getTimePast
;in:  eax-time to test...
;out: eax-time past in seconds...
push edx
push ecx
neg eax
add eax,cs:[StartupPastTick]
jns byte protmode_getTimePast_j1
add eax,cs:[TicksPerOneDay]
protmode_getTimePast_j1:
sub edx,edx
mov ecx,TimerClicksPerSec
div ecx
pop ecx
pop edx
retnd
endp
;-------------------------------

;------------------------------- convert bcd to binary...
proc protmode_convBcd2dec
;in:  al-byte to convert...
;out: eax-value converted...
push ecx
mov ah,0
mov cl,10h
div cl
mov ch,ah
mov cl,10
mul cl
add al,ch
movzx eax,al
pop ecx
retnd
endp
;-------------------------------

;------------------------------- read one byte from cmos...
proc protmode_getCmosBinByte
;in:  al-byte to read...
;     al-value readed...
out 70h,al
in al,71h
retnd
endp
;-------------------------------

;------------------------------- get cmos refresh state...
proc protmode_GetCmosRefresh
;out: zero-cleared if update in progress...
mov al,0ah
call dword protmode_getCmosBinByte
and al,80h
or al,al
retnd
endp
;-------------------------------



;------------------------------- get size of memory by testing...
proc protmode_getMemSize
;out: eax-size in pages
mov ebp,MemoryTestBlock
mov eax,cs:[MemorySizePages]
cmp eax,MemoryTestOver
ja byte protmode_getMemSize_j1
retnd
protmode_getMemSize_j1:
mov al,055h
call dword protmode_getMemSize_j2
jc byte protmode_getMemSize_j4
mov al,0aah
call dword protmode_getMemSize_j2
jc byte protmode_getMemSize_j4
add ebp,MemoryTestBlock
cmp ebp,MemoryTestMax
jb byte protmode_getMemSize_j1
protmode_getMemSize_j4:
mov eax,ebp
shr eax,12
cmp eax,cs:[MemorySizePages]
jb byte protmode_getMemSize_j5
mov edi,KernelPhyOfs
mov def:[edi+MemorySizePages],eax
protmode_getMemSize_j5:
retnd
protmode_getMemSize_j2:
mov ah,al
mov ebx,eax
shl eax,16
mov ax,bx
mov edi,ebp
mov ecx,MemoryTestBlock
shr ecx,2
rep
  stosd ptr32
mov edi,ebp
mov ecx,MemoryTestBlock
shr ecx,2
repe
  scasd ptr32
jnz byte protmode_getMemSize_j3
clc
retnd
protmode_getMemSize_j3:
stc
retnd
endp
;-------------------------------



;------------------------------- build memory usage...
proc protmode_buildMemUsage
;calculate offsets...
mov edi,KernelPhyOfs
mov eax,KernelStkPhy
add eax,KernelStkSiz
mov def:[edi+MemoryUsageMap],eax
mov eax,cs:[MemorySizePages]
add eax,3ffh
shr eax,10
imul ecx,eax,4096
add ecx,cs:[MemoryUsageMap]
mov def:[edi+LinearPagingTab],ecx
;fill memory usage map and paging table...
inc eax
imul ecx,eax,1024
mov edi,cs:[MemoryUsageMap]
sub eax,eax
rep
  stosd ptr32
;allocate kernel memory...
sub eax,eax
mov ecx,cs:[0]
shr ecx,12
mov ebx,KernelProcNum
call dword protmode_buildMemUsage_j1
;allocate rom bios memory...
mov eax,0a0000h
mov ecx,60h
mov ebx,KernelProcNum
call dword protmode_buildMemUsage_j1
;allocate tss memory...
mov eax,DefaultTSSphy
mov ecx,DefaultTSSsiz
add ecx,ecx
shr ecx,12
mov ebx,KernelProcNum
call dword protmode_buildMemUsage_j1
;allocate kernel stack, memory usage map and paging table...
mov eax,KernelStkPhy
mov ecx,cs:[LinearPagingTab]
sub ecx,eax
shr ecx,12
inc ecx
mov ebx,KernelProcNum
call dword protmode_buildMemUsage_j1
;allocate first process code...
mov eax,cs:[0]
mov ebx,cs:[NextNewProcNum]
inc ebx
mov ecx,def:[eax+execHdr_codSiz]
add ecx,0fffh
shr ecx,12
call dword protmode_buildMemUsage_j1
;fill up tss...
mov edi,DefaultTSSphy
mov ecx,DefaultTSSsiz
shr ecx,2
sub eax,eax
rep
  stosd ptr32
mov edi,DefaultTSSphy
mov word def:[edi+66h],01000h
sub eax,eax
stosd ptr32
mov ecx,3
protmode_buildMemUsage_j2:
mov eax,KernelStkSiz
stosd ptr32
mov eax,sel_KernelStk
stosd ptr32
loopd protmode_buildMemUsage_j2
mov esi,DefaultTSSphy
mov ecx,DefaultTSSsiz
lea edi,def:[esi+ecx]
shr ecx,2
rep
  movsd ptr32
mov edi,DefaultTSSphy
mov word def:[edi+66h],08000h
retnd
protmode_buildMemUsage_j1:      ;do one allocation...
call dword memmap_alloc
add eax,4096
loopd protmode_buildMemUsage_j1
retnd
endp
;-------------------------------

;------------------------------- build page table...
proc protmode_buildPageTab
;mapping for physical memory...
mov ecx,cs:[MemorySizePages]
sub esi,esi
sub edi,edi
call dword protmode_buildPageTab_j1
;mapping for kernel at high...
mov ecx,offset lastbyte
shr ecx,12
inc ecx
mov esi,KernelPhyOfs
mov edi,KernelLogOfs
call dword protmode_buildPageTab_j1
;mapping for kernel stack...
mov ecx,KernelStkSiz
shr ecx,12
mov esi,KernelStkPhy
mov edi,KernelStkLog
call dword protmode_buildPageTab_j1
;mapping for tss...
mov ecx,DefaultTSSsiz
add ecx,ecx
shr ecx,12
mov esi,DefaultTSSphy
mov edi,DefaultTSSlog
call dword protmode_buildPageTab_j1
;allocate and map kernel buffer...
mov ecx,KernelBufSiz
shr ecx,12
mov edi,KernelBufOfs
protmode_buildPageTab_j3:
call dword memmap_find
mov esi,eax
mov ebx,KernelProcNum
call dword memmap_alloc
mov edx,cs:[LinearPagingTab]
mov ebx,KernelProcNum
call dword pagtab_write
add edi,4096
loopd protmode_buildPageTab_j3
;create openfiles, processes and pipelines paging tables...
call dword protmode_buildPageTab_j2
mov def:[edi+ProcListPagTab],edx
call dword protmode_buildPageTab_j2
mov def:[edi+OpenFilePagTab],edx
call dword protmode_buildPageTab_j2
mov def:[edi+PipeLinePagTab],edx
retnd
protmode_buildPageTab_j1:       ;do one mapping...
mov edx,cs:[LinearPagingTab]
mov ebx,KernelProcNum
call dword pagtab_writeMore
retnd
protmode_buildPageTab_j2:       ;create one list...
mov ebx,KernelProcNum
call dword pagtab_create
mov esi,KernelStkPhy
mov edi,KernelStkLog
mov ebx,KernelProcNum
call dword pagtab_write
mov ecx,DefaultTSSsiz
shr ecx,12
mov esi,DefaultTSSphy
mov edi,DefaultTSSlog
mov ebx,KernelProcNum
call dword pagtab_writeMore
mov edi,KernelPhyOfs
retnd
endp
;-------------------------------

;------------------------------- build first processes...
proc protmode_buildInitTasks
;ramdrive...
call dword process_create
mov esi,offset text007
mov edi,procStk_pathName
call dword protmode_buildInitTasks_j1
mov esi,offset text035
mov edi,procStk_currDir
call dword protmode_buildInitTasks_j1
mov dword def:[ecx+procStk_right],rights_rootPriv
mov esi,cs:[0]
mov edi,ProcessCodeOfs
mov ebx,eax
mov ecx,def:[esi+execHdr_codSiz]
add ecx,0fffh
shr ecx,12
call dword pagtab_writeMore
call dword process_begRun
;init process...
call dword process_create
mov esi,offset text008
mov edi,procStk_pathName
call dword protmode_buildInitTasks_j1
mov esi,offset text035
mov edi,procStk_currDir
call dword protmode_buildInitTasks_j1
mov dword def:[ecx+procStk_right],rights_rootPriv
mov ebx,eax
call dword memmap_find
mov esi,eax
call dword memmap_alloc
mov edi,ProcessCodeOfs
call dword pagtab_write
mov edi,esi
mov esi,offset initProcess_beg
mov ecx,offset initProcess_end
sub ecx,esi
rep
  movsb cs,ptr32
mov eax,ebx
call dword process_begRun
retnd
protmode_buildInitTasks_j1:
add edi,ecx
push ecx
push eax
push esi
sub ecx,ecx
protmode_buildInitTasks_j2:
lodsb cs,ptr32
inc ecx
or al,al
jnz byte protmode_buildInitTasks_j2
pop esi
lea eax,def:[ecx-1]
stosb ptr32
rep
  movsb cs,ptr32
pop eax
pop ecx
retnd
endp
;-------------------------------
