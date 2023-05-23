use16                           ;starts in real mode,
org 100h                        ;and has 256 bytes psp!
firstbyte:
cli                             ;setup cpu...
cld
mov ax,cs                       ;setup stack...
mov cx,offset firstbyte
mov ss,ax
mov sp,cx
mov ds,ax                       ;setup segments...
mov es,ax

;check crc on image...
mov si,offset firstbyte
sub cx,cx
dec cx
mov dx,cx
init_j1:
sub ax,ax
lodsb ptr16
xor al,cl
add ax,ax
add ax,ax
mov di,offset crc32table
add di,ax
mov cl,ch
mov ch,dl
mov dl,dh
mov dh,0
xor cx,def:[di+0]
xor dx,def:[di+2]
cmp si,offset kernelChecksum_lo
jb byte init_j1
not cx
not dx
lodsw ptr16
cmp ax,cx
jne byte init_j2
lodsw ptr16
cmp ax,dx
je byte init_j3
init_j2:
mov ax,0e63h
int 10h
mov ax,0e72h
int 10h
mov ax,0e63h
int 10h
mov ax,0e21h
int 10h
int 20h
init_j3:

mov si,offset textCRLF          ;init message...
call word realmode_write
mov si,offset kernel_id2
call word realmode_write
mov si,offset textCRLF
call word realmode_write

call word realmode_test386
mov si,offset text002
or ax,ax
jnz byte init_j4
init_err:
push si
mov si,offset text001
call word realmode_write
pop si
call word realmode_write
mov si,offset textCRLF
call word realmode_write
int 20h
init_j4:
call word realmode_testPM
mov si,offset text003
or al,al
jnz byte init_err
call word realmode_doA20work
mov si,offset text004
or al,al
jnz byte init_err
call word realmode_getMemSize
mov si,offset text005
cmp eax,200h
jb byte init_err

call word realmode_cpuType      ;get type of cpu...
jmp word switch2prot            ;switch to protected mode...
;-------------------------------


;------------------------------- include files...
include datablk.inc             ;data structures...
include realmode.inc            ;real mode stuff...
include switch.inc              ;mode switch...
include protmode.inc            ;prot mode stuff...
include v86mode.inc             ;v86 mode stuff...
include memory.inc              ;memory stuff...
include dma.inc                 ;dma stuff...
include irq.inc                 ;interrupt handlers...
include exc.inc                 ;exception handlers...
include process.inc             ;process stuff...
include pipeline.inc            ;pipeline stuff...
include fileio.inc              ;fileio stuff...
include syscall0.inc            ;syscall servicer...
include syscall1.inc            ;syscall servicer...
include syscall2.inc            ;syscall servicer...
include syscall3.inc            ;syscall servicer...
include syscall4.inc            ;syscall servicer...
include syscall5.inc            ;syscall servicer...
include crc32.inc               ;crc32 calculator...
;-------------------------------
align 10h
db 12 dup (90h)
kernelChecksum_lo dw ?
kernelChecksum_hi dw ?
lastbyte:
