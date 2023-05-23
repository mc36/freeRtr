;-------------------------------
proc fdc_senseChange
;out: al-0=none, 1=changed...
mov edx,7                       ;dir...
add edx,base_prt
in al,dx
shr al,7
retnd
endp
;-------------------------------

;-------------------------------
proc fdc_processResults
call dword fdc_inDataByte
push eax
call dword fdc_inDataByte
call dword fdc_inDataByte
call dword fdc_inDataByte
call dword fdc_inDataByte
call dword fdc_inDataByte
call dword fdc_inDataByte
call dword fdc_purgeData
mov dl,base_dma
clts                            ;stop dma channel...
dd 29h
pop ecx
mov eax,def:[dataBlock_sawIrq]
or eax,eax
jz byte fdc_processResults_j1
and cl,0c0h
jnz byte fdc_processResults_j1
clc
retnd
fdc_processResults_j1:
mov esi,offset text02
call dword writeCodeStr
stc
retnd
endp
;-------------------------------

;-------------------------------
proc fdc_writeSector
;out: carry cleared if successful...
mov esi,offset text03
call dword writeCodeStr
call dword fdc_purgeData
call dword fdc_clearIrqCount
mov dl,base_dma
sub esi,esi
mov edi,512
mov bh,1
mov ch,2
clts                            ;start dma channel...
dd 28h
mov al,01000101b                ;write data...
call dword fdc_outDataByte
mov al,def:[dataBlock_curHed]
shl al,2
or al,base_dev
call dword fdc_outDataByte
mov al,def:[dataBlock_curCyl]
call dword fdc_outDataByte
mov al,def:[dataBlock_curHed]
call dword fdc_outDataByte
mov al,def:[dataBlock_curSec]
call dword fdc_outDataByte
mov al,def:[dataBlock_secSiz]
call dword fdc_outDataByte
mov al,def:[dataBlock_curSec]
call dword fdc_outDataByte
mov al,def:[dataBlock_gapLen]
call dword fdc_outDataByte
mov al,255
call dword fdc_outDataByte
call dword fdc_wait4irq
call dword fdc_processResults
retnd
endp
;-------------------------------

;-------------------------------
proc fdc_readSector
;out: carry cleared if successful...
mov esi,offset text04
call dword writeCodeStr
call dword fdc_purgeData
call dword fdc_clearIrqCount
mov dl,base_dma
sub esi,esi
mov edi,512
mov bh,1
mov ch,1
clts                            ;start dma channel...
dd 28h
mov al,01000110b                ;read data...
call dword fdc_outDataByte
mov al,def:[dataBlock_curHed]
shl al,2
or al,base_dev
call dword fdc_outDataByte
mov al,def:[dataBlock_curCyl]
call dword fdc_outDataByte
mov al,def:[dataBlock_curHed]
call dword fdc_outDataByte
mov al,def:[dataBlock_curSec]
call dword fdc_outDataByte
mov al,def:[dataBlock_secSiz]
call dword fdc_outDataByte
mov al,def:[dataBlock_curSec]
call dword fdc_outDataByte
mov al,def:[dataBlock_gapLen]
call dword fdc_outDataByte
mov al,255
call dword fdc_outDataByte
call dword fdc_wait4irq
call dword fdc_processResults
retnd
endp
;-------------------------------

;-------------------------------
proc fdc_seekHeads
;in: al-cylinder...
;    bl-head...
;    cl-sector...
;out: carry cleared if successful...
movzx ecx,cl
mov def:[dataBlock_curSec],ecx
mov ah,bl
cmp al,def:[dataBlock_curCyl]
jne byte fdc_seekHeads_j1
cmp ah,def:[dataBlock_curHed]
jne byte fdc_seekHeads_j1
retnd
fdc_seekHeads_j1:
movzx ecx,al
cmp ecx,def:[dataBlock_curCyl]
jae byte fdc_seekHeads_j2
push dword def:[dataBlock_curSec]
push eax
call dword fdc_recalibrate
pop eax
pop dword def:[dataBlock_curSec]
fdc_seekHeads_j2:
mov esi,offset text05
call dword writeCodeStr
movzx ecx,al
mov def:[dataBlock_curCyl],ecx
movzx ecx,ah
mov def:[dataBlock_curHed],ecx
call dword fdc_purgeData
call dword fdc_clearIrqCount
mov al,00001111b                ;seek...
call dword fdc_outDataByte
mov al,def:[dataBlock_curHed]
shl al,2
or al,base_dev
call dword fdc_outDataByte
mov al,def:[dataBlock_curCyl]
call dword fdc_outDataByte
call dword fdc_wait4irq
call dword fdc_senseInt
retnd
endp
;-------------------------------

;-------------------------------
proc fdc_recalibrate
;out: carry cleared if successful...
mov esi,offset text06
call dword writeCodeStr
call dword fdc_purgeData
call dword fdc_clearIrqCount
mov al,00000111b                ;recalibrate...
call dword fdc_outDataByte
mov al,base_dev
call dword fdc_outDataByte
call dword fdc_wait4irq
mov dword def:[dataBlock_curCyl],0
mov dword def:[dataBlock_curHed],0ffh
mov dword def:[dataBlock_curSec],0ffh
call dword fdc_senseInt
retnd
endp
;-------------------------------

;-------------------------------
proc fdc_reconfigure
;out: carry cleared if successful...
mov esi,offset text07
call dword writeCodeStr
mov edx,7                       ;dcr...
add edx,base_prt
mov al,0h
out dx,al
call dword fdc_purgeData
call dword fdc_clearIrqCount
mov byte def:[dataBlock_curCyl],0ffh
mov al,00000011b                ;specify step & head load time...
call dword fdc_outDataByte
mov al,0cfh                     ;e?-fastest, 0?-lowest...
call dword fdc_outDataByte
mov al,02h                      ;head load time...
call dword fdc_outDataByte
call dword fdc_wait4irq
call dword fdc_senseInt
retnd
endp
;-------------------------------

;-------------------------------
proc fdc_senseInt
mov al,00001000b                ;sense interrupt status...
call dword fdc_outDataByte
call dword fdc_inDataByte
call dword fdc_inDataByte
retnd
endp
;-------------------------------



;-------------------------------
proc fdc_inDataByte
;al-byte
call dword fdc_wait4io
mov edx,base_prt
add edx,5
in al,dx
retnd
endp
;-------------------------------

;-------------------------------
proc fdc_outDataByte
;al-byte
push eax
call dword fdc_wait4io
pop eax
mov edx,base_prt
add edx,5
out dx,al
retnd
endp
;-------------------------------

;-------------------------------
proc fdc_purgeData
mov edx,base_prt
add edx,4
mov ecx,1024
fdc_purgeData_j1:
in al,dx
test al,80h
jz byte fdc_purgeData_j1
and al,0c0h
cmp al,0c0h
jne byte fdc_purgeData_j2
inc edx
in al,dx
dec edx
dec ecx
jns byte fdc_purgeData_j1
fdc_purgeData_j2:
retnd
endp
;-------------------------------

;-------------------------------
proc fdc_wait4io
call dword timer_start
fdc_wait4io_j1:
mov edx,base_prt
add edx,4
in al,dx
test al,80h
jnz byte fdc_wait4io_j2
call dword timer_releq
call dword timer_past
cmp al,2
jb byte fdc_wait4io_j1
fdc_wait4io_j2:
retnd
endp
;-------------------------------

;-------------------------------
proc fdc_enable
mov esi,offset text09
call dword writeCodeStr
mov byte def:[dataBlock_curCyl],0ffh
mov edx,2                       ;dor...
add edx,base_prt
mov al,10h
shl al,base_dev
or al,base_dev
or al,0ch
out dx,al
mov esi,def:[dataBlock_tickPS]
shl esi,2
call dword timer_delay
retnd
endp
;-------------------------------



;-------------------------------
proc fdc_disable
mov esi,offset text08
call dword writeCodeStr
mov esi,def:[dataBlock_tickPS]
call dword timer_delay
mov byte def:[dataBlock_curCyl],0ffh
mov edx,2                       ;dor...
add edx,base_prt
mov al,base_dev
out dx,al
mov esi,def:[dataBlock_tickPS]
call dword timer_delay
retnd
endp
;-------------------------------



;-------------------------------
proc fdc_irqHandler
inc dword def:[dataBlock_sawIrq]
clts                            ;terminate irq handler...
dd 07h
jmp byte fdc_irqHandler
endp
;-------------------------------

;-------------------------------
proc fdc_clearIrqCount
sub eax,eax
mov def:[dataBlock_sawIrq],eax
retnd
endp
;-------------------------------

;-------------------------------
proc fdc_wait4irq
;out: carry-cleared if successful...
call dword timer_start
fdc_wait4irq_j1:
mov eax,def:[dataBlock_sawIrq]
or eax,eax
jnz byte fdc_wait4irq_j2
call dword timer_releq
call dword timer_past
cmp al,5
jb byte fdc_wait4irq_j1
mov esi,offset text02
call dword writeCodeStr
stc
retnd
fdc_wait4irq_j2:
mov esi,offset text10
call dword writeCodeStr
clc
retnd
endp
;-------------------------------



;-------------------------------
base_prt equ 3f0h               ;base port number...
base_irq equ 6                  ;base irq number...
base_dma equ 2                  ;base dma number...
base_dev equ 0                  ;equipment number: 0..3...
;-------------------------------

;-------------------------------
dataBlock_wrtBuf equ 0000h      ;128: write buffer...
dataBlock_dmaMem equ 0080h      ;dd: dma-able memory offset...
dataBlock_maxCyl equ 0084h      ;dd: cylinders...
dataBlock_maxHed equ 0088h      ;dd: heads...
dataBlock_maxSec equ 008ch      ;dd: sectors...
dataBlock_gapLen equ 0090h      ;db: gap3 length...
dataBlock_gap4fo equ 0091h      ;db: gap3 length for format...
dataBlock_secSiz equ 0092h      ;db: sector size: 128*2^x...
dataBlock_frmFil equ 0093h      ;db: filler for format...
dataBlock_sawIrq equ 0094h      ;dd: irq occurance counter...
dataBlock_tickPS equ 0098h      ;dd: ticks per second...
dataBlock_maxTot equ 009ch      ;dd: total number of sectors...
dataBlock_curCyl equ 00a0h      ;dd: current cylinder number...
dataBlock_curHed equ 00a4h      ;dd: current head number...
dataBlock_curSec equ 00a8h      ;dd: current sector number...
dataBlock_curTot equ 00ach      ;dd: current total sec number...
dataBlock_pipLin equ 00b0h      ;dd: pipeline number...
dataBlock_prcNum equ 00b4h      ;dd: process number...
dataBlock_freMem equ 00b8h      ;3k: free memory...
dataBlock_secFlg equ 0e80h      ;384: sector buffer valid flags...
dataBlock_secDat equ 1000h      ;1.4m: sector buffer valid flags...
;-------------------------------
