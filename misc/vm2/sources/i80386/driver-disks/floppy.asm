org 0h
use32
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 169000h                      ;data
dd 4096                         ;stack
;-------------------------------

;-------------------------------
mov esi,offset text01
call dword writeCodeStr

sub eax,eax
mov def:[dataBlock_pipLin],eax
mov al,80
mov def:[dataBlock_maxCyl],eax
mov al,2
mov def:[dataBlock_maxHed],eax
mov al,18
mov def:[dataBlock_maxSec],eax
mov byte def:[dataBlock_gapLen],1bh
mov byte def:[dataBlock_gap4fo],6ch
mov byte def:[dataBlock_secSiz],2
mov byte def:[dataBlock_frmFil],0f6h

mov eax,def:[dataBlock_maxCyl]
imul eax,def:[dataBlock_maxHed]
imul eax,def:[dataBlock_maxSec]
mov def:[dataBlock_maxTot],eax

clts                            ;get uptime info...
dd 2ah
mov def:[dataBlock_tickPS],edx

mov al,1
clts                            ;switch io accessible mode...
dd 04h

mov al,base_irq
mov esi,offset fdc_irqHandler
lea edi,def:[esp-2048]
clts                            ;hook irq line...
dd 05h
or ebx,ebx
jnz dword vege

mov ecx,1024
clts                            ;allocate dma-able memory...
dd 25h
or ebx,ebx
jnz dword vege
mov def:[dataBlock_dmaMem],edi

call dword fdc_enable
call dword fdc_senseInt
call dword fdc_reconfigure
call dword cacheClear
call dword fdc_recalibrate
call dword fdc_disable
call dword fdc_senseInt

clts                            ;start listening...
dd 14h
or ebx,ebx
jnz dword vege

sub esi,esi
mov def:[esi],esi
mov ecx,4
clts                            ;write to console...
dd 20h

jmp byte main_j2

main_j1:
mov eax,def:[dataBlock_pipLin]
clts                            ;close pipeline side...
dd 18h
call dword cacheClear
call dword fdc_recalibrate
call dword fdc_disable
call dword fdc_senseInt
main_j2:
clts                            ;give away the control...
dd 01h
clts                            ;get next incoming pipeline...
dd 16h
or ebx,ebx
jnz byte main_j2
mov def:[dataBlock_pipLin],eax
clts                            ;get pipeline info...
dd 19h
mov def:[dataBlock_prcNum],eax
mov edi,dataBlock_freMem
clts                            ;get other process name...
dd 0bh
test dl,40h
jz byte main_j1
mov esi,dataBlock_freMem
mov dword def:[esi],1
mov ecx,4
mov eax,def:[dataBlock_pipLin]
clts                            ;nonblocking send through pipeline...
dd 1ah
mov ecx,16
main_j3:
clts                            ;give away the control...
dd 01h
loopd main_j3
mov edi,dataBlock_freMem
mov ecx,1024
mov eax,def:[dataBlock_pipLin]
clts                            ;nonblocking receive through pipeline...
dd 1bh
or ebx,ebx
jnz dword main_j1
cmp ecx,4
jne dword main_j1
mov eax,def:[dataBlock_freMem]
or eax,eax
jnz dword main_j1

call dword fdc_enable
call dword fdc_senseInt
call dword fdc_senseChange

main_j4:
mov eax,def:[dataBlock_pipLin]
clts                            ;get pipeline info...
dd 19h
or eax,eax
jz dword main_j1
clts                            ;give away the control...
dd 01h
mov edi,dataBlock_freMem
mov ecx,1024
mov eax,def:[dataBlock_pipLin]
clts                            ;nonblocking receive through pipeline...
dd 1bh
or ebx,ebx
jnz byte main_j4
cmp ecx,4
jb byte main_j4
mov eax,def:[dataBlock_freMem]
lea eax,def:[commandList_beg+eax*2]
cmp eax,offset commandList_end
jae dword main_j1
movzx word eax,cs:[eax]
mov edi,dataBlock_freMem
call eax
mov esi,dataBlock_freMem
mov ecx,edi
sub ecx,esi
mov eax,def:[dataBlock_pipLin]
clts                            ;nonblocking send through pipeline...
dd 1ah
jmp dword main_j4

vege:
mov esi,offset text02
call dword writeCodeStr
sub eax,eax
clts                            ;terminate process...
dd 00h
;-------------------------------

;-------------------------------
commandList_beg:
dw offset command00
dw offset command01
dw offset command02
commandList_end:
;-------------------------------

;-------------------------------
proc cacheClear
sub eax,eax
mov edi,dataBlock_secFlg
mov ecx,180
rep
  stosd ptr32
retnd
endp
;-------------------------------

;-------------------------------
proc command00
sub eax,eax
stosd ptr32
mov ax,base_prt
stosw ptr32
mov al,base_dev
stosb ptr32
mov al,0
stosb ptr32
mov eax,def:[dataBlock_maxCyl]
stosd ptr32
mov eax,def:[dataBlock_maxHed]
stosd ptr32
mov eax,def:[dataBlock_maxSec]
stosd ptr32
sub eax,eax
stosd ptr32
mov eax,def:[dataBlock_maxTot]
stosd ptr32
call dword command00_j1
call dword command00_j1
call dword command00_j1
retnd
command00_j1:
push edi
mov esi,offset text11
mov ecx,16
rep
  movsb ptr32
pop edi
add edi,256
retnd
endp
;-------------------------------

;-------------------------------
proc command01
mov esi,dataBlock_freMem
mov dword def:[esi],14
mov eax,def:[esi+4]
cmp eax,def:[dataBlock_maxTot]
jae byte command01_vege
mov def:[dataBlock_curTot],eax
bt def:[dataBlock_secFlg],eax
jc byte command01_j1
call dword convLba2chs
call dword fdc_seekHeads
call dword fdc_readSector
setc al
movzx eax,al
mov edi,dataBlock_freMem
stosd ptr32
mov eax,def:[dataBlock_curTot]
stosd ptr32
mov esi,def:[dataBlock_dmaMem]
mov ecx,80h
rep
  movsd ptr32
mov edi,def:[dataBlock_curTot]
bts def:[dataBlock_secFlg],edi
imul edi,512
add edi,dataBlock_secDat
mov esi,def:[dataBlock_dmaMem]
mov ecx,80h
rep
  movsd ptr32
command01_vege:
mov edi,dataBlock_freMem
add edi,520
retnd
command01_j1:
mov esi,offset text12
call dword writeCodeStr
mov edi,dataBlock_freMem
sub eax,eax
stosd ptr32
mov eax,def:[dataBlock_curTot]
stosd ptr32
mov esi,def:[dataBlock_curTot]
imul esi,512
add esi,dataBlock_secDat
mov ecx,80h
rep
  movsd ptr32
jmp byte command01_vege
endp
;-------------------------------

;-------------------------------
proc command02
mov esi,dataBlock_freMem
mov dword def:[esi],14
mov eax,def:[esi+4]
cmp eax,def:[dataBlock_maxTot]
jae byte command02_vege
mov def:[dataBlock_curTot],eax
call dword convLba2chs
call dword fdc_seekHeads
mov edi,def:[dataBlock_dmaMem]
mov esi,dataBlock_freMem
add esi,8
mov ecx,80h
rep
  movsd ptr32
call dword fdc_WriteSector
setc al
movzx eax,al
mov ecx,eax
mov edi,dataBlock_freMem
stosd ptr32
mov eax,def:[dataBlock_curTot]
stosd ptr32
or ecx,ecx
jnz byte command02_vege
mov edi,def:[dataBlock_curTot]
bts def:[dataBlock_secFlg],edi
imul edi,512
add edi,dataBlock_secDat
mov esi,def:[dataBlock_dmaMem]
mov ecx,80h
rep
  movsd ptr32
command02_vege:
mov edi,dataBlock_freMem
add edi,8
retnd
endp
;-------------------------------


;-------------------------------
proc convLba2chs
;in:  eax-lba sector number...
;out: al-cylinder...
;     bl-head...
;     cl-sector...
mov ecx,def:[dataBlock_maxSec]
sub edx,edx
div ecx
inc edx
push edx
mov ecx,def:[dataBlock_maxHed]
sub edx,edx
div ecx
mov ebx,edx
pop ecx
retnd
endp
;-------------------------------






;-------------------------------
proc timer_releq
clts                            ;give away the control...
dd 01h
retnd
endp
;-------------------------------

;-------------------------------
proc timer_past
;in:  ebp-timer value...
;     eax-seconds past...
push edx
clts                            ;get uptime info...
dd 2bh
sub eax,ebp
sub edx,edx
div dword def:[dataBlock_tickPS]
pop edx
retnd
endp
;-------------------------------

;-------------------------------
proc timer_delay
;in: esi-ticks to wait...
clts                            ;get uptime info...
dd 2bh
mov ebp,eax
timer_delay_j1:
clts                            ;give away the control...
dd 01h
clts                            ;get uptime info...
dd 2bh
sub eax,ebp
sub eax,esi
js byte timer_delay_j1
retnd
endp
;-------------------------------

;-------------------------------
proc timer_start
;out: ebp-timer value....
push eax
clts                            ;get uptime info...
dd 2bh
mov ebp,eax
pop eax
retnd
endp
;-------------------------------

;-------------------------------
proc writeCodeStr
;in: cs:esi-offset of text
push esi
push edi
push ecx
push eax
sub ecx,ecx
mov edi,dataBlock_wrtBuf
writeCodeStr_j1:
inc ecx
lodsb cs,ptr32
stosb ptr32
or al,al
jnz byte writeCodeStr_j1
dec ecx
mov esi,dataBlock_wrtBuf
clts                            ;write to console...
dd 20h
pop eax
pop ecx
pop edi
pop esi
retnd
endp
;-------------------------------

;-------------------------------
include floppy.inc
;-------------------------------

;-------------------------------
textCRLF db 13,10,0
text01 db 'floppy driver v1.0, done by Mc at ',%date,' ',%time,'.',13,10,0
text02 db ' error!',13,10,0
text03 db 'write sector...',0
text04 db 'read sector...',0
text05 db 'seek head...',0
text06 db 'recalibrate...',0
text07 db 'reconfigure...',0
text08 db 'turn off floppy...',13,10,0
text09 db 'turn on floppy...',13,10,0
text10 db ' ok!',13,10,0
text11 db 6,'floppy',0
text12 db 'cache hit, good!',13,10,0
;-------------------------------

lastbyte:
