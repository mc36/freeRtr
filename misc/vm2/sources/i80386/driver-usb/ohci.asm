org 0h
use32
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 8192                         ;data
dd 4096                         ;stack
;-------------------------------

mov esi,offset text01
call dword writeCodeStr
clts                            ;get uptime info...
dd 2ah
mov def:[dataSeg_tckSec],edx
sub eax,eax
mov def:[dataSeg_connNm],eax
mov def:[dataSeg_lastBf],eax

mov edi,dataSeg_wrtBuf
clts                            ;get process parameters...
dd 13h
mov esi,dataSeg_wrtBuf
movzx byte eax,def:[esi]
inc esi
mov def:[esi+eax],ah
call dword str2num
mov esi,offset text02
jc dword vege
or edx,edx
jz dword vege
mov def:[dataSeg_baseAddr],edx

mov ecx,bufferSeg_num
imul ecx,bufferSeg_siz
add ecx,bufferSeg_pack
add ecx,4096
clts                            ;allocate continuous memory...
dd 26h
mov def:[dataSeg_memLog],edi
mov def:[dataSeg_memPhy],eax
mov esi,offset text04
or ebx,ebx
jnz dword vege

mov eax,def:[dataSeg_baseAddr]
mov ecx,1024
clts                            ;map system memory...
dd 03h
mov ebp,def:[dataSeg_baseAddr]
sub ebp,eax
add ebp,edi
mov def:[dataSeg_baseAddr],ebp
mov esi,offset text04
or ebx,ebx
jnz dword vege

call dword ohci_testChip
mov esi,offset text03
jc dword vege

call dword ohci_resetChip
call dword ohci_setupBufs
call dword ohci_resetChip
call dword ohci_setupChip

clts                            ;start listening...
dd 14h
or ebx,ebx
mov esi,offset text05
jnz dword vege

sub esi,esi
mov def:[esi],esi
mov ecx,4
clts                            ;write to console...
dd 20h

main_j1:
clts                            ;give away the control...
dd 01h
mov ecx,def:[dataSeg_connNm]
mov eax,dataSeg_connDt
sub eax,connectSeg_siz
mov def:[dataSeg_wrtBuf],eax
main_j8:
dec ecx
js dword main_j2
add dword def:[dataSeg_wrtBuf],connectSeg_siz
push ecx

;process one connection...
mov esi,def:[dataSeg_wrtBuf]
mov eax,def:[esi+connectSeg_pipe]
clts                            ;get pipeline info...
dd 19h
or ebx,ebx
jnz byte main_j9
or eax,eax
jz byte main_j9
mov eax,def:[esi+connectSeg_actn]
or eax,eax
jnz dword main_j11
mov eax,def:[esi+connectSeg_pipe]
mov edi,dataSeg_freMem
mov ecx,1024
clts                            ;nonblocking receive through pipeline...
dd 1bh
or ebx,ebx
jnz byte main_j10
or ecx,ecx
jz byte main_j10
mov esi,dataSeg_freMem
mov eax,def:[esi]
and eax,0fffffffh
shl eax,2
add eax,offset CommandList_beg
cmp eax,offset CommandList_end
jae byte main_j9
add esi,4
mov edi,esi
mov eax,cs:[eax]
call eax
jc byte main_j10
mov ecx,edi
mov esi,dataSeg_freMem
sub ecx,esi
mov edi,def:[dataSeg_wrtBuf]
mov eax,def:[edi+connectSeg_pipe]
clts                            ;nonblocking send through pipeline...
dd 1ah
main_j10:
pop ecx
jmp dword main_j8
main_j9:
mov esi,def:[dataSeg_wrtBuf]
mov eax,def:[esi+connectSeg_pipe]
clts                            ;close pipeline side...
dd 18h
mov esi,def:[esi+connectSeg_buff]
call dword ohci_descStop
mov edi,def:[dataSeg_wrtBuf]
lea esi,def:[edi+connectSeg_siz]
pop ecx
imul ecx,connectSeg_siz
rep
  movsb ptr32
dec dword def:[dataSeg_connNm]
jmp dword main_j2

;finish action...
main_j11:
mov esi,def:[dataSeg_wrtBuf]
mov esi,def:[esi+connectSeg_buff]
call dword ohci_descStatus
jc dword main_j10
mov ebp,eax
mov esi,def:[dataSeg_wrtBuf]
sub eax,eax
xchg eax,def:[esi+connectSeg_actn]
mov edi,dataSeg_freMem
cmp al,1
je byte main_j12
mov eax,5
stosd ptr32
mov eax,ebp
stosd ptr32
main_j13:
mov esi,def:[dataSeg_wrtBuf]
mov eax,def:[esi+connectSeg_pipe]
mov ecx,edi
mov esi,dataSeg_freMem
sub ecx,esi
clts                            ;nonblocking send through pipeline...
dd 1ah
jmp dword main_j10
main_j12:
mov eax,6
stosd ptr32
mov eax,ebp
stosd ptr32
mov esi,def:[dataSeg_wrtBuf]
mov esi,def:[esi+connectSeg_buff]
call dword ohci_descRead
jmp byte main_j13

;check for incoming connection...
main_j2:
clts                            ;get next incoming pipeline...
dd 16h
or ebx,ebx
jnz dword main_j1
or eax,eax
jz dword main_j1
mov ebp,eax
clts                            ;get pipeline info...
dd 19h
mov edi,dataSeg_freMem
clts                            ;get other process name...
dd 0bh
and edx,40h
jz byte main_j3
mov eax,def:[dataSeg_connNm]
cmp eax,bufferSeg_num
jae byte main_j3
main_j4:
mov eax,def:[dataSeg_lastBf]
inc eax
cmp eax,bufferSeg_num
jb byte main_j5
sub eax,eax
main_j5:
mov def:[dataSeg_lastBf],eax
shl eax,6
add eax,def:[dataSeg_memLog]
lea edi,def:[eax+bufferSeg_desc]
mov ecx,def:[dataSeg_connNm]
mov esi,dataSeg_connDt
main_j6:
dec ecx
js byte main_j7
cmp edi,def:[esi+connectSeg_buff]
je byte main_j4
add esi,connectSeg_siz
jmp byte main_j6
main_j3:
mov eax,ebp
clts                            ;close pipeline side...
dd 18h
jmp dword main_j1
main_j7:
sub eax,eax
mov def:[esi+connectSeg_pipe],ebp
mov def:[esi+connectSeg_buff],edi
mov def:[esi+connectSeg_actn],eax
inc dword def:[dataSeg_connNm]
;mov esi,edi
;call dword ohci_descStop
jmp dword main_j1

mov esi,offset textNONE
vege:
call dword writeCodeStr
sub eax,eax
clts                            ;terminate process...
dd 00h
;-------------------------------

;-------------------------------
CommandList_beg:
dd offset command_portNum,offset command_frame,offset command_portEn
dd offset command_portDis,offset command_portStat,offset command_send
dd offset command_receive
CommandList_end:
;-------------------------------

;-------------------------------
proc command_portNum
mov eax,def:[dataSeg_portNm]
stosd ptr32
clc
retnd
endp
;-------------------------------

;-------------------------------
proc command_frame
call dword ohci_readFrameNum
stosd ptr32
clc
retnd
endp
;-------------------------------

;-------------------------------
proc command_portEn
push edi
lodsd ptr32
call dword ohci_portEnable
pop edi
clc
retnd
endp
;-------------------------------

;-------------------------------
proc command_portDis
push edi
lodsd ptr32
call dword ohci_portDisable
pop edi
clc
retnd
endp
;-------------------------------

;-------------------------------
proc command_portStat
push edi
lodsd ptr32
call dword ohci_portStatus
pop edi
mov ebx,eax
movzx eax,bl
stosd ptr32
movzx eax,bh
stosd ptr32
clc
retnd
endp
;-------------------------------

;-------------------------------
proc command_send
sub ecx,18h
mov def:[dataSeg_freMem],ecx
js byte command_send_j1
cmp ecx,bufferSeg_siz
ja byte command_send_j1
mov esi,def:[dataSeg_wrtBuf]
mov edi,def:[esi+connectSeg_buff]
mov dword def:[esi+connectSeg_actn],2
mov esi,dataSeg_freMem
call dword ohci_beginSending
command_send_j1:
stc
retnd
endp
;-------------------------------

;-------------------------------
proc command_receive
lodsd ptr32
cmp ecx,bufferSeg_siz
ja byte command_receive_j1
mov esi,def:[dataSeg_wrtBuf]
mov edi,def:[esi+connectSeg_buff]
mov dword def:[esi+connectSeg_actn],1
mov esi,dataSeg_freMem
add esi,4
call dword ohci_beginReceiving
command_receive_j1:
stc
retnd
endp
;-------------------------------

;-------------------------------
include utils.inc
include ohci.inc
include memio.inc
;-------------------------------

;-------------------------------
textCRLF db 13,10
textNONE db 0
text01 db 'usb ohci driver v1.0, done by Mc at ',%date,' ',%time,'.',13,10,0
text02 db 'using: ohci.code <memory base>',0
text03 db 'failed to open usb controller!',13,10,0
text04 db 'failed to allocate memory!',13,10,0
text05 db 'failed to start listening!',13,10,0
;-------------------------------

;-------------------------------
connectSeg_siz equ 00ch         ;size of scructure...
connectSeg_pipe equ 000h        ;dd: pipeline number...
connectSeg_buff equ 004h        ;dd: offset of descriptor...
connectSeg_actn equ 008h        ;dd: current action: 0=none, 1=rx, 2=tx...
;-------------------------------

;------------------------------- data segment layout...
dataSeg_wrtBuf equ 0000h        ;256: write buffer...
dataSeg_tckSec equ 0100h        ;dd: ticks per second...
dataSeg_baseAddr equ 0104h      ;dd: base address...
dataSeg_memPhy equ 0108h        ;dd: reserved memory physical offset..
dataSeg_memLog equ 010ch        ;dd: reserved memory logical offset..
dataSeg_connDt equ 0110h        ;4k: connection data area...
dataSeg_connNm equ 1110h        ;dd: number of connections...
dataSeg_lastBf equ 1114h        ;dd: last buffer used...
dataSeg_portNm equ 1118h        ;dd: number of ports...
dataSeg_freMem equ 111ch        ;free memory...
;-------------------------------

lastbyte:
