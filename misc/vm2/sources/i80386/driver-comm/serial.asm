org 0h
use32
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 4096                         ;data
dd 4096                         ;stack
;-------------------------------

;-------------------------------
mov esi,offset text01
call dword writeCodeStr

mov edi,dataBlock_freMem
clts                            ;get process parameters...
dd 13h
movzx byte eax,def:[edi]
inc edi
mov def:[edi+eax],ah
or eax,eax
jnz byte init_j1
mov esi,offset text08
mov ecx,64
rep
  movsd ptr32,cs
init_j1:
mov esi,offset text09
call dword writeCodeStr
mov esi,dataBlock_freMem
inc esi
call dword writeDataStr
mov esi,offset text06
call dword writeCodeStr

clts                            ;get uptime info...
dd 2ah
mov def:[dataBlock_tickPS],edx

mov al,1
clts                            ;switch io accessible mode...
dd 04h

sub eax,eax
mov def:[dataBlock_prtNum],eax
mov esi,dataBlock_freMem
inc esi
init_j2:
mov al,def:[esi]
or al,al
jz byte init_j3
call dword str2num
jc byte init_j3
mov ebp,edx
call dword str2num
jc byte init_j3
push esi
push edx
push ebp
mov ecx,def:[dataBlock_prtNum]
inc ecx
imul ecx,portData__size
mov ebx,ecx
clts                            ;resize extended memory...
dd 24h
mov def:[dataBlock_prtDat],edi
pop edx
pop eax
cmp ecx,ebx
jb dword vege
and al,1fh
call dword ser_detectOnePort
pop esi
jmp byte init_j2
init_j3:
mov ecx,def:[dataBlock_prtNum]
imul ecx,portData__size
clts                            ;resize extended memory...
dd 24h
mov def:[dataBlock_prtDat],edi

mov esi,offset text07
call dword writeCodeStr
mov ecx,def:[dataBlock_prtNum]
or ecx,ecx
jz dword vege
mov edi,def:[dataBlock_prtDat]
sub eax,eax
mov def:[dataBlock_wrtBuf],eax
init_j4:
movzx byte eax,def:[edi+portData_irqNumb]
bts def:[dataBlock_wrtBuf],eax
add edi,portData__size
loopd init_j4
mov ecx,16
sub eax,eax
main_j2:
push eax
push ecx
bt def:[dataBlock_wrtBuf],eax
jnc byte main_j3
mov esi,offset irqHndlr_core
mov edi,2048
clts                            ;hook irq line...
dd 05h
or ebx,ebx
jnz dword vege
main_j3:
pop ecx
pop eax
inc eax
loopd main_j2

mov ecx,def:[dataBlock_prtNum]
mov edi,def:[dataBlock_prtDat]
main_j4:
push ecx
push edi
mov esi,offset text03
call dword writeCodeStr
mov edx,def:[edi+portData_portNum]
inc edx
mov cl,2
call dword conv2hex
call dword writeDataStr
mov esi,offset text04
call dword writeCodeStr
mov edx,def:[edi+portData_basePrt]
mov cl,4
call dword conv2hex
call dword writeDataStr
mov esi,offset text05
call dword writeCodeStr
mov edx,def:[edi+portData_irqNumb]
mov cl,2
call dword conv2hex
call dword writeDataStr

mov esi,offset text06
call dword writeCodeStr
pop edi
pop ecx
add edi,portData__size
loopd main_j4

clts                            ;start listening...
dd 14h
or ebx,ebx
jnz dword vege

sub esi,esi
mov def:[esi],esi
mov ecx,4
clts                            ;write to console...
dd 20h

main_j5:
clts                            ;give away the control...
dd 01h
mov ecx,def:[dataBlock_prtNum]
mov edi,def:[dataBlock_prtDat]
main_j6:
push ecx
push edi
call dword releq2port
pop edi
pop ecx
add edi,portData__size
loopd main_j6
clts                            ;get next incoming pipeline...
dd 16h
or ebx,ebx
jnz byte main_j5
mov ebp,eax
mov ecx,def:[dataBlock_prtNum]
mov edi,def:[dataBlock_prtDat]
main_j7:
cmp ebp,def:[edi+portData_dataPip]
je byte main_j5
cmp ebp,def:[edi+portData_ctrlPip]
je byte main_j5
add edi,portData__size
loopd main_j7
mov eax,ebp
mov esi,dataBlock_wrtBuf
mov ecx,def:[dataBlock_prtNum]
mov def:[esi],ecx
mov ecx,4
clts                            ;nonblocking send through pipeline...
dd 1ah
mov esi,16
main_j8:
dec esi
jns byte main_j9
main_j11:
mov eax,ebp
clts                            ;close pipeline side...
dd 18h
jmp dword main_j5
main_j9:
clts                            ;give away the control...
dd 01h
mov eax,ebp
mov edi,dataBlock_wrtBuf
mov ecx,4
clts                            ;nonblocking receive through pipeline...
dd 1bh
or ebx,ebx
jnz byte main_j8
cmp ecx,4
jne byte main_j8
mov edi,dataBlock_wrtBuf
mov def:[edi+4],ebp
mov esi,def:[edi]
cmp esi,def:[dataBlock_prtNum]
jae byte main_j11
imul esi,portData__size
add esi,def:[dataBlock_prtDat]
mov eax,def:[esi+portData_ctrlPip]
or eax,eax
jnz byte main_j11
mov esi,16
main_j10:
dec esi
js byte main_j11
clts                            ;give away the control...
dd 01h
mov eax,ebp
mov edi,dataBlock_wrtBuf
add edi,8
mov ecx,4
clts                            ;nonblocking receive through pipeline...
dd 1bh
or ebx,ebx
jnz byte main_j10
cmp ecx,4
jne byte main_j10
mov esi,dataBlock_wrtBuf
mov edi,def:[esi+0]
imul edi,portData__size
add edi,def:[dataBlock_prtDat]
mov eax,def:[esi+4]
mov def:[edi+portData_ctrlPip],eax
mov eax,def:[esi+8]
mov def:[edi+portData_dataPip],eax
jmp dword main_j5


vege:
mov esi,offset text02
call dword writeCodeStr
sub eax,eax                     ;terminate process...
clts
dd 00h
;-------------------------------



;-------------------------------
proc releq2port
;in: edi-offset of data...
call dword irqHndlr_checkBug
mov eax,def:[edi+portData_dataPip]
or eax,eax
jz dword releq2port_j4
mov ecx,portData__buffer
sub ecx,def:[edi+portData_trnsSiz]
or ecx,ecx
jz byte releq2port_j1
cmp ecx,128
jb byte releq2port_j3
mov ecx,128
releq2port_j3:
push edi
mov edi,dataBlock_wrtBuf
clts                            ;nonblocking receive through pipeline...
dd 1bh
pop edi
or ebx,ebx
jnz byte releq2port_j1
or ecx,ecx
jz byte releq2port_j1
push ecx
call dword irqHndlr_block
pop ecx
mov esi,dataBlock_wrtBuf
releq2port_j2:
lodsb ptr32
push ecx
push esi
call dword user_sendByte
pop esi
pop ecx
loopd releq2port_j2
call dword irqHndlr_free
releq2port_j1:
mov eax,def:[edi+portData_recvSiz]
or eax,eax
jz byte releq2port_j5
call dword irqHndlr_block
lea esi,def:[edi+portData_recvBuf]
mov ecx,def:[edi+portData_recvSiz]
mov eax,def:[edi+portData_dataPip]
clts                            ;nonblocking send through pipeline...
dd 1ah
sub eax,eax
mov def:[edi+portData_recvSiz],eax
releq2port_j6:
call dword irqHndlr_free
releq2port_j5:
mov eax,def:[edi+portData_lastChk]
inc eax
and eax,0fh
mov def:[edi+portData_lastChk],eax
jnz dword releq2port_j4


mov eax,def:[edi+portData_dataPip]
clts                            ;get pipeline info...
dd 19h
or eax,eax
jz byte releq2port_err
mov eax,def:[edi+portData_ctrlPip]
clts                            ;get pipeline info...
dd 19h
or eax,eax
jz byte releq2port_err
mov eax,def:[edi+portData_ctrlPip]
push edi
mov edi,dataBlock_wrtBuf
mov ecx,128
clts                            ;nonblocking receive through pipeline...
dd 1bh
pop edi
or ebx,ebx
jnz byte releq2port_j4
or ecx,ecx
jz byte releq2port_j4
mov esi,dataBlock_wrtBuf
lodsd ptr32
lea ecx,def:[commandList_beg+eax*2]
cmp ecx,offset commandList_end
jae byte releq2port_err
movzx word eax,cs:[ecx]
call eax
mov esi,dataBlock_wrtBuf
sub ecx,esi
mov eax,def:[edi+portData_ctrlPip]
clts                            ;nonblocking send through pipeline...
dd 1ah
releq2port_j4:
retnd
releq2port_err:
mov eax,def:[edi+portData_dataPip]
clts                            ;close pipeline side...
dd 18h
mov eax,def:[edi+portData_ctrlPip]
clts                            ;close pipeline side...
dd 18h
sub eax,eax
mov def:[edi+portData_dataPip],eax
mov def:[edi+portData_ctrlPip],eax
retnd
endp
;-------------------------------

;-------------------------------
commandList_beg:
dw offset command00,offset command01,offset command02,offset command03
dw offset command04,offset command05,offset command06,offset command07
dw offset command08,offset command09,offset command10,offset command11
commandList_end:
;-------------------------------

;------------------------------- read line status counters...
proc command00
call dword irqHndlr_block
push edi
mov esi,edi
mov edi,dataBlock_wrtBuf
mov eax,0
stosd ptr32
mov eax,def:[esi+portData_numOvrr]
stosd ptr32
mov eax,def:[esi+portData_numPrty]
stosd ptr32
mov eax,def:[esi+portData_numFrme]
stosd ptr32
mov eax,def:[esi+portData_numBrek]
stosd ptr32
mov eax,def:[esi+portData_lineStt]
stosd ptr32
sub eax,eax
mov def:[esi+portData_numOvrr],eax
mov def:[esi+portData_numPrty],eax
mov def:[esi+portData_numFrme],eax
mov def:[esi+portData_numBrek],eax
mov def:[esi+portData_lineStt],eax
mov ecx,edi
pop edi
push ecx
call dword irqHndlr_free
pop ecx
retnd
endp
;-------------------------------

;------------------------------- read modem status counters...
proc command01
call dword irqHndlr_block
push edi
mov esi,edi
mov edi,dataBlock_wrtBuf
mov eax,1
stosd ptr32
mov eax,def:[esi+portData_numCTSc]
stosd ptr32
mov eax,def:[esi+portData_numDSRc]
stosd ptr32
mov eax,def:[esi+portData_numRing]
stosd ptr32
mov eax,def:[esi+portData_numDCDc]
stosd ptr32
mov eax,def:[esi+portData_modmStt]
stosd ptr32
sub eax,eax
mov def:[esi+portData_numCTSc],eax
mov def:[esi+portData_numDSRc],eax
mov def:[esi+portData_numRing],eax
mov def:[esi+portData_numDCDc],eax
mov ecx,edi
pop edi
push ecx
call dword irqHndlr_free
pop ecx
retnd
endp
;-------------------------------

;------------------------------- read modem control status...
proc command02
push edi
mov esi,edi
mov edi,dataBlock_wrtBuf
mov eax,2
stosd ptr32
mov eax,def:[esi+portData_modmCtr]
stosd ptr32
mov ecx,edi
pop edi
retnd
endp
;-------------------------------

;------------------------------- set modem control value...
proc command03
push edi
mov esi,edi
mov edi,dataBlock_wrtBuf
mov eax,3
stosd ptr32
mov eax,def:[edi]
mov def:[esi+portData_modmCtr],eax
mov ecx,edi
pop edi
push ecx
call dword irqHndlr_block
call dword ser_writeModemControl
call dword ser_readModemControl
call dword irqHndlr_free
pop ecx
retnd
endp
;-------------------------------

;------------------------------- read line status...
proc command04
push edi
mov esi,edi
mov edi,dataBlock_wrtBuf
mov eax,4
stosd ptr32
mov eax,def:[esi+portData_linSped]
stosd ptr32
sub eax,eax
stosd ptr32
mov eax,def:[esi+portData_linDbts]
stosd ptr32
mov eax,def:[esi+portData_linPrty]
stosd ptr32
mov eax,def:[esi+portData_linSbts]
stosd ptr32
mov eax,def:[esi+portData_linBrek]
stosd ptr32
mov ecx,edi
pop edi
retnd
endp
;-------------------------------

;------------------------------- write line status...
proc command05
push edi
mov esi,edi
mov edi,dataBlock_wrtBuf
mov eax,5
stosd ptr32
mov eax,def:[edi+0]
mov def:[esi+portData_linSped],eax
mov eax,def:[edi+8]
mov def:[esi+portData_linDbts],eax
mov eax,def:[edi+12]
mov def:[esi+portData_linPrty],eax
mov eax,def:[edi+16]
mov def:[esi+portData_linSbts],eax
mov eax,def:[edi+20]
mov def:[esi+portData_linBrek],eax
mov ecx,edi
pop edi
push ecx
call dword irqHndlr_block
call dword ser_writeLineControl
call dword ser_readLineControl
call dword irqHndlr_free
pop ecx
retnd
endp
;-------------------------------

;------------------------------- read flow control...
proc command06
push edi
mov esi,edi
mov edi,dataBlock_wrtBuf
mov eax,6
stosd ptr32
mov eax,def:[esi+portData_flowCtr]
stosd ptr32
mov ecx,edi
pop edi
retnd
endp
;-------------------------------

;------------------------------- write flow control...
proc command07
push edi
mov esi,edi
mov edi,dataBlock_wrtBuf
mov eax,7
stosd ptr32
mov eax,def:[edi]
and eax,11b
mov def:[esi+portData_flowCtr],eax
mov ecx,edi
pop edi
retnd
endp
;-------------------------------

;------------------------------- driver buffer status...
proc command08
push edi
mov esi,edi
mov edi,dataBlock_wrtBuf
mov eax,8
stosd ptr32
mov eax,def:[esi+portData_recvSiz]
stosd ptr32
mov eax,def:[esi+portData_trnsSiz]
stosd ptr32
mov ecx,edi
pop edi
retnd
endp
;-------------------------------

;------------------------------- clear driver rx buffer...
proc command09
push edi
mov esi,edi
mov edi,dataBlock_wrtBuf
mov eax,9
stosd ptr32
mov ecx,edi
pop edi
push ecx
call dword irqHndlr_block
sub eax,eax
mov def:[esi+portData_recvSiz],eax
call dword irqHndlr_free
pop ecx
retnd
endp
;-------------------------------

;------------------------------- clear driver tx buffer...
proc command10
push edi
mov esi,edi
mov edi,dataBlock_wrtBuf
mov eax,10
stosd ptr32
mov ecx,edi
pop edi
push ecx
call dword irqHndlr_block
sub eax,eax
mov def:[esi+portData_trnsSiz],eax
call dword irqHndlr_free
pop ecx
retnd
endp
;-------------------------------

;------------------------------- clear driver rx and tx buffer...
proc command11
push edi
mov esi,edi
mov edi,dataBlock_wrtBuf
mov eax,11
stosd ptr32
mov ecx,edi
pop edi
push ecx
call dword irqHndlr_block
sub eax,eax
mov def:[esi+portData_recvSiz],eax
mov def:[esi+portData_trnsSiz],eax
call dword irqHndlr_free
pop ecx
retnd
endp
;-------------------------------





;-------------------------------
proc conv2hex
;in:  edx-value to write...
;     cl-digits to convert...
;out: esi-where converted...
push edi
push edx
push ecx
push eax
mov edi,dataBlock_wrtBuf
dec ecx
and ecx,7
inc ecx
push ecx
neg cl
add cl,8
shl cl,2
rol edx,cl
pop ecx
conv2hex_j1:
rol edx,4
movzx eax,dl
and al,0fh
mov ah,'0'
cmp al,10
jb byte conv2hex_j2
mov ah,'A'
sub al,10
conv2hex_j2:
add al,ah
stosb ptr32
loopd conv2hex_j1
sub eax,eax
stosd ptr32
mov esi,dataBlock_wrtBuf
pop eax
pop ecx
pop edx
pop edi
retnd
endp
;-------------------------------

;-------------------------------
proc str2num
;in:  esi-where data is...
;out: carry-cleared if successful...
;     edx-number...
push eax
push ebx
mov ebx,10
sub edx,edx
mov al,def:[esi]
cmp al,'$'
jne byte str2num_j1
inc esi
mov ebx,16
str2num_j1:
lodsb ptr32
cmp al,'A'
jb byte str2num_j3
cmp al,'Z'
ja byte str2num_j3
or al,20h
str2num_j3:
or al,al
jz byte str2num_j2
cmp al,' '
je byte str2num_j2
call dword convDigit
cmp eax,ebx
jae byte str2num_err
imul edx,ebx
add edx,eax
jmp byte str2num_j1
str2num_j2:
dec esi
str2num_j4:
mov al,def:[esi]
cmp al,' '
jne byte str2num_j5
inc esi
jmp byte str2num_j4
str2num_j5:
clc
str2num_vege:
pop ebx
pop eax
retnd
str2num_err:
sub edx,edx
stc
jmp byte str2num_vege
endp
;-------------------------------

;-------------------------------
proc convDigit
;in:  al-byte...
;out: eax-value...
mov ah,al
sub al,'0'
cmp al,10
jb byte convDigit_j1
mov al,ah
sub al,'a'
add al,10
convDigit_j1:
movzx eax,al
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
proc writeDataStr
;in: ds:esi-offset of text
push ecx
push eax
push esi
sub ecx,ecx
writeDataStr_j1:
inc ecx
lodsb ptr32
or al,al
jnz byte writeDataStr_j1
dec ecx
pop esi
clts                            ;write to console...
dd 20h
pop eax
pop ecx
retnd
endp
;-------------------------------

;-------------------------------
include serial.inc
;-------------------------------

;-------------------------------
text01 db 'serial driver v1.0, done by Mc at ',%date,' ',%time,'.',13,10,0
text02 db 'error!',0
text03 db 'number=0x',0
text04 db '  base=0x',0
text05 db '  irq=0x',0
text06 db 13,10,0
text07 db 'the following ports are available:',13,10,0
text08 db '$3f8 4 $2f8 3 $3e8 4 $2e8 3',0
text09 db 'parameter: ',0
;-------------------------------

;-------------------------------
dataBlock_wrtBuf equ 0000h      ;256: write buffer...
dataBlock_prtNum equ 0100h      ;dd: number of connections...
dataBlock_prtDat equ 0104h      ;dd: pointer to data block...
dataBlock_tickPS equ 0108h      ;dd: ticks per second...
dataBlock_freMem equ 010ch      ;dd: free memory...
;-------------------------------

lastbyte:
