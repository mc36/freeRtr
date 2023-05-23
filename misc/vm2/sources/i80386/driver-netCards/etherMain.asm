org 0h
use32
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 8192                         ;data
dd 4096                         ;stack
;-------------------------------

;-------------------------------
mov esi,offset main_text01
call dword writeCodeStr
mov esi,offset main_text02
call dword writeCodeStr
mov esi,offset nic_name
call dword writeCodeStr
mov esi,offset main_text03
call dword writeCodeStr
mov esi,offset nic_date
call dword writeCodeStr
mov esi,offset main_text25
call dword writeCodeStr

sub edi,edi
sub eax,eax
mov ecx,dataSeg_freMem
shr ecx,2
rep
  stosd ptr32

clts                            ;get uptime info...
dd 2ah
mov def:[dataSeg_tckSec],edx

mov al,1
clts                            ;switch io accessible mode...
dd 04h

mov edi,dataSeg_wrtBuf
clts                            ;get process parameters...
dd 13h
mov esi,dataSeg_wrtBuf
movzx byte eax,def:[esi]
lea edi,def:[esi+eax+1]
sub eax,eax
stosd ptr32
inc esi
main_param_j1:
lodsb ptr32
cmp al,' '
je byte main_param_j1
or al,al
jz dword main_param_j2
lea ebp,def:[esi-1]
mov edi,offset main_text09
call dword main_comp
jc dword main_param_j3
mov edi,offset main_text10
call dword main_comp
jc dword main_param_j4
mov edi,offset main_text11
call dword main_comp
jc dword main_param_j5
mov edi,offset main_text12
call dword main_comp
jc dword main_param_j6
jmp dword main_using
main_param_j3:
call dword str2num
jc dword main_using
mov def:[dataSeg_parMem],edx
jmp dword main_param_j1
main_param_j4:
call dword str2num
jc dword main_using
and edx,0ffffh
mov def:[dataSeg_parPrt],edx
jmp dword main_param_j1
main_param_j5:
call dword str2num
jc dword main_using
and edx,0fh
mov def:[dataSeg_parIrq],edx
jmp dword main_param_j1
main_param_j6:
mov ecx,nic_addrSize
mov edi,dataSeg_parAdr
main_param_j7:
lodsb ptr32
call dword lowCase
call dword convDigit
cmp al,10h
jae dword main_using
shl eax,4
mov bl,al
lodsb ptr32
call dword lowCase
call dword convDigit
cmp al,10h
jae dword main_using
or al,bl
stosb ptr32
loopd main_param_j7
jmp dword main_param_j1
main_param_j2:

mov al,nic_reqParam
and al,04h
jz byte main_init_j1
mov eax,def:[dataSeg_parMem]
or eax,eax
jz dword main_using
main_init_j1:
mov al,nic_reqParam
and al,02h
jz byte main_init_j2
mov eax,def:[dataSeg_parPrt]
or eax,eax
jz dword main_using
main_init_j2:
mov al,nic_reqParam
and al,01h
jz byte main_init_j3
mov eax,def:[dataSeg_parIrq]
or eax,eax
jz dword main_using
main_init_j3:

mov esi,offset main_text09
call dword writeCodeStr
mov edx,def:[dataSeg_parMem]
mov cl,8
call dword conv2hex
call dword writeDataStr
mov esi,offset main_text13
call dword writeCodeStr
mov esi,offset main_text10
call dword writeCodeStr
mov edx,def:[dataSeg_parPrt]
mov cl,4
call dword conv2hex
call dword writeDataStr
mov esi,offset main_text13
call dword writeCodeStr
mov esi,offset main_text11
call dword writeCodeStr
mov edx,def:[dataSeg_parIrq]
call dword conv2dec
call dword writeDataStr
mov esi,offset main_text13
call dword writeCodeStr
mov esi,offset main_text16
call dword writeCodeStr
mov edx,nic_maxPack
call dword conv2dec
call dword writeDataStr
mov esi,offset main_textCRLF
call dword writeCodeStr

mov esi,dataSeg_parAdr
mov ecx,nic_addrSize
main_init_j4:
lodsb ptr32
push ax
loopd main_init_j4

call dword nic_present
jnc byte main_init_j5
mov esi,offset main_text17
call dword writeCodeStr
jmp dword main_vege
main_init_j5:

mov esi,offset main_text20
call dword writeCodeStr
mov esi,dataSeg_parBrd
call dword main_addr
mov esi,offset main_textCRLF
call dword writeCodeStr
mov esi,offset main_text21
call dword writeCodeStr
mov esi,dataSeg_parAdr
call dword main_addr
mov esi,offset main_textCRLF
call dword writeCodeStr
mov edi,dataSeg_freMem
mov ecx,nic_addrSize
add edi,ecx
sub edx,edx
main_init_j6:
pop ax
dec edi
mov def:[edi],al
or al,al
setnz ah
add dl,al
loopd main_init_j6
or dl,dl
jz byte main_init_j7
mov esi,dataSeg_freMem
mov edi,dataSeg_parAdr
mov ecx,nic_addrSize
rep
  movsb ptr32
mov esi,offset main_text18
call dword writeCodeStr
mov esi,dataSeg_parAdr
call dword main_addr
mov esi,offset main_textCRLF
call dword writeCodeStr
main_init_j7:
call dword nic_restart
jnc byte main_init_j8
mov esi,offset main_text19
call dword writeCodeStr
jmp dword main_vege
main_init_j8:

clts                            ;start listening...
dd 14h
or ebx,ebx
jnz dword main_vege

mov edi,dataSeg_wrtBuf
sub eax,eax
stosd ptr32
mov esi,dataSeg_wrtBuf
mov ecx,4
clts                            ;write to console...
dd 20h

clts                            ;get uptime info...
dd 2bh
mov def:[dataSeg_lastTx],eax
mov def:[dataSeg_lstChk],eax
sub eax,eax
mov def:[dataSeg_pipeLn],eax
mov eax,nic_addrSize
add eax,nic_maxPack
mov def:[dataSeg_totMax],eax

main_j1:
mov eax,def:[dataSeg_pipeLn]
clts                            ;close pipeline side...
dd 18h
sub eax,eax
mov def:[dataSeg_procNm],eax
mov def:[dataSeg_pipeLn],eax
main_j2:
call dword nic_test4dead
jnc byte main_j3
mov esi,offset main_text22
call dword writeCodeStr
mov esi,offset main_text24
call dword writeCodeStr
mov esi,def:[dataSeg_tckSec]
call dword timer_delay
call dword nic_restart
mov esi,offset main_textCRLF
call dword writeCodeStr
main_j3:
call dword nic_receive
or ecx,ecx
jnz byte main_j3
clts                            ;give away the control...
dd 01h
clts                            ;get next incoming pipeline...
dd 16h
or ebx,ebx
jnz byte main_j2
mov def:[dataSeg_pipeLn],eax
clts                            ;get pipeline info...
dd 19h
mov def:[dataSeg_procNm],eax
mov edi,dataSeg_freMem
clts                            ;get other process name...
dd 0bh
test dl,40h
jz dword main_j1
mov edi,dataSeg_freMem
mov eax,nic_addrSize
stosd ptr32
mov eax,nic_maxPack
stosd ptr32
mov eax,def:[dataSeg_parPrt]
stosd ptr32
mov eax,def:[dataSeg_parMem]
stosd ptr32
mov esi,dataSeg_parAdr
mov ecx,nic_addrSize
rep
  movsb ptr32
mov esi,dataSeg_parBrd
mov ecx,nic_addrSize
rep
  movsb ptr32
mov esi,offset nic_name
main_j4:
lodsb cs,ptr32
stosb ptr32
or al,al
jnz byte main_j4
mov ecx,edi
mov esi,dataSeg_freMem
sub ecx,esi
mov eax,def:[dataSeg_pipeLn]
clts                            ;nonblocking send through pipeline...
dd 1ah


mov dword def:[dataSeg_pckCnt],0

main_recv_j1:
mov eax,def:[dataSeg_pipeLn]
clts                            ;get pipeline info...
dd 19h
or eax,eax
jz dword main_j1
cmp ecx,def:[dataSeg_totMax]
jb byte main_recv_j2
call dword nic_receive
or ecx,ecx
jz byte main_recv_j2
mov esi,dataSeg_freMem
mov eax,def:[dataSeg_pipeLn]
add ecx,nic_addrSize
clts                            ;nonblocking send through pipeline...
dd 1ah
inc dword def:[dataSeg_pckCnt]
jmp byte main_recv_j1
main_recv_j2:

main_send_j1:
call dword nic_ready4tx
jc byte main_send_j2
mov edi,dataSeg_freMem
mov ecx,def:[dataSeg_totMax]
mov eax,def:[dataSeg_pipeLn]
clts                            ;nonblocking receive through pipeline...
dd 1bh
or ebx,ebx
jnz byte main_send_j2
sub ecx,nic_addrSize
jb byte main_send_j1
cmp ecx,nic_maxPack
ja byte main_send_j1
push ecx
lea edi,def:[dataSeg_freMem+ecx]
add edi,nic_addrSize
neg ecx
add ecx,nic_maxPack
add ecx,3
shr ecx,2
sub eax,eax
rep
  stosd ptr32
pop ecx
main_send_j4:
cmp ecx,nic_minPack
jb byte main_send_j3
call dword nic_send
clts                            ;get uptime info...
dd 2bh
mov def:[dataSeg_lastTx],eax
jmp byte main_send_j1
main_send_j3:
mov ecx,nic_minPack
jmp byte main_send_j4
main_send_j2:

main_test_j1:
mov eax,def:[dataSeg_pckCnt]
or eax,eax
jz byte main_test_j6
mov dword def:[dataSeg_pckCnt],0
mov eax,def:[dataSeg_procNm]
clts                            ;give away the control...
dd 02h
jmp byte main_test_j7
main_test_j6:
clts                            ;give away the control...
dd 01h
main_test_j7:
clts                            ;get uptime info...
dd 2bh
sub eax,def:[dataSeg_lstChk]
sub eax,def:[dataSeg_tckSec]
js dword main_recv_j1
clts                            ;get uptime info...
dd 2bh
mov def:[dataSeg_lstChk],eax
call dword nic_test4dead
mov esi,offset main_text22
jc byte main_test_j2
mov eax,def:[dataSeg_lstChk]
sub eax,def:[dataSeg_lastTx]
sub edx,edx
div dword def:[dataSeg_tckSec]
cmp eax,2
jb byte main_test_j3
mov eax,def:[dataSeg_lstChk]
mov def:[dataSeg_lastTx],eax
call dword nic_ready4tx
mov esi,offset main_text23
jc byte main_test_j2
main_test_j3:
jmp dword main_recv_j1

main_test_j2:
call dword writeCodeStr
mov esi,offset main_text24
call dword writeCodeStr
mov ecx,16
main_test_j4:
mov esi,def:[dataSeg_tckSec]
call dword timer_delay
push ecx
call dword nic_restart
pop ecx
jnc byte main_test_j5
mov esi,def:[dataSeg_tckSec]
dec ecx
jns byte main_test_j4
main_test_j5:
mov esi,offset main_textCRLF
call dword writeCodeStr
clts                            ;get uptime info...
dd 2bh
mov def:[dataSeg_lstChk],eax
mov def:[dataSeg_lastTx],eax
jmp byte main_test_j3








main_addr:
push esi
mov esi,offset main_text12
call dword writeCodeStr
pop esi
mov ecx,nic_addrSize
main_addr_j1:
lodsb ptr32
mov dl,al
push esi
push ecx
mov cl,2
call dword conv2hex
inc esi
call dword writeDataStr
mov esi,offset main_text14
call dword writeCodeStr
pop ecx
pop esi
loopd main_addr_j1
mov esi,offset main_text15
call dword writeCodeStr
retnd

main_comp:
mov esi,ebp
main_comp_j1:
mov ah,cs:[edi]
inc edi
or ah,ah
jz byte main_comp_j2
lodsb ptr32
call dword lowCase
cmp al,ah
je byte main_comp_j1
clc
retnd
main_comp_j2:
mov ebp,esi
stc
retnd

main_using:
mov esi,offset main_text04
call dword writeCodeStr
mov edi,dataSeg_wrtBuf
clts                            ;get process pathname...
dd 12h
mov esi,dataSeg_wrtBuf
movzx byte eax,def:[esi]
inc esi
mov def:[esi+eax],ah
mov edi,esi
main_using_j1:
lodsb ptr32
or al,al
jz byte main_using_j2
cmp al,'\'
jne byte main_using_j1
mov edi,esi
jmp byte main_using_j1
main_using_j2:
mov esi,edi
call dword writeDataStr
mov al,nic_reqParam
and al,04h
jz byte main_using_j3
mov esi,offset main_text05
call dword writeCodeStr
main_using_j3:
mov al,nic_reqParam
and al,02h
jz byte main_using_j4
mov esi,offset main_text06
call dword writeCodeStr
main_using_j4:
mov al,nic_reqParam
and al,01h
jz byte main_using_j5
mov esi,offset main_text07
call dword writeCodeStr
main_using_j5:
mov esi,offset main_text08
call dword writeCodeStr

main_vege:
sub eax,eax
clts                            ;terminate process...
dd 00h
;-------------------------------


;-------------------------------
proc writeCodeStr
;in: cs:esi-offset of text
push esi
push edi
push ecx
push eax
sub ecx,ecx
mov edi,dataSeg_wrtBuf
writeCodeStr_j1:
inc ecx
lodsb cs,ptr32
stosb ptr32
or al,al
jnz byte writeCodeStr_j1
dec ecx
mov esi,dataSeg_wrtBuf
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
;in: cs:esi-offset of text
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
proc conv2dec
;in:  edx-value to write...
;out: esi-where converted...
mov esi,offset conv2dec_d1
mov edi,dataSeg_wrtBuf
conv2dec_j3:
cmp esi,offset conv2dec_d2
jae byte conv2dec_j4
lodsd cs,ptr32
or eax,eax
jz byte conv2dec_j3
cmp edx,eax
jb byte conv2dec_j3
conv2dec_j4:
sub esi,4
conv2dec_j1:
lodsd cs,ptr32
or eax,eax
jnz byte conv2dec_j2
mov al,' '
stosb ptr32
jmp byte conv2dec_j1
conv2dec_j2:
mov ecx,eax
mov eax,edx
sub edx,edx
div ecx
add al,'0'
stosb ptr32
cmp esi,offset conv2dec_d2
jb byte conv2dec_j1
sub eax,eax
stosb ptr32
dec edi
mov esi,dataSeg_wrtBuf
retnd
conv2dec_d1:
dd 1000000000,100000000,10000000,1000000,100000,10000,1000,100,10,1
conv2dec_d2:
endp
;-------------------------------

;-------------------------------
proc conv2hex
;in:  edx-value to write...
;     cl-digits to convert...
;out: esi-where converted...
mov edi,dataSeg_wrtBuf
mov al,'$'
stosb ptr32
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
mov esi,dataSeg_wrtBuf
retnd
endp
;-------------------------------

;-------------------------------
proc lowCase
;in: al-char to convert...
cmp al,'A'
jb byte lowCase_j1
cmp al,'Z'
ja byte lowCase_j1
or al,20h
lowCase_j1:
retnd
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
call dword lowCase
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
proc timer_past
;in:  ebp-timer value...
;     eax-seconds past...
push edx
clts                            ;get uptime info...
dd 2bh
sub eax,ebp
sub edx,edx
div dword def:[dataSeg_tckSec]
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
proc timer_relequish
clts                            ;give away the control...
dd 01h
retnd
endp
;-------------------------------

;-------------------------------
proc system_mapMem
; in: eax-physical memory offset...
;     ecx-bytes to map in...
;out: ebx-error code...
;     edi-offset where mapped...
;     eax-physical memory offset...
;     ecx-bytes mapped in...
clts                            ;map system memory...
dd 03h
retnd
endp
;-------------------------------

;-------------------------------
proc system_allocCont
; in: ecx-size of continuous memory in bytes...
;out: ebx-error code...
;     ecx-size of continuous memory in bytes...
;     edi-offset of continuous memory...
;     eax-physical memory offset...
clts                            ;allocate continuous memory...
dd 26h
retnd
endp
;-------------------------------

;-------------------------------
proc system_allocDmable
; in: ecx-size of dma memory in bytes...
;out: ebx-error code...
;     ecx-size of dma memory in bytes...
;     edi-offset of dma memory...
;     eax-physical memory offset...
clts                            ;allocate dma-able memory...
dd 25h
retnd
endp
;-------------------------------

;-------------------------------
proc system_hookIrq
; in: al-irq number: 0..15...
;     esi-offset of handler...
;     edi-stack pointer...
;out: ebx-error code...
clts                            ;hook irq line...
dd 05h
retnd
endp
;-------------------------------

;-------------------------------
proc system_missdIrq
;out: eax-number of missed irqs...
clts                            ;get number of missed irqs...
dd 4ch
retnd
endp
;-------------------------------

;-------------------------------
proc system_stopIrq
clts                            ;terminate irq handler...
dd 07h
jmp byte system_stopIrq
endp
;-------------------------------


;-------------------------------
main_textCRLF db 13,10,0
main_text01 db 'network card driver v1.0, done by Mc at ',%date,' ',%time,'.',13,10,0
main_text02 db 'network interface card: ',0
main_text03 db ' (',0
main_text04 db 'using: ',0
main_text05 db ' <mem=NUM>',0
main_text06 db ' <io=NUM>',0
main_text07 db ' <irq=NUM>',0
main_text08 db ' [addr=NUM]',0
main_text09 db 'mem=',0
main_text10 db 'io=',0
main_text11 db 'irq=',0
main_text12 db 'addr=',0
main_text13 db '  ',0
main_text14 db '-',0
main_text15 db 8,' ',8,0
main_text16 db 'mtu=',0
main_text17 db 'failed to find device at specified location!',0
main_text18 db 'forced ',0
main_text19 db 'failed to initialize device!',0
main_text20 db 'broadcast ',0
main_text21 db 'local ',0
main_text22 db 'nic halted',0
main_text23 db 'tx timeout',0
main_text24 db ', resetting card...',0
main_text25 db ')',13,10,0
;-------------------------------

;-------------------------------
;|;;
;-------------------------------

;------------------------------- data segment layout...
dataSeg_client equ 0000h        ;4k: client datas...
dataSeg_wrtBuf equ 1000h        ;256: write buffer...
dataSeg_tckSec equ 1100h        ;dd: ticks per second...
dataSeg_parAdr equ 1104h        ;16: local address...
dataSeg_parBrd equ 1114h        ;16: broadcast address...
dataSeg_parMem equ 1124h        ;dd: memory parameter...
dataSeg_parPrt equ 1128h        ;dd: io parameter...
dataSeg_parIrq equ 112ch        ;dd: irq parameter...
dataSeg_lastTx equ 1130h        ;dd: tick when last sent...
dataSeg_lstChk equ 1134h        ;dd: tick when last checked...
dataSeg_pipeLn equ 1138h        ;dd: pipeline number...
dataSeg_procNm equ 113ch        ;dd: process number...
dataSeg_pckCnt equ 1140h        ;dd: packet counter/tick...
dataSeg_totMax equ 1144h        ;dd: max packet size...
dataSeg_preFre equ 1148h        ;64: dummy memory...
dataSeg_freMem equ 1188h        ;free memory...
;-------------------------------

lastbyte:
