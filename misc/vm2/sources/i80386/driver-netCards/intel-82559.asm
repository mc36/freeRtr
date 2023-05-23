;-------------------------------
nic_name db 'Intel EtherExpress Pro 10/100',0
nic_date db %date,' ',%time,0
nic_reqParam equ 010b
nic_addrSize equ 6
nic_maxPack equ 1502
nic_minPack equ 48
;-------------------------------

;-------------------------------
eep_RxBroad equ 000h            ;db: bit0=broadcasts, bit1=multicasts...
eep_RxNext equ 004h             ;dd: next descriptor to test...
eep_phyMem equ 08h              ;dd: memory physical offset...
eep_mapMem equ 0ch              ;dd: memory mapped offset...
;-------------------------------
eep_RxQueue equ 32              ;number of receive buffers...
eep_buf1 equ 1000h              ;48k: the rx buffers...
eep_buf2 equ 0000h              ;2k: the tx buffers...
;-------------------------------

;-------------------------------
proc eep_ReadEEPROM
;in:  bx-location...
;     cl-address length...
;out: bx-data read...
push eax
push ecx
push edx
mov ax,6                        ;load the read command...
shl ax,cl
or bx,ax
mov dx,14                       ;the serial eeprom port...
add edx,def:[dataSeg_parPrt]
mov ax,4800h                    ;the data to put...
out dx,ax
mov ax,4802h                    ;the data to put...
out dx,ax
mov ecx,12
eep_readEeprom_j3:
bt bx,cx
setc al
shl al,2
mov ah,48h
or al,2
out dx,ax
call dword eep_readEeprom_j1
or al,1
out dx,ax
call dword eep_readEeprom_j1
dec ecx
jns byte eep_readEeprom_j3
mov ax,4802h                    ;the data to put...
out dx,ax
sub ebx,ebx
mov ecx,16
eep_readEeprom_j4:
mov ax,4803h                    ;the data to put...
out dx,ax
call dword eep_readEeprom_j1
in ax,dx
shl bx,1
test al,8
setnz al
or bl,al
mov ax,4802h                    ;the data to put...
out dx,ax
call dword eep_readEeprom_j1
loopd eep_readEeprom_j4
mov ax,4800h                    ;the data to put...
out dx,ax
pop edx
pop ecx
pop eax
retnd
eep_readEeprom_j1:
push eax
push edx
push ecx
mov dx,0                        ;the status register...
add edx,def:[dataSeg_parPrt]
mov ecx,3
eep_readEeprom_j2:
in ax,dx
loopd eep_readEeprom_j2
pop ecx
pop edx
pop eax
retnd
endp
;-------------------------------

;-------------------------------
proc nic_present
mov edi,dataSeg_parBrd
sub eax,eax
dec eax
mov def:[eep_rxBroad],al
stosd ptr32
stosd ptr32
sub ebx,ebx
mov cl,6                        ;load address size...
call dword eep_ReadEEPROM
inc ebx
jnz byte nic_present_j1
mov cl,8                        ;load address size...
nic_present_j1:
sub esi,esi
mov edi,dataSeg_freMem
nic_present_j2:
mov ebx,esi
call dword eep_ReadEEPROM
mov eax,ebx
stosw ptr32
inc esi
cmp esi,40h
jb byte nic_present_j2
mov esi,dataSeg_freMem
sub ebx,ebx
mov ecx,40h
nic_present_j3:
lodsw ptr32
add ebx,eax
loopd nic_present_j3
cmp bx,0babah
je byte nic_present_j4
nic_present_err:
stc
retnd
nic_present_j4:
mov esi,dataSeg_freMem
mov edi,dataSeg_parAdr
movsd ptr32
movsw ptr32
mov ecx,10000h
call dword system_allocCont
or ebx,ebx
jnz dword nic_present_err
mov def:[eep_mapMem],edi
mov def:[eep_phyMem],eax
clc
retnd
endp
;-------------------------------

;-------------------------------
proc eep_BuildRings
mov ecx,eep_RxQueue             ;number of rounds to do...
mov edi,eep_buf1
add edi,def:[eep_mapMem]
eep_BuildRings_j1:
mov ebx,edi
sub eax,eax
stosw ptr32
sub eax,eax
stosw ptr32
lea eax,def:[ebx+620h]
sub eax,def:[eep_mapMem]
add eax,def:[eep_phyMem]
stosd ptr32
sub eax,eax
dec eax
stosd ptr32
sub eax,eax
stosw ptr32
mov ax,600h
stosw ptr32
lea edi,def:[ebx+620h]
loopd eep_BuildRings_j1
;mov ax,8000h                    ;bit to set...
;or def:[ebx+2],ax               ;set end of list bit...
mov eax,eep_buf1
add eax,def:[eep_phyMem]
mov def:[ebx+4],eax             ;form a ring from the list...
mov eax,eep_buf1
sub eax,def:[eep_mapMem]
mov def:[eep_RxNext],eax
retnd
endp
;-------------------------------

;-------------------------------
proc eep_WaitForCmdOk
push eax
push ecx
push edx
mov dx,0                        ;the status register...
add edx,def:[dataSeg_parPrt]
mov ecx,10000h
eep_WaitForCmdOk_j1:
in ax,dx
test ah,80h                     ;is command executed...
jnz byte eep_WaitForCmdOk_j2
loopd eep_WaitForCmdOk_j1
eep_WaitForCmdOk_j2:
pop edx
pop ecx
pop eax
retnd
endp
;-------------------------------

;-------------------------------
proc nic_restart
;suspend command & receive units...
mov dx,2                        ;the command register...
add edx,def:[dataSeg_parPrt]
mov ax,0fd44h                   ;suspend both units...
out dx,ax
call dword eep_WaitForCmdOk
mov esi,def:[dataSeg_tckSec]
call dword timer_delay
;reset card...
mov dx,8                        ;the port register...
add edx,def:[dataSeg_parPrt]
mov eax,2                       ;reset the card...
out dx,eax
;wait while done...
call dword eep_BuildRings
mov esi,def:[dataSeg_tckSec]
call dword timer_delay
mov dx,8                        ;the port register...
add edx,def:[dataSeg_parPrt]
nic_restart_j1:
in eax,dx
and ax,1111b
jnz byte nic_restart_j1
;suspend command & receive units...
mov dx,2                        ;the command register...
add edx,def:[dataSeg_parPrt]
mov ax,0fd44h                   ;suspend both units...
out dx,ax
call dword eep_WaitForCmdOk
mov esi,def:[dataSeg_tckSec]
call dword timer_delay
;disable interrupts...
mov dx,2                        ;the command register...
add edx,def:[dataSeg_parPrt]
mov ax,0fd00h                   ;disable all interrupts...
out dx,ax
call dword eep_WaitForCmdOk
mov esi,def:[dataSeg_tckSec]
call dword timer_delay
;start receiver unit...
mov dx,4                        ;the general pointer register...
add edx,def:[dataSeg_parPrt]
mov eax,eep_buf1
add eax,def:[eep_phyMem]
out dx,eax
mov dx,2                        ;the command register...
add edx,def:[dataSeg_parPrt]
mov ax,0fd01h                   ;start receiver unit...
out dx,ax
call dword eep_WaitForCmdOk
mov esi,def:[dataSeg_tckSec]
call dword timer_delay
;create individual address setup frame...
mov edi,eep_buf2
add edi,def:[eep_mapMem]
sub eax,eax
stosw ptr32
mov ax,0c001h                   ;command: EndOfList+Suspend+IAsetup...
stosw ptr32
sub eax,eax
dec eax
stosd ptr32
mov esi,dataSeg_parAdr
movsd ptr32
movsw ptr32
;send this command...
mov dx,4                        ;the general pointer register...
add edx,def:[dataSeg_parPrt]
mov eax,eep_buf2
add eax,def:[eep_phyMem]
out dx,eax
mov dx,2                        ;the command register...
add edx,def:[dataSeg_parPrt]
mov ax,0fd10h                   ;start command unit...
out dx,ax
call dword eep_WaitForCmdOk
mov esi,def:[dataSeg_tckSec]
call dword timer_delay
;receiver configuration...
mov ecx,10000h
nic_restart_j2:
call dword nic_ready4tx
jnc byte nic_restart_j3
loopd nic_restart_j2
nic_restart_j3:
mov esi,def:[dataSeg_tckSec]
call dword timer_delay
mov edi,eep_buf2
add edi,def:[eep_mapMem]
sub eax,eax
stosw ptr32
mov ax,0c002h                   ;command: EndOfList+Suspend+Configure...
stosw ptr32
sub eax,eax
dec eax
stosd ptr32
mov esi,offset nic_restart_d1
mov ecx,offset nic_restart_d2
sub ecx,esi
mov edx,edi
rep
  movsb cs,ptr32
mov edi,edx
mov cl,def:[eep_RxBroad]
test cl,1
jnz byte nic_restart_j4
or byte def:[edi+15],2          ;disable broadcasts...
nic_restart_j4:
test cl,2
jz byte nic_restart_j5
or byte def:[edi+21],8          ;enable multicasts...
nic_restart_j5:
mov dx,4                        ;the general pointer register...
add edx,def:[dataSeg_parPrt]
mov eax,eep_buf2
add eax,def:[eep_phyMem]
out dx,eax
mov dx,2                        ;the command register...
add edx,def:[dataSeg_parPrt]
mov ax,0fd10h                   ;start command unit...
out dx,ax
call dword eep_WaitForCmdOk
mov esi,def:[dataSeg_tckSec]
call dword timer_delay
clc
retnd
nic_restart_d1:
db 016h,008h,000h,000h,000h,080h,03Bh,003h
db 001h,000h,02Eh,000h,0F0h,000h,0F2h,048h
db 000h,040h,0F3h,000h,03Fh,005h
nic_restart_d2:
endp
;-------------------------------

;-------------------------------
proc nic_test4dead
clc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_ready4tx
mov esi,eep_buf2
add esi,def:[eep_mapMem]
mov al,def:[esi+1]              ;read the ownership bit...
shr al,7
or al,al
jz byte nic_ready4tx_err
clc
retnd
nic_ready4tx_err:
stc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_send
mov ebp,ecx
mov edi,eep_buf2
add edi,def:[eep_mapMem]
sub ax,ax
stosw ptr32
mov ax,0c004h                   ;command: EndOfList+Suspend+Transmit...
stosw ptr32
sub eax,eax
dec eax
stosd ptr32
stosd ptr32
lea eax,def:[ebp+12]            ;size of header+data...
or ax,8000h                     ;set the eof bit...
stosw ptr32
mov ax,200
stosw ptr32
mov esi,dataSeg_freMem
movsd ptr32
movsw ptr32
mov ebx,esi
mov esi,dataSeg_parAdr
movsd ptr32
movsw ptr32
mov esi,ebx
lea ecx,def:[ebp+3]
shr ecx,2
rep
  movsd ptr32
mov dx,4                        ;the general pointer register...
add edx,def:[dataSeg_parPrt]
mov eax,eep_buf2
add eax,def:[eep_phyMem]
out dx,eax
mov dx,2                        ;the command register...
add edx,def:[dataSeg_parPrt]
mov ax,0fd10h                   ;start command unit...
out dx,ax
retnd
endp
;-------------------------------

;-------------------------------
proc nic_receive
mov esi,def:[eep_RxNext]
mov ecx,eep_RxQueue
mov edx,ecx
imul edx,620h
add edx,eep_buf1
add edx,def:[eep_mapMem]
inc ecx
nic_receive_j1:
cmp esi,edx
jb byte nic_receive_j2
mov esi,eep_buf1
add esi,def:[eep_mapMem]
nic_receive_j2:
mov al,def:[esi+1]              ;read the status byte...
and al,80h                      ;is this packet completed?
jz byte nic_receive_j3
mov al,def:[esi+13]             ;read the status bits...
and al,11000000b                ;i need just the location bits...
sub al,11000000b                ;is this the only one descriptor?
jz byte nic_receive_j4
push eax
sub eax,eax
mov def:[esi+12],ax             ;clear size of packet...
mov def:[esi+0],ax              ;mark this buffer unused...
pop eax
nic_receive_j3:
add esi,620h
loopd nic_receive_j1
nic_receive_err:
sub ecx,ecx
retnd
nic_receive_j4:
mov def:[eep_RxNext],esi
mov ebx,esi
mov ebp,def:[esi+12]            ;read size of packet...
and ebp,3fffh                   ;get size of packet...
sub ebp,12                      ;minus size of header...
mov edi,dataSeg_freMem
mov eax,def:[esi+22]            ;read source address 1/2...
stosd ptr32
mov ax,def:[esi+26]             ;read source address 2/2...
stosw ptr32
add esi,28                      ;skip to data part...
lea ecx,def:[ebp+3]
shr ecx,2
rep
  movsd ptr32
sub eax,eax
mov def:[ebx+12],ax             ;clear size of packet...
mov def:[ebx+0],ax              ;mark this buffer unused...
add dword def:[eep_RxNext],620h
mov ecx,ebp
retnd
endp
;-------------------------------
