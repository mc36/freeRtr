;-------------------------------
nic_name db 'Standard Microsystems ArcNet',0
nic_date db %date,' ',%time,0
nic_reqParam equ 110b
nic_addrSize equ 1
nic_maxPack equ 508
nic_minPack equ 1
;-------------------------------

;-------------------------------
smc_rxBroad equ 000h            ;db: bit0-broadcast....
smc_mapMem equ 004h             ;mapped memory offset...
;-------------------------------

;------------------------------- ports, commands...
port_Status     equ 0           ;read, status is on this port
port_IntMask    equ 0           ;write, controls the interrupt masking
port_Command    equ 1           ;write, commands go to this port
port_ReSet      equ 8           ;read, resets the card

cmd_DisTrans    equ 1           ;disable transmitter
cmd_DisRece     equ 2           ;disable receiver
cmd_BegTrans    equ 3           ;enable transmitter
cmd_BegRece     equ 4           ;enable receiver
cmd_SetConf     equ 5           ;define configuration
cmd_ClrFlags    equ 6           ;clear flags
cmd_TestFlag    equ 7           ;load test flags

flag_TransFre   equ 01h         ;transmitter available
flag_TransACK   equ 02h         ;transmitted msg. ackd
flag_ReConf     equ 04h         ;system reconfigured
flag_Test       equ 08h         ;test flag
flag_ReSet      equ 10h         ;power-on-reset
flag_ExtTim1    equ 20h         ;extended timeout status 1
flag_ExtTim2    equ 40h         ;extended timeout status 2
flag_RecDis     equ 80h         ;receiver inhibited

Test_Value      equ 0d1h        ;test value, 321 in octal
Rece_BroadCast  equ 80h         ;receive broadcasts
conf_Normal     equ 0h          ;normal packets   /003..100/
conf_Extend     equ 8h          ;extended packets /004..200/
clfg_ReSet      equ 08h         ;clear flags: reset flag
clfg_ReConf     equ 10h         ;clear flags: reconf flag
normal_min      equ 1           ;normal packet minimum size
normal_max      equ 253         ;normal packet maximum size
extend_min      equ 257         ;extended packet minimum size
extend_max      equ 508         ;extended packet maximum size

Tx_Beg equ 200h
Tx_Pge equ 8h
Rx_Beg equ 400h
Rx_Pge equ 10h
;-------------------------------

;------------------------------- read byte from a port
proc smc_ReadPort
;in:  dx-port
;out: al-data
add edx,def:[dataSeg_parPrt]
in al,dx
retnd
endp
;-------------------------------

;------------------------------- write byte to a port
proc smc_WritePort
;in: dx-port
;    al-data
add edx,def:[dataSeg_parPrt]
out dx,al
retnd
endp
;-------------------------------


;-------------------------------
proc nic_present
sub eax,eax
mov def:[dataSeg_parBrd],eax
dec eax
mov def:[smc_rxBroad],al

mov eax,def:[dataSeg_parMem]
mov ecx,10000h
call dword system_mapMem
or ebx,ebx
jnz byte nic_present_err
sub eax,def:[dataSeg_parMem]
neg eax
add eax,edi
mov def:[smc_mapMem],eax

mov esi,def:[smc_mapMem]
mov byte def:[esi],0            ;clear the test value /if it is in...
mov dx,port_ReSet               ;reset the card..
call dword smc_readPort
call dword timer_start
nic_present_j1:
mov al,def:[esi]                ;read the first byte of card's address...
cmp al,Test_Value               ;test the value...
je byte nic_present_j2          ;if ok, then let's continue...
call dword timer_relequish
call dword timer_past
cmp al,2
jb byte nic_present_j1
nic_present_err:
stc
retnd
nic_present_j2:
mov al,def:[esi+1]              ;read local id...
mov def:[dataSeg_parAdr],al

mov al,cmd_clrflags             ;clear flags...
or al,clfg_ReSet                ;the reset flag...
mov dx,port_Command
call dword smc_WritePort

call dword timer_start
nic_present_j3:
mov dx,port_Status              ;read the status...
call dword smc_ReadPort
and al,flag_ReSet               ;test the reset flag...
jz byte nic_present_j4          ;not active-->ok...
call dword timer_relequish
call dword timer_past
cmp al,2
jb nic_present_j3               ;if there is time left, then let's wait...
jmp byte nic_present_err
nic_present_j4:

xor al,al                       ;disable all interrupts...
mov dx,port_IntMask
call dword smc_WritePort

mov al,cmd_SetConf              ;set configuration...
mov dx,port_Command
or al,8                         ;enable long packets...
call dword smc_WritePort

call dword timer_start
nic_present_j5:
mov dx,port_Status              ;read the status...
call dword smc_ReadPort
and al,flag_ReConf
jnz byte nic_present_j6         ;active-->ok..;))
call dword timer_relequish
call dword timer_past
cmp al,2
jb nic_present_j5               ;if there is time left, then let's wait...
jmp dword nic_present_err
nic_present_j6:

call dword smc_enableReceiver

clc
retnd
endp
;-------------------------------

;-------------------------------
proc smc_enableReceiver
;receiver configuration...
mov al,def:[smc_rxBroad]
and al,1                        ;just the broadcast status...
shl al,7
or al,Rx_Pge
or al,cmd_BegRece
mov dx,port_Command
call dword smc_WritePort
call dword timer_start
nic_present_j7:
mov dx,port_Status
call dword smc_ReadPort
and al,flag_RecDis              ;test the required flag...
jz byte nic_present_j8
call dword timer_relequish
call dword timer_past
cmp al,2
jb byte nic_present_j7
jmp dword nic_present_err
nic_present_j8:
retnd
endp
;-------------------------------

;-------------------------------
proc nic_restart
jmp dword nic_present
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
mov dx,port_Status              ;read status port...
call dword smc_ReadPort
and al,flag_TransFre
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
cmp ecx,normal_max              ;is this a short packet?
jbe byte SendOnePck_shrt
cmp ecx,extend_min              ;is this a long packet?
jae byte SendOnePck_long
mov ecx,extend_min
jmp byte SendOnePck_long

SendOnePck_beg:
mov edi,def:[smc_mapMem]
add edi,Tx_Beg
mov al,def:[dataSeg_parAdr]
mov ah,def:[dataSeg_freMem]
stosw ptr32
mov esi,dataSeg_freMem
inc esi
retnd

SendOnePck_shrt:
call dword SendOnePck_beg       ;store header...
mov eax,100h
sub eax,ecx
mov def:[edi],al
jmp byte SendOnePck_j1

SendOnePck_long:
call dword SendOnePck_beg       ;store header...
mov eax,200h
sub eax,ecx
xchg al,ah
mov def:[edi],ax
mov al,ah
jmp byte SendOnePck_j1

SendOnePck_j1:
sub edi,2                       ;and copy the data...
movzx eax,al
add edi,eax
rep
  movsb ptr32
mov dx,port_Command             ;command port...
mov al,cmd_BegTrans
or al,Tx_Pge
call dword smc_WritePort
retnd
endp
;-------------------------------

;-------------------------------
proc nic_receive
mov dx,port_Status              ;read status port...
call dword smc_ReadPort
and al,flag_RecDis
jnz byte nic_receive_j1
nic_receive_err:
sub ecx,ecx
retnd
nic_receive_j1:
mov esi,def:[smc_mapMem]
add esi,Rx_Beg
mov ax,def:[esi]                ;read up source and target address...
mov edi,dataSeg_freMem
stosb ptr32
sub eax,eax
mov al,ds:[esi+2]               ;read up size...
or al,al
jz byte ReadUpPck_long
mov ecx,100h                    ;size of short packet..
jmp byte ReadUpPck_ok
ReadUpPck_long:
mov ecx,200h                    ;size of long packet..
mov al,ds:[esi+3]               ;read up size...
ReadUpPck_ok:
sub ecx,eax
add esi,eax                     ;skip dummy data...
push ecx
rep
  movsb ptr32
call dword smc_enableReceiver
pop ecx
retnd
endp
;-------------------------------
