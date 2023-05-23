use16
org 2000h
mov ax,0e21h
int 10h
cli
cld
call main_j1
main_j1:
pop si
mov ax,cs
mov ds,ax
mov bx,8800h
mov dx,offset main_j2
mov di,offset main_j1
mov es,bx
mov ch,70h
rep
  movsw
mov ss,bx
mov sp,dx
push bx
push dx
retfw
align 2
main_j2:
mov ds,bx
mov fs,bx
mov gs,bx
mov si,offset main_txt01
call writeCode

mov dword def:[dataSeg_parMem],main_forcedMEM
mov dword def:[dataSeg_parPrt],main_forcedIO
mov dword def:[dataSeg_parIrq],main_forcedIRQ
mov dword def:[dataSeg_allocM],110000h
mov dword def:[dataSeg_tckSec],18

mov si,offset main_txt06
call writeCode
mov si,offset nic_name
call writeCode
mov si,offset main_txt18
call writeCode
mov si,offset protocol_name
call writeCode

mov si,offset main_txt07
call writeCode
call realflat
call enablea20
mov ax,cs
mov ds,ax
mov es,ax
mov fs,ax
mov gs,ax

;finding nic...
mov si,offset main_txt08
call writeCode
mov eax,def:[dataSeg_parPrt]
or eax,def:[dataSeg_parMem]
or eax,eax
jnz byte main_j3
mov bx,200h
call word pciBus_find
jc word showError
main_j3:
mov si,offset main_txt13
call writeCode
mov ax,def:[dataSeg_parPrt]
mov cl,4
call conv2hex
call writeCode
mov si,offset main_txt14
call writeCode
mov ax,def:[dataSeg_parMem]
mov cl,8
call conv2hex
call writeCode
mov si,offset main_txt15
call writeCode
mov ax,def:[dataSeg_parIrq]
mov cl,2
call conv2hex
call writeCode

;testing nic...
mov si,offset main_txt09
call writeCode
call dword nic_present
jc word showError
mov si,offset main_txt16
call writeCode
mov si,dataSeg_parAdr
call writeMac

;resetting nic...
mov si,offset main_txt10
call writeCode
call device_reset
mov si,offset main_txtCRLF
call writeCode

;;--------------- rpl
;call doRPL
;jc word showError
;;---------------

;--------------- tftp
call doDHCPautoConf
jc word showError
;call doTERM
call doTFTP
jc word showError
;---------------

;start image...
mov si,offset main_txt17
call writeCode
db 0eah
main_startOfs dw ?
main_startSeg dw ?
;-------------------------------

;------------------------------- data segment layout...
dataSeg_client equ 0000h        ;4k: client datas...
dataSeg_tckSec equ 1100h        ;dd: ticks per second...
dataSeg_parAdr equ 1104h        ;16: local address...
dataSeg_parBrd equ 1114h        ;16: broadcast address...
dataSeg_parMem equ 1124h        ;dd: memory parameter...
dataSeg_parPrt equ 1128h        ;dd: io parameter...
dataSeg_parIrq equ 112ch        ;dd: irq parameter...
dataSeg_allocM equ 1130h        ;dd: allocated memory...
dataSeg_tmpNm1 equ 1134h        ;dd: temporary number...
dataSeg_tmpNm2 equ 1138h        ;dd: temporary number...
dataSeg_tmpNm3 equ 113ch        ;dd: temporary number...
dataSeg_tmpNm4 equ 1140h        ;dd: temporary number...
dataSeg_tmpNm5 equ 1144h        ;dd: temporary number...
dataSeg_tmpNm6 equ 1148h        ;dd: temporary number...
dataSeg_tmpNm7 equ 114ch        ;dd: temporary number...
dataSeg_tmpNm8 equ 1150h        ;dd: temporary number...
dataSeg_preFre equ 1154h        ;64: dummy memory...
dataSeg_freMem equ 1194h        ;free memory...
;-------------------------------

;-------------------------------
main_txtCRLF db 13,10,0
main_txtNULL db 0
main_txtBS db 8,0
main_txtCLR db 8,32,8,0
main_txt01 db 8,' ',13,10,'network boot rom v1.0, done by Mc at ',%date,' ',%time,'.',13,10,10,0
main_txt02 db ' error!',13,10,'rebooting system... ',0
main_txt04 db '(',0
main_txt05 db ')',8,8,8,0
main_txt06 db 'nic driver: ',0
main_txt07 db 13,10,10,'initing realflat...',0
main_txt08 db 13,10,'finding nic...',0
main_txt09 db 13,10,'testing nic...',0
main_txt10 db 13,10,'resetting nic...',0
main_txt11 db 13,10,'failed to initialize nic!',0
main_txt12 db '-',0
main_txt13 db ' io=',0
main_txt14 db ' mem=',0
main_txt15 db ' irq=',0
main_txt16 db ' mac=',0
main_txt17 db 'starting image...',13,10,10,0
main_txt18 db 13,10,'protocol: ',0
;-------------------------------

;-------------------------------
include udpip.inc
include dhcp.inc
;include term.inc
include tftp.inc
;include rpl.inc
;-------------------------------
include pci.inc
include realflat.inc
include eth.inc
include util.inc
;-------------------------------
main_forcedIO equ 0
main_forcedMEM equ 0
main_forcedIRQ equ 0
;-------------------------------
