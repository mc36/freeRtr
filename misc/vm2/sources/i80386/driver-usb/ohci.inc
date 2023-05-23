;------------------------------- the allcoated memory...
bufferSeg_num equ 64            ;number of descriptors...
bufferSeg_siz equ 128           ;size of one buffer...
bufferSeg_hcca equ 000000h      ;offset of frame list...
bufferSeg_desc equ 001000h      ;offset of descriptors...
bufferSeg_pack equ 003000h      ;offset of packet buffers...
;-------------------------------


;-------------------------------
proc ohci_setupBufs
;hcca...
mov edi,def:[dataSeg_memLog]
add edi,bufferSeg_hcca
mov eax,def:[dataSeg_memPhy]
add eax,bufferSeg_desc
mov ecx,32
rep
  stosd ptr32
sub eax,eax
mov ecx,32
rep
  stosd ptr32
;descriptors...
mov edi,def:[dataSeg_memLog]
add edi,bufferSeg_desc
mov ecx,bufferSeg_num
mov ebx,def:[dataSeg_memPhy]
add ebx,bufferSeg_pack
ohci_setupBufs_j1:
lea edx,def:[edi+64]
mov eax,4000h
stosd ptr32
mov eax,edx
sub eax,def:[dataSeg_memLog]
add eax,def:[dataSeg_memPhy]
stosd ptr32
or al,1
stosd ptr32
xor al,1
stosd ptr32
mov eax,ebx
stosd ptr32
add ebx,bufferSeg_siz
add edi,44
loopd ohci_setupBufs_j1
sub eax,eax
mov def:[edi-52],eax
retnd
endp
;-------------------------------

;-------------------------------
proc ohci_testChip
;out: carry-cleared if succeeded...
mov dl,0                        ;the revision id port...
call dword mainio_readD
cmp al,10h
jne byte ohci_testChip_err
mov dl,48h                      ;the rhDescA port...
call dword mainio_readD
cmp al,16
ja byte ohci_testChip_err
or al,al
jz byte ohci_testChip_err
movzx eax,al
mov def:[dataSeg_portNm],eax
mov dl,4                        ;the control port...
call dword mainio_readD
test ah,1                       ;is the ir bit set?
jz byte ohci_testChip_j1
mov eax,8                       ;the ocr bit...
mov dl,8                        ;the command/status port...
call dword mainio_writeD
call dword timer_start
ohci_testChip_j2:
call dword timer_past
mov ecx,def:[dataSeg_tckSec]
shr ecx,2
cmp eax,ecx
jae byte ohci_testChip_err
mov dl,4                        ;the control port...
call dword mainio_readD
test ah,1                       ;is the ir bit set?
jz byte ohci_testChip_j1        ;no-->finish work...
jmp byte ohci_testChip_j2
ohci_testChip_err:
stc
jmp byte ohci_testChip_vege
ohci_testChip_j1:
clc
ohci_testChip_vege:
retnd
endp
;-------------------------------

;-------------------------------
proc ohci_resetChip
mov dl,8                        ;the command/status port...
mov eax,1                       ;the hcReset bit...
call dword mainio_writeD
mov esi,def:[dataSeg_tckSec]
shr esi,2
call dword timer_delay
call dword ohci_enablePower
retnd
endp
;-------------------------------

;-------------------------------
proc ohci_enablePower
mov al,100                      ;load time...
shl eax,16
mov ax,11001b                   ;set power modes...
shl eax,8
mov al,def:[dataSeg_portNm]     ;read number of ports on root hubs...
mov dl,48h                      ;the rhDescA port...
call dword mainio_writeD
mov ax,1                        ;load value...
mov cl,def:[dataSeg_portNm]
shl ax,cl
dec ax
shl eax,17
mov dl,4ch                      ;the rhDescB port...
call dword mainio_writeD
mov eax,80030000h               ;set power,clear wakeup...
mov dl,50h                      ;the rhStatus port...
call dword mainio_writeD
mov cx,def:[dataSeg_portNm]
ohci_enablePower_j1:
mov eax,1f0100h                 ;enable the port power...
add dl,4                        ;the rhPortStatus[n] port...
call dword mainio_writeD
loopw ohci_enablePower_j1       ;process all ports...
retnd
endp
;-------------------------------

;-------------------------------
proc ohci_setupChip
call dword ohci_enablePower
sub eax,eax
mov dl,18h                      ;the hcca port...
call dword mainio_writeD
mov eax,080h
mov dl,4h                       ;the control port...
call dword mainio_writeD
sub eax,eax
mov dl,8h                       ;the command/status port...
call dword mainio_writeD
sub eax,eax
mov dl,0ch                      ;the intStatus port...
call dword mainio_writeD
sub eax,eax
mov dl,10h                      ;the intEnable status port...
call dword mainio_writeD
sub eax,eax
dec eax
mov dl,14h                      ;the intDisable status port...
call dword mainio_writeD
mov eax,def:[dataSeg_memPhy]
add eax,bufferSeg_hcca
mov dl,18h                      ;the hcca port...
call dword mainio_writeD
mov eax,def:[dataSeg_memPhy]
add eax,bufferSeg_desc
mov dl,20h                      ;the controlHead port...
call dword mainio_writeD
mov dl,28h                      ;the bulkHead port...
call dword mainio_writeD
sub eax,eax
mov dl,30h                      ;the doneHead port...
call dword mainio_writeD
mov eax,27782edfh               ;load value...
mov dl,34h                      ;the fmInterval port...
call dword mainio_writeD
mov eax,2a2fh
mov dl,40h                      ;the periodicStart port...
call dword mainio_writeD
mov eax,628h
mov dl,44h                      ;the lsTreshold port...
call dword mainio_writeD
call dword ohci_enablePower
mov eax,8ch
mov dl,4h                       ;the control port...
call dword mainio_writeD
call dword ohci_enablePower
retnd
endp
;-------------------------------

;-------------------------------
proc ohci_readFrameNum
;out: eax-frame number...
mov dl,3ch                      ;the frameNumber port...
call dword mainio_readD
movzx eax,ax
retnd
endp
;-------------------------------









;-------------------------------
proc ohci_portEnable
;in: al-root hub port number...
and al,07h
movzx eax,al
lea dx,def:[eax*4+54h]          ;get port offset...
mov eax,1f0119h                 ;reset the port...
call dword mainio_writeD
push edx
mov esi,def:[dataSeg_tckSec]
shr esi,2
call dword timer_delay
pop edx
mov eax,1f0102h                 ;enable the port...
call dword mainio_writeD
mov esi,def:[dataSeg_tckSec]
shr esi,2
call dword timer_delay
retnd
endp
;-------------------------------

;-------------------------------
proc ohci_portDisable
;in: al-root hub port number...
and al,07h
movzx eax,al
lea dx,def:[eax*4+54h]          ;get port offset...
mov eax,1f0101h                 ;disable the port...
call dword mainio_writeD
mov esi,def:[dataSeg_tckSec]
shr esi,4
call dword timer_delay
retnd
endp
;-------------------------------

;-------------------------------
proc ohci_portStatus
;in:  al-root hub port number...
;out: al-bit0-connected, bit1-enabled, ah=speed: 0=low, 1=high...
and al,07h
movzx eax,al
lea dx,def:[eax*4+54h]          ;get port offset...
call dword mainio_readD
shr ah,1
xor ah,1
and eax,103h
retnd
endp
;-------------------------------







;-------------------------------
proc ohci_beginSending
;in: esi-offset of buffer:
;    00     dword-number of data bytes...
;    04     dword-device number...
;    08     dword-endpoint number (inside of device)...
;    0c     dword-type: 0=output, 1=setup, 2=isochronous...
;    10     dword-speed: 0=low, 1=high...
;    14     dword-data toggle bit...
;    18     bytes-data to send...
;    edi-descriptor to use...
mov ebp,edi
;endpoint config fields...
mov eax,def:[esi+00h]           ;read size...
inc eax
shl eax,16
sub ecx,ecx
mov cl,def:[esi+0ch]            ;isochronous type...
cmp cl,2
sete cl
shl ecx,15
or eax,ecx
sub ecx,ecx
mov cl,def:[esi+10h]            ;read speed...
not cl
and cl,1
shl ecx,13
or eax,ecx
sub ecx,ecx
mov cl,def:[esi+08h]            ;read endpoint...
and cl,0fh
shl ecx,7
or eax,ecx
mov cl,def:[esi+04h]            ;read device...
and cl,7fh
or al,cl
or eax,4000h
mov def:[edi+00h],eax
;queue tail...
lea eax,def:[edi+64]
sub eax,def:[dataSeg_memLog]
add eax,def:[dataSeg_memPhy]
mov def:[edi+04h],eax
;queue head...
sub eax,32
mov def:[edi+08h],eax
;descriptor config fields...
mov eax,def:[esi+0ch]           ;read type...
and eax,11b
mov al,cs:[ohci_beginSending_d1+eax]
shl eax,19
mov ecx,def:[esi+14h]           ;data toggle...
and ecx,1
shl ecx,24
or eax,ecx
or eax,0f2e40000h
mov def:[edi+20h],eax
;buffer pointer...
mov ebx,def:[edi+16]
mov def:[edi+24h],ebx
;next descriptor...
lea eax,def:[edi+64]
sub eax,def:[dataSeg_memLog]
add eax,def:[dataSeg_memPhy]
mov def:[edi+28h],eax
;buffer end...
mov eax,def:[esi+00h]           ;read size...
dec eax
add eax,def:[edi+16]
mov def:[edi+2ch],eax
;copy data...
mov ecx,def:[esi+00h]           ;read size...
mov edi,def:[edi+16]
sub edi,def:[dataSeg_memPhy]
add edi,def:[dataSeg_memLog]
add esi,18h
rep
  movsb ptr32
;enable endpoint...
mov eax,0ffffbfffh
and ds:[ebp+00h],eax
retnd
ohci_beginSending_d1 db 01b,00b,01b,01b
endp
;-------------------------------

;-------------------------------
proc ohci_beginReceiving
;in: esi-offset of buffer:
;    00     dword-number of data bytes...
;    04     dword-device number...
;    08     dword-endpoint number (inside of device)...
;    0c     dword-type: 0=input, 1=isochronous...
;    10     dword-speed: 0=low, 1=high...
;    14     dword-data toggle bit...
;    edi-descriptor to use...
;endpoint config fields...
mov eax,def:[esi+00h]           ;read size...
inc eax
shl eax,16
sub ecx,ecx
mov cl,def:[esi+0ch]            ;isochronous type...
and cl,1
shl ecx,15
or eax,ecx
sub ecx,ecx
mov cl,def:[esi+10h]            ;read speed...
not cl
and cl,1
shl ecx,13
or eax,ecx
sub ecx,ecx
mov cl,def:[esi+08h]            ;read endpoint...
and cl,0fh
shl ecx,7
or eax,ecx
mov cl,def:[esi+04h]            ;read device...
and cl,7fh
or al,cl
or eax,4000h
mov def:[edi+00h],eax
;queue tail...
lea eax,def:[edi+64]
sub eax,def:[dataSeg_memLog]
add eax,def:[dataSeg_memPhy]
mov def:[edi+04h],eax
;queue head...
sub eax,32
mov def:[edi+08h],eax
;descriptor config fields...
mov eax,def:[esi+14h]           ;data toggle...
and eax,1
shl eax,24
or eax,0f2f40000h
mov def:[edi+20h],eax
;buffer pointer...
mov ebx,def:[edi+16]
mov def:[edi+24h],ebx
;next descriptor...
lea eax,def:[edi+64]
sub eax,def:[dataSeg_memLog]
add eax,def:[dataSeg_memPhy]
mov def:[edi+28h],eax
;buffer end...
mov eax,def:[esi+00h]           ;read size...
dec eax
add eax,def:[edi+16]
mov def:[edi+2ch],eax
;enable endpoint...
mov eax,0ffffbfffh
and def:[edi+00h],eax
retnd
endp
;-------------------------------

;-------------------------------
proc ohci_descRead
;in:  esi-descriptor to use...
;     edi-where to copy...
;out: ecx-bytes copied...
mov ecx,def:[esi+24h]
or ecx,ecx
jnz byte ohci_descRead_j1
mov ecx,def:[esi+2ch]
inc ecx
ohci_descRead_j1:
mov esi,def:[esi+16]
sub ecx,esi
sub esi,def:[dataSeg_memPhy]
add esi,def:[dataSeg_memLog]
mov ebx,ecx
rep
  movsb ptr32
mov ecx,ebx
retnd
endp
;-------------------------------

;-------------------------------
proc ohci_descStatus
;in:  esi-descriptor to use...
;out: carry-cleared if finished...
;     eax-status: 0=ok, 1=stalled, 2=buffer, 3=babble, 4=nak, 5=crc, 6=bitstuff, 7=piderror, 8=timeout...
mov eax,def:[esi+20h]
shr eax,28
mov al,cs:[ohci_descStatus_d1+eax]
mov ecx,def:[esi+08h]           ;head pointer...
and cl,0f0h
cmp ecx,def:[esi+04h]           ;tail pointer...
jne byte ohci_descStatus_j1
clc
retnd
ohci_descStatus_j1:
stc
retnd
ohci_descStatus_d1 db 00,05,06,07,01,08,07,07,02,02,99,99,02,02,00,00
endp
;-------------------------------

;-------------------------------
proc ohci_descStop
;in:  esi-descriptor to use...
call dword ohci_readFrameNum
mov edi,eax
ohci_descStop_j3:
mov eax,4000h
or def:[esi+00h],eax
call dword ohci_readFrameNum
cmp eax,edi
je byte ohci_descStop_j3
retnd
endp
;-------------------------------
