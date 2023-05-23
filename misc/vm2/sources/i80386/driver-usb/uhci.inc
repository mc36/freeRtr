;------------------------------- the allcoated memory...
bufferSeg_num equ 64            ;number of descriptors...
bufferSeg_siz equ 128           ;size of one buffer...
bufferSeg_list equ 000000h      ;offset of frame list...
bufferSeg_desc equ 001000h      ;offset of descriptors...
bufferSeg_pack equ 003000h      ;offset of packet buffers...
;-------------------------------


;-------------------------------
proc uhci_testChip
;out: carry-cleared if succeeded...
mov ax,40h
mov dl,0                        ;the command port...
call dword mainio_writeW
mov dl,0                        ;the command port...
call dword mainio_readW
test al,40h
jz byte uhci_testChip_err
mov ax,2
mov dl,0                        ;the command port...
call dword mainio_writeW
call dword timer_start
uhci_testChip_j1:
call dword timer_past
or eax,eax
jnz byte uhci_testChip_err
mov dl,0                        ;the command port...
call dword mainio_readW
test al,2                       ;is the reset bit set?
jnz byte uhci_testChip_j1
test al,40h
jnz byte uhci_testChip_err
clc
retnd
uhci_testChip_err:
stc
retnd
endp
;-------------------------------

;-------------------------------
proc uhci_resetChip
mov ax,0
mov dl,0                        ;the command port...
call dword mainio_writeW
uhci_resetChip_j1:
mov dl,0                        ;the command port...
call dword mainio_readW
test al,1                       ;is the run bit set?
jnz byte uhci_resetChip_j1
mov ax,2
mov dl,0                        ;the command port...
call dword mainio_writeW
uhci_resetChip_j2:
mov dl,0                        ;the command port...
call dword mainio_readW
test al,2                       ;is the reset bit set?
jnz byte uhci_resetChip_j2
retnd
endp
;-------------------------------

;-------------------------------
proc uhci_setupBufs
;setup list...
mov edi,def:[dataSeg_memLog]
add edi,bufferSeg_list
mov eax,def:[dataSeg_memPhy]
add eax,bufferSeg_desc
mov ecx,1024
rep
  stosd ptr32
;setup descriptors...
mov edi,def:[dataSeg_memLog]
add edi,bufferSeg_desc
mov ebx,def:[dataSeg_memPhy]
add ebx,bufferSeg_pack
mov ecx,bufferSeg_num
uhci_setupBufs_j1:
mov eax,edi
sub eax,def:[dataSeg_memLog]
add eax,def:[dataSeg_memPhy]
add eax,32
stosd ptr32
sub eax,eax
stosd ptr32
stosd ptr32
mov eax,ebx
stosd ptr32
add ebx,bufferSeg_siz
sub eax,eax
stosd ptr32
stosd ptr32
stosd ptr32
stosd ptr32
loopd uhci_setupBufs_j1
mov eax,def:[dataSeg_memPhy]
add eax,bufferSeg_desc
or al,1
mov def:[edi-32],eax
retnd
endp
;-------------------------------

;-------------------------------
proc uhci_setupChip
mov ax,0
mov dl,0                        ;the command port...
call dword mainio_writeW
mov ax,0fh
mov dl,2                        ;the status port...
call dword mainio_writeW
mov ax,0
mov dl,4                        ;the interrupt port...
call dword mainio_writeW
mov ax,0
mov dl,6                        ;the frame# port...
call dword mainio_writeW
mov eax,def:[dataSeg_memPhy]
add eax,bufferSeg_list
mov dl,8                        ;the frame list port...
call dword mainio_writeD              ;write the port...
mov ax,40h                      ;load value...
mov dl,12                       ;the sof modify port...
call dword mainio_writeB
mov ax,1
mov dl,0                        ;the command port...
call dword mainio_writeW
retnd
endp
;-------------------------------

;-------------------------------
proc uhci_readFrameNum
;out: eax-frame number...
mov dl,6                        ;the frame# port...
call dword mainio_readW
movzx eax,ax
retnd
endp
;-------------------------------




;-------------------------------
proc uhci_portEnable
;in: al-root hub port number...
and al,07h
movzx eax,al
lea dx,def:[eax*2+10h]          ;get port offset...
call dword timer_start
uhci_portEnable_j1:
call dword timer_past
mov ecx,def:[dataSeg_tckSec]
shr ecx,4
cmp eax,ecx
ja byte uhci_portEnable_j2
mov ax,200h                     ;the reset bit...
call dword mainio_writeW
call dword mainio_readW
test ah,2                       ;is the reset bit set?
jz byte uhci_portEnable_j1
uhci_portEnable_j2:
mov esi,def:[dataSeg_tckSec]
shr esi,4
call dword timer_delay
call dword timer_start
uhci_portEnable_j3:
call dword timer_past
mov ecx,def:[dataSeg_tckSec]
shr ecx,4
cmp eax,ecx
ja byte uhci_portEnable_j4
mov ax,0eh                      ;enable bit...
call dword mainio_writeW
call dword mainio_readW
test al,4                       ;is the enable bit set?
jz byte uhci_portEnable_j3
test ah,2                       ;is the reset bit set?
jnz byte uhci_portEnable_j3
uhci_portEnable_j4:
mov esi,def:[dataSeg_tckSec]
shr esi,4
call dword timer_delay
retnd
endp
;-------------------------------

;-------------------------------
proc uhci_portDisable
;in: al-root hub port number...
and al,07h
movzx eax,al
lea dx,def:[eax*2+10h]          ;get port offset...
call dword timer_start
uhci_portDisable_j1:
call dword timer_past
mov ecx,def:[dataSeg_tckSec]
shr ecx,4
cmp eax,ecx
ja byte uhci_portDisable_j2
mov ax,0ah
call dword mainio_writeW
call dword mainio_readW
test al,4                       ;is the enable bit set?
jnz byte uhci_portDisable_j1
test ah,2                       ;is the reset bit set?
jnz byte uhci_portDisable_j1
uhci_portDisable_j2:
mov esi,def:[dataSeg_tckSec]
shr esi,4
call dword timer_delay
retnd
endp
;-------------------------------

;-------------------------------
proc uhci_portStatus
;in:  al-root hub port number...
;out: al-bit0-connected, bit1-enabled, ah=speed: 0=low, 1=high...
and al,07h
movzx eax,al
lea dx,def:[eax*2+10h]          ;get port offset...
call dword mainio_readW
test al,4                       ;enabled bit...
setnz cl
add cl,cl
test al,1                       ;connected bit...
setnz ch
or cl,ch
test ah,1                       ;test speed bit...
setz ch
mov ax,cx
retnd
endp
;-------------------------------



;-------------------------------
proc uhci_beginSending
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
;write control dword...
sub eax,eax
mov al,def:[esi+10h]            ;speed value...
not al
and al,1
shl eax,26
sub ecx,ecx
mov cl,def:[esi+0ch]            ;isochronous type...
cmp cl,2
sete cl
shl ecx,25
or eax,ecx
or eax,18000000h
mov def:[edi+04h],eax
;write token dword...
mov eax,def:[esi+00h]           ;read size...
dec eax
and eax,7ffh
shl eax,21
sub ecx,ecx
mov cl,def:[esi+14h]            ;data toggle...
and cl,1
shl ecx,19
or eax,ecx
sub ecx,ecx
mov cl,def:[esi+08h]            ;read endpoint...
and cl,0fh
shl ecx,15
or eax,ecx
mov al,def:[esi+04h]            ;read device...
and al,7fh
or ah,al
sub ecx,ecx
mov cl,def:[esi+0ch]            ;read type...
and cl,11b
mov al,cs:[uhci_beginSending_d1+ecx]
mov def:[edi+08h],eax
;copy buffer...
mov ecx,def:[esi+00h]
mov edi,def:[edi+0ch]
sub edi,def:[dataSeg_memPhy]
add edi,def:[dataSeg_memLog]
add esi,18h
rep
  movsb ptr32
;enable descriptor...
mov eax,800000h
or ds:[ebp+04h],eax
retnd
uhci_beginSending_d1 db 0e1h,2dh,0e1h,0e1h
endp
;-------------------------------

;-------------------------------
proc uhci_beginReceiving
;in: esi-offset of buffer:
;    00     dword-number of data bytes...
;    04     dword-device number...
;    08     dword-endpoint number (inside of device)...
;    0c     dword-type: 0=input, 1=isochronous...
;    10     dword-speed: 0=low, 1=high...
;    14     dword-data toggle bit...
;    edi-descriptor to use...
;write control dword...
sub eax,eax
mov al,def:[esi+10h]            ;speed value...
not al
and al,1
shl eax,26
sub ecx,ecx
mov cl,def:[esi+0ch]            ;isochronous type...
and cl,1
shl ecx,25
or eax,ecx
or eax,18000000h
mov def:[edi+04h],eax
;write token dword...
mov eax,def:[esi+00h]           ;read size...
dec eax
and eax,7ffh
shl eax,21
sub ecx,ecx
mov cl,def:[esi+14h]            ;data toggle...
and cl,1
shl ecx,19
or eax,ecx
sub ecx,ecx
mov cl,def:[esi+08h]            ;read endpoint...
and cl,0fh
shl ecx,15
or eax,ecx
mov al,def:[esi+04h]            ;read device...
and al,7fh
or ah,al
mov al,69h
mov def:[edi+08h],eax
;enable descriptor...
mov eax,800000h
or def:[edi+04h],eax
retnd
endp
;-------------------------------

;-------------------------------
proc uhci_descRead
;in:  esi-descriptor to use...
;     edi-where to copy...
;out: ecx-bytes copied...
mov ecx,def:[esi+04h]
mov esi,def:[esi+0ch]
sub esi,def:[dataSeg_memPhy]
add esi,def:[dataSeg_memLog]
inc ecx
and ecx,7ffh
mov ebx,ecx
rep
  movsb ptr32
mov ecx,ebx
retnd
endp
;-------------------------------

;-------------------------------
proc uhci_descStatus
;in:  esi-descriptor to use...
;out: carry-cleared if finished...
;     eax-status: 0=ok, 1=stalled, 2=buffer, 3=babble, 4=nak, 5=crc, 6=bitstuff...
mov ecx,def:[esi+04h]
mov eax,3                       ;babble...
test ecx,100000h
jnz byte uhci_descStatus_j1
mov al,2                        ;buffer...
test ecx,200000h
jnz byte uhci_descStatus_j1
mov al,4                        ;nak...
test ecx,80000h
jnz byte uhci_descStatus_j1
mov al,5                        ;crc/timeout...
test ecx,40000h
jnz byte uhci_descStatus_j1
mov al,1                        ;stalled...
test ecx,400000h
jnz byte uhci_descStatus_j1
mov al,6                        ;bitstuff...
test ecx,20000h
jnz byte uhci_descStatus_j1
mov al,0                        ;no error...
uhci_descStatus_j1:
test ecx,800000h                ;active?
jnz byte uhci_descStatus_j2
clc
retnd
uhci_descStatus_j2:
stc
retnd
endp
;-------------------------------

;-------------------------------
proc uhci_descStop
;in:  esi-descriptor to use...
uhci_descStop_j1:
mov eax,def:[esi+04h]
test eax,800000h                ;active?
jz byte uhci_descStop_j2
call dword uhci_readFrameNum
mov edi,eax
uhci_descStop_j3:
mov dword def:[esi+04h],0
call dword uhci_readFrameNum
cmp eax,edi
je byte uhci_descStop_j3
call dword uhci_readFrameNum
mov edi,eax
uhci_descStop_j4:
call dword uhci_readFrameNum
cmp eax,edi
je byte uhci_descStop_j4
jmp byte uhci_descStop_j1
uhci_descStop_j2:
retnd
endp
;-------------------------------
