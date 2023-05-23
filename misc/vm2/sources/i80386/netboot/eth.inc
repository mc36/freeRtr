;-------------------------------
proc device_recv
;out: cx-bytes received... (0=nothing)
;     ds:[dataSeg_freMem]-packet received...
;     si-offset of received data...
call dword nic_test4dead
jc word device_reset
call dword nic_receive
mov si,dataSeg_freMem
ret
endp
;-------------------------------

;-------------------------------
proc device_send
;in: di-offset after last byte...
;    ds:[dataSeg_freMem]-packet to send...
push di
call dword nic_test4dead
pop cx
jc word device_reset
movzx ecx,cx
sub cx,dataSeg_freMem
sub cx,nic_addrSize
cmp cx,nic_minPack
ja byte device_send_j1
mov cx,nic_minPack
device_send_j1:
cmp cx,nic_maxPack
jb byte device_send_j2
mov cx,nic_maxPack
device_send_j2:
add cx,nic_addrSize
call dword nic_send
call dword timer_start
device_send_j3:
push ebp
call dword nic_ready4tx
pop ebp
jnc byte device_send_j4
call dword timer_past
sub eax,5
js byte device_send_j3
jmp word device_reset
device_send_j4:
ret
endp
;-------------------------------

;-------------------------------
proc device_reset
mov bp,16
device_reset_j1:
push bp
call dword nic_restart
pop bp
jnc byte device_reset_j2
dec bp
js byte device_reset_j3
mov esi,18
call dword timer_delay
jmp byte device_reset_j1
device_reset_j3:
mov si,offset main_txt11
call writeCode
jmp word showError
device_reset_j2:
sub ecx,ecx
ret
endp
;-------------------------------



;-------------------------------
proc timer_past
;in:  ebp-timer value...
;     eax-seconds past...
push edx
push ds
sub ax,ax
mov ds,ax
mov eax,def:[46ch]
pop ds
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
push edx
push eax
push ds
sub dx,dx
mov ds,dx
mov edx,def:[46ch]
timer_delay_j1:
mov eax,def:[46ch]
sub eax,edx
sub eax,esi
js byte timer_delay_j1
pop ds
pop eax
pop edx
retnd
endp
;-------------------------------

;-------------------------------
proc timer_start
;out: ebp-timer value....
push ds
sub bp,bp
mov ds,bp
mov ebp,def:[46ch]
pop ds
retnd
endp
;-------------------------------

;-------------------------------
proc timer_relequish
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
mov ebx,ds
shl ebx,4
mov edi,eax
sub edi,ebx
sub ebx,ebx
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
add ecx,0fffh
and cx,0f000h
mov edi,ds
shl edi,4
neg edi
mov eax,def:[dataSeg_allocM]
add edi,eax
add def:[dataSeg_allocM],ecx
add dword def:[dataSeg_allocM],1000h
sub ebx,ebx
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
jmp byte system_allocCont
endp
;-------------------------------
