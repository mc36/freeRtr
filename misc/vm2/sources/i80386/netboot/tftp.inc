;-------------------------------
protocol_name db 'trivial file transfer',0
;-------------------------------
proc doTFTP
;out: carry if error...
mov eax,100h
mov ecx,200h
mov def:[main_startOfs],ax
mov def:[main_startSeg],cx
shl ecx,4
add eax,ecx
mov def:[dataSeg_tmpNm1],eax
mov word def:[dataSeg_tmpNm2],10
mov dword def:[dataSeg_tmpNm4],1
mov word def:[dataSeg_tmpNm5],511
doTFTP_j1:
dec word def:[dataSeg_tmpNm2]
jns byte doTFTP_j2
stc
ret
doTFTP_j2:
mov si,offset doTFTP_txt01
call writeCode
mov word def:[dataSeg_peerPT],69
mov byte def:[dataSeg_ignore],4
call dword timer_start
mov def:[dataSeg_tmpNm3],ebp
mov di,dataSeg_freMem
lea si,def:[di+804h]
mov ax,100h             ;opcode...
stosw
call doTFTP_j4
mov si,offset doTFTP_txt02
call doTFTP_j4
call sendUDPpacket
doTFTP_j3:
mov ebp,def:[dataSeg_tmpNm3]
call dword timer_past
cmp ax,5
ja byte doTFTP_j1
call doReleq2wire
dec ax
jnz byte doTFTP_j3
mov ax,def:[si-8]
xchg al,ah
mov def:[dataSeg_peerPT],ax
mov byte def:[dataSeg_ignore],0

doTFTP_j5:
lodsw                   ;opcode...
cmp ax,300h
je byte doTFTP_j6
cmp ax,500h
je byte doTFTP_j7
mov si,offset doTFTP_txt03
call writeCode
stc
ret
doTFTP_j7:
push si
mov si,offset doTFTP_txt04
call writeCode
pop si
sub eax,eax
lodsw
xchg al,ah
push si
call conv2dec
call writeData
mov si,offset doTFTP_txt05
call writeCode
pop si
call writeData
mov si,offset main_txtCRLF
call writeCode
stc
ret
doTFTP_j6:
lodsw                   ;packet number...
xchg al,ah
cmp ax,def:[dataSeg_tmpNm4]
jne word doTFTP_j10
sub cx,4
movzx ecx,cx
mov eax,def:[dataSeg_tmpNm1]
add def:[dataSeg_tmpNm1],ecx
mov def:[dataSeg_tmpNm5],cx
mov di,ax
shr eax,4
and di,0fh
mov es,ax
rep
  movsb
mov ax,ds
mov es,ax
mov word def:[dataSeg_tmpNm2],10
inc dword def:[dataSeg_tmpNm4]

doTFTP_j8:
mov eax,def:[dataSeg_tmpNm4]
call conv2dec
call writeData
mov si,offset doTFTP_txt06
call writeCode
call dword timer_start
mov def:[dataSeg_tmpNm3],ebp
dec word def:[dataSeg_tmpNm2]
jns byte doTFTP_j9
stc
ret
doTFTP_j9:
mov di,dataSeg_freMem
mov ax,400h             ;opcode...
stosw
mov ax,def:[dataSeg_tmpNm4]     ;block number...
dec ax
xchg al,ah
stosw
call sendUDPpacket
mov ax,def:[dataSeg_tmpNm5]
cmp ax,512
jae byte doTFTP_j10
mov eax,def:[dataSeg_tmpNm4]
call conv2dec
call writeData
mov si,offset doTFTP_txt07
call writeCode
clc
ret
doTFTP_j10:
mov ebp,def:[dataSeg_tmpNm3]
call dword timer_past
cmp ax,5
ja byte doTFTP_j8
call recvUDPpacket
or cx,cx
jz byte doTFTP_j10
jmp word doTFTP_j5

doTFTP_txt01 db 'sending TFTP read request...',13,10,0
doTFTP_txt02 db 'octet',0
doTFTP_txt03 db 'got unknown opcode!',13,10,0
doTFTP_txt04 db 'got error! code=',0
doTFTP_txt05 db ' text=',0
doTFTP_txt06 db 13,0
doTFTP_txt07 db ' packets received.',13,10,0
doTFTP_j4:
lodsb
stosb
or al,al
jnz byte doTFTP_j4
ret
endp
;-------------------------------
