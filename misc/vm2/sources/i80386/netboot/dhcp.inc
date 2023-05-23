;-------------------------------
proc doDHCPautoConf
;out: carry cleared if succeed..
;     dataSeg_freMem+800h-ip, file...
mov word def:[dataSeg_tmpNm2],10
doDHCPautoConf_j1:
sub eax,eax
mov def:[dataSeg_loclIP],eax
mov def:[dataSeg_defGat],eax
mov def:[dataSeg_netMsk],eax
mov def:[dataSeg_ipIDvl],ax
dec eax
mov def:[dataSeg_peerIP],eax
mov di,dataSeg_perMac
mov cx,nic_addrSize
mov al,0ffh
rep
  stosb
mov word def:[dataSeg_peerPT],67
mov word def:[dataSeg_loclPT],68
mov byte def:[dataSeg_ignore],3
call dword timer_start
mov def:[dataSeg_tmpNm3],ebp
dec word def:[dataSeg_tmpNm2]
jns byte doDHCPautoConf_j2
stc
ret
doDHCPautoConf_j2:
mov si,offset doDHCPautoConf_txt01
call writeCode
mov di,dataSeg_freMem
mov al,1                ;opcode...
stosb
mov al,1                ;hw type...
stosb
mov al,nic_addrSize     ;hw addr size...
stosb
mov al,0                ;hops...
stosb
movzx word eax,def:[dataSeg_tmpNm2]     ;transaction id...
stosd
neg ax                  ;secs...
add ax,50
xchg al,ah
stosw
sub ax,ax               ;flags...
stosw
mov cx,16               ;addresses...
rep
  stosb
mov si,dataSeg_parAdr   ;chaddr...
mov cx,nic_addrSize
rep
  movsb
mov cx,16
sub cx,nic_addrSize
sub ax,ax
rep
  stosb
mov cx,64               ;sname...
rep
  stosb
mov cx,128              ;file...
rep
  stosb
mov eax,63538263h       ;magic cookie...
stosd
mov al,53               ;type...
mov ah,1                ;length
stosw
mov al,1                ;value...
stosb
mov al,61               ;type...
mov ah,nic_addrSize     ;length...
inc ah
stosw
mov al,1                ;value...
stosb
mov si,dataSeg_parAdr
mov cx,nic_addrSize
rep
  movsb
mov al,0ffh             ;end...
stosb
call sendUDPpacket
doDHCPautoConf_j3:
mov ebp,def:[dataSeg_tmpNm3]
call dword timer_past
cmp ax,5
ja word doDHCPautoConf_j1
call doReleq2wire
dec ax
jnz byte doDHCPautoConf_j3
mov bx,si
lodsb                   ;opcode...
cmp al,2
jne byte doDHCPautoConf_j3
lodsb                   ;hw type...
cmp al,1
jne byte doDHCPautoConf_j3
lodsb                   ;hw addr size...
cmp al,nic_addrSize
jne byte doDHCPautoConf_j3
lodsb                   ;hops...
mov eax,def:[si+12]
mov def:[dataSeg_tmpNm4],eax
add si,232              ;skip to magic number...
lodsd
cmp eax,63538263h
jne byte doDHCPautoConf_j3
neg bx
add bx,si
sub cx,bx
call doDHCPautoConf_j4
cmp ax,2
jne byte doDHCPautoConf_j3
mov si,offset doDHCPautoConf_txt02
call writeCode
mov ebp,def:[dataSeg_tmpNm4]
mov def:[dataSeg_loclIP],ebp
call writeIPaddr
mov si,offset doDHCPautoConf_txt03
call writeCode
mov ebp,def:[dataSeg_tmpNm5]
mov def:[dataSeg_defGat],ebp
call writeIPaddr
mov si,offset doDHCPautoConf_txt04
call writeCode
mov ebp,def:[dataSeg_tmpNm6]
mov def:[dataSeg_netMsk],ebp
call writeIPaddr
mov si,offset main_txtCRLF
call writeCode

mov word def:[dataSeg_tmpNm2],10
doDHCPautoConf_j12:
mov byte def:[dataSeg_ignore],3
call dword timer_start
mov def:[dataSeg_tmpNm3],ebp
dec word def:[dataSeg_tmpNm2]
jns byte doDHCPautoConf_j13
stc
ret
doDHCPautoConf_j13:
mov si,offset doDHCPautoConf_txt05
call writeCode
mov di,dataSeg_freMem
mov al,1                ;opcode...
stosb
mov al,1                ;hw type...
stosb
mov al,nic_addrSize     ;hw addr size...
stosb
mov al,0                ;hops...
stosb
movzx word eax,def:[dataSeg_tmpNm2]     ;transaction id...
stosd
neg ax                  ;secs...
add ax,50
xchg al,ah
stosw
sub ax,ax               ;flags...
stosw
mov cx,16               ;addresses...
rep
  stosb
mov si,dataSeg_parAdr   ;chaddr...
mov cx,nic_addrSize
rep
  movsb
mov cx,16
sub cx,nic_addrSize
sub ax,ax
rep
  stosb
mov cx,64               ;sname...
rep
  stosb
mov cx,128              ;file...
rep
  stosb
mov eax,63538263h       ;magic cookie...
stosd
mov al,53               ;type...
mov ah,1                ;length...
stosw
mov al,3                ;value...
stosb
mov al,61               ;type...
mov ah,nic_addrSize     ;length...
inc ah
stosw
mov al,1                ;value...
stosb
mov si,dataSeg_parAdr
mov cx,nic_addrSize
rep
  movsb
mov al,50               ;type...
mov ah,4                ;length...
stosw
mov eax,def:[dataSeg_loclIP]    ;value...
stosd
mov al,51               ;type...
mov ah,4                ;length...
stosw
mov eax,def:[dataSeg_tmpNm8]    ;value...
stosd
mov al,54               ;type...
mov ah,4                ;length...
stosw
mov eax,def:[dataSeg_peerIP]    ;value...
stosd
mov al,55               ;type...
mov ah,3                ;length...
stosw
mov al,1
stosb
mov al,3
stosb
mov al,5
stosb
mov al,0ffh             ;end...
stosb
call sendUDPpacket

doDHCPautoConf_j14:
mov ebp,def:[dataSeg_tmpNm3]
call dword timer_past
cmp ax,5
ja word doDHCPautoConf_j12
call recvUDPpacket
jcxz byte doDHCPautoConf_j14
mov bx,si
lodsb                   ;opcode...
cmp al,2
jne byte doDHCPautoConf_j14
lodsb                   ;hw type...
cmp al,1
jne byte doDHCPautoConf_j14
lodsb                   ;hw addr size...
cmp al,nic_addrSize
jne byte doDHCPautoConf_j14
lodsb                   ;hops...
mov eax,def:[si+12]
mov def:[dataSeg_tmpNm4],eax
add si,232              ;skip to magic number...
lodsd
cmp eax,63538263h
jne byte doDHCPautoConf_j14
neg bx
add bx,si
sub cx,bx
call doDHCPautoConf_j4
cmp ax,5
jne byte doDHCPautoConf_j14

mov si,dataSeg_freMem
add si,44
call parseIPaddr
mov di,dataSeg_freMem
add di,800h
mov eax,ebp
stosd
mov si,dataSeg_freMem
add si,108
mov cx,128
doDHCPautoConf_j15:
lodsb
stosb
dec cx
jz byte doDHCPautoConf_j16
or al,al
jnz byte doDHCPautoConf_j15
doDHCPautoConf_j16:
sub ax,ax
stosb

mov si,offset doDHCPautoConf_txt06
call writeCode
call dword timer_start
mov ax,bp
shr ebp,16
xor ax,bp
and ah,3fh
or ah,80h
mov def:[dataSeg_loclPT],ax
sub ax,ax
mov def:[dataSeg_peerPT],ax
mov def:[dataSeg_ignore],al
mov si,dataSeg_freMem
add si,800h
lodsd
mov def:[dataSeg_peerIP],eax
push si
mov ebp,eax
call writeIPaddr
mov si,offset doDHCPautoConf_txt07
call writeCode
pop si
call writeData
mov si,offset main_txtCRLF
call writeCode

jmp word doArpLookup
doDHCPautoConf_txt01 db 'sending DHCP discover...',13,10,0
doDHCPautoConf_txt02 db 'got DHCP offer: ip=',0
doDHCPautoConf_txt03 db ' gw=',0
doDHCPautoConf_txt04 db ' mask=',0
doDHCPautoConf_txt05 db 'sending DHCP request...',13,10,0
doDHCPautoConf_txt06 db 'got DHCP ack: server=',0
doDHCPautoConf_txt07 db ' file=',0
doDHCPautoConf_j4:
sub eax,eax
mov def:[dataSeg_tmpNm8],eax    ;lease time...
mov def:[dataSeg_tmpNm5],eax    ;gateway ip address...
mov def:[dataSeg_tmpNm6],eax    ;netmask address...
dec ax
mov def:[dataSeg_tmpNm7],ax     ;dhcp type...
mov bp,si
doDHCPautoConf_j5:
mov si,bp
or cx,cx
js byte doDHCPautoConf_j6
jz byte doDHCPautoConf_j6
sub ax,ax
lodsb                   ;type...
or al,al
jz byte doDHCPautoConf_j5
mov dx,ax
lodsb                   ;size...
mov bx,ax
lea bp,def:[si+bx]
sub cx,bx
cmp dl,53
je byte doDHCPautoConf_j7
cmp dl,1
je byte doDHCPautoConf_j8
cmp dl,3
je byte doDHCPautoConf_j9
cmp dl,54
je byte doDHCPautoConf_j10
cmp dl,51
je byte doDHCPautoConf_j11
cmp dl,0ffh
jne byte doDHCPautoConf_j5
doDHCPautoConf_j6:
mov eax,def:[dataSeg_tmpNm7]
ret
doDHCPautoConf_j7:
lodsb
mov def:[dataSeg_tmpNm7],ax
jmp byte doDHCPautoConf_j5
doDHCPautoConf_j8:
lodsd
mov def:[dataSeg_tmpNm6],eax
jmp byte doDHCPautoConf_j5
doDHCPautoConf_j9:
lodsd
mov def:[dataSeg_tmpNm5],eax
jmp byte doDHCPautoConf_j5
doDHCPautoConf_j10:
lodsd
mov def:[dataSeg_peerIP],eax
jmp byte doDHCPautoConf_j5
doDHCPautoConf_j11:
lodsd
mov def:[dataSeg_tmpNm8],eax
jmp byte doDHCPautoConf_j5
endp
;-------------------------------
