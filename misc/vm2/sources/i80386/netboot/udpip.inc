;-------------------------------
dataSeg_perMac equ 1000h        ;16: peer mac address...
dataSeg_peerIP equ 1010h        ;dd: peer ip address...
dataSeg_loclIP equ 1014h        ;dd: local ip address...
dataSeg_peerPT equ 1018h        ;dw: peer udp port...
dataSeg_loclPT equ 101ah        ;dw: local udp port...
dataSeg_netMsk equ 101ch        ;dd: netmask value...
dataSeg_defGat equ 1020h        ;dd: default gateway...
dataSeg_ipIDvl equ 1024h        ;dw: ip identify number...
dataSeg_ignore equ 1026h        ;db: ignore: 1-remIP, 2-locIP, 4-remPT, 8-locPT...
;-------------------------------



;-------------------------------
proc doArpLookup
;out: carry cleared if succeed..
mov eax,def:[dataSeg_peerIP]
mov ecx,def:[dataSeg_loclIP]
mov edx,def:[dataSeg_netMsk]
mov ebx,eax
and eax,edx
and ecx,edx
cmp eax,ecx
je byte doArpLookup_j1
mov ebx,def:[dataSeg_defGat]
doArpLookup_j1:
mov def:[dataSeg_tmpNm1],ebx
mov word def:[dataSeg_tmpNm2],10
doArpLookup_j2:
call dword timer_start
mov def:[dataSeg_tmpNm3],ebp
dec word def:[dataSeg_tmpNm2]
jns byte doArpLookup_j3
stc
ret
doArpLookup_j3:
mov si,offset udpip_txt01
call writeCode
mov ebp,def:[dataSeg_tmpNm1]
call writeIPaddr
mov si,offset udpip_txt02
call writeCode
mov di,dataSeg_freMem
mov si,dataSeg_parBrd   ;broadcast address...
mov cx,nic_addrSize
rep
  movsb
mov ax,0608h            ;ethertype...
stosw
mov ax,100h             ;ethernet...
stosw
mov ax,008h             ;ip protocol...
stosw
mov al,nic_addrSize     ;hw address size...
stosb
mov al,4                ;protocol addr size...
stosb
mov ax,100h             ;opcode: request...
stosw
mov si,dataSeg_parAdr   ;my hw address...
mov cx,nic_addrSize
rep
  movsb
mov eax,def:[dataSeg_loclIP]    ;my ip address...
stosd
sub ax,ax               ;peer hw address...
mov cx,nic_addrSize
rep
  stosb
mov eax,def:[dataSeg_tmpNm1]    ;peer ip address...
stosd
call device_send        ;send the packet...
doArpLookup_j4:
mov ebp,def:[dataSeg_tmpNm3]
call dword timer_past
cmp ax,5
ja word doArpLookup_j2
call doReleq2wire
dec ax
js byte doArpLookup_j4
jz byte doArpLookup_j4
mov si,dataSeg_freMem
add si,nic_addrSize
lodsw                   ;ethertype...
cmp ax,0608h
jne byte doArpLookup_j4
lodsw                   ;hw type...
cmp ax,100h
jne byte doArpLookup_j4
lodsw                   ;protocol type...
cmp ax,008h
jne byte doArpLookup_j4
lodsb                   ;hw address size...
cmp al,nic_addrSize
jne byte doArpLookup_j4
lodsb                   ;protocol addr size...
cmp al,4
jne byte doArpLookup_j4
lodsw                   ;opcode...
cmp ax,200h             ;reply?
jne byte doArpLookup_j4
mov eax,def:[si+nic_addrSize]
cmp eax,def:[dataSeg_tmpNm1]
jne byte doArpLookup_j4
mov di,dataSeg_perMac
mov cx,nic_addrSize
rep
  movsb
mov si,offset udpip_txt04
call writeCode
mov esi,dataSeg_perMac
call writeMac
mov si,offset udpip_txt02
call writeCode
clc
ret
endp
;-------------------------------

;-------------------------------
proc recvUDPpacket
;out: dataSeg_freMem-payload...
;     cx-size of payload... (0=none)
call doReleq2wire
cmp al,1
jne byte recvUDPpacket_j1
mov di,dataSeg_freMem
mov bx,cx
rep
  movsb
mov si,dataSeg_freMem
mov cx,bx
ret
recvUDPpacket_j1:
mov si,dataSeg_freMem
sub cx,cx
ret
endp
;-------------------------------

;-------------------------------
proc sendUDPpacket
;in: dataSeg_freMem-payload...
;    di-offset of last+1 byte...
mov cx,di
sub cx,dataSeg_freMem
mov bp,cx
mov bx,nic_addrSize
add bx,30
mov si,di
add di,bx
std
dec si
dec di
rep
  movsb
cld
mov di,dataSeg_freMem
mov si,dataSeg_perMac
mov cx,nic_addrSize
rep
  movsb
mov ax,008h             ;ethertype...
stosw
mov al,45h              ;ihl/ver...
stosb
mov al,0                ;tos...
stosb
mov ax,28               ;total length...
add ax,bp
xchg al,ah
stosw
inc word def:[dataSeg_ipIDvl]   ;ip id value...
mov ax,def:[dataSeg_ipIDvl]
xchg al,ah
stosw
sub ax,ax               ;fragment offset...
stosw
mov al,0ffh             ;ttl...
stosb
mov al,11h              ;protocol...
stosb
sub ax,ax               ;chksum...
stosw
mov eax,def:[dataSeg_loclIP]    ;source address...
stosd
mov eax,def:[dataSeg_peerIP]    ;target address...
stosd
push di
mov cx,20
mov si,di
sub si,cx
sub dx,dx
call updateUDPchksum
pop di
not dx
mov def:[di-10],dx
mov ax,def:[dataSeg_loclPT]     ;source port...
xchg al,ah
stosw
mov ax,def:[dataSeg_peerPT]     ;target port...
xchg al,ah
stosw
lea ax,def:[bp+8]       ;length...
xchg al,ah
stosw
sub ax,ax               ;chksum...
stosw
push di
lea si,def:[di-8]
lea cx,def:[bp+8]
sub dx,dx
call updateUDPchksum
pop di
push di
add di,bp               ;skip data
mov si,di
mov eax,def:[dataSeg_loclIP]    ;source address...
stosd
mov eax,def:[dataSeg_peerIP]    ;target address...
stosd
mov ax,1100h            ;protocol...
stosw
lea ax,def:[bp+8]       ;length...
xchg al,ah
stosw
mov ecx,12              ;chksum...
call updateUDPchksum
pop si
not dx
mov def:[si-2],dx
call device_send        ;send the packet...
ret
endp
;-------------------------------

;-------------------------------
proc doReleq2wire
;out: ax-packet got: 0-none, 1-udp, 2-unknown...
;     si-offset of payload...
;     cx-size of packet...
call device_recv
cld
sub cx,2
jns byte doReleq2wire_j1
sub ax,ax
ret
doReleq2wire_j1:
movzx ebp,cx
mov si,dataSeg_freMem
add si,nic_addrSize
lodsw                   ;ethertype...
cmp ax,0608h
je byte doReleq2wire_j3
cmp ax,0008h
je word doReleq2wire_j4
doReleq2wire_j2:
sub ax,ax
inc ax
inc ax
ret
doReleq2wire_j3:        ;arp...
lodsw                   ;hw type...
cmp ax,100h
jne byte doReleq2wire_j2
lodsw                   ;protocol...
cmp ax,008h
jne byte doReleq2wire_j2
lodsb                   ;hw address size...
cmp al,nic_addrSize
jne byte doReleq2wire_j2
lodsb                   ;protocol addr size...
cmp al,4
jne byte doReleq2wire_j2
lodsw                   ;opcode...
cmp ax,100h
jne byte doReleq2wire_j2
add si,nic_addrSize
add si,nic_addrSize
add si,4
lodsd
cmp eax,def:[dataSeg_loclIP]
jne byte doReleq2wire_j2
mov di,dataSeg_freMem
add di,nic_addrSize
mov ax,0608h            ;ethertype...
stosw
mov ax,100h             ;hw type...
stosw
mov ax,008h             ;protocol type...
stosw
mov al,nic_addrSize     ;hw address size...
stosb
mov al,4                ;protocol addr size...
stosb
mov ax,200h             ;opcode...
stosw
push di                 ;target address...
mov si,di
mov bp,nic_addrSize
add bp,4
mov cx,bp
add di,bp
rep
  movsb
pop di
mov si,dataSeg_parAdr   ;my hw address...
mov cx,nic_addrSize
rep
  movsb
mov eax,def:[dataSeg_loclIP]    ;my ip address...
stosd
add di,bp               ;peer addresses...
call device_send        ;send the packet...
sub ax,ax
ret
doReleq2wire_j4:        ;ip...
lodsb                   ;ver+ihl...
and al,0f0h
cmp al,40h
jne word doReleq2wire_j2
inc si                  ;skip tos...
lodsw                   ;total length...
xchg al,ah
cmp ax,bp
ja word doReleq2wire_j2
mov bp,ax
inc si                  ;skip id...
inc si
lodsw                   ;fragment offset...
xchg al,ah
and ax,2fffh
jnz word doReleq2wire_j2
inc si                  ;skip ttl...
lodsb                   ;protocol...
cmp al,1
je byte doReleq2wire_j5
cmp al,11h
jne word doReleq2wire_j2
doReleq2wire_j5:
push si
mov si,dataSeg_freMem
add si,nic_addrSize
add si,2
movzx byte cx,def:[si]
and cl,0fh
shl cx,2
sub dx,dx
call updateUDPchksum
pop si
inc dx
jnz word doReleq2wire_j2
inc si                  ;skip checksum...
inc si
lodsd                   ;source addresss...
mov ebx,eax
lodsd                   ;target address...
mov edi,eax
;skip header...
mov si,dataSeg_freMem
add si,nic_addrSize
add si,2
movzx byte cx,def:[si]
and cl,0fh
shl cx,2
mov dl,def:[si+9]       ;protocol...
add si,cx
sub bp,cx
js word doReleq2wire_j2
cmp dl,1                ;icmp?
jne word doReleq2wire_j6
cmp edi,def:[dataSeg_loclIP]
jne word doReleq2wire_j2
pushad
mov cx,bp
sub dx,dx
call updateUDPchksum
inc dx
popad
jnz word doReleq2wire_j2
lodsb                   ;type...
cmp al,8
jne word doReleq2wire_j2
lodsb                   ;code...
or al,al
jnz word doReleq2wire_j2
inc si                  ;skip chksum...
inc si
sub bp,4
js word doReleq2wire_j2
push si
push bp
mov di,dataSeg_freMem
add di,nic_addrSize
mov ax,008h             ;ethertype...
stosw
push di
mov al,45h              ;ihl/ver...
stosb
sub ax,ax               ;tos...
stosb
mov ax,24               ;total length...
add ax,bp
xchg al,ah
stosw
inc word def:[dataSeg_ipIDvl]   ;ip id value...
mov ax,def:[dataSeg_ipIDvl]
xchg al,ah
stosw
sub ax,ax               ;fragment offset...
stosw
mov al,0ffh             ;ttl...
stosb
mov al,1                ;protocol...
stosb
sub ax,ax               ;chksum...
stosw
mov eax,def:[dataSeg_loclIP]    ;source ip...
stosd
mov eax,ebx             ;target ip...
stosd
pop si                  ;update chksum...
mov cx,di
sub cx,si
push di
sub dx,dx
call updateUDPchksum
pop di
not dx
mov def:[di-10],dx
mov bp,di
mov al,0                ;type...
stosb
mov al,0                ;code...
stosb
sub ax,ax               ;chksum...
stosw
pop cx                  ;copy payload...
pop si
rep
  movsb
mov si,bp               ;update chksum...
mov cx,di
sub cx,si
push bp
push di
sub dx,dx
call updateUDPchksum
pop di
pop si
not dx
mov def:[si+2],dx
call device_send        ;send the packet...
sub ax,ax
ret
doReleq2wire_j6:        ;udp packet...
mov dl,def:[dataSeg_ignore]
test dl,1               ;need src ip...
jnz byte doReleq2wire_j7
cmp ebx,def:[dataSeg_peerIP]
jne word doReleq2wire_j2
doReleq2wire_j7:
test dl,2               ;need trg ip...
jnz byte doReleq2wire_j8
cmp edi,def:[dataSeg_loclIP]
jne word doReleq2wire_j2
doReleq2wire_j8:
mov edx,edi
lea di,def:[si-12]
mov cx,di
mov eax,ebx             ;source address...
stosd
mov eax,edx             ;target address...
stosd
mov ax,1100h            ;protocol...
stosw
mov ax,bp               ;length...
xchg al,ah
stosw
mov bx,cx
mov dl,def:[dataSeg_ignore]
lodsw                   ;src port...
test dl,4
jnz byte doReleq2wire_j9
xchg al,ah
cmp ax,def:[dataSeg_peerPT]
jne word doReleq2wire_j2
doReleq2wire_j9:
lodsw                   ;trg port...
test dl,8
jnz byte doReleq2wire_j10
xchg al,ah
cmp ax,def:[dataSeg_loclPT]
jne word doReleq2wire_j2
doReleq2wire_j10:
lodsw                   ;size...
xchg al,ah
cmp ax,bp
ja word doReleq2wire_j2
mov bp,ax
push si
mov si,bx
lea cx,def:[bp+12]
sub dx,dx
call updateUDPchksum
pop si
lodsw                   ;chksum...
or ax,ax
jz byte doReleq2wire_j11
inc dx
jnz word doReleq2wire_j2
doReleq2wire_j11:
sub bp,8
js word doReleq2wire_j2
mov cx,bp
sub ax,ax
inc ax
ret
endp
;-------------------------------

;-------------------------------
proc updateUDPchksum
;in:  ds:si-buffer...
;     cx-bytes in it...
;     dx-old result...
;out: dx-result...
mov bl,cl
shr cx,1
jcxz updateUDPchksum_remain
clc
updateUDPchksum_deloop:
lodsw
adc dx,ax
loop updateUDPchksum_deloop
call updateUDPchksum_flushcarry
updateUDPchksum_remain:
and bl,1
jz updateUDPchksum_done
xor ah,ah
lodsb
add dx,ax
call updateUDPchksum_flushcarry
jmp updateUDPchksum_done
updateUDPchksum_flushcarry:
adc dx,0
adc dx,0
updateUDPchksum_done:
ret
endp
;-------------------------------

;-------------------------------
proc parseIPaddr
;in:  si-offset of data...
;out: ebp-address...
push si
mov cx,3
parseIPaddr_j1:
lodsb
cmp al,'.'
jne byte parseIPaddr_j1
mov byte def:[si-1],0
loop parseIPaddr_j1
pop si
push si
sub ebp,ebp
mov cx,4
parseIPaddr_j2:
push cx
push ebp
call str2num
pop ebp
pop cx
jc byte parseIPaddr_j3
test edx,0ffffff00h
jnz byte parseIPaddr_j3
or bp,dx
ror ebp,8
parseIPaddr_j3:
loop parseIPaddr_j2
mov dx,si
pop si
mov cx,3
parseIPaddr_j4:
lodsb
or al,al
jnz byte parseIPaddr_j4
mov byte def:[si-1],'.'
loop parseIPaddr_j4
mov si,dx
ret
endp
;-------------------------------

;-------------------------------
proc writeIPaddr
;in: ebp-address...
mov cx,4
writeIPaddr_j1:
push ebp
push cx
movzx eax,bp
mov ah,0
call conv2dec
call writeData
pop cx
push cx
cmp cl,1
jbe byte writeIPaddr_j2
mov si,offset udpip_txt03
call writeCode
writeIPaddr_j2:
pop cx
pop ebp
ror ebp,8
loop writeIPaddr_j1
ret
endp
;-------------------------------

;-------------------------------
udpip_txt01 db 'sending ARP request to ',0
udpip_txt02 db '...',13,10,0
udpip_txt03 db '.',0
udpip_txt04 db 'got ARP reply from ',0
;-------------------------------
