;-------------------------------
proc i82365_enableCIS
;in: ebp-where from data...
;system mem map0 offset high...
mov al,ds:[ebp+oneSocket_sock]
shl al,6
add al,15h
push eax
call dword i82365_read8
pop edx
or al,40h
mov ah,al
mov al,dl
call dword i82365_write8
retnd
endp
;-------------------------------

;-------------------------------
proc i82365_putStat
;in: ebp-where from data...
;io mappings...
lea edi,def:[ebp+oneSocket_iomp]
sub esi,esi
sub edx,edx
mov ecx,1
i82365_putStat_j1:
push ecx
push esi
push edi
push edx
;system io map start...
mov eax,esi
shl eax,2
mov ah,ds:[ebp+oneSocket_sock]
shl ah,6
or al,ah
add al,08h
mov ebx,def:[edi+oneMaping_begH]
call dword i82365_write16
;system io map end...
mov eax,esi
shl eax,2
mov ah,ds:[ebp+oneSocket_sock]
shl ah,6
or al,ah
add al,0ah
mov ebx,def:[edi+oneMaping_begH]
add ebx,def:[edi+oneMaping_size]
dec ebx
call dword i82365_write16
;card io map offset...
mov eax,esi
shl eax,1
mov ah,ds:[ebp+oneSocket_sock]
shl ah,6
or al,ah
add al,36h
mov ebx,def:[edi+oneMaping_begC]
sub ebx,def:[edi+oneMaping_begH]
and bl,0feh
call dword i82365_write16
;io window control...
sub eax,eax
mov al,def:[edi+oneMaping_bits]
cmp al,16
sete al
mov ah,def:[edi+oneMaping_auto]
and ah,1
shl ah,1
or al,ah
mov ah,def:[edi+oneMaping_time]
and ah,1
shl ah,3
or al,ah
mov ecx,esi
shl ecx,2
shl eax,cl
pop edx
or edx,eax
pop edi
pop esi
pop ecx
inc esi
add edi,oneMaping__siz
dec ecx
jns dword i82365_putStat_j1
;io window control...
mov al,ds:[ebp+oneSocket_sock]
shl al,6
add al,07h
mov ah,dl
call dword i82365_write8
;mem mappings...
lea edi,def:[ebp+oneSocket_mems]
sub esi,esi
mov ecx,4
i82365_putStat_j2:
push ecx
push esi
push edi
;system mem map start...
mov eax,esi
shl eax,3
mov ah,ds:[ebp+oneSocket_sock]
shl ah,6
or al,ah
add al,10h
mov ebx,def:[edi+oneMaping_begH]
shr ebx,12
and ebx,0fffh
mov cl,def:[edi+oneMaping_bits]
cmp cl,16
sete cl
shl ecx,7
or bh,cl
call dword i82365_write16
;system mem map end...
mov eax,esi
shl eax,3
mov ah,ds:[ebp+oneSocket_sock]
shl ah,6
or al,ah
add al,12h
mov ebx,def:[edi+oneMaping_begH]
add ebx,def:[edi+oneMaping_size]
dec ebx
shr ebx,12
mov cl,def:[edi+oneMaping_time]
and cl,1
shl ecx,6
or bh,cl
call dword i82365_write16
;card mem map offset...
mov eax,esi
shl eax,3
mov ah,ds:[ebp+oneSocket_sock]
shl ah,6
or al,ah
add al,14h
mov ebx,def:[edi+oneMaping_begC]
sub ebx,def:[edi+oneMaping_begH]
shr ebx,12
and ebx,3fffh
mov cl,def:[edi+oneMaping_wrpr]
and cl,1
shl cl,7
or bh,cl
call dword i82365_write16
pop edi
pop esi
pop ecx
inc esi
add edi,oneMaping__siz
dec ecx
jns dword i82365_putStat_j2
;mapping enable...
sub edx,edx
sub esi,esi
lea edi,def:[ebp+oneSocket_mems]
mov ecx,5
i82365_putStat_j3:
push ecx
mov al,def:[edi+oneMaping_actv]
and al,1
mov ecx,esi
shl eax,cl
or dl,al
pop ecx
inc esi
add edi,oneMaping__siz
loopd i82365_putStat_j3
inc esi
lea edi,def:[ebp+oneSocket_iomp]
mov ecx,2
i82365_putStat_j4:
push ecx
mov al,def:[edi+oneMaping_actv]
and al,1
mov ecx,esi
shl eax,cl
or dl,al
pop ecx
inc esi
add edi,oneMaping__siz
loopd i82365_putStat_j4
or dl,20h
mov ah,dl
mov al,ds:[ebp+oneSocket_sock]
shl al,6
add al,06h
call dword i82365_write8
mov edi,ebp
;general control...
mov ah,def:[edi+oneSocket_irqn]
and ah,0fh
mov al,def:[edi+oneSocket_iocd]
and al,1
shl al,5
or ah,al
mov al,def:[edi+oneSocket_rset]
and al,1
xor al,1
shl al,6
or ah,al
mov al,def:[edi+oneSocket_sock]
shl al,6
add al,03h
call dword i82365_write8
;management interrupt control...
mov al,def:[edi+oneSocket_sock]
shl al,6
add al,05h
mov ah,0
call dword i82365_write8
;misc control 1...
mov ah,def:[edi+oneSocket_vccp]
cmp ah,33
setbe ah
shl ah,1
or ah,10h
mov al,def:[edi+oneSocket_sock]
shl al,6
add al,16h
call dword i82365_write8
;power control...
mov al,def:[edi+oneSocket_vppp]
mov ah,2
cmp al,120
jae byte i82365_putStat_j5
mov ah,1
cmp al,33
jae byte i82365_putStat_j5
mov ah,0
i82365_putStat_j5:
mov al,def:[edi+oneSocket_vccp]
or al,al
setnz al
shl al,4
or ah,al
mov al,def:[edi+oneSocket_atpw]
and al,1
shl al,5
or ah,al
mov al,def:[edi+oneSocket_cden]
and al,1
shl al,7
or ah,al
mov al,def:[edi+oneSocket_sock]
shl al,6
add al,02h
call dword i82365_write8
retnd
endp
;-------------------------------



;-------------------------------
proc i82365_getStat
;in: ebp-where to get data...
mov edi,ebp
mov dl,def:[edi]
mov ecx,oneSocket__siz
sub eax,eax
rep
  stosb ptr32
mov edi,ebp
mov def:[edi],dl
;interface status...
mov al,def:[edi+oneSocket_sock]
shl al,6
add al,01h
call dword i82365_read8
mov ah,al
and al,0ch
cmp al,0ch
sete al
mov def:[edi+oneSocket_dtct],al
test ah,10h
setnz al
mov def:[edi+oneSocket_wrpr],al
test ah,20h
setnz al
mov def:[edi+oneSocket_redy],al
test ah,40h
setnz al
mov def:[edi+oneSocket_pwon],al
test ah,80h
setnz al
mov def:[edi+oneSocket_pwok],al
;power control...
mov al,def:[edi+oneSocket_sock]
shl al,6
add al,02h
call dword i82365_read8
movzx ecx,al
and cl,3
mov cl,cs:[i82365_getStat_d1+ecx]
mov def:[edi+oneSocket_vppp],cl
test al,10h
setnz cl
imul ecx,50
mov def:[edi+oneSocket_vccp],cl
test al,20h
setnz cl
mov def:[edi+oneSocket_atpw],cl
test al,80h
setnz cl
mov def:[edi+oneSocket_cden],cl
;general control...
mov al,def:[edi+oneSocket_sock]
shl al,6
add al,03h
call dword i82365_read8
test al,20h
setnz ah
mov def:[edi+oneSocket_iocd],ah
test al,40h
setz ah
mov def:[edi+oneSocket_rset],ah
and al,0fh
mov def:[edi+oneSocket_irqn],al
;card status change...
mov al,def:[edi+oneSocket_sock]
shl al,6
add al,04h
call dword i82365_read8
test al,04h
setnz ah
mov def:[edi+oneSocket_crdy],ah
test al,08h
setnz ah
mov def:[edi+oneSocket_cdtc],ah
;misc control 1...
mov al,def:[edi+oneSocket_sock]
shl al,6
add al,16h
call dword i82365_read8
movzx eax,al
shr eax,1
and al,1
mov al,cs:[i82365_getStat_d2+eax]
mov ah,def:[edi+oneSocket_vccp]
or ah,ah
setz ah
dec ah
and al,ah
mov def:[edi+oneSocket_vccp],al
;io mappings...
lea edi,def:[ebp+oneSocket_iomp]
sub esi,esi
mov ecx,1
i82365_getStat_j1:
push ecx
push esi
push edi
;mappings enable...
mov al,ds:[ebp+oneSocket_sock]
shl al,6
add al,06h
call dword i82365_read8
lea cx,def:[esi+6]
shr eax,cl
and al,1
setnz al
mov def:[edi+oneMaping_actv],al
;io window control...
mov al,ds:[ebp+oneSocket_sock]
shl al,6
add al,07h
call dword i82365_read8
mov ecx,esi
shl ecx,1
shr eax,cl
test al,01h
setnz cl
inc ecx
shl ecx,3
mov def:[edi+oneMaping_bits],cl
test al,02h
setnz cl
mov def:[edi+oneMaping_auto],cl
test al,08h
setnz cl
mov def:[edi+oneMaping_time],cl
;system io map start...
mov eax,esi
shl eax,2
mov ah,ds:[ebp+oneSocket_sock]
shl ah,6
or al,ah
add al,08h
call dword i82365_read16
mov def:[edi+oneMaping_begH],ebx
;system io map end...
mov eax,esi
shl eax,2
mov ah,ds:[ebp+oneSocket_sock]
shl ah,6
or al,ah
add al,0ah
call dword i82365_read16
sub ebx,def:[edi+oneMaping_begH]
inc ebx
mov def:[edi+oneMaping_size],ebx
;card io map offset...
mov eax,esi
shl eax,1
mov ah,ds:[ebp+oneSocket_sock]
shl ah,6
or al,ah
add al,36h
call dword i82365_read16
and bl,0feh
add ebx,def:[edi+oneMaping_begH]
and ebx,0ffffh
mov def:[edi+oneMaping_begC],ebx
pop edi
pop esi
pop ecx
inc esi
add edi,oneMaping__siz
dec ecx
jns dword i82365_getStat_j1
;mem mappings...
lea edi,def:[ebp+oneSocket_mems]
sub esi,esi
mov ecx,4
i82365_getStat_j2:
push ecx
push esi
push edi
;mappings enable...
mov al,ds:[ebp+oneSocket_sock]
shl al,6
add al,06h
call dword i82365_read8
mov ecx,esi
shr eax,cl
and al,1
setnz al
mov def:[edi+oneMaping_actv],al
;system mem map start...
mov eax,esi
shl eax,3
mov ah,ds:[ebp+oneSocket_sock]
shl ah,6
or al,ah
add al,10h
call dword i82365_read16
push ebx
and bh,0fh
shl ebx,12
mov def:[edi+oneMaping_begH],ebx
pop eax
test ah,80h
setnz al
inc eax
shl eax,3
mov def:[edi+oneMaping_bits],al
;system mem map end...
mov eax,esi
shl eax,3
mov ah,ds:[ebp+oneSocket_sock]
shl ah,6
or al,ah
add al,12h
call dword i82365_read16
push ebx
and bh,0fh
shl ebx,12
add ebx,1000h
sub ebx,def:[edi+oneMaping_begH]
mov def:[edi+oneMaping_size],ebx
pop eax
test ah,0c0h
setnz al
mov def:[edi+oneMaping_time],al
;card mem map offset...
mov eax,esi
shl eax,3
mov ah,ds:[ebp+oneSocket_sock]
shl ah,6
or al,ah
add al,14h
call dword i82365_read16
push ebx
and bh,3fh
shl ebx,12
add ebx,def:[edi+oneMaping_begH]
and ebx,3ffffffh
mov def:[edi+oneMaping_begC],ebx
pop eax
test ah,80h
setnz al
mov def:[edi+oneMaping_wrpr],al
pop edi
pop esi
pop ecx
inc esi
add edi,oneMaping__siz
dec ecx
jns dword i82365_getStat_j2
retnd
i82365_getStat_d1 db 0,50,120,0
i82365_getStat_d2 db 50,30
endp
;-------------------------------



;-------------------------------
proc i82365_detect
;out: ebp-number of sockets found...
sub ebp,ebp
i82365_detect_j1:
mov eax,ebp
shl al,6
call dword i82365_read8
and al,0f0h
cmp al,80h
jne byte i82365_detect_j2
inc ebp
cmp ebp,4
jb byte i82365_detect_j1
i82365_detect_j2:
retnd
endp
;-------------------------------




;-------------------------------
proc i82365_read8
;in:  al-register to read...
;out: al-register readed...
mov dx,i82365_index
out dx,al
mov dx,i82365_data
in al,dx
retnd
endp
;-------------------------------

;-------------------------------
proc i82365_read16
;in:  al-register to read...
;out: ebx-register readed...
mov dx,i82365_index
out dx,al
inc eax
mov ah,al
mov dx,i82365_data
in al,dx
movzx ebx,al
mov dx,i82365_index
mov al,ah
out dx,al
mov dx,i82365_data
in al,dx
mov bh,al
retnd
endp
;-------------------------------

;-------------------------------
proc i82365_write8
;in: al-register to write...
;    ah-value to write..
mov dx,i82365_index
out dx,al
mov dx,i82365_data
mov al,ah
out dx,al
retnd
endp
;-------------------------------

;-------------------------------
proc i82365_write16
;in: al-register to write...
;    bx-value to write...
mov dx,i82365_index
out dx,al
inc eax
mov ah,al
mov dx,i82365_data
mov al,bl
out dx,al
mov dx,i82365_index
mov al,ah
out dx,al
mov dx,i82365_data
mov al,bh
out dx,al
retnd
endp
;-------------------------------

;-------------------------------
i82365_index equ 3e0h
i82365_data equ 3e1h
;-------------------------------
