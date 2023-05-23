use32                           ;not a question...
;-------------------------------

;------------------------------- data list of dma ports...
DMAchannelData:
;0:mask,1:flip,2:mode,3:addr,4:page,5:count,6:shl,7:num
db 00ah,00ch,00bh,000h,087h,001h,0,0  ;dma 0
db 00ah,00ch,00bh,002h,083h,003h,0,1  ;dma 1
db 00ah,00ch,00bh,004h,081h,005h,0,2  ;dma 2
db 00ah,00ch,00bh,006h,082h,007h,0,3  ;dma 3
db 0d4h,0d8h,0d6h,0c0h,08fh,0c2h,1,0  ;dma 4
db 0d4h,0d8h,0d6h,0c4h,08bh,0c6h,1,1  ;dma 5
db 0d4h,0d8h,0d6h,0c8h,089h,0cah,1,2  ;dma 6
db 0d4h,0d8h,0d6h,0cch,08ah,0ceh,1,3  ;dma 7
;-------------------------------

;------------------------------- get byte count of a dma channel..
proc DMAchannelByteCount
;in:  dl-dma number... /0..7/
;out: ebx-byte counter...
push edx
push ecx
push eax
push ebp
and edx,7
lea ebp,def:[edx*8+DMAchannelData]
sub edx,edx
mov dl,cs:[ebp+1]
mov al,0
out dx,al
sub ebx,ebx
mov dl,cs:[ebp+5]
in al,dx
mov bl,al
in al,dx
mov bh,al
mov cl,cs:[ebp+6]
inc ebx
shl ebx,cl
pop ebp
pop eax
pop ecx
pop edx
retnd
endp
;-------------------------------

;------------------------------- start a transfer on a dma channel...
proc DMAchannelStart
;in: dl-dma number... /0..7/
;    esi-offset of transfer...
;    edi-bytes to transfer...
;    bh-mode: 0=demand, 1=single, 2=block, 3=cascade
;    ch-direction: 0=verify, 1=card>>mem, 2=mem>>card
pushad
and edx,7
lea ebp,def:[edx*8+DMAchannelData]
sub edx,edx
mov dl,cs:[ebp+0]
mov al,cs:[ebp+7]
or al,4
out dx,al
mov dl,cs:[ebp+1]
mov al,0
out dx,al
mov cl,cs:[ebp+6]
shr esi,cl
shr edi,cl
dec edi
mov dl,cs:[ebp+2]
mov al,cs:[ebp+7]
and ch,11b
shl ch,2
or al,ch
shl bh,6
or al,ch
out dx,al
mov dl,cs:[ebp+5]
mov eax,edi
out dx,al
mov al,ah
out dx,al
mov dl,cs:[ebp+3]
mov eax,esi
out dx,al
mov al,ah
out dx,al
shr esi,16
mov eax,esi
mov cl,cs:[ebp+6]
shl al,cl
mov dl,cs:[ebp+4]
out dx,al
mov dl,cs:[ebp+0]
mov al,cs:[ebp+7]
out dx,al
popad
retnd
endp
;-------------------------------

;------------------------------- stop transfer on a dma channel...
proc DMAchannelStop
;in: dl-dma number... /0..7/
push edx
push ebx
push eax
push ebp
and edx,7
lea ebp,def:[edx*8+DMAchannelData]
sub edx,edx
mov dl,cs:[ebp+0]
mov al,cs:[ebp+7]
or al,4
out dx,al
pop ebp
pop eax
pop ebx
pop edx
retnd
endp
;-------------------------------

;------------------------------- test address for dma actions...
proc DMAchannelTestAddr
;in:  esi-offset of transfer...
;     edi-bytes to transfer...
;out: carry-cleared if successful...
push esi
push edi
push ecx
test esi,1
jnz byte DMAchannelTestAddr_err
test edi,1
jnz byte DMAchannelTestAddr_err
test edi,0ffff0000h
jnz byte DMAchannelTestAddr_err
mov ecx,esi
shr ecx,16
add esi,edi
test esi,0ff000000h
jnz byte DMAchannelTestAddr_err
shr esi,16
cmp ecx,esi
jne byte DMAchannelTestAddr_err
clc
DMAchannelTestAddr_vege:
pop ecx
pop edi
pop esi
retnd
DMAchannelTestAddr_err:
stc
jmp byte DMAchannelTestAddr_vege
endp
;-------------------------------
