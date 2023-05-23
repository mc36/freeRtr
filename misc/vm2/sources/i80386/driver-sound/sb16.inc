;-------------------------------
proc sb16dspGet
;out: al-value read...
mov edx,0eh                     ;dsp rx status port...
add edx,def:[dataBlock_basePrt]
sb16dspGet_j1:
in al,dx
and al,80h
jz sb16dspGet_j1
sub edx,4                       ;dsp rx data port...
in al,dx
retnd
endp
;-------------------------------

;-------------------------------
proc sb16dspPut
;out: al-value to write...
mov ah,al
mov edx,0ch                     ;dsp tx status port...
add edx,def:[dataBlock_basePrt]
sb16dspPut_j1:
in al,dx
and al,80h
jnz sb16dspPut_j1
mov al,ah
out dx,al
retnd
endp
;-------------------------------

;-------------------------------
proc sb16mixerGet
;in:  al-offset...
;out: al-value read...
mov edx,4                       ;mixer index port...
add edx,def:[dataBlock_basePrt]
out dx,al
inc edx                         ;mixer data port...
in al,dx
retnd
endp
;-------------------------------

;-------------------------------
proc sb16mixerPut
;in: al-offset...
;    ah-value to write...
mov edx,4                       ;mixer index port...
add edx,def:[dataBlock_basePrt]
out dx,al
inc edx                         ;mixer data port...
mov al,ah
out dx,al
retnd
endp
;-------------------------------

;-------------------------------
proc sb16detect
;out: carry-cleared if cound...
mov edx,06h                     ;reset port...
add edx,def:[dataBlock_basePrt]
mov al,1                        ;set reset...
out dx,al
mov edx,0eh                     ;dsp rx status port...
add edx,def:[dataBlock_basePrt]
mov ecx,256
sb16detect_j1:
in al,dx
loopd sb16detect_j1
mov edx,06h                     ;reset port...
add edx,def:[dataBlock_basePrt]
mov al,0                        ;clear reset...
out dx,al
mov edx,0eh                     ;dsp rx status port...
add edx,def:[dataBlock_basePrt]
mov ecx,512
sb16detect_j2:
in al,dx
and al,80h
jnz byte sb16detect_j3
loopd sb16detect_j2
sb16detect_err:
stc
retnd
sb16detect_j3:
mov edx,0ah                     ;dsp rx data port...
add edx,def:[dataBlock_basePrt]
in al,dx
cmp al,0aah
jne byte sb16detect_err
;get sb version...
mov al,0e1h
sub ebx,ebx
call dword sb16dspPut
mov bh,al
call dword sb16dspGet
mov bl,al
mov def:[dataBlock_hwVersn],ebx
cmp bh,4
jb byte sb16detect_err
;get sb string...
mov al,0e3h
call dword sb16dspPut
mov edi,dataBlock_hwText
sb16detect_j4:
call dword sb16dspGet
cmp al,13
je byte sb16detect_j5
cmp al,10
je byte sb16detect_j5
stosb ptr32
sb16detect_j5:
or al,al
jnz byte sb16detect_j4
;get dma1 number...
mov al,81h
call dword sb16mixerGet
and eax,0ebh
sub ecx,ecx
bsf ecx,eax
mov al,cs:[sb16detect_d1+ecx]
mov def:[dataBlock_basDmaL],eax
;get dma2 number...
mov al,81h
call dword sb16mixerGet
and eax,0ebh
shr eax,4
sub ecx,ecx
bsf ecx,eax
mov al,cs:[sb16detect_d2+ecx]
mov def:[dataBlock_basDmaH],eax
;get irq number...
mov al,81h
call dword sb16mixerGet
and eax,0fh
sub ecx,ecx
bsf ecx,eax
mov al,cs:[sb16detect_d3+ecx]
mov def:[dataBlock_baseIrq],eax
sub eax,eax
mov def:[dataBlock_bufNext],eax
mov def:[dataBlock_bufUsed],eax
mov def:[dataBlock_bufMode],eax
clc
retnd
sb16detect_d1 db 0,1,0,3
sb16detect_d2 db 0,5,6,7
sb16detect_d3 db 2,5,7,10
endp
;-------------------------------

;-------------------------------
proc sb16reset
mov edx,06h                     ;reset port...
add edx,def:[dataBlock_basePrt]
mov al,1                        ;set reset...
out dx,al
mov edx,0eh                     ;dsp rx status port...
add edx,def:[dataBlock_basePrt]
mov ecx,256
sb16reset_j1:
in al,dx
loopd sb16reset_j1
mov edx,06h                     ;reset port...
add edx,def:[dataBlock_basePrt]
mov al,0                        ;clear reset...
out dx,al
mov edx,0eh                     ;dsp rx status port...
add edx,def:[dataBlock_basePrt]
mov ecx,256
sb16reset_j2:
in al,dx
loopd sb16reset_j2
call dword sb16dspGet
retnd
endp
;-------------------------------

;-------------------------------
proc sb16mixeReset
;reset mixer chip...
mov eax,0ff00h
call dword sb16mixerPut
sub eax,eax
call dword sb16mixerPut
retnd
endp
;-------------------------------

;-------------------------------
proc sb16speaker
;in: al-state of speaker: 0=off, 1=on...
and al,1
xor al,1
add al,al
add al,0d1h
call dword sb16dspPut
retnd
endp
;-------------------------------

;-------------------------------
proc sb16sample
;in: eax-sampling rate...
push eax
mov al,41h
call dword sb16dspPut
pop eax
push eax
mov al,ah
call dword sb16dspPut
pop eax
call dword sb16dspPut
retnd
endp
;-------------------------------

;-------------------------------
proc sb16record
;in: edi-bytes to read...
;    esi-buffer physical offset...
sub esi,def:[dataBlock_buf1phy]
mov dl,def:[dataBlock_basDmaH]
mov bh,1                        ;single mode...
mov ch,1                        ;card-->mem...
clts                            ;start dma channel...
dd 28h
shr edi,1
dec edi
push edi
mov al,0bah                     ;16bit...
call dword sb16dspPut
mov al,030h                     ;stereo...
call dword sb16dspPut
pop eax
push eax
call dword sb16dspPut
pop eax
mov al,ah
call dword sb16dspPut
retnd
endp
;-------------------------------

;-------------------------------
proc sb16play
;in: edi-bytes to send...
;    esi-buffer physical offset...
sub esi,def:[dataBlock_buf1phy]
mov dl,def:[dataBlock_basDmaH]
mov bh,1                        ;single mode...
mov ch,2                        ;mem-->card...
clts                            ;start dma channel...
dd 28h
shr edi,1
dec edi
push edi
mov al,0b2h                     ;16bit...
call dword sb16dspPut
mov al,030h                     ;stereo...
call dword sb16dspPut
pop eax
push eax
call dword sb16dspPut
pop eax
mov al,ah
call dword sb16dspPut
retnd
endp
;-------------------------------

;-------------------------------
proc sb16waiter
;in: al-mode...
jmp byte sb16waiter_j2
sb16waiter_j1:
clts                            ;give away the control...
dd 01h
sb16waiter_j2:
mov ecx,def:[dataBlock_bufUsed]
cmp cl,2
jae byte sb16waiter_j1
cmp al,def:[dataBlock_bufMode]
je byte sb16waiter_j3
or cl,cl
jnz byte sb16waiter_j1
mov def:[dataBlock_bufMode],al
sb16waiter_j3:
retnd
endp
;-------------------------------




;-------------------------------
proc sb16startPlay
mov eax,44100
call dword sb16sample
mov eax,def:[dataBlock_bufNext]
mov esi,def:[dataBlock_buf1phy+eax*4]
mov edi,bufferSize
call dword sb16play
retnd
endp
;-------------------------------


;-------------------------------
proc sb16startRecord
mov eax,44100
call dword sb16sample
mov eax,def:[dataBlock_bufNext]
mov esi,def:[dataBlock_buf1phy+eax*4]
mov edi,bufferSize
call dword sb16record
retnd
endp
;-------------------------------


;-------------------------------
proc sb16irq
mov edx,0fh                     ;ack irq...
add edx,def:[dataBlock_basePrt]
in al,dx
dec edx
in al,dx
mov al,def:[dataBlock_bufUsed]
or al,al
jz byte sb16irq_vege
mov al,def:[dataBlock_bufMode]
cmp al,2
je byte sb16irq_rec
cmp al,1
je byte sb16irq_play
;-----------------------
sb16irq_vege:
clts                            ;terminate irq handler...
dd 07h
retnd
;-----------------------
sb16irq_rec:
xor byte def:[dataBlock_bufNext],1
dec byte def:[dataBlock_bufUsed]
jz byte sb16irq_rec_j1
mov eax,def:[dataBlock_bufNext]
mov esi,def:[dataBlock_buf1phy+eax*4]
mov edi,bufferSize
call dword sb16record
jmp byte sb16irq_rec_j2
sb16irq_rec_j1:
call dword sb16reset
sb16irq_rec_j2:
mov eax,def:[dataBlock_bufNext]
xor al,1
mov esi,def:[dataBlock_buf1log+eax*4]
mov eax,def:[dataBlock_usrPipe]
mov ecx,bufferSize
clts                            ;nonblocking send through pipeline...
dd 1ah
jmp byte sb16irq_vege
;-----------------------
sb16irq_play:
dec byte def:[dataBlock_bufUsed]
jz byte sb16irq_play_j1
mov eax,def:[dataBlock_bufNext]
xor al,1
mov esi,def:[dataBlock_buf1phy+eax*4]
mov edi,bufferSize
call dword sb16play
jmp dword sb16irq_vege
sb16irq_play_j1:
call dword sb16reset
jmp dword sb16irq_vege
endp
;-------------------------------
