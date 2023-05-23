org 0h
di
db 3fh dup (0)
di
ld sp,1000h
ld a,0bh
out (0efh),a
call resetSIO

ld hl,2
ld (10h),hl

main_j1:
ld hl,(00h)
inc hl
ld (00h),hl

recvA_j1:
ld a,0                          ;check for character...
out (7eh),a
in a,(7eh)
and a,1
cp a,1
jr nz,recvA_j2
ld hl,(10h)                     ;receive it...
ld a,h
and a,7fh
add a,80h
ld h,a
in a,(7ch)
ld (hl),a
ld hl,(10h)
inc hl
ld (10h),hl
ld hl,(14h)
inc hl
ld (14h),hl
ld a,1                          ;check for end of message...
out (7eh),a
in a,(7eh)
ld b,a
and a,80h
cp a,0
jr z,recvA_j2
ld a,b                          ;check for error...
and a,40h
cp a,0
jr nz,recvA_j3
ld ix,(12h)
ld a,ixh
and a,7fh
add a,80h
ld ixh,a
ld bc,(14h)
dec bc
dec bc
ld (ix+0),c
ld (ix+1),b
ld hl,(10h)
dec hl
dec hl
ld (12h),hl
jr recvA_j4
recvA_j3:
ld hl,(12h)
inc hl
inc hl
ld (10h),hl
recvA_j4:
ld h,0
ld l,h
ld (14h),hl
recvA_j2:

sendA_j1:
ld ix,(18h)
ld a,ixh
or a,ixl
cp a,0
jr z,sendA_j2
ld a,0                          ;check for buffer space...
out (7eh),a
in a,(7eh)
and a,4
cp a,4
jr nz,sendA_j2
ld hl,(1ah)
ld a,h
or a,l
cp a,0
jr nz,sendA_j3
ld a,80h                        ;reset tx crc generator...
out (7eh),a
sendA_j3:
ld hl,(1ah)
ld a,h
add a,10h
ld h,a
ld a,(hl)                       ;send character...
out (7ch),a
ld hl,(1ah)
inc hl
ld (1ah),hl
ld a,h                          ;test end of packet...
cp a,ixh
jr nz,sendA_j2
ld a,l
cp a,ixl
jr nz,sendA_j2
ld a,0c0h                       ;sign end of packet...
out (7eh),a
ld hl,0
ld (18h),hl
ld (1ah),hl
sendA_j2:

jp main_j1
;-------------------------------


;-------------------------------
proc resetSIO
ld a,00h                ;noop...
call resetSIO_j1
ld a,18h                ;reset...
call resetSIO_j1
ld a,02h                ;wr 2...
call resetSIO_j1
ld a,0e0h               ;interrupt vector...
call resetSIO_j1
ld a,01h                ;select wr1...
call resetSIO_j1
ld a,04h                ;allow vectored interrupts, don't use any
call resetSIO_j1
ld a,04h                ;select wr4...
call resetSIO_j1
ld a,20h                ;sdlc mode...
call resetSIO_j1
ld a,03h                ;select wr3...
call resetSIO_j1
ld a,0c1h               ;turn on receiver 8 data bits
call resetSIO_j1
ld a,05h                ;select wr5...
call resetSIO_j1
ld a,0ebh               ;dtr on, rts on, 8 data bits, transmit on
call resetSIO_j1
ld a,06h                ;select wr6...
call resetSIO_j1
ld a,03h                ;address field...
call resetSIO_j1
ld a,07h                ;select wr7...
call resetSIO_j1
ld a,7eh                ;flag value...
call resetSIO_j1
ret
resetSIO_j1:
out (7eh),a
out (7fh),a
ret
endp
;-------------------------------
