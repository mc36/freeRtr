DES_size equ 8
DES_param equ 0

;-----------------------------------------
proc DES_bitSel
;in:  esi-offset to bitnums...
;     cx-the size value...
;out: edx-the return value...

;    ret = 0;
sub edx,edx
;    while (size--) {
DES_bitSel_j1:
dec cx
js dword DES_bitSel_j2
;        ret <<= 1;
shl edx,1
;        bitpos = *bitnums++;
sub ax,ax
mov al,cs:[esi]
inc esi
;        if (bitpos >= 0)
or al,al
js dword DES_bitSel_j1
;            ret |= 1 & (input[bitpos / 32] >> (bitpos % 32));
bt ds:[DataBlock_temp1],ax
jnc dword DES_bitSel_j1
or dl,1
;    }
jmp dword DES_bitSel_j1
;    return ret;
DES_bitSel_j2:
retnd
endp
;-----------------------------------------

;-----------------------------------------
proc DES_rol28
;in:  eax-value to rotate...
;     ch-the rotater...
;out: eax-the rotated value...
;#define rotl28(x, c) ( ( (x << c) | (x >> (28-c)) ) & 0x0FFFFFFF)
mov edx,eax
mov cl,ch
shl eax,cl
mov cl,28
sub cl,ch
shr edx,cl
or eax,edx
and eax,0fffffffh
retnd
endp
;-----------------------------------------

;-----------------------------------------
proc DES_init
;in: ds:bx-record...
;    ds:si-key text (8 bytes)...
mov ds:[DataBlock_temp3],bx

mov di,DataBlock_temp1
;    buf[0] = GET_32BIT_MSB_FIRST(key+4);
mov eax,ds:[si+4]
xchg al,ah
rol eax,16
xchg al,ah
mov ds:[di+0],eax
;    buf[1] = GET_32BIT_MSB_FIRST(key);
mov eax,ds:[si+0]
xchg al,ah
rol eax,16
xchg al,ah
mov ds:[di+4],eax

;    C = bitsel(buf, DES_Cbits, 28);
mov esi,offset DES_Cbits
mov cx,28
call dword DES_bitSel
mov edi,edx  ;C
;    D = bitsel(buf, DES_Dbits, 28);
mov esi,offset DES_Dbits
mov cx,28
call dword DES_bitSel
mov ebp,edx  ;D

;    for (i = 0; i < 16; i++) {
sub ax,ax
mov ds:[DataBlock_temp2],ax
DES_init_j1:
mov bx,ds:[DataBlock_temp2]
cmp bl,16
jae dword DES_init_j2
movzx ebx,bx
mov ch,cs:[DES_Shifts+ebx]
;        C = rotl28(C, DES_Shifts[i]);
mov eax,edi    ;C
call dword DES_rol28
mov edi,eax
;        D = rotl28(D, DES_Shifts[i]);
mov eax,ebp    ;D
call dword DES_rol28
mov ebp,eax
mov si,DataBlock_temp1
;        buf[0] = D;
mov ds:[si+0],ebp
;        buf[1] = C;
mov ds:[si+4],edi
;        sched->k0246[i] = bitsel(buf, DES_0246, 32);
mov esi,offset DES_0246
mov cx,32
call dword DES_bitSel
mov bx,ds:[DataBlock_temp2]
shl bx,2
add bx,ds:[DataBlock_temp3]
mov ds:[bx+00h],edx
;        sched->k1357[i] = bitsel(buf, DES_1357, 32);
mov esi,offset DES_1357
mov cx,32
call dword DES_bitSel
mov bx,ds:[DataBlock_temp2]
shl bx,2
add bx,ds:[DataBlock_temp3]
mov ds:[bx+40h],edx
;    }
inc word ds:[DataBlock_temp2]
jmp dword DES_init_j1
DES_init_j2:

mov bx,ds:[DataBlock_temp3]
retnd
endp
;-----------------------------------------



;-----------------------------------------
proc DES_bitSwp
;in: edi-xl...
;    ebp-xr...
;    edx-mask...
;    cl-rotate...
;#define bitswap(L, R, n, mask) (\
;    swap = mask & ( (R >> n) ^ L ), \
mov eax,ebp
shr eax,cl
xor eax,edi
and eax,edx
;    R ^= swap << n, \
;    L ^= swap)
xor edi,eax
shl eax,cl
xor ebp,eax
retnd
endp
;-----------------------------------------

;-----------------------------------------
proc DES_permI
;in: edi-xl...
;    ebp-xr...
;#define IP(L, R) (\
;    bitswap(R, L,  4, 0x0F0F0F0F), \
xchg edi,ebp
mov cl,4
mov edx,0f0f0f0fh
call dword DES_bitSwp
;    bitswap(R, L, 16, 0x0000FFFF), \
mov cl,16
mov edx,0000ffffh
call dword DES_bitSwp
;    bitswap(L, R,  2, 0x33333333), \
xchg edi,ebp
mov cl,2
mov edx,33333333h
call dword DES_bitSwp
;    bitswap(L, R,  8, 0x00FF00FF), \
mov cl,8
mov edx,00ff00ffh
call dword DES_bitSwp
;    bitswap(R, L,  1, 0x55555555))
xchg edi,ebp
mov cl,1
mov edx,55555555h
call dword DES_bitSwp
xchg edi,ebp
retnd
endp
;-----------------------------------------

;-----------------------------------------
proc DES_permF
;in: edi-xl...
;    ebp-xr...
;#define FP(L, R) (\
;    bitswap(R, L,  1, 0x55555555), \
xchg edi,ebp
mov cl,1
mov edx,55555555h
call dword DES_bitSwp
;    bitswap(L, R,  8, 0x00FF00FF), \
xchg edi,ebp
mov cl,8
mov edx,00ff00ffh
call dword DES_bitSwp
;    bitswap(L, R,  2, 0x33333333), \
mov cl,2
mov edx,33333333h
call dword DES_bitSwp
;    bitswap(R, L, 16, 0x0000FFFF), \
xchg edi,ebp
mov cl,16
mov edx,0000ffffh
call dword DES_bitSwp
;    bitswap(R, L,  4, 0x0F0F0F0F))
mov cl,4
mov edx,0f0f0f0fh
call dword DES_bitSwp
xchg edi,ebp
retnd
endp
;-----------------------------------------

;-----------------------------------------
proc DES_bound
;in: edi-what to update (xl)...
;    ebp-the R updater (xr)...
;    al-pointer to sched->k0246 and sched->k1357...
;#define f(R, K0246, K1357) (\
mov ah,0
mov cx,ds:[DataBlock_temp3]
lea si,ds:[eax*4+ecx]
mov ecx,ds:[si+00h]      ;s0246 value...
mov edx,ds:[si+40h]      ;s1357 value...
;    s0246 = R ^ K0246, \
;    s1357 = R ^ K1357, \
xor ecx,ebp
xor edx,ebp
;    s0246 = rotl(s0246, 28), \
rol ecx,28

;    DES_boxes[6] [(s0246      ) & 0x3F] | \
movzx ebx,cl
and bl,3fh
shl bx,2
mov eax,cs:[DES_boxes6+ebx]
;    DES_boxes[7] [(s1357      ) & 0x3F] | \
movzx ebx,dl
and bl,3fh
shl bx,2
or eax,cs:[DES_boxes7+ebx]
;    DES_boxes[4] [(s0246 >>  8) & 0x3F] | \
shr ecx,8
movzx ebx,cl
and bl,3fh
shl bx,2
or eax,cs:[DES_boxes4+ebx]
;    DES_boxes[5] [(s1357 >>  8) & 0x3F] | \
shr edx,8
movzx ebx,dl
and bl,3fh
shl bx,2
or eax,cs:[DES_boxes5+ebx]
;    DES_boxes[2] [(s0246 >> 16) & 0x3F] | \
shr ecx,8
movzx ebx,cl
and bl,3fh
shl bx,2
or eax,cs:[DES_boxes2+ebx]
;    DES_boxes[3] [(s1357 >> 16) & 0x3F] | \
shr edx,8
movzx ebx,dl
and bl,3fh
shl bx,2
or eax,cs:[DES_boxes3+ebx]
;    DES_boxes[0] [(s0246 >> 24) & 0x3F] | \
shr ecx,8
movzx ebx,cl
and bl,3fh
shl bx,2
or eax,cs:[DES_boxes0+ebx]
;    DES_boxes[1] [(s1357 >> 24) & 0x3F] | \
shr edx,8
movzx ebx,dl
and bl,3fh
shl bx,2
or eax,cs:[DES_boxes1+ebx]

xor edi,eax
retnd
endp
;-----------------------------------------


;-----------------------------------------
proc DES_encrypt
;in: ds:bx-record...
;    edi-xl...
;    ebp-xr...
mov ds:[DataBlock_temp3],bx
;    IP(L, R);
call dword DES_permI
;    L = rotl(L, 1);
;    R = rotl(R, 1);
rol edi,1
rol ebp,1
;    L ^= f(R, sched->k0246[ 0], sched->k1357[ 0]);
mov al,0
call dword DES_bound
;    R ^= f(L, sched->k0246[ 1], sched->k1357[ 1]);
xchg edi,ebp
mov al,1
call dword DES_bound
;    L ^= f(R, sched->k0246[ 2], sched->k1357[ 2]);
xchg edi,ebp
mov al,2
call dword DES_bound
;    R ^= f(L, sched->k0246[ 3], sched->k1357[ 3]);
xchg edi,ebp
mov al,3
call dword DES_bound
;    L ^= f(R, sched->k0246[ 4], sched->k1357[ 4]);
xchg edi,ebp
mov al,4
call dword DES_bound
;    R ^= f(L, sched->k0246[ 5], sched->k1357[ 5]);
xchg edi,ebp
mov al,5
call dword DES_bound
;    L ^= f(R, sched->k0246[ 6], sched->k1357[ 6]);
xchg edi,ebp
mov al,6
call dword DES_bound
;    R ^= f(L, sched->k0246[ 7], sched->k1357[ 7]);
xchg edi,ebp
mov al,7
call dword DES_bound
;    L ^= f(R, sched->k0246[ 8], sched->k1357[ 8]);
xchg edi,ebp
mov al,8
call dword DES_bound
;    R ^= f(L, sched->k0246[ 9], sched->k1357[ 9]);
xchg edi,ebp
mov al,9
call dword DES_bound
;    L ^= f(R, sched->k0246[10], sched->k1357[10]);
xchg edi,ebp
mov al,10
call dword DES_bound
;    R ^= f(L, sched->k0246[11], sched->k1357[11]);
xchg edi,ebp
mov al,11
call dword DES_bound
;    L ^= f(R, sched->k0246[12], sched->k1357[12]);
xchg edi,ebp
mov al,12
call dword DES_bound
;    R ^= f(L, sched->k0246[13], sched->k1357[13]);
xchg edi,ebp
mov al,13
call dword DES_bound
;    L ^= f(R, sched->k0246[14], sched->k1357[14]);
xchg edi,ebp
mov al,14
call dword DES_bound
;    R ^= f(L, sched->k0246[15], sched->k1357[15]);
xchg edi,ebp
mov al,15
call dword DES_bound
;    L = rotl(L, 31);
;    R = rotl(R, 31);
;    swap = L; L = R; R = swap;
rol edi,31
rol ebp,31
;    FP(L, R);
call dword DES_permF
mov bx,ds:[DataBlock_temp3]
retnd
endp
;-----------------------------------------

;-----------------------------------------
proc DES_decrypt
;in: ds:bx-record...
;    edi-xl...
;    ebp-xr...
mov ds:[DataBlock_temp3],bx
;    IP(L, R);
call dword DES_permI
;    L = rotl(L, 1);
;    R = rotl(R, 1);
rol edi,1
rol ebp,1
;    L ^= f(R, sched->k0246[15], sched->k1357[15]);
mov al,15
call dword DES_bound
;    R ^= f(L, sched->k0246[14], sched->k1357[14]);
xchg edi,ebp
mov al,14
call dword DES_bound
;    L ^= f(R, sched->k0246[13], sched->k1357[13]);
xchg edi,ebp
mov al,13
call dword DES_bound
;    R ^= f(L, sched->k0246[12], sched->k1357[12]);
xchg edi,ebp
mov al,12
call dword DES_bound
;    L ^= f(R, sched->k0246[11], sched->k1357[11]);
xchg edi,ebp
mov al,11
call dword DES_bound
;    R ^= f(L, sched->k0246[10], sched->k1357[10]);
xchg edi,ebp
mov al,10
call dword DES_bound
;    L ^= f(R, sched->k0246[ 9], sched->k1357[ 9]);
xchg edi,ebp
mov al,9
call dword DES_bound
;    R ^= f(L, sched->k0246[ 8], sched->k1357[ 8]);
xchg edi,ebp
mov al,8
call dword DES_bound
;    L ^= f(R, sched->k0246[ 7], sched->k1357[ 7]);
xchg edi,ebp
mov al,7
call dword DES_bound
;    R ^= f(L, sched->k0246[ 6], sched->k1357[ 6]);
xchg edi,ebp
mov al,6
call dword DES_bound
;    L ^= f(R, sched->k0246[ 5], sched->k1357[ 5]);
xchg edi,ebp
mov al,5
call dword DES_bound
;    R ^= f(L, sched->k0246[ 4], sched->k1357[ 4]);
xchg edi,ebp
mov al,4
call dword DES_bound
;    L ^= f(R, sched->k0246[ 3], sched->k1357[ 3]);
xchg edi,ebp
mov al,3
call dword DES_bound
;    R ^= f(L, sched->k0246[ 2], sched->k1357[ 2]);
xchg edi,ebp
mov al,2
call dword DES_bound
;    L ^= f(R, sched->k0246[ 1], sched->k1357[ 1]);
xchg edi,ebp
mov al,1
call dword DES_bound
;    R ^= f(L, sched->k0246[ 0], sched->k1357[ 0]);
xchg edi,ebp
mov al,0
call dword DES_bound

;    L = rotl(L, 31);
;    R = rotl(R, 31);
;    swap = L; L = R; R = swap;
rol edi,31
rol ebp,31

;    FP(L, R);
call dword DES_permF
mov bx,ds:[DataBlock_temp3]
retnd
endp
;-----------------------------------------



;-----------------------------------------
DES_Cbits db 7, 15, 23, 31, 39, 47, 55, 63, 6, 14, 22, 30, 38, 46
          db 54, 62, 5, 13, 21, 29, 37, 45, 53, 61, 4, 12, 20, 28
DES_Dbits db 1, 9, 17, 25, 33, 41, 49, 57, 2, 10, 18, 26, 34, 42
          db 50, 58, 3, 11, 19, 27, 35, 43, 51, 59, 36, 44, 52, 60
DES_0246 db 49, 36, 59, 55, -1, -1, 37, 41, 48, 56, 34, 52, -1, -1, 15, 4
         db 25, 19, 9, 1, -1, -1, 12, 7, 17, 0, 22, 3, -1, -1, 46, 43
DES_1357 db -1, -1, 57, 32, 45, 54, 39, 50, -1, -1, 44, 53, 33, 40, 47, 58
         db -1, -1, 26, 16, 5, 11, 23, 8, -1, -1, 10, 14, 6, 20, 27, 24
DES_Shifts db 1,1,2,2,2,2,2,2,1,2,2,2,2,2,2,1
;-----------------------------------------

;-----------------------------------------
DES_boxes0 dd 001010400h,000000000h,000010000h,001010404h
dd 001010004h,000010404h,000000004h,000010000h
dd 000000400h,001010400h,001010404h,000000400h
dd 001000404h,001010004h,001000000h,000000004h
dd 000000404h,001000400h,001000400h,000010400h
dd 000010400h,001010000h,001010000h,001000404h
dd 000010004h,001000004h,001000004h,000010004h
dd 000000000h,000000404h,000010404h,001000000h
dd 000010000h,001010404h,000000004h,001010000h
dd 001010400h,001000000h,001000000h,000000400h
dd 001010004h,000010000h,000010400h,001000004h
dd 000000400h,000000004h,001000404h,000010404h
dd 001010404h,000010004h,001010000h,001000404h
dd 001000004h,000000404h,000010404h,001010400h
dd 000000404h,001000400h,001000400h,000000000h
dd 000010004h,000010400h,000000000h,001010004h
;-----------------------------------------

;-----------------------------------------
DES_boxes1 dd 080108020h,080008000h,000008000h,000108020h
dd 000100000h,000000020h,080100020h,080008020h
dd 080000020h,080108020h,080108000h,080000000h
dd 080008000h,000100000h,000000020h,080100020h
dd 000108000h,000100020h,080008020h,000000000h
dd 080000000h,000008000h,000108020h,080100000h
dd 000100020h,080000020h,000000000h,000108000h
dd 000008020h,080108000h,080100000h,000008020h
dd 000000000h,000108020h,080100020h,000100000h
dd 080008020h,080100000h,080108000h,000008000h
dd 080100000h,080008000h,000000020h,080108020h
dd 000108020h,000000020h,000008000h,080000000h
dd 000008020h,080108000h,000100000h,080000020h
dd 000100020h,080008020h,080000020h,000100020h
dd 000108000h,000000000h,080008000h,000008020h
dd 080000000h,080100020h,080108020h,000108000h
;-----------------------------------------

;-----------------------------------------
DES_boxes2 dd 000000208h,008020200h,000000000h,008020008h
dd 008000200h,000000000h,000020208h,008000200h
dd 000020008h,008000008h,008000008h,000020000h
dd 008020208h,000020008h,008020000h,000000208h
dd 008000000h,000000008h,008020200h,000000200h
dd 000020200h,008020000h,008020008h,000020208h
dd 008000208h,000020200h,000020000h,008000208h
dd 000000008h,008020208h,000000200h,008000000h
dd 008020200h,008000000h,000020008h,000000208h
dd 000020000h,008020200h,008000200h,000000000h
dd 000000200h,000020008h,008020208h,008000200h
dd 008000008h,000000200h,000000000h,008020008h
dd 008000208h,000020000h,008000000h,008020208h
dd 000000008h,000020208h,000020200h,008000008h
dd 008020000h,008000208h,000000208h,008020000h
dd 000020208h,000000008h,008020008h,000020200h
;-----------------------------------------

;-----------------------------------------
DES_boxes3 dd 000802001h,000002081h,000002081h,000000080h
dd 000802080h,000800081h,000800001h,000002001h
dd 000000000h,000802000h,000802000h,000802081h
dd 000000081h,000000000h,000800080h,000800001h
dd 000000001h,000002000h,000800000h,000802001h
dd 000000080h,000800000h,000002001h,000002080h
dd 000800081h,000000001h,000002080h,000800080h
dd 000002000h,000802080h,000802081h,000000081h
dd 000800080h,000800001h,000802000h,000802081h
dd 000000081h,000000000h,000000000h,000802000h
dd 000002080h,000800080h,000800081h,000000001h
dd 000802001h,000002081h,000002081h,000000080h
dd 000802081h,000000081h,000000001h,000002000h
dd 000800001h,000002001h,000802080h,000800081h
dd 000002001h,000002080h,000800000h,000802001h
dd 000000080h,000800000h,000002000h,000802080h
;-----------------------------------------

;-----------------------------------------
DES_boxes4 dd 000000100h,002080100h,002080000h,042000100h
dd 000080000h,000000100h,040000000h,002080000h
dd 040080100h,000080000h,002000100h,040080100h
dd 042000100h,042080000h,000080100h,040000000h
dd 002000000h,040080000h,040080000h,000000000h
dd 040000100h,042080100h,042080100h,002000100h
dd 042080000h,040000100h,000000000h,042000000h
dd 002080100h,002000000h,042000000h,000080100h
dd 000080000h,042000100h,000000100h,002000000h
dd 040000000h,002080000h,042000100h,040080100h
dd 002000100h,040000000h,042080000h,002080100h
dd 040080100h,000000100h,002000000h,042080000h
dd 042080100h,000080100h,042000000h,042080100h
dd 002080000h,000000000h,040080000h,042000000h
dd 000080100h,002000100h,040000100h,000080000h
dd 000000000h,040080000h,002080100h,040000100h
;-----------------------------------------

;-----------------------------------------
DES_boxes5 dd 020000010h,020400000h,000004000h,020404010h
dd 020400000h,000000010h,020404010h,000400000h
dd 020004000h,000404010h,000400000h,020000010h
dd 000400010h,020004000h,020000000h,000004010h
dd 000000000h,000400010h,020004010h,000004000h
dd 000404000h,020004010h,000000010h,020400010h
dd 020400010h,000000000h,000404010h,020404000h
dd 000004010h,000404000h,020404000h,020000000h
dd 020004000h,000000010h,020400010h,000404000h
dd 020404010h,000400000h,000004010h,020000010h
dd 000400000h,020004000h,020000000h,000004010h
dd 020000010h,020404010h,000404000h,020400000h
dd 000404010h,020404000h,000000000h,020400010h
dd 000000010h,000004000h,020400000h,000404010h
dd 000004000h,000400010h,020004010h,000000000h
dd 020404000h,020000000h,000400010h,020004010h
;-----------------------------------------

;-----------------------------------------
DES_boxes6 dd 000200000h,004200002h,004000802h,000000000h
dd 000000800h,004000802h,000200802h,004200800h
dd 004200802h,000200000h,000000000h,004000002h
dd 000000002h,004000000h,004200002h,000000802h
dd 004000800h,000200802h,000200002h,004000800h
dd 004000002h,004200000h,004200800h,000200002h
dd 004200000h,000000800h,000000802h,004200802h
dd 000200800h,000000002h,004000000h,000200800h
dd 004000000h,000200800h,000200000h,004000802h
dd 004000802h,004200002h,004200002h,000000002h
dd 000200002h,004000000h,004000800h,000200000h
dd 004200800h,000000802h,000200802h,004200800h
dd 000000802h,004000002h,004200802h,004200000h
dd 000200800h,000000000h,000000002h,004200802h
dd 000000000h,000200802h,004200000h,000000800h
dd 004000002h,004000800h,000000800h,000200002h
;-----------------------------------------

;-----------------------------------------
DES_boxes7 dd 010001040h,000001000h,000040000h,010041040h
dd 010000000h,010001040h,000000040h,010000000h
dd 000040040h,010040000h,010041040h,000041000h
dd 010041000h,000041040h,000001000h,000000040h
dd 010040000h,010000040h,010001000h,000001040h
dd 000041000h,000040040h,010040040h,010041000h
dd 000001040h,000000000h,000000000h,010040040h
dd 010000040h,010001000h,000041040h,000040000h
dd 000041040h,000040000h,010041000h,000001000h
dd 000000040h,010040040h,000001000h,000041040h
dd 010001000h,000000040h,010000040h,010040000h
dd 010040040h,010000000h,000040000h,010001040h
dd 000000000h,010041040h,000040040h,010000040h
dd 010040000h,010001000h,010001040h,000000000h
dd 010041040h,000041000h,000041000h,000001040h
dd 000001040h,000040040h,010000000h,010041000h
;-----------------------------------------


;rec:
;k0246 dd 16 dup (?)         0000h
;k1357 dd 16 dup (?)         0040h
