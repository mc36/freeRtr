BlowFish_size equ 8
BlowFish_param equ 0

;-----------------------------------------
proc BlowFish_init
;in: ds:bx-record...
;    ds:si-key text...
;    cx-size of key...

;for (i = 0; i < 18; i++) {
; P[i] = parray[i];
; P[i] ^= ((unsigned long)(unsigned char)(key[ (i*4+0) % keybytes ])) << 24;
; P[i] ^= ((unsigned long)(unsigned char)(key[ (i*4+1) % keybytes ])) << 16;
; P[i] ^= ((unsigned long)(unsigned char)(key[ (i*4+2) % keybytes ])) << 8;
; P[i] ^= ((unsigned long)(unsigned char)(key[ (i*4+3) % keybytes ])) << 0;
; }
sub bp,bp
mov di,1000h
mov dx,cx
mov cx,18
BlowFish_init_j3:
call dword BlowFish_init_j1
call dword BlowFish_init_j1
call dword BlowFish_init_j1
call dword BlowFish_init_j1
mov ds:[bx+di],eax
add di,4
loopw BlowFish_init_j3

mov di,1000h
mov ebp,offset BlowFish_OrigP
mov cx,18
BlowFish_init_j4:
mov eax,cs:[ebp]
xor ds:[bx+di],eax
add ebp,4
add di,4
loopw BlowFish_init_j4

;for (i = 0; i < 256; i++) {
;    S0[i] = sbox0[i];
;    S1[i] = sbox1[i];
;    S2[i] = sbox2[i];
;    S3[i] = sbox3[i];    }
sub di,di
mov cx,1024
mov esi,offset BlowFish_OrigS0
BlowFish_init_j5:
mov eax,cs:[esi]
mov ds:[bx+di],eax
add esi,4
add di,4
loopw BlowFish_init_j5

;str[0] = str[1] = 0;
sub edi,edi     ;xL, str0
sub ebp,ebp     ;xR, str1

;for (i = 0; i < 18; i += 2) {
;    blowfish_encrypt(str[0], str[1], str, ctx);
;    P[i] = str[0]; P[i+1] = str[1];                 }
mov si,1000h
mov cx,18
call dword BlowFish_init_j6

;for (i = 0; i < 256; i += 2) {
;    blowfish_encrypt(str[0], str[1], str, ctx);
;    S0[i] = str[0]; S0[i+1] = str[1];               }
sub si,si
mov cx,256
call dword BlowFish_init_j6

;for (i = 0; i < 256; i += 2) {
;    blowfish_encrypt(str[0], str[1], str, ctx);
;    S1[i] = str[0]; S1[i+1] = str[1];               }
mov cx,256
call dword BlowFish_init_j6

;for (i = 0; i < 256; i += 2) {
;    blowfish_encrypt(str[0], str[1], str, ctx);
;    S2[i] = str[0]; S2[i+1] = str[1];               }
mov cx,256
call dword BlowFish_init_j6

;for (i = 0; i < 256; i += 2) {
;    blowfish_encrypt(str[0], str[1], str, ctx);
;    S3[i] = str[0]; S3[i+1] = str[1];               }
mov cx,256
call dword BlowFish_init_j6

retnd
BlowFish_init_j1:
shl eax,8
mov al,ds:[si+bp]
inc bp
cmp bp,dx
jb dword BlowFish_init_j2
sub bp,bp
BlowFish_init_j2:
retnd
BlowFish_init_j6:
;for (i = 0; i < n; i += 2) {
shr cx,1
BlowFish_init_j7:
;  blowfish_encrypt(Lstr[0], Rstr[1], RLstr, ctx);
;  S0[i] = str[0];
;  S0[i+1] = str[1];                                }
push cx
push si
call dword BlowFish_encrypt
pop si
pop cx
mov ds:[bx+si+0],edi
mov ds:[bx+si+4],ebp
add si,8
loopw BlowFish_init_j7
retnd
endp
;-----------------------------------------


;-----------------------------------------
proc BlowFish_bound
;in: ds:bx-record...
;    al-n...
;    edi-xl...
;    ebp-xr...

;ROUND(n) (
;   xL ^= P[n]
;   t = F(xL)
movzx si,al
shl si,2
xor edi,ds:[bx+si+1000h]
mov edx,edi

;Fprime(a,b,c,d) (     (  ( S0[a] + S1[b] )  ^  S2[c]  )    + S3[d]     )
;F(x) Fprime(  ((x>>24)&0xFF),  ((x>>16)&0xFF),  ((x>>8)&0xFF),  (x&0xFF)  )
rol edx,8
movzx si,dl
shl si,2
mov eax,ds:[bx+si+0]
rol edx,8
movzx si,dl
shl si,2
add eax,ds:[bx+si+400h]
rol edx,8
movzx si,dl
shl si,2
xor eax,ds:[bx+si+800h]
rol edx,8
movzx si,dl
shl si,2
add eax,ds:[bx+si+0c00h]

;   xR ^= t
;   t = xL
;   xL = xR
;   xR = t
;   )
xor ebp,eax
xchg ebp,edi

retnd
endp
;-----------------------------------------

;-----------------------------------------
proc BlowFish_encrypt
;in: ds:bx-record...
;    edi-xl...
;    ebp-xr...

;    ROUND(0);    ROUND(1);    ROUND(2);    ROUND(3);
;    ROUND(4);    ROUND(5);    ROUND(6);    ROUND(7);
;    ROUND(8);    ROUND(9);   ROUND(10);   ROUND(11);
;   ROUND(12);   ROUND(13);   ROUND(14);   ROUND(15);
sub ax,ax
BlowFish_encrypt_j1:
push ax
call dword BlowFish_bound
pop ax
inc ax
cmp al,16
jb dword BlowFish_encrypt_j1
;    xL ^= P[16];
;    xR ^= P[17];
xor edi,ds:[bx+1040h]
xor ebp,ds:[bx+1044h]
xchg edi,ebp

retnd
endp
;-----------------------------------------

;-----------------------------------------
proc BlowFish_decrypt
;in: ds:bx-record...
;    edi-xl...
;    ebp-xr...

;   ROUND(17);   ROUND(16);   ROUND(15);   ROUND(14);
;   ROUND(13);   ROUND(12);   ROUND(11);   ROUND(10);
;    ROUND(9);    ROUND(8);    ROUND(7);    ROUND(6);
;    ROUND(5);    ROUND(4);    ROUND(3);    ROUND(2);
mov al,17
BlowFish_decrypt_j1:
push ax
call dword BlowFish_bound
pop ax
dec ax
cmp al,2
jae dword BlowFish_decrypt_j1

;    xL ^= P[1];
;    xR ^= P[0];
xor edi,ds:[bx+1004h]
xor ebp,ds:[bx+1000h]
xchg edi,ebp

retnd
endp
;-----------------------------------------





;-----------------------------------------
BlowFish_OrigP:
dd 0243f6a88h,085a308d3h,013198a2eh,003707344h,0a4093822h,0299f31d0h
dd 0082efa98h,0ec4e6c89h,0452821e6h,038d01377h,0be5466cfh,034e90c6ch
dd 0c0ac29b7h,0c97c50ddh,03f84d5b5h,0b5470917h,09216d5d9h,08979fb1bh
;-----------------------------------------
BlowFish_OrigS0:
dd 0d1310ba6h,098dfb5ach,02ffd72dbh,0d01adfb7h,0b8e1afedh,06a267e96h
dd 0ba7c9045h,0f12c7f99h,024a19947h,0b3916cf7h,00801f2e2h,0858efc16h
dd 0636920d8h,071574e69h,0a458fea3h,0f4933d7eh,00d95748fh,0728eb658h
dd 0718bcd58h,082154aeeh,07b54a41dh,0c25a59b5h,09c30d539h,02af26013h
dd 0c5d1b023h,0286085f0h,0ca417918h,0b8db38efh,08e79dcb0h,0603a180eh
dd 06c9e0e8bh,0b01e8a3eh,0d71577c1h,0bd314b27h,078af2fdah,055605c60h
dd 0e65525f3h,0aa55ab94h,057489862h,063e81440h,055ca396ah,02aab10b6h
dd 0b4cc5c34h,01141e8ceh,0a15486afh,07c72e993h,0b3ee1411h,0636fbc2ah
dd 02ba9c55dh,0741831f6h,0ce5c3e16h,09b87931eh,0afd6ba33h,06c24cf5ch
dd 07a325381h,028958677h,03b8f4898h,06b4bb9afh,0c4bfe81bh,066282193h
dd 061d809cch,0fb21a991h,0487cac60h,05dec8032h,0ef845d5dh,0e98575b1h
dd 0dc262302h,0eb651b88h,023893e81h,0d396acc5h,00f6d6ff3h,083f44239h
dd 02e0b4482h,0a4842004h,069c8f04ah,09e1f9b5eh,021c66842h,0f6e96c9ah
dd 0670c9c61h,0abd388f0h,06a51a0d2h,0d8542f68h,0960fa728h,0ab5133a3h
dd 06eef0b6ch,0137a3be4h,0ba3bf050h,07efb2a98h,0a1f1651dh,039af0176h
dd 066ca593eh,082430e88h,08cee8619h,0456f9fb4h,07d84a5c3h,03b8b5ebeh
dd 0e06f75d8h,085c12073h,0401a449fh,056c16aa6h,04ed3aa62h,0363f7706h
dd 01bfedf72h,0429b023dh,037d0d724h,0d00a1248h,0db0fead3h,049f1c09bh
dd 0075372c9h,080991b7bh,025d479d8h,0f6e8def7h,0e3fe501ah,0b6794c3bh
dd 0976ce0bdh,004c006bah,0c1a94fb6h,0409f60c4h,05e5c9ec2h,0196a2463h
dd 068fb6fafh,03e6c53b5h,01339b2ebh,03b52ec6fh,06dfc511fh,09b30952ch
dd 0cc814544h,0af5ebd09h,0bee3d004h,0de334afdh,0660f2807h,0192e4bb3h
dd 0c0cba857h,045c8740fh,0d20b5f39h,0b9d3fbdbh,05579c0bdh,01a60320ah
dd 0d6a100c6h,0402c7279h,0679f25feh,0fb1fa3cch,08ea5e9f8h,0db3222f8h
dd 03c7516dfh,0fd616b15h,02f501ec8h,0ad0552abh,0323db5fah,0fd238760h
dd 053317b48h,03e00df82h,09e5c57bbh,0ca6f8ca0h,01a87562eh,0df1769dbh
dd 0d542a8f6h,0287effc3h,0ac6732c6h,08c4f5573h,0695b27b0h,0bbca58c8h
dd 0e1ffa35dh,0b8f011a0h,010fa3d98h,0fd2183b8h,04afcb56ch,02dd1d35bh
dd 09a53e479h,0b6f84565h,0d28e49bch,04bfb9790h,0e1ddf2dah,0a4cb7e33h
dd 062fb1341h,0cee4c6e8h,0ef20cadah,036774c01h,0d07e9efeh,02bf11fb4h
dd 095dbda4dh,0ae909198h,0eaad8e71h,06b93d5a0h,0d08ed1d0h,0afc725e0h
dd 08e3c5b2fh,08e7594b7h,08ff6e2fbh,0f2122b64h,08888b812h,0900df01ch
dd 04fad5ea0h,0688fc31ch,0d1cff191h,0b3a8c1adh,02f2f2218h,0be0e1777h
dd 0ea752dfeh,08b021fa1h,0e5a0cc0fh,0b56f74e8h,018acf3d6h,0ce89e299h
dd 0b4a84fe0h,0fd13e0b7h,07cc43b81h,0d2ada8d9h,0165fa266h,080957705h
dd 093cc7314h,0211a1477h,0e6ad2065h,077b5fa86h,0c75442f5h,0fb9d35cfh
dd 0ebcdaf0ch,07b3e89a0h,0d6411bd3h,0ae1e7e49h,000250e2dh,02071b35eh
dd 0226800bbh,057b8e0afh,02464369bh,0f009b91eh,05563911dh,059dfa6aah
dd 078c14389h,0d95a537fh,0207d5ba2h,002e5b9c5h,083260376h,06295cfa9h
dd 011c81968h,04e734a41h,0b3472dcah,07b14a94ah,01b510052h,09a532915h
dd 0d60f573fh,0bc9bc6e4h,02b60a476h,081e67400h,008ba6fb5h,0571be91fh
dd 0f296ec6bh,02a0dd915h,0b6636521h,0e7b9f9b6h,0ff34052eh,0c5855664h
dd 053b02d5dh,0a99f8fa1h,008ba4799h,06e85076ah
;-----------------------------------------
BlowFish_OrigS1:
dd 04b7a70e9h,0b5b32944h,0db75092eh,0c4192623h,0ad6ea6b0h,049a7df7dh
dd 09cee60b8h,08fedb266h,0ecaa8c71h,0699a17ffh,05664526ch,0c2b19ee1h
dd 0193602a5h,075094c29h,0a0591340h,0e4183a3eh,03f54989ah,05b429d65h
dd 06b8fe4d6h,099f73fd6h,0a1d29c07h,0efe830f5h,04d2d38e6h,0f0255dc1h
dd 04cdd2086h,08470eb26h,06382e9c6h,0021ecc5eh,009686b3fh,03ebaefc9h
dd 03c971814h,06b6a70a1h,0687f3584h,052a0e286h,0b79c5305h,0aa500737h
dd 03e07841ch,07fdeae5ch,08e7d44ech,05716f2b8h,0b03ada37h,0f0500c0dh
dd 0f01c1f04h,00200b3ffh,0ae0cf51ah,03cb574b2h,025837a58h,0dc0921bdh
dd 0d19113f9h,07ca92ff6h,094324773h,022f54701h,03ae5e581h,037c2dadch
dd 0c8b57634h,09af3dda7h,0a9446146h,00fd0030eh,0ecc8c73eh,0a4751e41h
dd 0e238cd99h,03bea0e2fh,03280bba1h,0183eb331h,04e548b38h,04f6db908h
dd 06f420d03h,0f60a04bfh,02cb81290h,024977c79h,05679b072h,0bcaf89afh
dd 0de9a771fh,0d9930810h,0b38bae12h,0dccf3f2eh,05512721fh,02e6b7124h
dd 0501adde6h,09f84cd87h,07a584718h,07408da17h,0bc9f9abch,0e94b7d8ch
dd 0ec7aec3ah,0db851dfah,063094366h,0c464c3d2h,0ef1c1847h,03215d908h
dd 0dd433b37h,024c2ba16h,012a14d43h,02a65c451h,050940002h,0133ae4ddh
dd 071dff89eh,010314e55h,081ac77d6h,05f11199bh,0043556f1h,0d7a3c76bh
dd 03c11183bh,05924a509h,0f28fe6edh,097f1fbfah,09ebabf2ch,01e153c6eh
dd 086e34570h,0eae96fb1h,0860e5e0ah,05a3e2ab3h,0771fe71ch,04e3d06fah
dd 02965dcb9h,099e71d0fh,0803e89d6h,05266c825h,02e4cc978h,09c10b36ah
dd 0c6150ebah,094e2ea78h,0a5fc3c53h,01e0a2df4h,0f2f74ea7h,0361d2b3dh
dd 01939260fh,019c27960h,05223a708h,0f71312b6h,0ebadfe6eh,0eac31f66h
dd 0e3bc4595h,0a67bc883h,0b17f37d1h,0018cff28h,0c332ddefh,0be6c5aa5h
dd 065582185h,068ab9802h,0eecea50fh,0db2f953bh,02aef7dadh,05b6e2f84h
dd 01521b628h,029076170h,0ecdd4775h,0619f1510h,013cca830h,0eb61bd96h
dd 00334fe1eh,0aa0363cfh,0b5735c90h,04c70a239h,0d59e9e0bh,0cbaade14h
dd 0eecc86bch,060622ca7h,09cab5cabh,0b2f3846eh,0648b1eafh,019bdf0cah
dd 0a02369b9h,0655abb50h,040685a32h,03c2ab4b3h,0319ee9d5h,0c021b8f7h
dd 09b540b19h,0875fa099h,095f7997eh,0623d7da8h,0f837889ah,097e32d77h
dd 011ed935fh,016681281h,00e358829h,0c7e61fd6h,096dedfa1h,07858ba99h
dd 057f584a5h,01b227263h,09b83c3ffh,01ac24696h,0cdb30aebh,0532e3054h
dd 08fd948e4h,06dbc3128h,058ebf2efh,034c6ffeah,0fe28ed61h,0ee7c3c73h
dd 05d4a14d9h,0e864b7e3h,042105d14h,0203e13e0h,045eee2b6h,0a3aaabeah
dd 0db6c4f15h,0facb4fd0h,0c742f442h,0ef6abbb5h,0654f3b1dh,041cd2105h
dd 0d81e799eh,086854dc7h,0e44b476ah,03d816250h,0cf62a1f2h,05b8d2646h
dd 0fc8883a0h,0c1c7b6a3h,07f1524c3h,069cb7492h,047848a0bh,05692b285h
dd 0095bbf00h,0ad19489dh,01462b174h,023820e00h,058428d2ah,00c55f5eah
dd 01dadf43eh,0233f7061h,03372f092h,08d937e41h,0d65fecf1h,06c223bdbh
dd 07cde3759h,0cbee7460h,04085f2a7h,0ce77326eh,0a6078084h,019f8509eh
dd 0e8efd855h,061d99735h,0a969a7aah,0c50c06c2h,05a04abfch,0800bcadch
dd 09e447a2eh,0c3453484h,0fdd56705h,00e1e9ec9h,0db73dbd3h,0105588cdh
dd 0675fda79h,0e3674340h,0c5c43465h,0713e38d8h,03d28f89eh,0f16dff20h
dd 0153e21e7h,08fb03d4ah,0e6e39f2bh,0db83adf7h
;-----------------------------------------
BlowFish_OrigS2:
dd 0e93d5a68h,0948140f7h,0f64c261ch,094692934h,0411520f7h,07602d4f7h
dd 0bcf46b2eh,0d4a20068h,0d4082471h,03320f46ah,043b7d4b7h,0500061afh
dd 01e39f62eh,097244546h,014214f74h,0bf8b8840h,04d95fc1dh,096b591afh
dd 070f4ddd3h,066a02f45h,0bfbc09ech,003bd9785h,07fac6dd0h,031cb8504h
dd 096eb27b3h,055fd3941h,0da2547e6h,0abca0a9ah,028507825h,0530429f4h
dd 00a2c86dah,0e9b66dfbh,068dc1462h,0d7486900h,0680ec0a4h,027a18deeh
dd 04f3ffea2h,0e887ad8ch,0b58ce006h,07af4d6b6h,0aace1e7ch,0d3375fech
dd 0ce78a399h,0406b2a42h,020fe9e35h,0d9f385b9h,0ee39d7abh,03b124e8bh
dd 01dc9faf7h,04b6d1856h,026a36631h,0eae397b2h,03a6efa74h,0dd5b4332h
dd 06841e7f7h,0ca7820fbh,0fb0af54eh,0d8feb397h,0454056ach,0ba489527h
dd 055533a3ah,020838d87h,0fe6ba9b7h,0d096954bh,055a867bch,0a1159a58h
dd 0cca92963h,099e1db33h,0a62a4a56h,03f3125f9h,05ef47e1ch,09029317ch
dd 0fdf8e802h,004272f70h,080bb155ch,005282ce3h,095c11548h,0e4c66d22h
dd 048c1133fh,0c70f86dch,007f9c9eeh,041041f0fh,0404779a4h,05d886e17h
dd 0325f51ebh,0d59bc0d1h,0f2bcc18fh,041113564h,0257b7834h,0602a9c60h
dd 0dff8e8a3h,01f636c1bh,00e12b4c2h,002e1329eh,0af664fd1h,0cad18115h
dd 06b2395e0h,0333e92e1h,03b240b62h,0eebeb922h,085b2a20eh,0e6ba0d99h
dd 0de720c8ch,02da2f728h,0d0127845h,095b794fdh,0647d0862h,0e7ccf5f0h
dd 05449a36fh,0877d48fah,0c39dfd27h,0f33e8d1eh,00a476341h,0992eff74h
dd 03a6f6eabh,0f4f8fd37h,0a812dc60h,0a1ebddf8h,0991be14ch,0db6e6b0dh
dd 0c67b5510h,06d672c37h,02765d43bh,0dcd0e804h,0f1290dc7h,0cc00ffa3h
dd 0b5390f92h,0690fed0bh,0667b9ffbh,0cedb7d9ch,0a091cf0bh,0d9155ea3h
dd 0bb132f88h,0515bad24h,07b9479bfh,0763bd6ebh,037392eb3h,0cc115979h
dd 08026e297h,0f42e312dh,06842ada7h,0c66a2b3bh,012754ccch,0782ef11ch
dd 06a124237h,0b79251e7h,006a1bbe6h,04bfb6350h,01a6b1018h,011caedfah
dd 03d25bdd8h,0e2e1c3c9h,044421659h,00a121386h,0d90cec6eh,0d5abea2ah
dd 064af674eh,0da86a85fh,0bebfe988h,064e4c3feh,09dbc8057h,0f0f7c086h
dd 060787bf8h,06003604dh,0d1fd8346h,0f6381fb0h,07745ae04h,0d736fccch
dd 083426b33h,0f01eab71h,0b0804187h,03c005e5fh,077a057beh,0bde8ae24h
dd 055464299h,0bf582e61h,04e58f48fh,0f2ddfda2h,0f474ef38h,08789bdc2h
dd 05366f9c3h,0c8b38e74h,0b475f255h,046fcd9b9h,07aeb2661h,08b1ddf84h
dd 0846a0e79h,0915f95e2h,0466e598eh,020b45770h,08cd55591h,0c902de4ch
dd 0b90bace1h,0bb8205d0h,011a86248h,07574a99eh,0b77f19b6h,0e0a9dc09h
dd 0662d09a1h,0c4324633h,0e85a1f02h,009f0be8ch,04a99a025h,01d6efe10h
dd 01ab93d1dh,00ba5a4dfh,0a186f20fh,02868f169h,0dcb7da83h,0573906feh
dd 0a1e2ce9bh,04fcd7f52h,050115e01h,0a70683fah,0a002b5c4h,00de6d027h
dd 09af88c27h,0773f8641h,0c3604c06h,061a806b5h,0f0177a28h,0c0f586e0h
dd 0006058aah,030dc7d62h,011e69ed7h,02338ea63h,053c2dd94h,0c2c21634h
dd 0bbcbee56h,090bcb6deh,0ebfc7da1h,0ce591d76h,06f05e409h,04b7c0188h
dd 039720a3dh,07c927c24h,086e3725fh,0724d9db9h,01ac15bb4h,0d39eb8fch
dd 0ed545578h,008fca5b5h,0d83d7cd3h,04dad0fc4h,01e50ef5eh,0b161e6f8h
dd 0a28514d9h,06c51133ch,06fd5c7e7h,056e14ec4h,0362abfceh,0ddc6c837h
dd 0d79a3234h,092638212h,0670efa8eh,0406000e0h
;-----------------------------------------
BlowFish_OrigS3:
dd 03a39ce37h,0d3faf5cfh,0abc27737h,05ac52d1bh,05cb0679eh,04fa33742h
dd 0d3822740h,099bc9bbeh,0d5118e9dh,0bf0f7315h,0d62d1c7eh,0c700c47bh
dd 0b78c1b6bh,021a19045h,0b26eb1beh,06a366eb4h,05748ab2fh,0bc946e79h
dd 0c6a376d2h,06549c2c8h,0530ff8eeh,0468dde7dh,0d5730a1dh,04cd04dc6h
dd 02939bbdbh,0a9ba4650h,0ac9526e8h,0be5ee304h,0a1fad5f0h,06a2d519ah
dd 063ef8ce2h,09a86ee22h,0c089c2b8h,043242ef6h,0a51e03aah,09cf2d0a4h
dd 083c061bah,09be96a4dh,08fe51550h,0ba645bd6h,02826a2f9h,0a73a3ae1h
dd 04ba99586h,0ef5562e9h,0c72fefd3h,0f752f7dah,03f046f69h,077fa0a59h
dd 080e4a915h,087b08601h,09b09e6adh,03b3ee593h,0e990fd5ah,09e34d797h
dd 02cf0b7d9h,0022b8b51h,096d5ac3ah,0017da67dh,0d1cf3ed6h,07c7d2d28h
dd 01f9f25cfh,0adf2b89bh,05ad6b472h,05a88f54ch,0e029ac71h,0e019a5e6h
dd 047b0acfdh,0ed93fa9bh,0e8d3c48dh,0283b57cch,0f8d56629h,079132e28h
dd 0785f0191h,0ed756055h,0f7960e44h,0e3d35e8ch,015056dd4h,088f46dbah
dd 003a16125h,00564f0bdh,0c3eb9e15h,03c9057a2h,097271aech,0a93a072ah
dd 01b3f6d9bh,01e6321f5h,0f59c66fbh,026dcf319h,07533d928h,0b155fdf5h
dd 003563482h,08aba3cbbh,028517711h,0c20ad9f8h,0abcc5167h,0ccad925fh
dd 04de81751h,03830dc8eh,0379d5862h,09320f991h,0ea7a90c2h,0fb3e7bceh
dd 05121ce64h,0774fbe32h,0a8b6e37eh,0c3293d46h,048de5369h,06413e680h
dd 0a2ae0810h,0dd6db224h,069852dfdh,009072166h,0b39a460ah,06445c0ddh
dd 0586cdecfh,01c20c8aeh,05bbef7ddh,01b588d40h,0ccd2017fh,06bb4e3bbh
dd 0dda26a7eh,03a59ff45h,03e350a44h,0bcb4cdd5h,072eacea8h,0fa6484bbh
dd 08d6612aeh,0bf3c6f47h,0d29be463h,0542f5d9eh,0aec2771bh,0f64e6370h
dd 0740e0d8dh,0e75b1357h,0f8721671h,0af537d5dh,04040cb08h,04eb4e2cch
dd 034d2466ah,00115af84h,0e1b00428h,095983a1dh,006b89fb4h,0ce6ea048h
dd 06f3f3b82h,03520ab82h,0011a1d4bh,0277227f8h,0611560b1h,0e7933fdch
dd 0bb3a792bh,0344525bdh,0a08839e1h,051ce794bh,02f32c9b7h,0a01fbac9h
dd 0e01cc87eh,0bcc7d1f6h,0cf0111c3h,0a1e8aac7h,01a908749h,0d44fbd9ah
dd 0d0dadecbh,0d50ada38h,00339c32ah,0c6913667h,08df9317ch,0e0b12b4fh
dd 0f79e59b7h,043f5bb3ah,0f2d519ffh,027d9459ch,0bf97222ch,015e6fc2ah
dd 00f91fc71h,09b941525h,0fae59361h,0ceb69cebh,0c2a86459h,012baa8d1h
dd 0b6c1075eh,0e3056a0ch,010d25065h,0cb03a442h,0e0ec6e0eh,01698db3bh
dd 04c98a0beh,03278e964h,09f1f9532h,0e0d392dfh,0d3a0342bh,08971f21eh
dd 01b0a7441h,04ba3348ch,0c5be7120h,0c37632d8h,0df359f8dh,09b992f2eh
dd 0e60b6f47h,00fe3f11dh,0e54cda54h,01edad891h,0ce6279cfh,0cd3e7e6fh
dd 01618b166h,0fd2c1d05h,0848fd2c5h,0f6fb2299h,0f523f357h,0a6327623h
dd 093a83531h,056cccd02h,0acf08162h,05a75ebb5h,06e163697h,088d273cch
dd 0de966292h,081b949d0h,04c50901bh,071c65614h,0e6c6c7bdh,0327a140ah
dd 045e1d006h,0c3f27b9ah,0c9aa53fdh,062a80f00h,0bb25bfe2h,035bdd2f6h
dd 071126905h,0b2040222h,0b6cbcf7ch,0cd769c2bh,053113ec0h,01640e3d3h
dd 038abbd60h,02547adf0h,0ba38209ch,0f746ce76h,077afa1c5h,020756060h
dd 085cbfe4eh,08ae88dd8h,07aaaf9b0h,04cf9aa7eh,01948c25ch,002fb8a8ch
dd 001c36ae4h,0d6ebe1f9h,090d4f869h,0a65cdea0h,03f09252dh,0c208e69fh
dd 0b74e6132h,0ce77e25bh,0578fdfe3h,03ac372e6h
;-----------------------------------------


;rec:
;s0 dd 256 dup (?)           0000h
;s1 dd 256 dup (?)           0400h
;s2 dd 256 dup (?)           0800h
;s3 dd 256 dup (?)           0c00h
;p  dd 18  dup (?)           1000h
