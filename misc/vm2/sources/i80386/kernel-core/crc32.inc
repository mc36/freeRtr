use32                           ;not a question...
;-------------------------------

;-------------------------------
proc crc32update
;in:  al-character...
;     edx-original value...
;out: eax-destroyed...
;     edx-updated value...
xor al,dl
movzx eax,al
shr edx,8
xor edx,cs:[crc32table+eax*4]
retnd
endp
;-------------------------------

;-------------------------------
;crc value updating:
;crc:=crc32table[(crc and $ff) xor c] xor (crc shr 8);
;
;table entry generation:
;t:=i;
;for x:=1 to 8 do
; if (t and 1<>0) then t:=(t shr 1) xor $edb88320 else t:=t shr 1;
;crc32table[i]:=t;
;-------------------------------

;-------------------------------
align 4
crc32table:
dd 000000000h,077073096h,0ee0e612ch,0990951bah,0076dc419h,0706af48fh
dd 0e963a535h,09e6495a3h,00edb8832h,079dcb8a4h,0e0d5e91eh,097d2d988h
dd 009b64c2bh,07eb17cbdh,0e7b82d07h,090bf1d91h,01db71064h,06ab020f2h
dd 0f3b97148h,084be41deh,01adad47dh,06ddde4ebh,0f4d4b551h,083d385c7h
dd 0136c9856h,0646ba8c0h,0fd62f97ah,08a65c9ech,014015c4fh,063066cd9h
dd 0fa0f3d63h,08d080df5h,03b6e20c8h,04c69105eh,0d56041e4h,0a2677172h
dd 03c03e4d1h,04b04d447h,0d20d85fdh,0a50ab56bh,035b5a8fah,042b2986ch
dd 0dbbbc9d6h,0acbcf940h,032d86ce3h,045df5c75h,0dcd60dcfh,0abd13d59h
dd 026d930ach,051de003ah,0c8d75180h,0bfd06116h,021b4f4b5h,056b3c423h
dd 0cfba9599h,0b8bda50fh,02802b89eh,05f058808h,0c60cd9b2h,0b10be924h
dd 02f6f7c87h,058684c11h,0c1611dabh,0b6662d3dh,076dc4190h,001db7106h
dd 098d220bch,0efd5102ah,071b18589h,006b6b51fh,09fbfe4a5h,0e8b8d433h
dd 07807c9a2h,00f00f934h,09609a88eh,0e10e9818h,07f6a0dbbh,0086d3d2dh
dd 091646c97h,0e6635c01h,06b6b51f4h,01c6c6162h,0856530d8h,0f262004eh
dd 06c0695edh,01b01a57bh,08208f4c1h,0f50fc457h,065b0d9c6h,012b7e950h
dd 08bbeb8eah,0fcb9887ch,062dd1ddfh,015da2d49h,08cd37cf3h,0fbd44c65h
dd 04db26158h,03ab551ceh,0a3bc0074h,0d4bb30e2h,04adfa541h,03dd895d7h
dd 0a4d1c46dh,0d3d6f4fbh,04369e96ah,0346ed9fch,0ad678846h,0da60b8d0h
dd 044042d73h,033031de5h,0aa0a4c5fh,0dd0d7cc9h,05005713ch,0270241aah
dd 0be0b1010h,0c90c2086h,05768b525h,0206f85b3h,0b966d409h,0ce61e49fh
dd 05edef90eh,029d9c998h,0b0d09822h,0c7d7a8b4h,059b33d17h,02eb40d81h
dd 0b7bd5c3bh,0c0ba6cadh,0edb88320h,09abfb3b6h,003b6e20ch,074b1d29ah
dd 0ead54739h,09dd277afh,004db2615h,073dc1683h,0e3630b12h,094643b84h
dd 00d6d6a3eh,07a6a5aa8h,0e40ecf0bh,09309ff9dh,00a00ae27h,07d079eb1h
dd 0f00f9344h,08708a3d2h,01e01f268h,06906c2feh,0f762575dh,0806567cbh
dd 0196c3671h,06e6b06e7h,0fed41b76h,089d32be0h,010da7a5ah,067dd4acch
dd 0f9b9df6fh,08ebeeff9h,017b7be43h,060b08ed5h,0d6d6a3e8h,0a1d1937eh
dd 038d8c2c4h,04fdff252h,0d1bb67f1h,0a6bc5767h,03fb506ddh,048b2364bh
dd 0d80d2bdah,0af0a1b4ch,036034af6h,041047a60h,0df60efc3h,0a867df55h
dd 0316e8eefh,04669be79h,0cb61b38ch,0bc66831ah,0256fd2a0h,05268e236h
dd 0cc0c7795h,0bb0b4703h,0220216b9h,05505262fh,0c5ba3bbeh,0b2bd0b28h
dd 02bb45a92h,05cb36a04h,0c2d7ffa7h,0b5d0cf31h,02cd99e8bh,05bdeae1dh
dd 09b64c2b0h,0ec63f226h,0756aa39ch,0026d930ah,09c0906a9h,0eb0e363fh
dd 072076785h,005005713h,095bf4a82h,0e2b87a14h,07bb12baeh,00cb61b38h
dd 092d28e9bh,0e5d5be0dh,07cdcefb7h,00bdbdf21h,086d3d2d4h,0f1d4e242h
dd 068ddb3f8h,01fda836eh,081be16cdh,0f6b9265bh,06fb077e1h,018b74777h
dd 088085ae6h,0ff0f6a70h,066063bcah,011010b5ch,08f659effh,0f862ae69h
dd 0616bffd3h,0166ccf45h,0a00ae278h,0d70dd2eeh,04e048354h,03903b3c2h
dd 0a7672661h,0d06016f7h,04969474dh,03e6e77dbh,0aed16a4ah,0d9d65adch
dd 040df0b66h,037d83bf0h,0a9bcae53h,0debb9ec5h,047b2cf7fh,030b5ffe9h
dd 0bdbdf21ch,0cabac28ah,053b39330h,024b4a3a6h,0bad03605h,0cdd70693h
dd 054de5729h,023d967bfh,0b3667a2eh,0c4614ab8h,05d681b02h,02a6f2b94h
dd 0b40bbe37h,0c30c8ea1h,05a05df1bh,02d02ef8dh
;-------------------------------
