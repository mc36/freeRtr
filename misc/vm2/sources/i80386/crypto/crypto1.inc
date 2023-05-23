;-------------------------------
context_size equ 1400h          ;size of one context...
DataBlock_contx equ 0           ;8k: hash/cipher context values...
DataBlock_algor equ 13ech       ;dd: algorithm number...
DataBlock_ivec0 equ 13f0h       ;dd: iv0...
DataBlock_ivec1 equ 13f4h       ;dd: iv1...
DataBlock_ivec2 equ 13f8h       ;dd: iv2...
DataBlock_ivec3 equ 13fch       ;dd: iv3...
DataBlock_temp1 equ 2000h       ;1k: temporary buffer...
DataBlock_temp2 equ 2400h       ;1k: temporary buffer...
DataBlock_temp3 equ 2800h       ;1k: temporary buffer...
DataBlock_temp4 equ 2c00h       ;1k: temporary buffer...
DataBlock_temp30 equ 3000h      ;16: temporary value...
DataBlock_temp31 equ 3010h      ;16: temporary value...
DataBlock_temp32 equ 3020h      ;16: temporary value...
DataBlock_temp33 equ 3030h      ;16: temporary value...
DataBlock_temp34 equ 3040h      ;16: temporary value...
DataBlock_temp35 equ 3050h      ;16: temporary value...
DataBlock_temp36 equ 3060h      ;16: temporary value...
DataBlock_temp37 equ 3070h      ;16: temporary value...
DataBlock_temp38 equ 3080h      ;16: temporary value...
DataBlock_temp39 equ 3090h      ;16: temporary value...
DataBlock_temp40 equ 30a0h      ;16: temporary value...
DataBlock_temp41 equ 30b0h      ;16: temporary value...
DataBlock_temp42 equ 30c0h      ;16: temporary value...
DataBlock_temp43 equ 30d0h      ;16: temporary value...
DataBlock_temp44 equ 30e0h      ;16: temporary value...
DataBlock_temp45 equ 30f0h      ;16: temporary value...
DataBlock_temp46 equ 3100h      ;16: temporary value...
DataBlock_temp47 equ 3110h      ;16: temporary value...
DataBlock_temp48 equ 3120h      ;16: temporary value...
DataBlock_temp49 equ 3130h      ;16: temporary value...
DataBlock_temp50 equ 3140h      ;16: temporary value...
DataBlock_Handle equ 3144h      ;dd: offset of transformer...
DataBlock_bufPos equ 3148h      ;dd: position in buffer...
DataBlock_bufSiz equ 314ch      ;dd: size of buffer...
DataBlock_algOfs equ 3150h      ;dd: offset of algorithm...
DataBlock_bufDat equ 3180h      ;16k: data buffer...
;-------------------------------





;------------------------------- algorithm names...
name_blowfish db 'blowfish',0
name_cast db 'cast',0
name_des db 'des',0
name_des3 db '3des',0
name_rc2 db 'rc2',0
name_rc4 db 'rc4',0
name_rc5 db 'rc5',0
name_rc6 db 'rc6',0
name_rijndael db 'aes',0
name_twofish db 'twofish',0
name_idea db 'idea',0
name_tea db 'tea',0
name_camellia db 'camellia',0
nameCBC_blowfish db 'blowfish-cbc',0
nameCBC_cast db 'cast-cbc',0
nameCBC_des db 'des-cbc',0
nameCBC_des3 db '3des-cbc',0
nameCBC_rc2 db 'rc2-cbc',0
nameCBC_rc5 db 'rc5-cbc',0
nameCBC_rc6 db 'rc6-cbc',0
nameCBC_rijndael db 'aes-cbc',0
nameCBC_twofish db 'twofish-cbc',0
nameCBC_idea db 'idea-cbc',0
nameCBC_tea db 'tea-cbc',0
nameCBC_camellia db 'camellia-cbc',0
;hash
name_crc16 db 'crc16',0
name_crc32 db 'crc32',0
name_md2 db 'md2',0
name_md4 db 'md4',0
name_md5 db 'md5',0
name_rmd160 db 'rmd160',0
name_sha1 db 'sha1',0
name_sha256 db 'sha256',0
;-------------------------------

;------------------------------- cipher data...
cipherMax equ 21
cipherNames:
dd offset name_blowfish,offset name_cast,offset name_des,offset name_des3
dd offset name_rc2,offset name_rc4,offset name_rc5,offset name_rc6
dd offset name_Rijndael,offset name_twofish,offset name_idea
dd offset name_tea,offset name_camellia
;cbc
dd offset nameCBC_blowfish,offset nameCBC_cast,offset nameCBC_des
dd offset nameCBC_des3,offset nameCBC_rc2,offset nameCBC_rc5
dd offset nameCBC_rc6,offset nameCBC_Rijndael,offset nameCBC_twofish
dd offset nameCBC_idea,offset nameCBC_tea,offset nameCBC_camellia
cipherSizes:
dd BlowFish_size,CAST_size,DES_size,DES3_size,RC2_size,RC4_size,RC5_size
dd RC6_size,Rijndael_size,TwoFish_size,idea_size,tea_size,camellia_size
;cbc
dd BlowFish_size,CAST_size,DES_size,DES3_size,RC2_size,RC5_size,RC6_size
dd Rijndael_size,TwoFish_size,idea_size,tea_size,camellia_size
cipherParams:
dd BlowFish_param,CAST_param,DES_param,DES3_param,RC2_param,RC4_param
dd RC5_param,RC6_param,Rijndael_param,TwoFish_param,idea_param
dd tea_param,camellia_param
;cbc
dd BlowFish_param,CAST_param,DES_param,DES3_param,RC2_param,RC5_param
dd RC6_param,Rijndael_param,TwoFish_param,idea_param,tea_param
dd camellia_param
cipherInits:
dd offset BlowFish_init,offset CAST_init,offset DES_init,offset DES3_init
dd offset RC2_init,offset RC4_init,offset RC5_init,offset RC6_init
dd offset Rijndael_init,offset TwoFish_init,offset idea_init
dd offset tea_init,offset camellia_init
;cbc
dd offset BlowFish_init,offset CAST_init,offset DES_init,offset DES3_init
dd offset RC2_init,offset RC5_init,offset RC6_init,offset Rijndael_init
dd offset TwoFish_init,offset idea_init,offset tea_init,offset camellia_init
cipherEncoders:
dd offset doCipher_code1,offset doCipher_code1,offset doCipher_code1
dd offset doCipher_code1,offset doCipher_code1,offset doCipher_code3
dd offset doCipher_code1,offset doCipher_code2,offset doCipher_code2
dd offset doCipher_code2,offset doCipher_code1,offset doCipher_code1
dd offset doCipher_code2
;cbc
dd offset doCipher_code4,offset doCipher_code4,offset doCipher_code4
dd offset doCipher_code4,offset doCipher_code4,offset doCipher_code4
dd offset doCipher_code6,offset doCipher_code6,offset doCipher_code6
dd offset doCipher_code4,offset doCipher_code4,offset doCipher_code6
cipherDecoders:
dd offset doCipher_code1,offset doCipher_code1,offset doCipher_code1
dd offset doCipher_code1,offset doCipher_code1,offset doCipher_code3
dd offset doCipher_code1,offset doCipher_code2,offset doCipher_code2
dd offset doCipher_code2,offset doCipher_code1,offset doCipher_code1
dd offset doCipher_code2
;cbc
dd offset doCipher_code5,offset doCipher_code5,offset doCipher_code5
dd offset doCipher_code5,offset doCipher_code5,offset doCipher_code5
dd offset doCipher_code7,offset doCipher_code7,offset doCipher_code7
dd offset doCipher_code5,offset doCipher_code5,offset doCipher_code7
cipherEncrypts:
dd offset BlowFish_encrypt,offset CAST_encrypt,offset DES_encrypt
dd offset DES3_encrypt,offset RC2_encrypt,offset RC4_process
dd offset RC5_encrypt,offset RC6_encrypt,offset Rijndael_encrypt
dd offset TwoFish_encrypt,offset idea_encrypt,offset tea_encrypt
dd offset camellia_encrypt
;cbc
dd offset BlowFish_encrypt,offset CAST_encrypt,offset DES_encrypt
dd offset DES3_encrypt,offset RC2_encrypt,offset RC5_encrypt
dd offset RC6_encrypt,offset Rijndael_encrypt,offset TwoFish_encrypt
dd offset idea_encrypt,offset tea_encrypt,offset camellia_encrypt
cipherDecrypts:
dd offset BlowFish_decrypt,offset CAST_decrypt,offset DES_decrypt
dd offset DES3_decrypt,offset RC2_decrypt,offset RC4_process
dd offset RC5_decrypt,offset RC6_decrypt,offset Rijndael_decrypt
dd offset TwoFish_decrypt,offset idea_decrypt,offset tea_decrypt
dd offset camellia_decrypt
;cbc
dd offset BlowFish_decrypt,offset CAST_decrypt,offset DES_decrypt
dd offset DES3_decrypt,offset RC2_decrypt,offset RC5_decrypt
dd offset RC6_decrypt,offset Rijndael_decrypt,offset TwoFish_decrypt
dd offset idea_decrypt,offset tea_decrypt,offset camellia_decrypt
cipherIvPut:
dd offset doCipher_ivInit1,offset doCipher_ivInit1,offset doCipher_ivInit1
dd offset doCipher_ivInit1,offset doCipher_ivInit1,offset doCipher_ivInit1
dd offset doCipher_ivInit1,offset doCipher_ivInit1,offset doCipher_ivInit1
dd offset doCipher_ivInit1,offset doCipher_ivInit1,offset doCipher_ivInit1
dd offset doCipher_ivInit1
;cbc
dd offset doCipher_ivInit2,offset doCipher_ivInit2,offset doCipher_ivInit2
dd offset doCipher_ivInit2,offset doCipher_ivInit2,offset doCipher_ivInit2
dd offset doCipher_ivInit3,offset doCipher_ivInit3,offset doCipher_ivInit3
dd offset doCipher_ivInit2,offset doCipher_ivInit2,offset doCipher_ivInit3
;-------------------------------

;------------------------------- hasher data...
hasherMax equ 8
hasherNames:
dd offset name_crc16,offset name_crc32,offset name_md2,offset name_md4
dd offset name_md5,offset name_rmd160,offset name_sha1,offset name_sha256
hasherSizes:
dd crc16_size,crc32_size,md2_size,md4_size,md5_size,rmd160_size,sha1_size
dd sha256_size
hasherInits:
dd offset crc16_init,offset crc32_init,offset md2_init,offset md4_init
dd offset md5_init,offset rmd160_init,offset sha1_init,offset sha256_init
hasherAdds:
dd offset crc16_addChar,offset crc32_addChar,offset md2_addChar
dd offset md4_addChar,offset md5_addChar,offset rmd160_addChar
dd offset sha1_addChar,offset sha256_addChar
hasherFinish:
dd offset crc16_finish,offset crc32_finish,offset md2_finish,offset md4_finish
dd offset md5_finish,offset rmd160_finish,offset sha1_finish
dd offset sha256_finish
;-------------------------------
