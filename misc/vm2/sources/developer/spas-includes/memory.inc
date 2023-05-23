Function readWordLSB(var buf):Word;
Begin;
asm varAddr src buf;
asm addrLod src [src];
asm varAddr trg @result;
asm movR luwuw a [src];
asm movW duwuw [trg] a;
End;

Function readWordMSB(var buf):Word;
Begin;
asm varAddr src buf;
asm addrLod src [src];
asm varAddr trg @result;
asm movR muwuw a [src];
asm movW duwuw [trg] a;
End;

Function readLongLSB(var buf):LongWord;
Begin;
asm varAddr src buf;
asm addrLod src [src];
asm varAddr trg @result;
asm movR ludud a [src];
asm movW dudud [trg] a;
End;

Function readLongMSB(var buf):LongWord;
Begin;
asm varAddr src buf;
asm addrLod src [src];
asm varAddr trg @result;
asm movR mudud a [src];
asm movW dudud [trg] a;
End;



Procedure writeWordLSB(var buf;dat:Word);
Begin;
asm varAddr trg buf;
asm addrLod trg [trg];
asm varAddr src dat;
asm movR duwuw a [src];
asm movW luwuw [trg] a;
End;

Procedure writeWordMSB(var buf;dat:Word);
Begin;
asm varAddr trg buf;
asm addrLod trg [trg];
asm varAddr src dat;
asm movR duwuw a [src];
asm movW muwuw [trg] a;
End;

Procedure writeLongLSB(var buf;dat:LongWord);
Begin;
asm varAddr trg buf;
asm addrLod trg [trg];
asm varAddr src dat;
asm movR dudud a [src];
asm movW ludud [trg] a;
End;

Procedure writeLongMSB(var buf;dat:LongWord);
Begin;
asm varAddr trg buf;
asm addrLod trg [trg];
asm varAddr src dat;
asm movR dudud a [src];
asm movW mudud [trg] a;
End;
