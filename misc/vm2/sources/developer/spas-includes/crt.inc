Procedure ClrScr;
Begin;
asm sysCall console.clear;
End;

Function KeyPressed:Boolean;
Begin;
asm sysCall console.iskey;
asm varAddr trg @result;
asm movW dubub [trg] a;
End;

Procedure ConsoleSize(var x,y:Word);
Begin;
asm sysCall console.size;
asm varAddr trg x;
asm AddrLod trg [trg];
asm movW duwuw [trg] a;
asm varAddr trg y;
asm AddrLod trg [trg];
asm movW duwuw [trg] b;
End;

Function ReadKey:Word;
Begin;
asm label @j1;
asm varAddr trg @result;
asm move udud c 2;
asm sysCall console.read;
asm comp udud c 2;
asm jmpc e @j2;
asm sysCall sleep;
asm jump @j1;
asm label @j2;
End;

Procedure GotoXY(x,y:Word);
Begin;
asm varAddr trg x;
asm movR duwuw a [trg];
asm varAddr trg y;
asm movR duwuw b [trg];
asm sysCall console.GotoXY;
End;

Procedure TextColor(color:Byte);
Begin;
asm varAddr trg color;
asm movR dubub a [trg];
asm sysCall console.setColor;
End;
