{$heap 63k}
{$stack 1k}
{$sysinc system.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc memory.inc}

Const maxScreenWidth=4*1024;
Var upperProcessPipeLine:LongInt;

Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

Procedure doOneUpperRequest(siz:LongInt;var buf);
Label f1;
Var i,o:LongInt;
Begin;
if (pipeLineSend(upperProcessPipeLine,buf,siz)<>0) then immErr('error sending request!');
f1:
relequish;
siz:=maxScreenWidth+32;
if (pipeLineRecv(upperProcessPipeLine,buf,siz)<>0) then siz:=0;
if (siz<>0) then exit;
pipeLineStats(upperProcessPipeLine,o,i,i);
if (o<>0) then goto f1;
immErr('upper closed connection!');
End;



Function uppFileSize:LongInt;
Var i:LongInt;
Begin;
i:=1;
doOneUpperRequest(sizeof(i),i);
uppFileSize:=i;
End;

Procedure uppTruncFile(ps:LongInt);
Var buf:array[1..2] of LongInt;
Begin;
buf[1]:=2;
buf[2]:=ps;
doOneUpperRequest(sizeof(buf),buf);
End;

Procedure uppWriteFile(ps,siz:LongInt;var buffer);
Var buf:array[1..1] of byte absolute buffer;
Begin;
move(buf,buf[9],siz);
move(ps,buf[5],sizeof(ps));
ps:=3;
move(ps,buf,sizeof(ps));
doOneUpperRequest(siz+sizeof(ps)*2,buf);
End;

Procedure uppReadFile(ps,siz:LongInt;var buffer);
Var buf:array[1..1] of LongInt absolute buffer;
Begin;
buf[1]:=4;
buf[2]:=ps;
buf[3]:=siz;
doOneUpperRequest(sizeof(ps)*3,buf);
End;

Procedure uppPutPalette(sx,sy:LongInt;var buffer);
Var buf:array[1..1024] of byte;
Begin;
move(sx,buf[5],sizeof(sx));
move(sy,buf[9],sizeof(sy));
sx:=5;
move(sx,buf,sizeof(sx));
move(buffer,buf[13],768);
doOneUpperRequest(780,buf);
End;

Procedure uppGetPalette(var sx,sy:LongInt;var buffer);
Var buf:array[1..1024] of byte;
Begin;
sx:=6;
move(sx,buf,sizeof(sx));
doOneUpperRequest(sizeof(sx),buf);
move(buf[1],sx,sizeof(sx));
move(buf[5],sy,sizeof(sy));
move(buf[9],buffer,768);
End;

Procedure uppPutLine(bx,by,siz:LongInt;var buffer);
Var buf:array[1..1] of byte absolute buffer;
Begin;
move(buf,buf[13],siz);
move(bx,buf[5],sizeof(bx));
move(by,buf[9],sizeof(by));
bx:=7;
move(bx,buf,sizeof(bx));
doOneUpperRequest(siz+sizeof(bx)*3,buf);
End;

Procedure uppGetLine(bx,by,siz:LongInt;var buffer);
Var buf:array[1..1] of byte absolute buffer;
Begin;
move(bx,buf[5],sizeof(bx));
move(by,buf[9],sizeof(by));
move(siz,buf[13],sizeof(siz));
bx:=8;
move(bx,buf,sizeof(bx));
doOneUpperRequest(sizeof(bx)*4,buf);
End;

Procedure uppFinishAction;
Var i:LongInt;
Begin;
i:=9;
doOneUpperRequest(sizeof(i),i);
End;
