{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc hex.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
{$sysinc memory.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Function dup(n:Byte;c:Char):String;
Var
  a:string;
  i:Word;
Begin;
a:='';
for i:=1 to n do a:=a+c;
dup:=a;
End;

Function xLevesz(a:String):String;
Begin;
Kicserel(#9,' ',a);
Kicserel(#255,' ',a);
Kicserel(#0,' ',a);
while (Copy(a,1,1)=' ') do Delete(a,1,1);
xLevesz:=Levesz(a);
End;

Function disAsm2hex(dat:LongInt;s:byte):String;
Var
  a:String;
  d:array[1..4] of byte absolute dat;
  i:Byte;
Begin;
a:='$';
for i:=s downto 1 do a:=a+byte2hextype(d[i]);
disAsm2hex:=a;
End;

{$include disasm-mips.inc}

Label f1,f2;
Const
  ProggyName='disAsm-mips v1.0';
Const
  FieldSep='  ';
  HexCodSiz=4*2;


Var
  OrgValue:LongInt;
  f:xFile;
  t:xtText;
  bufD:array[1..1024] of byte;
  bufS:LongInt;
  bufB:LongInt;
  filS:LongInt;
  filP:LongInt;
  a,b,c:String;
  i,o,p:LongInt;
BEGIN;
WriteLn(ProggyName+', done by Mc at '#%date' '#%time'.');

OrgValue:=0;
a:=xLevesz(ParamStr(1));
b:=a+'.list';
if (a='') then begin;
  WriteLn('using: codeLister.code <binary> [options]');
  WriteLn('options: org=NUM');
  WriteLn('         out=fileName');
  Halt(1);
  end;
for i:=2 to ParamCount do begin;
  c:=kicsi(xLevesz(ParamStr(i)));
  if (copy(c,1,4)='out=') then b:=copy(c,5,255);
  if (copy(c,1,4)='org=') then orgValue:=BVal(copy(c,5,255));
  end;

WriteLn('source: '+a);
WriteLn('target: '+b);

if (xOpen(f,a,xGenFilMod_r)<>0) then immErr('error opening source!');
if (xCreate(b)<>0) then immErr('error creating target!');
if (xtOpen(t,b,false)<>0) then immErr('error opening target!');
filS:=xFileSize(f);filP:=0;
bufS:=0;bufB:=0;
WriteLn('  size: '+alakit(filS));
WriteLn('disassembling...');

a:='listing of '+a;
xtWriteLn(t,' '+a);
xtWriteLn(t,dup(length(a)+2,'~'));
xtWriteLn(t,'created by '+ProggyName+'.');
xtWriteLn(t,'');
xtWriteLn(t,'the file contains '+alakit(filS)+' bytes.');
xtWriteLn(t,'relative offsets are organised to '+disAsm2hex(OrgValue,4)+'.');
xtWriteLn(t,'');

f1:
Write(#13+disAsm2hex(filP,4)+#13);
if (filP>=filS) then goto f2;
if (filP+sizeof(bufD) shr 1>bufB+bufS) then begin;
  bufB:=filP;
  bufS:=filS-bufB;
  if (bufS>sizeof(bufD)) then bufS:=sizeof(bufD);
  xSeek(f,bufB);
  if (xBlockRead(f,bufD,bufS)<>0) then immErr('error reading source!');
  end;

o:=filP-bufB;
i:=bufS-o;
if (i>sizeof(a)-1) then i:=sizeof(a)-1;
b[0]:=chr(i);
move(bufD[o+1],b[1],i);

c:=DisAssemblerBinaryCode(b);
if (c='') then c:=#1;
p:=ord(c[1]);
delete(c,1,1);
b:=copy(b,1,p);
kicserel(#13,'  ',c);
o:=pos(#10,c);
if (o<>0) then begin;
  i:=BVal(copy(c,o+1,255));
  c:=copy(c,1,o-1)+disAsm2hex(i+filP+OrgValue,3);
  end;
a:='';
for o:=1 to p do a:=a+byte2hextype(ord(b[o]));
a:=copy(a+dup(128,' '),1,HexCodSiz);

xtWrite(t,disAsm2hex(filP+OrgValue,3));
xtWrite(t,FieldSep);
xtWrite(t,a);
xtWrite(t,FieldSep);
xtWriteLn(t,c);
inc(filP,p);
goto f1;
f2:

xtWriteLn(t,'');
xtWriteLn(t,'eof.');
xtClose(t);
WriteLn(#13'Successfully finished!');
END.