{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc filesys.inc}
{$sysinc param.inc}

Function xLevesz(a:String):String;
Begin;
a:=' '+a+' ';
kicserel(#0,' ',a);
kicserel(#255,' ',a);
kicserel(#9,' ',a);
kicserel('  ',' ',a);
a:=copy(a,2,length(a)-2);
xLevesz:=a;
End;

{$include gsmPDUcodec.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;


Label f1;
Var
  f:xFile;
  b1,b2:gsmPDUcodecRecord;
  a:String;
  mode,dir:byte;
  i:LongInt;
BEGIN;
WriteLn('gsm pdu codec v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<>3) then begin;
  f1:
  WriteLn('using: gsmPDUcodec <command><direction> <input> <output>');
  WriteLn('commands: e-encode  d-decode');
  WriteLn('directions: r-received s-sent');
  halt(1);
  end;
a:=kicsi(paramstr(1));
case a[1] of
  'e':begin; mode:=1;a:=a+'s'; end;
  'd':begin; mode:=2;a:=a+'r'; end;
  else goto f1;
  end;
case a[2] of
  'r':dir:=1;
  's':dir:=0;
  else goto f1;
  end;

if (xOpen(f,paramStr(2),xGenFilMod_r)<>0) then immErr('error opening input file!');
i:=xFileSize(f);
if (i>sizeof(b1.d)) then i:=sizeof(b1.d);
b1.s:=i;
xBlockRead(f,b1.d,i);
xClose(f);

case mode of
  1:i:=gsmPDUcodecEncodeOne(b1,b2,dir);
  2:i:=gsmPDUcodecDecodeOne(b1,b2,dir);
  end;
if (i<>0) then immErr('failed!');

a:=paramStr(3);
if (xCreate(a)<>0) then immErr('error creating output file!');
if (xOpen(f,a,xGenFilMod_rw)<>0) then immErr('error opening output file!');
xBlockWrite(f,b2,b2.s);
xClose(f);
WriteLn('done!');
END.