{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc crt.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

Label f1;
Var
  i,o,p,p1,p2,s:LongInt;
  a:string;
BEGIN;
o:=BVal(GetAllParameters);
if (o<1) then begin;
  WriteLn('using: serial-test.code <port>');
  exit;
  end;
p:=BugOS_findProcNam('serial.code');
writeln(bstr(p));
pipeLineCreate(p1,p,4096,true);
pipeLineCreate(p2,p,4096,false);
writeln(bstr(p1)+' '+bstr(p2));

s:=16;
while (s>0) do begin;
  relequish;
  dec(s);
  i:=sizeof(a);
  if (pipeLineRecv(p1,a,i)<>0) then continue;
  if (i>0) then break;
  end;

dec(o);
pipeLineSend(p1,o,sizeof(o));
pipeLineSend(p1,p2,sizeof(p2));

f1:
if keypressed then begin;
  i:=readKey;
  case i of
    $8002:i:=9;
    $8003:i:=8;
    $8004:i:=13;
    $8005:i:=27;
    end;
  if (i and $8000<>0) then exit;
  pipeLineSend(p2,i,1);
  end;
i:=128;
if (pipeLineRecv(p2,a[1],i)<>0) then i:=0;
a[0]:=chr(i);
write(a);
relequish;
goto f1;
END.