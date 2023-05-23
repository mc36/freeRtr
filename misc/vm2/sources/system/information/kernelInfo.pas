{$heap 47k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc bugos.inc}

Var buf:array[1..32*1024] of char;

procedure printBuf;
var
  a:String;
  p:LongInt;
begin;
WriteLn('');
p:=1;
while (buf[p]<>#0) do begin;
  a:='';
  while (buf[p]<>#13) do begin;
    a:=a+buf[p];
    inc(p);
    end;
  inc(p,2);
  WriteLn(a);
  end;
end;

function conv(i:LongInt):String;
var a:string;
begin;
a:=bstr(i);
while (length(a)<2) do a:='0'+a;
conv:=a;
end;

Var
  a:String;
  i,o,p:longInt;
BEGIN;
WriteLn('kernel information v1.0, done by Mc at '#%date' '#%time'.');
BugOS_KernelLogo(buf);
printBuf;
BugOS_KernelInfo(buf);
printBuf;
BugOS_KernelUptime(i,o,p);
o:=o div p;
a:=conv(o div 3600);
o:=o mod 3600;
a:=a+':'+conv(o div 60)+':'+conv(o mod 60);
WriteLn('system is up for '+BStr(i)+' days and '+a+' seconds.');
END.