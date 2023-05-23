{$heap 7k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

Var
  pipe:LongInt;
  bufD:array[1..256] of longInt;
  bufB:array[1..1024] of byte absolute bufD;
  bufC:array[1..1024] of char absolute bufD;
  bufS:longInt;

procedure xchg(i:LongInt);
label f1;
begin;
pipeLineSend(pipe,bufD,i);
f1:
Relequish;
bufS:=sizeof(bufD);
if (pipeLineRecv(pipe,bufD,bufS)<>0) then goto f1;
if (bufS<1) then goto f1;
end;


procedure doPort(i:LongInt);
begin;
write('port '+bstr(i)+': ');
bufD[1]:=3;bufD[2]:=i;xchg(8);
bufD[1]:=2;bufD[2]:=i;xchg(8);
bufD[1]:=4;bufD[2]:=i;xchg(8);
write('[');
if (bufD[2] and 1=0) then write('conn') else write('CONN');
write('] [');
if (bufD[2] and 2=0) then write('ena') else write('ENA');
write('] speed:');
writeln(bstr(bufD[3]));
end;


const dev=$25;
Var
  i,o,p,q:LongInt;
BEGIN;
p:=BugOS_findProcNam('uhci.code');
pipeLineCreate(pipe,p,65536,true);
writeln(bstr(p)+'  '+bstr(pipe));

bufD[1]:=0;
xchg(4);
writeln('# of ports: '+bstr(bufD[2]));

doPort(0);
doPort(1);

bufD[1]:=5;
bufD[2]:=0;
bufD[3]:=0;
bufD[4]:=1;
bufD[5]:=1;
bufD[6]:=0;
bufB[25]:=0;
bufB[26]:=5;
bufB[27]:=dev;
bufB[28]:=dev shr 8;
bufB[29]:=0;
bufB[30]:=0;
bufB[31]:=0;
bufB[32]:=0;
xchg(32);
write('result: '+bstr(bufD[2]));
bufD[1]:=6;
bufD[2]:=64;
bufD[3]:=0;
bufD[4]:=0;
bufD[5]:=0;
bufD[6]:=1;
bufD[7]:=1;
xchg(28);
writeLn(', '+bstr(bufD[2])+', '+bstr(bufS));

bufD[1]:=5;
bufD[2]:=dev;
bufD[3]:=0;
bufD[4]:=1;
bufD[5]:=1;
bufD[6]:=0;
bufB[25]:=$80;
bufB[26]:=6;
bufB[27]:=$01;
bufB[28]:=$03;
bufB[29]:=0;
bufB[30]:=0;
bufB[31]:=128;
bufB[32]:=0;
xchg(32);
writeLn('result: '+bstr(bufD[2]));

p:=1;
repeat
  bufD[1]:=6;
  bufD[2]:=64;
  bufD[3]:=dev;
  bufD[4]:=0;
  bufD[5]:=0;
  bufD[6]:=1;
  bufD[7]:=p;
  xchg(28);
  for i:=9 to bufS do write(bufC[i]);
  p:=p xor 1;
  until (bufS<>8+16);
writeLn('results: '+bstr(bufD[2])+', '+bstr(bufS));

END.