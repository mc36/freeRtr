{{$define debug1}
{{$define debug2}
{{$define debug3}
{{$define debug4}
{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc random.inc}
{$sysinc memory.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

{$include hci0.inc}
{$include hci1.inc}
{$include hci2.inc}
{$include hci3.inc}
{$include hci4.inc}

Procedure ImmErr(a:String);
Begin;
WriteLn(a);
Halt(2);
End;

Function getCurrentHostname:String;
Var t:xtText;
Begin;
if (xtOpen(t,'c:\system\localHost.text',true)=0) then begin;
  getCurrentHostname:=xtReadLn(t,666);
  xtClose(t);
  end else getCurrentHostname:='localHost';
End;




Label f1;
Var
  a:String;
  i,o,p:LongInt;
BEGIN;
WriteLn('bluetooth hci v1.0, done by Mc at '#%date' '#%time'.');
Randomize;
a:=ParamStr(1);
if (a='') then immErr('using: hci.code <process>');
WriteLn('process: '+a);
o:=BVal(a);
if (o=0) then o:=BugOS_findProcNam(a);
if (o=0) then immErr('process not found!');
i:=pipeLineCreate(localPipe,o,65536,true);
if (i<>0) then immErr('unabled to create pipeline!');
for i:=1 to 16 do relequish;
i:=sizeof(a);
if (pipeLineRecv(localPipe,a[1],i)<>0) then i:=0;
if (i<1) then immErr('initial packet not received!');
a[0]:=chr(i);
move(a[1],o,sizeof(o));
localDevice:=copy(a,o+o+17,666);
localDevice:=copy(localDevice,1,pos(#0,localDevice)-1);
writeln('device name: "'+localDevice+'"');
localName:=getCurrentHostname;

localPinCode:='123456';
psmConnNum:=0;
lcpConnNum:=0;
hciConnNum:=0;

if hciResetController then immErr('error!');
if hciConfigureController then immErr('error!');

writeLn('serving others...');
pipeLineBegListen;
BugOS_SignDaemoning;

f1:
relequish;
timer2start;
acceptIncomingConns;
psmReleq2conns;
hciReleq2device;
hciReleq2conns;
lcpReleq2conns;
goto f1;
END.