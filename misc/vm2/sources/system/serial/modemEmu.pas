{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

{$include \sources\internet\kernel\utils\timer2.inc}
{$include serial.inc}
{$include modemEmu1.inc}
{$include modemEmu2.inc}

Label f1,f2,f3;
Var
  port1,port2:modemStateRecord;
  lastTest:LongInt;
  a:String;
  i,o:LongInt;
BEGIN;
WriteLn(modemProggy+' '+modemVersion+', done by Mc at '#%date' '#%time'.');
timer2start;

fillchar(port1,sizeof(port1),0);
modemInitialize(port1.curr);
modemInitialize(port1.prof[0]);
modemInitialize(port1.prof[1]);
port1.lastSign:=-1;
port2:=port1;

ignoreBadCommands:=false;
if (paramStr(1)='1') then ignoreBadCommands:=true;

a:=paramStr(1);
serialProc:=BVal(a);
if (serialProc=0) then serialProc:=BugOS_findProcNam(a);
serialPort:=BVal(paramStr(2));
if (serialPort<1) then begin;
  f1:
  immErr('using: modemEmu.code <process1> <port1> <process2> <port2> [ignoreUnknownCommands]');
  end;
WriteLn('going to open '+a+' '+BStr(serialPort)+'...');
SerialOpen;
port1.linkName:=a+' port '+BStr(serialPort);
port1.dataPipe:=serialData;
port1.ctrlPipe:=serialCtrl;

a:=paramStr(3);
serialProc:=BVal(a);
if (serialProc=0) then serialProc:=BugOS_findProcNam(a);
serialPort:=BVal(paramStr(4));
if (serialPort<1) then goto f1;
WriteLn('going to open '+a+' '+BStr(serialPort)+'...');
SerialOpen;
port2.linkName:=a+' port '+BStr(serialPort);
port2.dataPipe:=serialData;
port2.ctrlPipe:=serialCtrl;

serialProc:=0;
serialPort:=0;
timer2start;
lastTest:=currentTime;

f2:
relequish;
serialSelectPort(port1);
modemCommandSign(port1);
modemCommandData(port1);
serialSendData(port1,false);
serialSelectPort(port2);
modemCommandSign(port2);
modemCommandData(port2);
serialSendData(port2,false);
timer2start;
if (GetTimePast(lastTest)<3) then goto f2;
lastTest:=currentTime;
if modemCommandConn(port1,port2) then goto f3;
if modemCommandConn(port2,port1) then goto f3;
goto f2;

f3:
if modemConnectConn(port1,port2) then goto f2;
relequish;
serialSelectPort(port1);
serialSendData(port1,false);
modemConnectData(port1,port2);
serialSelectPort(port2);
serialSendData(port2,false);
modemConnectData(port2,port1);
timer2start;
if (GetTimePast(lastTest)=0) then goto f3;
lastTest:=currentTime;
serialSelectPort(port1);
modemConnectSign(port1);
serialSelectPort(port2);
modemConnectSign(port2);
goto f3;
END.