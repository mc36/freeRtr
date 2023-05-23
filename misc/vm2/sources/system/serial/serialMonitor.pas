{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc filesys.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

{$include \sources\internet\kernel\utils\timer2.inc}
{$include serial.inc}

Type
  portStateRecord=record
    dataPipe:LongInt;           {data pipeline}
    ctrlPipe:LongInt;           {control pipeline}
    linkName:String;            {name of link}
    portNum:LongInt;            {port number}
    lastSign:LongInt;           {last signal status}
    txSiz:LongInt;              {tx buffer size}
    txBuf:array[1..1024] of char;               {tx buffer}
    end;
Var logFile:xFile;

Procedure add2log(port,size:LongInt;var buffer);
Var buf:array[1..2048] of byte;
Begin;
buf[1]:=port;
move(size,buf[2],sizeof(size));
move(currentTime,buf[6],sizeof(currentTime));
move(buffer,buf[10],size);
xBlockWrite(logFile,buf,size+9);
End;



Procedure serialSelectPort(var d:portStateRecord);
Begin;
serialData:=d.dataPipe;
serialCtrl:=d.ctrlPipe;
End;

Procedure serialSetSignals(i:LongInt);
Begin;
serialBuff[1]:=3;
serialBuff[2]:=i;
serialCmd(2);
End;

Function serialGetSignals:LongInt;
Var i:LongInt;
Begin;
serialBuff[1]:=1;
serialCmd(1);
i:=0;
if (serialBuff[6] and 1<>0) and (serialBuff[2]=0) then inc(i,2);
if (serialBuff[6] and 2<>0) and (serialBuff[3]=0) then inc(i,1);
if (serialBuff[6] and 4<>0) and (serialBuff[4]=0) then inc(i,8);
if (serialBuff[6] and 8<>0) and (serialBuff[5]=0) then inc(i,4);
serialGetSignals:=i;
End;

Procedure serialSendData(var d:portStateRecord);
Label f1;
Begin;
if (d.txSiz<1) then goto f1;
if (pipeLineSend(serialData,d.txBuf,d.txSiz)<>0) then exit;
f1:
d.txSiz:=0;
End;

Procedure serialRecvData(var d:portStateRecord);
Label f1;
Var
  buf:array[1..128] of char;
  i,o:LongInt;
Begin;
i:=sizeof(d.txBuf)-d.txSiz;
if (i<16) then exit;
if (pipeLineRecv(serialData,d.txBuf[d.txSiz+1],i)<>0) then i:=0;
if (i<1) then exit;
add2log(d.portNum,i,d.txBuf[d.txSiz+1]);
inc(d.txSiz,i);
End;


Label f1,f2;
Var
  port1,port2:portStateRecord;
  lastTest:LongInt;
  a:String;
  i,o:LongInt;
BEGIN;
WriteLn('serial monitor v1.0, done by Mc at '#%date' '#%time'.');
timer2start;

fillchar(port1,sizeof(port1),0);
port1.lastSign:=-1;
port2:=port1;

a:=paramStr(1);
serialProc:=BVal(a);
if (serialProc=0) then serialProc:=BugOS_findProcNam(a);
serialPort:=BVal(paramStr(2));
if (serialPort<1) then begin;
  f1:
  immErr('using: sermon.code <process1> <port1> <process2> <port2> [logfile]');
  end;
WriteLn('going to open '+a+' '+BStr(serialPort)+'...');
SerialOpen;
port1.linkName:=a+' port '+BStr(serialPort);
port1.dataPipe:=serialData;
port1.ctrlPipe:=serialCtrl;
port1.portNum:=0;

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
port2.portNum:=1;

a:=paramStr(5);
if (xCreate(a)<>0) then a:='';
if (xOpen(logFile,a,xGenFilMod_rw)=0) then begin;
  writeLn('logging to '+a+'...');
  end else begin;
  fillchar(logFile,sizeof(logFile),0);
  writeLn('logging disabled.');
  end;

serialProc:=0;
serialPort:=0;
lastTest:=0;
timer2start;
i:=2;
xBlockWrite(logFile,i,sizeof(i));
add2log(0,length(port1.linkName),port1.linkName[1]);
add2log(1,length(port2.linkName),port2.linkName[1]);

f2:
relequish;
timer2start;
serialSelectPort(port1);
serialSendData(port1);
serialRecvData(port2);
serialSelectPort(port2);
serialSendData(port2);
serialRecvData(port1);
if (GetTimePast(lastTest)<1) then goto f2;
lastTest:=currentTime;
serialSelectPort(port1);
i:=serialGetSignals;
if (i<>port1.lastSign) then begin;
  port1.lastSign:=i;
  add2log($40,sizeof(i),i);
  serialSelectPort(port2);
  serialSetSignals(i);
  end;
serialSelectPort(port2);
i:=serialGetSignals;
if (i<>port2.lastSign) then begin;
  port2.lastSign:=i;
  add2log($41,sizeof(i),i);
  serialSelectPort(port1);
  serialSetSignals(i);
  end;
goto f2;
END.