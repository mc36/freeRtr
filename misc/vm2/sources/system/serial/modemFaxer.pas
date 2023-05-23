{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc controlChars.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc crt.inc}
{$sysinc memory.inc}
{$sysinc datetime.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Procedure clrEol;
Begin;
Write(#13'                                                                    '#13);
End;

{$include \sources\internet\kernel\utils\timer2.inc}
{$include modemFaxer.inc}
{$include serial.inc}
{$include modem.inc}
{$include fcs16.inc}
{$include faxclass.inc}
{$include faxclass1.inc}
{$include faxclass2.inc}
{$include faxclass20.inc}

Var
  a:String;
  i,o,p:LongInt;
  mode,class:Byte;
BEGIN;
WriteLn('modem faxer v1.0, done by Mc at '#%date' '#%time'.');
a:=paramStr(1);
serialProc:=BVal(a);
if (serialProc=0) then serialProc:=BugOS_findProcNam(a);
serialPort:=BVal(paramStr(2));
if (serialPort<1) then begin;
  WriteLn('using: modemFaxer.code <process> <port> [options]');
  WriteLn('options:');
  WriteLn('  c<command> - command: s=send, r=receive');
  WriteLn('  f<file>    - file to process');
  WriteLn('  l<logfile> - log file to append');
  WriteLn('  m<mode>    - 1=fclass1, 2=fclass2, 3=fclass2.0');
  WriteLn('  i<phone>   - local identifier');
  WriteLn('  s<speed>   - fax send/receive speed');
  WriteLn('  p<phone>   - phone number to dial');
  WriteLn('  d<debug>   - bit0=modem, bit1=packets');
  Halt(1);
  end;
mode:=0;
LocalIdentifier:='';
phoneNumber:='';
filNam:='';
logNam:='';
FaxSpeedLimit:=128;
ModemShow:=False;
hidePackets:=True;
for i:=3 to paramCount do begin;
  a:=paramStr(i);
  case LowCase(a[1]) of
    'c':case LowCase(a[2]) of
      'r':mode:=1;
      's':mode:=2;
      end;
    's':begin;
      p:=BVal(copy(a,2,255));
      FaxSpeedLimit:=128;
      for o:=0 to MaxFaxSpeeds do if (p=FaxSpeeds[o]) then FaxSpeedLimit:=o;
      end;
    'd':begin;
      p:=BVal(copy(a,2,255));
      ModemShow:=(p and 1<>0);
      hidePackets:=(p and 2=0);
      end;
    'f':filNam:=copy(a,2,255);
    'l':logNam:=copy(a,2,255);
    'm':class:=BVal(copy(a,2,255));
    'i':LocalIdentifier:=copy(a,2,255);
    'p':phoneNumber:=copy(a,2,255);
    end;
  end;
if (FaxSpeedLimit>MaxFaxSpeeds) then immErr('invalid fax speed!');
if (mode<1) or (mode>2) then immErr('invalid command!');
if (filNam='') then immErr('file not specified!');

for i:=0 to 255 do begin;
  bitCountTable[i]:=CountBitsInByte(i);
  bitReverseTable[i]:=ReverseBits(i);
  end;
CreateFCS16Table;

SerialOpen;

case mode of
  1:a:='receive to';
  2:a:='send to '+phoneNumber+' from';
  else a:='?';
  end;
write2logS('going to '+a+' '+filNam+'...');
case class of
  1:a:='1';
  2:a:='2';
  3:a:='2.0';
  else a:='?';
  end;
write2logP('speed: '+BStr(FaxSpeeds[FaxSpeedLimit])+' protocol: '+FaxProtos[FaxSpeedLimit]+' commandset: '+a+' localID:'+LocalIdentifier+'...');

case class of
  1:i:=FaxClass1init;
  2:i:=FaxClass2init;
  3:i:=FaxClass20init;
  else i:=1;
  end;
if (i<>0) then begin;
  write2logS('error initializing modem!');
  halt(1);
  end;

xCreate(filNam);
filPos:=0;
filPag:=0;
if (mode=1) then begin;
  if (xOpen(filHdr,filNam,xGenFilMod_rw)<>0) then immErr('error opening file!');
  filSiz:=xFileSize(filhdr);
  if (filSiz<>0) then immErr('file not empty!');
  case class of
    1:i:=FaxClass1recv;
    2:i:=FaxClass2recv;
    3:i:=FaxClass20recv;
    else i:=1;
    end;
  end else begin;
  if (xOpen(filHdr,filNam,xGenFilMod_r)<>0) then immErr('error opening file!');
  filSiz:=xFileSize(filhdr);
  if (filSiz=0) then immErr('file empty!');
  case class of
    1:i:=FaxClass1send;
    2:i:=FaxClass2send;
    3:i:=FaxClass20send;
    else i:=1;
    end;
  end;
xClose(filHdr);
if (i=0) then a:='successful' else a:='error';
write2logS('result: '+a+'  pages:'+BStr(filPag)+'  bytes: '+BStr(filPos));
halt(i);
END.