{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

{$include serial.inc}

Var
  LineStatus:record
    overrun:LongInt;
    parity:LongInt;
    framing:LongInt;
    break:LongInt;
    current:LongInt;
    end;
  ModemStatus:record
    cts:LongInt;
    dsr:LongInt;
    rng:LongInt;
    dcd:LongInt;
    current:LongInt;
    end;
  ModemControl:LongInt;
  LineControl:record
    speedLow:LongInt;
    speedhigh:LongInt;
    length:LongInt;
    parity:LongInt;
    stop:LongInt;
    break:LongInt;
    end;
  FlowControl:LongInt;

Type
  OneCurrentRecord=record
    v:LongWord;
    n:String[15];
    end;
Const
  LineStatusCurrent:array[1..5] of OneCurrentRecord=(
  (v:$01;n:'overrun'),
  (v:$02;n:'parity'),
  (v:$04;n:'framing'),
  (v:$08;n:'break'),
  (v:0;n:'')
  );
  ModemStatusCurrent:array[1..5] of OneCurrentRecord=(
  (v:$01;n:'cts'),
  (v:$02;n:'dsr'),
  (v:$04;n:'ring'),
  (v:$08;n:'dcd'),
  (v:0;n:'')
  );
  ModemControlCurrent:array[1..3] of OneCurrentRecord=(
  (v:$01;n:'dtr'),
  (v:$02;n:'rts'),
  (v:0;n:'')
  );
  FlowControlCurrent:array[1..3] of OneCurrentRecord=(
  (v:$01;n:'rts-cts'),
  (v:$02;n:'dtr-dsr'),
  (v:0;n:'')
  );



Procedure ReadStuff(stat:Boolean);
Begin;
if stat then begin;
  serialBuff[1]:=0;serialCmd(1);
  move(serialBuff[2],LineStatus,sizeof(LineStatus));
  serialBuff[1]:=1;serialCmd(1);
  move(serialBuff[2],ModemStatus,sizeof(ModemStatus));
  end;
serialBuff[1]:=2;serialCmd(1);
move(serialBuff[2],ModemControl,sizeof(ModemControl));
serialBuff[1]:=4;serialCmd(1);
move(serialBuff[2],LineControl,sizeof(LineControl));
serialBuff[1]:=6;serialCmd(1);
move(serialBuff[2],FlowControl,sizeof(FlowControl));
End;

Procedure displayCurrent(v:LongInt;var stuff);
Label f1;
Var
  dat:array[1..1] of OneCurrentRecord absolute stuff;
  i:LongInt;
Begin;
Write(' $'+byte2hexType(v shr 24)+byte2hexType(v shr 16)+byte2hexType(v shr 8)+byte2hexType(v));
i:=0;
f1:
inc(i);
if (dat[i].v=0) then begin;
  WriteLn('');
  exit;
  end;
if (dat[i].v and v=0) then goto f1;
Write(' '+dat[i].n);
goto f1;
End;


Var
  a:String;
  i,o:LongInt;
BEGIN;
WriteLn('serial setup v1.0, done by Mc at '#%date' '#%time'.');
a:=paramStr(1);
serialProc:=BVal(a);
if (serialProc=0) then serialProc:=BugOS_findProcNam(a);
serialPort:=BVal(paramStr(2));
if (serialPort<1) then begin;
  WriteLn('using: serialSetup.code <process> <port> [options]');
  WriteLn('options: ');
  WriteLn('  b<speed>');
  WriteLn('  d<dataBits>');
  WriteLn('  s<stopBits>');
  WriteLn('  p<parity>');
  WriteLn('  m<modemControl>');
  WriteLn('  f<flowControl>');
  WriteLn('  r<break>');
  WriteLn('  u<speed>(d=8 s=0 p=1 m=3 f=0 r=0)');
  halt(1);
  end;
SerialOpen;

ReadStuff(false);

Write('changing:');
for i:=3 to paramCount do begin;
  a:=kicsi(ParamStr(i));
  o:=BVal(copy(a,2,255));
  case a[1] of
    'u':begin;
      LineControl.speedLow:=o;
      LineControl.speedHigh:=0;
      LineControl.length:=8;
      LineControl.parity:=0;
      LineControl.stop:=1;
      ModemControl:=3;
      FlowControl:=0;
      LineControl.break:=0;
      Write(' everything');
      end;
    'b':begin;
      LineControl.speedLow:=o;
      LineControl.speedHigh:=0;
      Write(' speed');
      end;
    'd':begin;
      LineControl.length:=o;
      Write(' databits');
      end;
    's':begin;
      LineControl.stop:=o;
      Write(' stopbits');
      end;
    'p':begin;
      LineControl.parity:=o;
      Write(' parity');
      end;
    'm':begin;
      ModemControl:=o;
      Write(' modemcontrol');
      end;
    'f':begin;
      FlowControl:=o;
      Write(' flowcontrol');
      end;
    'r':begin;
      LineControl.break:=o;
      Write(' break');
      end;
    end;
  end;
WriteLn('');

move(ModemControl,serialBuff[2],sizeof(ModemControl));
serialBuff[1]:=3;serialCmd(2);
move(LineControl,serialBuff[2],sizeof(LineControl));
serialBuff[1]:=5;serialCmd(7);
move(FlowControl,serialBuff[2],sizeof(FlowControl));
serialBuff[1]:=7;serialCmd(2);

ReadStuff(true);

WriteLn('line status:');
WriteLn('  overrun errors: '+BStr(LineStatus.overrun));
WriteLn('  parity errors: '+BStr(LineStatus.parity));
WriteLn('  framing errors: '+BStr(LineStatus.framing));
WriteLn('  breaks detected: '+BStr(LineStatus.break));
Write('  current:');
displayCurrent(LineStatus.current,LineStatusCurrent);

WriteLn('modem status:');
WriteLn('  cts changes: '+BStr(ModemStatus.cts));
WriteLn('  dsr changes: '+BStr(ModemStatus.dsr));
WriteLn('  ri changes:  '+BStr(ModemStatus.rng));
WriteLn('  dcd changes: '+BStr(ModemStatus.dcd));
Write('  current:');
displayCurrent(ModemStatus.current,ModemStatusCurrent);

WriteLn('line control:');
WriteLn('  speed: '+BStr(LineControl.speedLow));
WriteLn('  data bits: '+BStr(LineControl.length));
WriteLn('  parity: '+BStr(LineControl.parity));
WriteLn('  stop bits: '+BStr(LineControl.stop));
WriteLn('  break: '+BStr(LineControl.break));

Write('modem control:');
displayCurrent(ModemControl,ModemControlCurrent);

Write('flow control:');
displayCurrent(FlowControl,FlowControlCurrent);

END.