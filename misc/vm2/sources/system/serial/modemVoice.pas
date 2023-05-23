{$heap 191k}
{$stack 7k}
{$sysinc system.inc}
{$sysinc controlChars.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc crt.inc}
{$sysinc datetime.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc random.inc}

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
{$include serial.inc}
{$include modem.inc}
{$include voice.inc}
{$include modemVoice.inc}


Var
  a,b:String;
  i,o:LongInt;
BEGIN;
WriteLn('modem voices v1.0, done by Mc at '#%date' '#%time'.');
Randomize;
a:=paramStr(1);
serialProc:=BVal(a);
if (serialProc=0) then serialProc:=BugOS_findProcNam(a);
serialPort:=BVal(paramStr(2));
VoiceCommandSet:=BVal(paramStr(3));
a:=ParamStr(4);
b:='';
for i:=5 to paramCount do b:=b+' '+paramStr(i);
b:=copy(b,2,255);
if (a='') then begin;
  WriteLn('using: modemVoice.code <process> <port> <modem> <script> [parameters]');
  WriteLn('modem: 0=data, 1=rockwell, 2=zoltrix');
  Halt(2);
  end;
SerialOpen;

WorkingDirectory:='.\';
WorkingExtension:='.voc';
VoiceDeviceSel:=0;
LastConnectStr:='';
LastStatus:=0;
LogFileName:='';
fillchar(ScriptVariableDat,sizeof(ScriptVariableDat),0);
ModemLastReadLn:=b;
ClearDetectModem;
debugScriptCmd:=False;
ModemShow:=False;
ScriptTimer:=0;
fillchar(ScriptFile,sizeof(ScriptFile),0);

BugOS_SignDaemoning;

RunOneScript(a);
END.