{$heap 383k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}



Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Const sampleRate=44100;

{$include sinus.inc}
{$include dft.inc}
{$include fsk.inc}
{$include dtmf.inc}
{$include sound.inc}
{$include modem1.inc}
{$include modem2.inc}
{$include modem3.inc}



Function processParams:LongInt;
Var
  i,o,p:LongInt;
  a,b:String;
Begin;
a:=paramStr(1);
if (a='') then immErr('using: sm.code <process> [sqr] [prc<x>] [bps<x>] [dat<x>] [par<x>] [cll<x>] [ans] [org]');
soundOpen(a);
p:=0;
for i:=2 to paramCount do begin;
  b:=paramStr(i);
  a:=kicsi(copy(b,1,3));
  b:=copy(b,4,666);
  o:=BVal(b);
  if (a='sqr') then modemSquareOut:=1;
  if (a='prc') then modemPrecisity:=o;
  if (a='bps') then modemBitPerSec:=o;
  if (a='dat') then modemDataBit:=o;
  if (a='par') then modemParity:=o;
  if (a='ans') then p:=1;
  if (a='org') then p:=2;
  if (a<>'cll') then continue;
  Write('dialing '+b+'... ');
  if soundDial(b) then a:='successful!' else a:='failed!';
  WriteLn(a);
  end;
processParams:=p;
End;

Procedure doOneRound;
Begin;
relequish;
connectReleq;
upperReleq;
End;


Label f1;
Var i:LongInt;
BEGIN;
WriteLn(modemProggy+' '+modemVersion+', done by Mc at '#%date' '#%time'.');

upPipeData:=0;
upPipeCtrl:=0;
modemResetParam;
i:=processParams;
if (i<>0) then begin;
  connectStart(i=1);
  while (modemConnected>1) do doOneRound;
  modemRxSiz:=0;
  WriteLn('waiting for upper layer...');
  pipeLineBegListen;
  BugOS_SignDaemoning;
  while (upPipeData=0) and (modemConnected=1) do doOneRound;
  pipeLineEndListen;
  WriteLn('doing connection...');
  while (upPipeData<>0) and (modemConnected=1) do doOneRound;
  immErr('exiting...');
  end;

pipeLineBegListen;
BugOS_SignDaemoning;
f1:
doOneRound;
goto f1;
END.