{$heap 31k}
{$stack 1k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
{$sysinc param.inc}
{$sysinc hex.inc}
{$sysinc crt.inc}
{$sysinc bugos.inc}

Type
  OneCardRecord=record
    vendor:LongInt;
    device:LongInt;
    ioaddr:LongInt;
    memaddr:LongInt;
    cmd:String;
    end;
Var
  cardDat:array[1..16] of OneCardRecord;
  cardNum:LongInt;
  ethParam:String;
  ipAddr:String;
  gateAddr:String;
  netmask:String;
  dnsAddr:String;
  temp:String;

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Function convHex(i:LongInt):String;
Var a:String;
Begin;
a:='';
repeat
  a:=byte2hextype(i)+a;
  i:=i shr 8;
  until (i=0);
convHex:='$'+a;
End;

Procedure captureRun(a:String);
Var
  i:LongInt;
  w:Word;
Begin;
xErase(temp);
i:=xExec('\system\process\capturerun.code',temp+' '+a,w);
if (i or w=0) then exit;
xErase(temp);
immErr('error running '+a+'!');
End;

Procedure simpleRun(a,b:String);
Var
  i:LongInt;
  w:Word;
Begin;
i:=xExec(a,b,w);
if (i or w<>0) then immErr('error running '+a+'!');
End;

Procedure processKiller(a:String);
Label f1;
Var i:LongInt;
Begin;
f1:
i:=BugOS_findProcNam(a);
if (i=0) then exit;
BugOS_KillProcess(i);
goto f1;
End;

Procedure unComment(var a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
Begin;
o:=666;
for i:=ab0 downto 1 do if (ab[i]=59) then o:=i;
a:=copy(a,1,o-1);
while (ab0>0) and (ab[ab0]=32) do dec(ab0);
while (ab0>0) and (ab[1]=32) do a:=copy(a,2,666);
End;

Function getNextWord(var a:String):String;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
Begin;
unComment(a);
o:=666;
for i:=ab0 downto 1 do if (ab[i]=32) then o:=i;
getNextWord:=copy(a,1,o-1);
a:=copy(a,o+1,666);
unComment(a);
End;

Function getParamWord(var a:String;b:String):String;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
Begin;
getParamWord:='';
i:=pos(kicsi(b),kicsi(a));
if (i<1) then exit;
b:=copy(a,1,i-1);
a:=copy(a,i,666);
i:=pos(':',a);
a:=copy(a,i+1,666);
o:=666;
for i:=1 to pos(':',a) do if (ab[i]=32) then o:=i;
getParamWord:=copy(a,1,o-1);
a:=b+copy(a,o+1,666);
End;

Function ReadLine:String;
Label f0,f1;
Var
  a:String;
  w:Word;
Begin;
f0:
a:='';
Write(#13'>');
f1:
w:=ReadKey;
if (w and $fe00=0) then begin;{simple key}
  w:=w and $ff;
  if (w in [0,255,13,10,8,9]) then w:=ord(' ');
  if (length(a)>250) then goto f1;
  a:=a+chr(w);
  write(chr(w));
  goto f1;
  end;
case w of
  $8001:begin;{redraw}
    clrscr;
    goto f0;
    end;
  $8003:begin;{backspace}
    if (a='') then goto f1;
    Write(#8' '#8);
    a:=copy(a,1,length(a)-1);
    goto f1;
    end;
  $8004:begin;{enter}
    WriteLn('');
    ReadLine:=a;
    exit;
    end;
  $8005:begin;{escape}
    WriteLn('');
    goto f0;
    end;
  end;
goto f1;
End;

Procedure writeQuest(a:String);
Begin;
textColor($0f);
Write(a);
textColor($07);
WriteLn('');
End;


Function readUpOneCard(var t:xtText;var d:OneCardRecord):Boolean;
Label f1;
Var
  a,b:String;
  i,o:LongInt;
Begin;
readUpOneCard:=false;
fillchar(d,sizeof(d),0);
f1:
if xtEOF(t) then exit;
b:=kicsi(xtReadLn(t,666));
if (copy(b,1,7)='-------') then exit;
a:=getParamWord(b,'class');
getNextWord(a);
if (getNextWord(a)='network') then begin;
  a:=getParamWord(b,'subclass');
  getNextWord(a);
  readUpOneCard:=(getNextWord(a)='ethernet');
  end;
a:=getParamWord(b,'vendorid');
if (a<>'') then d.vendor:=BVal(getNextWord(a));
a:=getParamWord(b,'deviceid');
if (a<>'') then d.device:=BVal(getNextWord(a));
i:=pos('io=',b);
if (i<>0) then begin;
  a:=copy(b,i+3,666);
  d.ioaddr:=BVal(getNextWord(a));
  end;
i:=pos('mem=',b);
if (i<>0) then begin;
  a:=copy(b,i+4,666);
  d.memaddr:=BVal(getNextWord(a));
  end;
goto f1;
End;



Procedure completeOneCard(var t:xtText;var d:OneCardRecord);
Label f1,f2;
Var
  a:String;
  i,o:LongInt;
Begin;
xtSetPos(t,0);
d.cmd:='';
f1:
if xtEOF(t) then exit;
a:=xtReadLn(t,666);
unComment(a);
if (BVal(getNextWord(a))<>d.vendor) then goto f1;
if (BVal(getNextWord(a))<>d.device) then goto f1;
d.cmd:=getNextWord(a);
kicserel('io=*','io='+convHex(d.ioaddr),a);
kicserel('mem=*','mem='+convHex(d.memaddr),a);
d.cmd:=d.cmd+' '+a;
End;

Procedure checkForCard;
Var
  a:String;
  t:xtText;
  i,o:LongInt;
  d:OneCardRecord;
Begin;
captureRun('\system\drivers\system\pci_devices.code');
cardNum:=0;
if (xtOpen(t,temp,true)<>0) then immErr('error opening result!');
while not xtEOF(t) do if readUpOneCard(t,d) then begin;
  inc(cardNum);
  cardDat[cardNum]:=d;
  end;
xtClose(t);
a:=GetMyFullFileName;
a:=xFileName(a,1)+xFileName(a,2)+'.list';
if (xtOpen(t,a,true)<>0) then immErr('error opning listing!');
for i:=1 to cardNum do completeOneCard(t,cardDat[i]);
xtClose(t);
o:=0;
for i:=1 to cardNum do if (cardDat[i].cmd<>'') then begin;
  inc(o);
  cardDat[o]:=cardDat[i];
  end;
cardNum:=o;
writeQuest('found '+BStr(cardNum)+' cards:');
for i:=1 to cardNum do WriteLn(BStr(i)+': '+cardDat[i].cmd);
if (cardNum=1) then exit;
if (cardNum<1) then begin;
  writeQuest('please enter command line to use:');
  repeat a:=readLine; until (a<>'');
  cardDat[1].cmd:=a;
  cardNum:=1;
  exit;
  end;
writeQuest('please enter which to use:');
repeat i:=BVal(readLine); until (i>=1) and (i<=cardNum);
d:=cardDat[1];
cardDat[1]:=cardDat[i];
cardDat[i]:=d;
End;

Procedure WriteScript(var t:xtText;p:LongInt);
Begin;
if (p and $01<>0) then xtWriteLn(t,'\system\drivers\netcards\'+cardDat[1].cmd);
if (p and $02<>0) then xtWriteLn(t,'\internet\kernel\ip4.code');
if (p and $04<>0) then xtWriteLn(t,'\internet\kernel\eth4.code '+ethParam);
if (p and $08<>0) then xtWriteLn(t,'\internet\kernel\ip4conf.code iface add eth4.code eth0');
if (p and $10<>0) then xtWriteLn(t,'\internet\kernel\ip4conf.code route add 1 '+ipAddr+' '+netmask+' '+ipAddr+' '+netmask+' 0.0.0.0');
if (p and $20<>0) then xtWriteLn(t,'\internet\kernel\ip4conf.code route add 2 '+ipAddr+' '+netmask+' 0.0.0.0 0.0.0.0 '+gateAddr);
if (p and $40<>0) then xtWriteLn(t,'\internet\kernel\tcp.code ip4.code 4096');
if (p and $80<>0) then xtWriteLn(t,'\internet\kernel\dns.code 512 '+dnsAddr);
End;

Procedure configureCard;
Var
  a,b:String;
  i,o:LongInt;
  t:xtText;
Begin;
WriteLn('');
writeQuest('going to use '+cardDat[1].cmd+'...');
WriteLn('');
WriteLn('1-static address');
WriteLn('2-dhcp');
WriteLn('3-bootp');
writeQuest('please enter which to use:');
repeat i:=BVal(readLine); until (i>=1) and (i<=3);
case i of
  1:begin;
    writeQuest('enter my ip address:');
    repeat a:=readLine; until (a<>'');
    ethParam:='user '+a;
    writeQuest('enter gateway ip address:');
    repeat a:=readLine; until (a<>'');
    ethParam:=ethParam+' '+a;
    writeQuest('enter netmask address:');
    repeat a:=readLine; until (a<>'');
    ethParam:=ethParam+' '+a;
    writeQuest('enter dns address(es):');
    repeat a:=readLine; until (a<>'');
    ethParam:=ethParam+' '+a;
    end;
  2:ethParam:='dhcp';
  3:ethParam:='bootp';
  else immErr('bug!');
  end;
a:=cardDat[1].cmd;
i:=pos(' ',a);
if (i<1) then i:=666;
a:=copy(a,1,i-1);
ethParam:=a+' '+ethParam;
xErase(temp);
xCreate(temp);
if (xtOpen(t,temp,false)<>0) then immErr('error opening target!');
WriteScript(t,5);
xtClose(t);
writeQuest('starting ethernet interface...');
processKiller('eth4.code');
processKiller('ip4.code');
processKiller('eth6.code');
processKiller('ip6.code');
processKiller('tcp.code');
processKiller('dns.code');
processKiller(a);
simpleRun('\system\drivers\console\sysload.code',temp);
writeQuest('getting interface information...');
captureRun('\internet\kernel\ip4iface.code eth4.code');
if (xtOpen(t,temp,true)<>0) then immErr('error opening result!');
ipAddr:='';
gateAddr:='';
netmask:='';
dnsAddr:='';
while not xtEOF(t) do begin;
  b:=kicsi(xtReadLn(t,666));
  unComment(b);
  i:=pos(':',b);
  a:=copy(b,1,i-1);
  b:=copy(b,i+1,666);
  unComment(b);
  if (a='ip address') then ipAddr:=b;
  if (a='gateway ip') then gateAddr:=b;
  if (a='netmask') then netmask:=b;
  if (a='dns') then dnsAddr:=b;
  end;
xtClose(t);
xErase(temp);
writeQuest('generating script...');
xErase(temp);
xCreate(temp);
if (xtOpen(t,temp,false)<>0) then immErr('error opening file!');
WriteScript(t,$ffffffff);
xtClose(t);
End;




BEGIN;
WriteLn('network configurator v1.0, done by Mc at '#%date' '#%time'.');
temp:=GetAllParameters;
if (temp='') then temp:='networkConfig.sysload';
checkForCard;
configureCard;
WriteLn('successful!');
Halt(0);
END.