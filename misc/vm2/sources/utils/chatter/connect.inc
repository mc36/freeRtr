Var
  lineGood:Boolean;
  lineNick:String;
  lineUser:String;
  lineHost:String;
  lineCmd:String;
  lineParam:String;
  linePosit:Byte; {1-first, 2-nick, 3-user, 4-host, 5-cmd, 6-param}
  pipe:LongInt;





Procedure clearLine;
Begin;
linePosit:=1;
lineNick:='';
lineUser:='';
lineHost:='';
lineCmd:='';
lineParam:='';
lineGood:=false;
End;

Function openConnect(a:String;p:LongInt):Boolean;
Var
  b:String;
  i,o:LongInt;
Begin;
openConnect:=True;
clearLine;
pipe:=0;
Write('resolving '+a+'...');
DNSresolvePut(1,a);
while (1=1) do begin;
  i:=DNSresolveGet(a,b);
  if (i=0) then begin; relequish;continue; end;
  if (i and $80=0) then break;
  WriteLn(' failed!');
  exit;
  end;
WriteLn(' ok!');
Write('connecting to '+ipAddr2string(b)+' '+BStr(p)+'...');
TCPbeginConnect(pipe,4096,b,p);
while TCPlookConnected(pipe,a,i,o) do begin;
  relequish;
  if (pipe<>0) then continue;
  WriteLn(' failed!');
  exit;
  end;
WriteLn(' ok!');
WriteLn('local side is '+ipAddr2string(a)+' '+BStr(i)+'...');
openConnect:=False;
End;


Procedure sendMessage(a:String);
Var
  buf:array[1..1024] of byte;
  i:LongInt;
Begin;
i:=length(a);
if (i<1) then exit;
move(a[1],buf,i);
inc(i);buf[i]:=13;
inc(i);buf[i]:=10;
pipeLineSend(pipe,buf,i);
End;



Procedure gotMessage;
Label vege;
Var
  a,b:String;
  i,o,p:LongInt;
Begin;
lineCmd:=kicsi(lineCmd);
if (lineCmd='') then goto vege;
if (lineCmd='quit') then begin;
  broadcastMessage(lineNick+' exited'#13+copy(lineParam,2,666));
  goto vege;
  end;
if (lineCmd='join') then begin;
  unicastMessage(lineParam,lineNick+' joined'#13);
  goto vege;
  end;
if (lineCmd='part') then begin;
  a:=getWord(lineParam);
  eatColon(lineParam);
  unicastMessage(a,lineNick+' leaved'#13+lineParam);
  goto vege;
  end;
if (lineCmd='mode') then begin;
  a:=getWord(lineParam);
  unicastMessage(a,lineNick+' changed mode'#13+lineParam);
  goto vege;
  end;
if (lineCmd='topic') then begin;
  a:=getWord(lineParam);
  unicastMessage(a,lineNick+' set topic'#13+lineParam);
  goto vege;
  end;
if (lineCmd='invite') then begin;
  a:=getWord(lineParam);
  unicastMessage(lineParam,lineNick+' invited '+a+' to '+lineParam+#13);
  goto vege;
  end;
if (lineCmd='kick') then begin;
  a:=getWord(lineParam);
  unicastMessage(a,lineNick+' kicked '+lineParam+' from '+a+#13);
  goto vege;
  end;
if (lineCmd='privmsg') or (lineCmd='notice') then begin;
  a:=getWord(lineParam);
  eatColon(lineParam);
  i:=FindOneWindow(a);
  if (i<=1) then i:=FindOneWindow(lineNick);
  windowAddLin(wins[i],lineNick+#13+lineParam);
  goto vege;
  end;
if (lineCmd='ping') then begin;
  eatColon(lineParam);
  sendMessage('pong '+lineParam);
  windowAddLin(wins[1],'ping from'#13+lineParam);
  goto vege;
  end;
if (lineCmd='pong') then begin;
  eatColon(lineParam);
  windowAddLin(wins[1],'pong from'#13+lineParam);
  goto vege;
  end;
if (lineCmd='error') then begin;
  eatColon(lineParam);
  windowAddLin(wins[1],'error'#13+lineParam);
  goto vege;
  end;
i:=BVal(lineCmd);
if (i in [1,2,3,4,5,351,371,374,375,372,376,381,382,383,391,211,212,219,421,422,423,424,431,433]) then begin;
  windowAddLin(wins[1],'info'#13+lineParam);
  goto vege;
  end;
if (i in [242,243,234,235,251,252,253,254,255,256,257,258,259,263,402,409,411,412,413,414,415,436]) then begin;
  windowAddLin(wins[1],'info'#13+lineParam);
  goto vege;
  end;
if (i in [445,446,451,461,462,463,464,465,466,483,484,485,491,501,502]) then begin;
  windowAddLin(wins[1],'info'#13+lineParam);
  goto vege;
  end;
if (i in [200,201,202,203,204,205,206,207,208,209,210,261,262]) then begin;
  windowAddLin(wins[1],'trace'#13+lineParam);
  goto vege;
  end;
if (i in [392,393,394,395]) then begin;
  windowAddLin(wins[1],'user'#13+lineParam);
  goto vege;
  end;
if (i in [305,306]) then begin;
  windowAddLin(wins[1],'away'#13+lineParam);
  goto vege;
  end;
if (i=302) then begin;
  windowAddLin(wins[1],'userhost'#13+lineParam);
  goto vege;
  end;
if (i=303) then begin;
  windowAddLin(wins[1],'ison'#13+lineParam);
  goto vege;
  end;
if (i=301) then begin;
  a:=getWord(lineParam);
  eatColon(lineParam);
  unicastMessage(a,a+' away'#13+lineParam);
  goto vege;
  end;
if (i in [311,312,313,317,318,319,314,369,401,406,407,408,432,441,443,444]) then begin;
  a:=getWord(lineParam);
  eatColon(lineParam);
  unicastMessage(a,a+' user'#13+lineParam);
  goto vege;
  end;
if (i in [322,323,324,325,403,404,405,437,442,467,471,472,473,474,475,476,477,478,481,482]) then begin;
  a:=getWord(lineParam);
  eatColon(lineParam);
  unicastMessage(a,a+' channel'#13+lineParam);
  goto vege;
  end;
if (i in [331,332]) then begin;
  a:=getWord(lineParam);
  eatColon(lineParam);
  unicastMessage(a,a+' topic'#13+lineParam);
  goto vege;
  end;
if (i in [341,346,347,348,349]) then begin;
  a:=getWord(lineParam);
  unicastMessage(a,'invited'#13+lineParam);
  goto vege;
  end;
if (i in [367,368]) then begin;
  a:=getWord(lineParam);
  unicastMessage(a,'banned'#13+lineParam);
  goto vege;
  end;
if (i=342) then begin;
  windowAddLin(wins[1],'summon'#13+lineParam);
  goto vege;
  end;
if (i in [364,365]) then begin;
  windowAddLin(wins[1],'links'#13+lineParam);
  goto vege;
  end;
if (i in [352,315,353,366]) then begin;
  a:=getWord(lineParam);
  unicastMessage(a,'who'#13+lineParam);
  goto vege;
  end;
if (i=221) then begin;
  broadcastMessage('mode'#13+lineParam);
  goto vege;
  end;


windowAddLin(wins[1],'unknown message'#13+lineNick+' '+lineCmd+' '+lineParam);
vege:
clearLine;
End;



Procedure gotOneChar(i:LongInt);
Label f1;

Procedure addChar(var a:String;i:LongInt);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
if (ab0>=255) then exit;
inc(ab0);
ab[ab0]:=i;
End;

Begin;
if lineGood then exit;
if (i in [13,10]) then begin; lineGood:=true;gotMessage;exit; end;
if (i in [0,9,255]) then i:=$20;
if (i<$20) then i:=$3f;
f1:
case linePosit of
  1:begin; {first byte}
    if (i=$3a) then begin;
      linePosit:=2;
      exit;
      end;
    linePosit:=5;
    goto f1;
    end;
  2:case i of {nickname}
    $21:linePosit:=3;
    $40:linePosit:=4;
    $20:linePosit:=5;
    else addChar(lineNick,i);
    end;
  3:case i of {username}
    $40:linePosit:=4;
    $20:linePosit:=5;
    else addChar(lineUser,i);
    end;
  4:case i of {hostname}
    $20:linePosit:=5;
    else addChar(lineHost,i);
    end;
  5:case i of {command}
    $20:linePosit:=6;
    else addChar(lineCmd,i);
    end;
  6:addChar(lineParam,i);
  end;
End;



Procedure relequish2connect;
Var
  i,o,p:LongInt;
  buf:array[1..1024] of byte;
Begin;
o:=sizeof(buf);
if (pipeLineRecv(pipe,buf,o)<>0) then o:=0;
if (o<1) then begin;
  pipeLineStats(pipe,o,i,i);
  if (o<>0) then exit;
  textColor(7);
  gotoXY(1,screenSizY);
  WriteLn('');
  halt(0);
  end;
for i:=1 to o do gotOneChar(buf[i]);
End;


Procedure userMessage(var d:OneWindowRecord);
Var
  a,b:String;
  i,o:LongInt;
Begin;
b:=d.input;
d.input:='';
windowArrange(d);
if (b='') then exit;
shouldFresh:=shouldFresh or $01;
if (currentWin>1) then begin;
  if (copy(b,1,1)<>'/') then begin;
    if (d.channel='') then begin; windowAddLin(d,'not joined!'#13); exit; end;
    sendMessage('privmsg '+d.channel+' :'+b);
    windowAddLin(d,myNick+#13+b);
    exit;
    end;
  a:=kicsi(copy(getWord(b),2,666));
  if (a='help') or (a='?') then begin;
    windowAddLin(d,'commands:'#13);
    windowAddLin(d,'join #channel/user');
    windowAddLin(d,'part');
    windowAddLin(d,'mode params');
    windowAddLin(d,'topic text');
    windowAddLin(d,'names');
    windowAddLin(d,'invite user');
    windowAddLin(d,'kick user');
    exit;
    end;
  if (a='join') then begin;
    if (d.channel<>'') then begin; windowAddLin(d,'already joined!'#13); exit; end;
    if (b='') then begin; windowAddLin(d,'missing channel/user name'#13); exit; end;
    d.channel:=b;
    if (copy(b,1,1)='#') then begin; sendMessage('join '+b);exit; end;
    sendMessage('whois '+b);
    sendMessage('whowas '+b);
    sendMessage('userhost '+b);
    sendMessage('ison '+b);
    exit;
    end;
  if (d.channel='') then begin; windowAddLin(d,'not joined!'#13); exit; end;
  if (a='part') then begin;
    a:=d.channel;
    d.channel:='';
    if (copy(a,1,1)='#') then sendMessage('part '+a);
    exit;
    end;
  if (a='mode') then begin;
    sendMessage('mode '+d.channel+' '+b);
    exit;
    end;
  if (a='topic') then begin;
    sendMessage('topic '+d.channel+' :'+b);
    exit;
    end;
  if (a='names') then begin;
    sendMessage('names '+d.channel+' '+b);
    exit;
    end;
  if (a='invite') then begin;
    sendMessage('invite '+b+' '+d.channel);
    exit;
    end;
  if (a='kick') then begin;
    sendMessage('kick '+d.channel+' '+b);
    exit;
    end;

  windowAddLin(d,'unknown command:'#13+a+' '+b);
  exit;
  end;
if (copy(b,1,1)='/') then b:=copy(b,2,666);
a:=kicsi(getWord(b));
if (a='quit') then begin;
  sendMessage('quit '+b);
  exit;
  end;
if (a='help') or (a='?') then begin;
  windowAddLin(d,'commands'#13);
  windowAddLin(d,'quit');
  windowAddLin(d,'whois user');
  windowAddLin(d,'whowas user');
  windowAddLin(d,'mode modes');
  windowAddLin(d,'msg user message');
  windowAddLin(d,'list');
  windowAddLin(d,'motd');
  windowAddLin(d,'lusers');
  windowAddLin(d,'version');
  windowAddLin(d,'stats');
  windowAddLin(d,'links');
  windowAddLin(d,'time');
  windowAddLin(d,'trace');
  windowAddLin(d,'info');
  windowAddLin(d,'servlist');
  windowAddLin(d,'command irccommand');
  exit;
  end;
if (a='msg') then begin;
  a:=getWord(b);
  sendMessage('privmsg '+a+' :'+b);
  exit;
  end;
if (a='whois') or (a='whowas') or (a='mode') or (a='list') or (a='motd') or (a='lusers') or (a='version') or (a='stats') or (a='links') or (a='time') or (a='trace') or (a='info') or (a='servlist') then begin;
  if (b<>'') then b:=' '+b;
  sendMessage(a+b);
  exit;
  end;
if (a='command') then begin;
  sendMessage(b);
  exit;
  end;

windowAddLin(d,'unknown command:'#13+a+' '+b);
End;
