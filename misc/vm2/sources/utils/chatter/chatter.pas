{$heap 700k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc crt.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$sysinc inet_dns.inc}

{$include window.inc}
{$include connect.inc}

Label f1;
Var
  i,o:LongInt;
  a,b:String;
BEGIN;
WriteLn('irc client v1.0, done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');
if DNSstartResolver then immErr('failed to find dns process!');

b:=GetAllParameters;
a:=getWord(b);
i:=BVal(getWord(b));
if (b='') then immErr('using: irc.code <host> <port> <nick> [realname]');
if openConnect(a,i) then halt(2);

a:=getWord(b);
if (b='') then b:=a;
sendMessage('nick '+a);
sendMessage('user '+a+' 0 * :'+b);
myNick:=a;

fillchar(wins,sizeof(wins),0);
for i:=1 to numberOfWins do begin;
  wins[i].number:=i;
  wins[i].disp:=windowMaxLines;
  end;
wins[1].channel:='system messages';
shouldFresh:=$ff;
currentWin:=1;
doRefresh;

f1:
relequish;
relequish2connect;
doRefresh;
while keypressed do if windowGotKey(wins[currentWin],readKey) then userMessage(wins[currentWin]);
goto f1;
END.