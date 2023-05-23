Const
  channelsMax=256;              {maximum number of channels}
  channelUser=16;               {channels allowed for user}
  proggyName='irc server v1.0';
  proggyDate=#%date' '#%time;
Type
  OneChannelRecord=record
    name:String;                {name of channel}
    topic:String;               {topic of channel}
    users:LongInt;              {users on this channel}
    end;
  OneConnectionRecord=record
    pipe:LongInt;               {pipeline id of control connection}
    time:LongInt;               {time of last action}
    timeR:LongInt;              {time of last receive}
    adr:OneTCPaddressRecord;    {remote address}
    prt:LongInt;                {remote port}
    bcast:LongInt;              {broadcast helper}
    nick:String;                {nick name of user}
    stat:LongInt;               {status: 0-disconn, 1-init, 2-first, 3-prefix, 4-cmd}
    chanD:array[1..channelUser] of LongInt;     {channels subscribed}
    chanN:LongInt;              {number of channels}
    cmd:String;                 {command}
    end;
Var
  channelsDat:array[1..channelsMax] of OneChannelRecord;
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  greetMsgDat:array[1..1024] of byte;
  greetMsgSiz:LongInt;
  serverPort:LongInt;
  serverAddr:OneTCPaddressRecord;
  serverName:String;



Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Function ResizeMem(n:LongInt):Boolean;
Var
  p:Pointer;
  i:LongInt;
Begin;
ResizeMem:=True;
i:=n*sizeof(OneConnectionRecord);
if (ExtendedMemoryResize(p,i)<i) then exit;
ConnectionNum:=n;
ConnectionDat:=p^;
ResizeMem:=False;
End;

Function getDateTime:String;
Var
  s:String;
  a,b,c:Word;
function x(i:longInt):string;var a:string;begin; a:=bstr(i);while (length(a)<2) do a:='0'+a;x:=a; end;
Begin;
xGetDate(a,b,c);
s:=x(a)+'-'+x(b)+'-'+x(c);
xGetTime(a,b,c);
getDateTime:=s+' '+x(a)+':'+x(b)+':'+x(c);
End;

Function findOneNick(a:String):LongInt;
Label f1;
Var i:LongInt;
Begin;
a:=kicsi(a);
for i:=1 to ConnectionNum do
 if (kicsi(ConnectionDat^[i].nick)=a) then goto f1;
i:=0;
f1:
findOneNick:=i;
End;

Function findOneChan(a:String):LongInt;
Label f1;
Var i:LongInt;
Begin;
a:=kicsi(a);
for i:=1 to channelsMax do begin;
  if (channelsDat[i].users<1) then continue;
  if (kicsi(channelsDat[i].name)=a) then goto f1;
  end;
i:=0;
f1:
findOneChan:=i;
End;
