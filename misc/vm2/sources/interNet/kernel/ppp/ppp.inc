Const
  LCPprotocolNumber=$c021;
  LCPauthPAP=$c023;
  LCPauthCHAP=$c223;
  LCPauthEAP=$c227;
  IPCPV4protocolNumber=$8021;
  IPCPV6protocolNumber=$8057;
  IPV4protocolNumber=$0021;
  IPV6protocolNumber=$0057;
  addressField=$ff;
  controlField=$03;
  timeoutVal=4;
Var
  md5algorithm:LongInt;
  name:String;
  proc:LongInt;
  pipe:LongInt;
  upper:LongInt;
  ipVer:LongInt;
  LocIP:String;
  RemIP:String;
  dns1ip:String;
  dns2ip:String;
  LocChrMap:LongInt;
  RemChrMap:LongInt;
  userName:String;
  passWord:String;
  ipConfig:String;
  serverMode:Boolean;
  RemRecvUnit:LongInt;
  LocRecvUnit:LongInt;
  LocMagicNum:LongInt;
  RemMagicNum:LongInt;
  lastSent:LongInt;
  buggyTyp:LongInt;
  buggyCnt:LongInt;
Type
  OnePPPpacketRecord=record
    a:byte;                     {address}
    x:byte;                     {control}
    p:word;                     {protocol}
    c:byte;                     {code}
    i:byte;                     {identifier}
    s:word;                     {length}
    d:array[1..2*1024] of byte; {data}
    pos:LongInt;                {position}
    siz:LongInt;                {size of data}
    end;

function conv2hex(i:longint;s:byte):string;
var a:string;
begin;
a:=byte2hextype(i);
if (s>1) then a:=byte2hextype(i shr 8)+a;
if (s>2) then a:=byte2hextype(i shr 16)+a;
if (s>3) then a:=byte2hextype(i shr 24)+a;
conv2hex:='$'+a;
end;

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Procedure registerMyself;
Var
  i,o:LongInt;
  a:String;
Begin;
if (ipConfig='') then exit;
BugOS_MyProcessInfo(o,i,i);
a:=BStr(o);
xExecBgnd(ipConfig,'iface add '+a+' ppp'+a,i);
End;


Function PPPtestPacket(var pck:OnePPPpacketRecord;i:LongInt):Boolean;
Begin;
PPPtestPacket:=True;
if (i<8) then begin;
  WriteLn('got a too short packet!');
  exit;
  end;
if (ReadWordMSB(pck.s)+4>i) then begin;
  WriteLn('got a truncated packet!');
  exit;
  end;
PPPtestPacket:=False;
End;

Procedure PPPoptionStart(var pck:OnePPPpacketRecord);
Begin;
pck.pos:=0;
pck.siz:=ReadWordMSB(pck.s)-4;
End;

Function PPPoptionGet(var pck:OnePPPpacketRecord;var opt:OnePPPpacketRecord):Boolean;
Begin;
PPPoptionGet:=False;
opt.i:=pck.d[pck.pos+1];
opt.s:=pck.d[pck.pos+2];
if (opt.s<2) then exit;
if (opt.s+pck.pos>pck.siz) then exit;
dec(opt.s,2);
move(pck.d[pck.pos+3],opt.d,opt.s);
inc(pck.pos,opt.s+2);
PPPoptionGet:=True;
End;

Procedure PPPoptionPut(var pck:OnePPPpacketRecord;var opt:OnePPPpacketRecord);
Begin;
pck.d[pck.pos+1]:=opt.i;
pck.d[pck.pos+2]:=opt.s+2;
move(opt.d,pck.d[pck.pos+3],opt.s);
inc(pck.pos,opt.s+2);
End;

Function PPPstringGet(var pck:OnePPPpacketRecord):String;
Var a:String;
Begin;
move(pck.d[pck.pos+1],a,sizeof(a));
if (pck.pos+length(a)+1>pck.siz) then a:='';
inc(pck.pos,length(a)+1);
PPPstringGet:=a;
End;

Procedure PPPstringPut(var pck:OnePPPpacketRecord;a:String);
Begin;
move(a,pck.d[pck.pos+1],sizeof(a));
inc(pck.pos,length(a)+1);
End;

Function PPPbinaryGet(var pck:OnePPPpacketRecord):String;
Var
  a:String;
  i:LongInt;
Begin;
i:=pck.siz-pck.pos;
if (i>255) then i:=255;
if (i<1) then a:='' else begin;
  move(pck.d[pck.pos+1],a[1],i);
  a[0]:=chr(i);
  end;
pck.pos:=pck.siz;
PPPbinaryGet:=a;
End;

Procedure PPPbinaryPut(var pck:OnePPPpacketRecord;a:String);
Begin;
move(a[1],pck.d[pck.pos+1],sizeof(a));
inc(pck.pos,length(a));
End;

Function PPPgetProtocolName(i:Word):String;
Begin;
case i of
  LCPprotocolNumber:PPPgetProtocolName:='LCP';
  LCPauthPAP:PPPgetProtocolName:='PAP';
  LCPauthCHAP:PPPgetProtocolName:='CHAP';
  LCPauthEAP:PPPgetProtocolName:='EAP';
  IPCPV4protocolNumber:PPPgetProtocolName:='IPCPv4';
  IPV4protocolNumber:PPPgetProtocolName:='IPv4';
  IPCPV6protocolNumber:PPPgetProtocolName:='IPCPv6';
  IPV6protocolNumber:PPPgetProtocolName:='IPv6';
  $c02b:PPPgetProtocolName:='BACP';
  $c025:PPPgetProtocolName:='LQR';
  else PPPgetProtocolName:='unknown: '+conv2hex(i,2);
  end;
End;
