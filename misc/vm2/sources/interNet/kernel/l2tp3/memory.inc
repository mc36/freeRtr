Type
  OneConnectionRecord=record
    addr:OneTCPaddressRecord;   {remote address}
    conn:LongInt;               {tunnel number}
    stat:LongInt;               {tunnel status: 0=none, 1=sentRQ, 2=gotRQ, 3=sentRP, 4=gotRP, 5=gotDN, 100=conn, 101=prep}
    pckR:LongInt;               {next packet to receive}
    pckT:LongInt;               {next packet to transmit}
    timT:LongInt;               {time last received}
    timR:LongInt;               {time last transmitted}
    bufD:array[1..256] of byte; {transfer buffer}
    bufS:LongInt;               {transfer size}
    remI:LongInt;               {remote tunnel id}
    locI:LongInt;               {local tunnel id}
    rtry:LongInt;               {number of retries}
    need:Boolean;               {need to send ack}
    end;
  OneSessionRecord=record
    addr:OneTCPaddressRecord;   {remote address}
    pipe:LongInt;               {pipeline id of control connection}
    stat:LongInt;               {session status: 0=none, 1=sentRQ, 2=gotRQ, 3=sentRP, 4=gotRP, 5=gotDN, 100=conn}
    mode:LongInt;               {pseudowire type}
    vcid:LongInt;               {virtual circuit}
    conn:LongInt;               {connection number}
    sess:LongInt;               {session number}
    time:LongInt;               {time of last action}
    remI:LongInt;               {remote session id}
    locI:LongInt;               {local session id}
    end;
Var
  addr:OneTCPaddressRecord;
  ipProto:LongInt;
  pipe:LongInt;
  ConnectionDat:array[1..32] of OneConnectionRecord;
  ConnectionNum:LongInt;
  SessionDat:array[1..64] of OneSessionRecord;
  SessionNum:LongInt;
  hostName:String;
  macAddr:array[1..6] of byte;



Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;


Function findOnePeerAddr(var adr):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to ConnectionNum do
 if TCPcompareAddress(adr,ConnectionDat[i].addr) then goto f1;
i:=0;
f1:
findOnePeerAddr:=i;
End;

Function findOneSessionVC(var con:OneConnectionRecord;id:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to SessionNum do begin;
  if (SessionDat[i].vcid<>id) then continue;
  if (SessionDat[i].conn=con.conn) then goto f1;
  end;
i:=0;
f1:
findOneSessionVC:=i;
End;

Function findOneSessionID(var con:OneConnectionRecord;id:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to SessionNum do begin;
  if (SessionDat[i].locI<>id) then continue;
  if (SessionDat[i].conn=con.conn) then goto f1;
  end;
i:=0;
f1:
findOneSessionID:=i;
End;

Procedure generateConnID(var con:OneConnectionRecord);
Begin;
WriteLongMSB(con.locI,random($ffff) or 1);
End;

Procedure generateSessID(var ses:OneSessionRecord);
Var
  i:LongInt;
  b:array[1..1] of byte absolute i;
Begin;
i:=random($7fffffff);
b[sizeof(i)]:=ses.sess;
ses.locI:=i;
End;

Procedure clearOneSess(var ses:OneSessionRecord);
Begin;
ses.stat:=0;
ses.time:=currentTime;
ses.remI:=0;
ses.locI:=0;
End;

Procedure clearSessOfConn(var con:OneConnectionRecord);
Var i:LongInt;
Begin;
for i:=1 to SessionNum do if (SessionDat[i].conn=con.conn) then clearOneSess(SessionDat[i]);
End;

Procedure clearOneConn(var con:OneConnectionRecord);
Begin;
clearSessOfConn(con);
con.stat:=0;
con.pckR:=0;
con.pckT:=0;
con.timT:=currentTime;
con.timR:=currentTime;
con.bufS:=0;
con.remI:=0;
con.locI:=0;
con.rtry:=0;
con.need:=false;
End;


Function getOneAVPtype(i:LongInt):String;
Var a:String;
Begin;
case i of
  0:a:='msg type';
  1:a:='result code';
  2:a:='protocol version';
  3:a:='framing';
  5:a:='tie breaker';
  6:a:='firmware';
  7:a:='host name';
  8:a:='vendor name';
  10:a:='window';
  15:a:='serial';
  24:a:='connect speed';
  25:a:='physical channel';
  34:a:='wan error';
  36:a:='random vector';
  59:a:='msg digest';
  60:a:='router id';
  61:a:='conn id';
  62:a:='pw capa';
  63:a:='loc sess id';
  64:a:='rem sess id';
  65:a:='cookie';
  66:a:='remote id';
  68:a:='pw type';
  69:a:='sublayer';
  70:a:='sequencing';
  71:a:='circuit status';
  72:a:='lang';
  73:a:='auth nonce';
  74:a:='tx conn speed';
  75:a:='rx conn speed';
  else a:='unknown:'+BStr(i);
  end;
getOneAVPtype:=a;
End;

Procedure placeOneAVP(var buffer;var ps:LongInt;t:LongInt;a:String);
Var
  buf:array[1..1] of byte absolute buffer;
  ab:array[0..128] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
Begin;
WriteWordMSB(buf[ps+1],ab0+$8006);
WriteWordMSB(buf[ps+3],t shr 16);
WriteWordMSB(buf[ps+5],t);
move(ab[1],buf[ps+7],ab0);
inc(ps,ab0+6);
{$ifdef debug}
write(' '+getOneAVPtype(t));
for i:=1 to ab0 do write(' '+byte2hextype(ab[i]));
writeln('');
{$endif}
End;

Function readOneAVP(var buffer;var ps,tp:LongInt):String;
Var
  buf:array[1..1] of byte absolute buffer;
  a:String;
  ab:array[0..128] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
Begin;
i:=ReadWordMSB(buf[ps+1])-6;
if (i<1) then i:=0;
o:=i and $3ff;
if (o>255) then ab0:=255 else ab0:=o;
move(buf[ps+7],ab[1],ab0);
tp:=(readWordMSB(buf[ps+3]) shl 16)+readWordMSB(buf[ps+5]);
inc(ps,6);
inc(ps,o);
readOneAVP:=a;
{$ifdef debug}
write(' '+getOneAVPtype(tp));
for i:=1 to ab0 do write(' '+byte2hextype(ab[i]));
writeln('');
{$endif}
End;
