Const
  ChannelMaximum=32;
  LocalWindowSize=3*1024;
  RemoteWindowSize=$10000000;
Type
  OnePacketRecord=record
    s:LongInt;                  {size of whole packet}
    p:Byte;                     {size of padding}
    d:array[1..4*1024] of byte; {the data in packet}
    end;
  OneChannelRecord=record
    stat:LongInt;               {status: 0-opened, 1-connected, 2-wait, 3-open}
                                {4-request, 5-reqReply}
    LocID:LongInt;              {local identifier}
    remID:LongInt;              {remote identifier}
    Window:LongInt;             {transmit window left}
    Adjust:LongInt;             {bytes to adjust}
    MaxPck:LongInt;             {max packet size}
    pipe:LongInt;               {pipeline id}
    port:LongInt;               {target port number}
    end;
  OneConnectionRecord=record
    err:String;                 {error string}
    pipe:LongInt;               {pipeline id of tcp connection}
    proc:LongInt;               {process id of owner process}
    adr:OneTCPaddressRecord;    {remote address}
    prt:LongInt;                {remote port}
    time:LongInt;               {last action}
    stat:LongInt;               {status: 0-disconn, 1-init, 2-clientSW, 3-kexInit}
                                {4-gexReq, 5-gexGrup, 6-kexReq, 7-newKeys, 8-auth}
                                {9-authWait}
                                {77-server, 777-client}
                                {99-tcpemu, 100-tcpconn, 101-version, 102-kexInit}
                                {103-gexReq, 104-gexGrup, 105-gexReply, 106-kexReq}
                                {107-kexReply, 108-newKeys, 109-srvAccpt, 110-user}
                                {111-pass, 112-auth}
    remoteSW:String;            {remote software}
    secure:Boolean;             {channel is secure}
    authed:Boolean;             {user authenticated}
    userid:LongInt;             {authenticated user id}
    cokieL:array[1..8] of word; {local cookie}
    sessID:array[1..20] of byte;{current session id}
    packLR:LongInt;             {packets loc-->rem saw}
    macLRa:CryptoContextRecord; {hash loc-->rem part one}
    macLRb:CryptoContextRecord; {hash loc-->rem part two}
    macLRs:LongInt;             {hash loc-->rem size}
    encLRc:CryptoContextRecord; {cipher loc-->rem context}
    encLRs:LongInt;             {cipher loc-->rem size}
    packRL:LongInt;             {packets rem-->loc saw}
    macRLa:CryptoContextRecord; {hash rem-->loc part one}
    macRLb:CryptoContextRecord; {hash rem-->loc part two}
    macRLs:LongInt;             {hash rem-->loc size}
    encRLc:CryptoContextRecord; {cipher rem-->loc context}
    encRLs:LongInt;             {cipher rem-->loc size}
    recDat:array[1..4*1024] of byte;    {receive buffer}
    recPos:LongInt;             {bytes in receive buffer}
    recSiz:LongInt;             {size of packet}
    planKex:Byte;               {planned key exchange: 1-DHgroup1, 2-DHgroupXchg}
    planSig:Byte;               {planned signature: 1-dss, 2-rsa}
    planGroup:LongInt;          {planned number of group}
    planMacLR:Byte;             {planned loc-->rem hash}
    planMacRL:Byte;             {planned rem-->loc hash}
    planEncLR:Byte;             {planned loc-->rem cipher}
    planEncRL:Byte;             {planned rem-->loc cipher}
    planNum1:BigNumContextRecord;   {planned secret number}
    planNum2:BigNumContextRecord;   {planned secret number}
    planHsh:CryptoContextRecord;    {planned hash data}
    planGrp:OneDiffieHellmanGroup;  {group for clients}
    ChanDat:array[1..ChannelMaximum] of OneChannelRecord;  {channels}
    ChanNum:Byte;               {number of channels}
    ChanCur:Byte;               {current channel number}
    ChanSeq:LongInt;            {channel number to give}
    end;
Var
  hostKeyDSS:OneDSSkeyRecord;
  hostKeyRSA:OneRSAkeyRecord;
  ListenerDat:array[1..128] of record
    name:String;
    port:Word;
    flag:LongInt;               {1-pty, 2-subsys}
    proc:LongInt;
    pipe:LongInt;
    wind:LongInt;
    end;
  ListenerNum:LongInt;
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  serverPort:LongInt;
  serverAddr:OneTCPaddressRecord;
  lastSent:LongInt;
  PreferredCiphers:String;
  PreferredHashers:String;
  diffieHellmanGroups:String;
  diffieHellmanDiv:LongInt;
  diffieHellmanMin:LongInt;
  diffieHellmanMax:LongInt;
  sha1algoNum:LongInt;
  md5algoNum:LongInt;
Const
  CipherAlgoMax=7;
  CipherAlgoList:array[1..CipherAlgoMax] of record
    n:String[15];               {name}
    ks:Byte;                    {key size}
    s:byte;                     {cipher size}
    a:longint;                  {algorithm}
    end=(
  (n:'aes256-cbc';ks:32;s:16;a:0),
  (n:'twofish256-cbc';ks:32;s:16;a:0),
  (n:'blowfish-cbc';ks:16;s:8;a:0),
  (n:'3des-cbc';ks:24;s:8;a:0),
  (n:'rc6-256-cbc';ks:32;s:16;a:0),
  (n:'rc5-cbc';ks:16;s:8;a:0),
  (n:'rc2-cbc';ks:16;s:8;a:0)
  );
  HasherAlgoMax=3;
  HasherAlgoList:array[1..HasherAlgoMax] of record
    n:String[15];               {name}
    s:byte;                     {hash size}
    a:longint;                  {algorithm}
    end=(
  (n:'hmac-md5';s:16;a:0),
  (n:'hmac-sha1';s:20;a:0),
  (n:'hmac-ripemd160';s:20;a:0)
  );


Procedure immErr(a:string);
Begin;
writeLn(a);
halt(1);
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

Function getWord(var a:String):String;
Var i:LongInt;
Begin;
i:=pos(' ',a);
if (i<1) then i:=666;
getWord:=copy(a,1,i-1);
a:=copy(a,i+1,255);
End;

Function GetKeyXchgAlgName(n:Byte):String;
Begin;
GetKeyXchgAlgName:='';
case n of
  1:GetKeyXchgAlgName:='diffie-hellman-group-exchange-sha1';
  2:GetKeyXchgAlgName:='diffie-hellman-group1-sha1';
  end;
End;

Function GetHostKeyAlgName(n:Byte):String;
Var i:LongInt;
Begin;
GetHostKeyAlgName:='';
case n of
  1:begin;
    move(hostKeyDSS.p,i,sizeof(i));
    if (i<>0) then GetHostKeyAlgName:='ssh-dss';
    end;
  2:begin;
    move(hostKeyRSA.p,i,sizeof(i));
    if (i<>0) then GetHostKeyAlgName:='ssh-rsa';
    end;
  end;
End;

Function GetServerVersionString:String;
Begin;
GetServerVersionString:='SSH-2.0-'+ProggyName+' for BugOS';
End;

Function ReadDiffieHellmanGroup(n:LongInt;var d:OneDiffieHellmanGroup):Boolean;
Var f:xFile;
Begin;
ReadDiffieHellmanGroup:=True;
if (xOpen(f,diffieHellmanGroups,xGenFilMod_r)<>0) then exit;
if (n<0) then exit;
if (n>=xFileSize(f) div sizeof(d)) then exit;
xSeek(f,n*sizeof(d));
xBlockRead(f,d,sizeof(d));
xClose(f);
ReadDiffieHellmanGroup:=False;
End;

Function FindDiffieHellmanGroup(siz:LongInt):LongInt;
Var
  f:xFile;
  d:OneDiffieHellmanGroup;
  i,o:LongInt;
  w:Word;
Begin;
FindDiffieHellmanGroup:=0;
siz:=siz shr 5;
if (xOpen(f,diffieHellmanGroups,xGenFilMod_r)<>0) then exit;
o:=xFileSize(f) div sizeof(d);
for i:=1 to o do begin;
  xBlockRead(f,d,sizeof(d));
  move(d.p,w,sizeof(w));
  if (w<siz) then continue;
  o:=i;
  if (w=siz) then break;
  if (i>1) then dec(o);
  break;
  end;
dec(o);
xClose(f);
FindDiffieHellmanGroup:=o;
End;


Function FindOneLocalConnectID(var con:OneConnectionRecord;id:LongInt):LongInt;
Var i:LongInt;
Begin;
for i:=1 to con.ChanNum do if (con.ChanDat[i].LocID=id) then begin;
  FindOneLocalConnectID:=i;
  exit;
  end;
FindOneLocalConnectID:=0;
End;

Function GenerateLocalConnectID(var con:OneConnectionRecord):LongInt;
Label f1;
Var
  i:LongInt;
Begin;
i:=con.ChanSeq;
f1:
i:=(i+1) and $7fffffff;
if (i<1) then goto f1;
if (FindOneLocalConnectID(con,i)<>0) then goto f1;
con.ChanSeq:=i;
GenerateLocalConnectID:=i;
End;

Procedure GetRndBigNum(var d:BigNumContextRecord;s:LongInt);
Var
  buf:array[1..1] of byte absolute d;
  i,o:LongInt;
Begin;
fillchar(d,sizeof(d),0);
if (s<8) then s:=8;
i:=0;
while (s>0) do begin;
  o:=random($100);
  inc(i);
  buf[i]:=o;
  dec(s,8);
  end;
inc(s,8);
buf[i]:=o and ((1 shl s)-1);
inc(i);
buf[i]:=0;
BigNumConvLSB2BN(i,d,d);
End;
