Type
  OnePacketRecord=record
    s:LongInt;                  {size of whole packet}
    t:LongInt;                  {type of packet}
    d:array[1..32*1024] of byte;{the data in packet}
    end;
  OneConnectionRecord=record
    err:String;                 {error string}
    pipeT:LongInt;              {pipeline id of tcp connection}
    pipeL:LongInt;              {pipeline id of local connection}
    proc:LongInt;               {process id of owner process}
    addr:OneTCPaddressRecord;   {remote address}
    port:LongInt;               {remote port}
    time:LongInt;               {last action}
    vers:LongInt;               {version number}
    stat:LongInt;               {status: 0-disconn, 1-sessID, 2-inConn}
                                {3-getCommand, 4-listen, 5-conning, 6-srvHelo}
                                {7-srvCert, 8-srvKex, 9-srvHeloDon, 10-chgCphr}
                                {11-finished, 12-chgCiphr, 13-finished}
                                {76-sending, 77-normal operation, 78-closing}
                                {100-clntHelo, 101-clntKex, 102-chgCphr}
                                {103-finished, 104-chgCphr, 105-finished}
    clnRnd:array[1..32] of byte;{client random data}
    srvRnd:array[1..32] of byte;{server random data}
    secret:array[1..48] of byte;{master secret}
    sessID:String[32];          {session id}
    planKex:Byte;               {key exchange method: 1-dh, 2-rsa}
    planCrt:Byte;               {certificate type: 1-dss, 2-rsa}
    planEnc:Byte;               {cipher type: 1-aes, 2-des, 3-3des, 4-rc2, 5-rc4}
    planMac:Byte;               {hash type: 1-md5, 2-sha1}
    planExp:Boolean;            {exportable flag}
    secure:Byte;                {0-none, 1-partical, 2-good}
    peerTp:Byte;                {which side of connection: 1-client, 2-server}
    macSiz:LongInt;             {size of hash}
    encSiz:LongInt;             {size of cipher}
    pckLRa:LongInt;             {packets loc-->rem low}
    pckRLa:LongInt;             {packets rem-->loc low}
    pckLRb:LongInt;             {packets loc-->rem high}
    pckRLb:LongInt;             {packets rem-->loc high}
    macLRa:CryptoContextRecord; {hash loc-->rem part one}
    macLRb:CryptoContextRecord; {hash loc-->rem part two}
    macRLa:CryptoContextRecord; {hash rem-->loc part one}
    macRLb:CryptoContextRecord; {hash rem-->loc part two}
    encLRc:CryptoContextRecord; {cipher loc-->rem context}
    encRLc:CryptoContextRecord; {cipher rem-->loc context}
    recDat:array[1..24*1024] of byte;    {receive buffer}
    recPos:LongInt;             {bytes in receive buffer}
    recSiz:LongInt;             {size of packet}
    end;
Var
  hostKeyDSS:OneDSSkeyRecord;
  hostKeyRSA:OneRSAkeyRecord;
  hostKeyRSAe:OneRSAkeyRecord;
  hostKeyDH:OneDiffieHellmanGroup;
  hostCrtDSS:OnePacketRecord;
  hostCrtRSA:OnePacketRecord;
  hostKeyDHdiv:LongInt;
  hostKeyDHmin:LongInt;
  hostKeyDHmax:LongInt;
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  lastSent:LongInt;
  sha1algoNum:LongInt;
  md5algoNum:LongInt;
Const
  CipherAlgoMax=5;
  CipherAlgoList:array[1..CipherAlgoMax] of record
    ks:Byte;                    {key size}
    es:Byte;                    {export key size}
    s:byte;                     {cipher size}
    a:longint;                  {algorithm}
    end=(
  (ks:32;es:32;s:16;a:0),
  (ks:8 ;es:5 ;s:8 ;a:0),
  (ks:24;es:24;s:8 ;a:0),
  (ks:16;es:5 ;s:8 ;a:0),
  (ks:16;es:5 ;s:1 ;a:0)
  );
  HasherAlgoMax=2;
  HasherAlgoList:array[1..HasherAlgoMax] of record
    s:byte;                     {hash size}
    a:longint;                  {algorithm}
    end=(
  (s:16;a:0),
  (s:20;a:0)
  );
Const
  CipherSuitesMax=32;
Var
  CipherSuitesDat:array[1..CipherSuitesMax] of LongInt;
  CipherSuitesNum:LongInt;


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

Function FindSessionIDclnt(adr:OneTCPaddressRecord;prt:LongInt):LongInt;
Var
  d:OneConnectionRecord;
  i:LongInt;
Begin;
for i:=ConnectionNum downto 1 do begin;
  d:=ConnectionDat^[i];
  if (d.port<>prt) then continue;
  if (d.peerTp<>1) then continue;
  if (d.secure<2) then continue;
  if not (d.stat in [1,77,78]) then continue;
  if not TCPcompareAddress(adr,d.addr) then continue;
  FindSessionIDclnt:=i;
  exit;
  end;
FindSessionIDclnt:=0;
End;

Function FindSessionIDsrvr(adr:OneTCPaddressRecord;prt:LongInt;sid:String):LongInt;
Label f1;
Var
  d:OneConnectionRecord;
  i:LongInt;
Begin;
if (sid='') then goto f1;
for i:=ConnectionNum downto 1 do begin;
  d:=ConnectionDat^[i];
  if (d.port<>prt) then continue;
  if (d.peerTp<>2) then continue;
  if (d.secure<2) then continue;
  if not (d.stat in [1,77,78]) then continue;
  if not TCPcompareAddress(adr,d.addr) then continue;
  if (d.sessID<>sid) then continue;
  FindSessionIDsrvr:=i;
  exit;
  end;
f1:
FindSessionIDsrvr:=0;
End;
