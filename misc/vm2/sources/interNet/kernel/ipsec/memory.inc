Type
  OneConnectionRecord=record
    stat:LongInt;               {status: 0-disconn, 1-id1, 102-id2, 11-mm1, 121-am1, 30-id0, 31-id1, 132-id2, 201-qm1, 202-qm2, 233-qm1, 234-qm2, 1000-esp}
    pipe:LongInt;               {pipeline id of control connection}
    time:LongInt;               {time of last action}
    retry:LongInt;              {retry number}
    addr:OneTCPaddressRecord;   {remote address}
    portR:LongInt;              {remote port}
    portL:LongInt;              {local port}
    lstGot:LongInt;             {last got headers}
    shky:String;                {shared key}
    mode:LongInt;               {0-passive, 1-active}
    isaE:LongInt;               {isakmp encr}
    isaH:LongInt;               {isakmp hash}
    isaG:LongInt;               {isakmp group}
    isaT:LongInt;               {isakmp time}
    isaB:LongInt;               {isakmp byte}
    isaN:LongInt;               {isakmp nat: 1,2-draft, 3-rfc}
    espE:LongInt;               {esp encr}
    espH:LongInt;               {esp hash}
    espT:LongInt;               {esp time}
    espB:LongInt;               {esp byte}
    initCok:array[1..8] of byte;{initiator cookie}
    respCok:array[1..8] of byte;{responder cookie}
    initNon:String[65];         {initiator nonce}
    respNon:String[65];         {initiator nonce}
    remSPI:LongInt;             {remote spi}
    locSPI:LongInt;             {local spi}
    locPcks:LongInt;            {received packet}
    remPcks:LongInt;            {transmitted packets}
    skeysG:String[33];          {skeys generated}
    skeysD:String[33];          {skeys derived}
    skeysA:String[33];          {skeys auth}
    skeysE:String[33];          {skeys encr}
    encK:String[65];            {encryption key}
    encI:String[65];            {encryption iv}
    encS:LongInt;               {size of iv}
    encA:LongInt;               {encryption algo}
    msgID:LongInt;              {message id}
    remID:String[65];           {remote id value}
    encHS:LongInt;              {hash size}
    encCS:LongInt;              {cipher size}
    macLRa:CryptoContextRecord; {hash loc-->rem part one}
    macLRb:CryptoContextRecord; {hash loc-->rem part two}
    macLRs:LongInt;             {hash loc-->rem size}
    encLRc:CryptoContextRecord; {cipher loc-->rem context}
    encLRs:LongInt;             {cipher loc-->rem size}
    macRLa:CryptoContextRecord; {hash rem-->loc part one}
    macRLb:CryptoContextRecord; {hash rem-->loc part two}
    macRLs:LongInt;             {hash rem-->loc size}
    encRLc:CryptoContextRecord; {cipher rem-->loc context}
    encRLs:LongInt;             {cipher rem-->loc size}
    end;
Const
  CipherAlgoMax=6;
  CipherAlgoList:array[1..CipherAlgoMax] of record
    n:String[15];               {name}
    ks:Byte;                    {key size}
    s:byte;                     {cipher size}
    a:longint;                  {algorithm}
    p:Byte;                     {prefer}
    end=(
  (n:'aes-cbc';ks:16;s:16;a:0;p:0),
  (n:'blowfish-cbc';ks:56;s:8;a:0;p:0),
  (n:'3des-cbc';ks:24;s:8;a:0;p:0),
  (n:'rc5-cbc';ks:16;s:8;a:0;p:0),
  (n:'des-cbc';ks:8;s:8;a:0;p:0),
  (n:'cast-cbc';ks:16;s:8;a:0;p:0)
  );
  HasherAlgoMax=2;
  HasherAlgoList:array[1..HasherAlgoMax] of record
    n:String[15];               {name}
    s:byte;                     {hash size}
    a:longint;                  {algorithm}
    p:Byte;                     {prefer}
    end=(
  (n:'md5';s:16;a:0;p:0),
  (n:'sha1';s:20;a:0;p:0)
  );
  DiffieHellmanGroup1:array[1..97] of byte=(0,
  $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$c9,$0f,$da,$a2,$21,$68,$c2,$34,$c4,$c6,
  $62,$8b,$80,$dc,$1c,$d1,$29,$02,$4e,$08,$8a,$67,$cc,$74,$02,$0b,$be,$a6,
  $3b,$13,$9b,$22,$51,$4a,$08,$79,$8e,$34,$04,$dd,$ef,$95,$19,$b3,$cd,$3a,
  $43,$1b,$30,$2b,$0a,$6d,$f2,$5f,$14,$37,$4f,$e1,$35,$6d,$6d,$51,$c2,$45,
  $e4,$85,$b5,$76,$62,$5e,$7e,$c6,$f4,$4c,$42,$e9,$a6,$3a,$36,$20,$ff,$ff,
  $ff,$ff,$ff,$ff,$ff,$ff
  );
  DiffieHellmanGroup2:array[1..129] of byte=(0,
  $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$c9,$0f,$da,$a2,$21,$68,$c2,$34,$c4,$c6,
  $62,$8b,$80,$dc,$1c,$d1,$29,$02,$4e,$08,$8a,$67,$cc,$74,$02,$0b,$be,$a6,
  $3b,$13,$9b,$22,$51,$4a,$08,$79,$8e,$34,$04,$dd,$ef,$95,$19,$b3,$cd,$3a,
  $43,$1b,$30,$2b,$0a,$6d,$f2,$5f,$14,$37,$4f,$e1,$35,$6d,$6d,$51,$c2,$45,
  $e4,$85,$b5,$76,$62,$5e,$7e,$c6,$f4,$4c,$42,$e9,$a6,$37,$ed,$6b,$0b,$ff,
  $5c,$b6,$f4,$06,$b7,$ed,$ee,$38,$6b,$fb,$5a,$89,$9f,$a5,$ae,$9f,$24,$11,
  $7c,$4b,$1f,$e6,$49,$28,$66,$51,$ec,$e6,$53,$81,$ff,$ff,$ff,$ff,$ff,$ff,
  $ff,$ff
  );
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  diffieHellmanDiv:LongInt;
  diffieHellmanMin:LongInt;
  diffieHellmanMax:LongInt;
  isakmpAddr1:OneTCPaddressRecord;
  isakmpPort1:LongInt;
  isakmpPipe1:LongInt;
  isakmpAddr2:OneTCPaddressRecord;
  isakmpPort2:LongInt;
  isakmpPipe2:LongInt;



Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Procedure clearOneStateRecord(var con:OneConnectionRecord);
Var
  pipe:LongInt;
  addr:OneTCPaddressRecord;
  shky:String;
  mode:LongInt;
Begin;
if (con.stat=1000) then WriteLn('tunnel down with '+ipAddr2string(con.addr)+' '+BStr(con.portR));
pipe:=con.pipe;
addr:=con.addr;
shky:=con.shky;
mode:=con.mode;
fillchar(con,sizeof(con),0);
con.pipe:=pipe;
con.addr:=addr;
con.shky:=shky;
con.mode:=mode;
End;

Procedure dumpBinaryData(name:String;var buffer;size:LongInt);
Var
  buf:array[1..1] of byte absolute buffer;
  i:LongInt;
Begin;
Write(name+' '+BStr(size)+':');
for i:=1 to size do Write(' '+byte2hextype(buf[i]));
WriteLn('');
End;

Procedure savePaddedBigNumber(var number,buffer;siz:LongInt);
Var
  num:BigNumContextRecord absolute number;
  buf:array[1..1] of byte absolute buffer;
  i,o:LongInt;
Begin;
BigNumConvBN2MSB(num,o,buf);
i:=siz-o;
if (i=0) then exit;
if (i<=0) then begin;
  move(buf[o-siz+1],buf,siz);
  exit;
  end;
move(buf[i+1],buf,o);
fillchar(buf,i,0);
End;

Procedure getDiffieHellmanGroup(g:LongInt;var num:BigNumContextRecord);
Begin;
case g of
  1:BigNumConvMSB2BN(sizeof(DiffieHellmanGroup1),DiffieHellmanGroup1,num);
  2:BigNumConvMSB2BN(sizeof(DiffieHellmanGroup2),DiffieHellmanGroup2,num);
  else fillchar(num,sizeof(num),0);
  end;
End;



Procedure doFindCrytoAlgo(var a:LongInt;var d;s:LongInt;n:String);
Begin;
a:=CryptoFindOneAlgo(d,s,n);
if (a<1) then immErr('failed to find '+n+' algorithm!');
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

Function findOneConnection(var addr):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to ConnectionNum do if TCPcompareAddress(addr,ConnectionDat^[i].addr) then goto f1;
i:=0;
f1:
findOneConnection:=i;
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



Procedure hmacInitialize(var c1,c2:CryptoContextRecord;alg:LongInt;key:String);

Function padup(key:String;num:byte):string;
Const max=64;
Var
  dat:array[0..1] of byte absolute key;
  i:LongInt;
Begin;
for i:=dat[0]+1 to max do dat[i]:=0;
dat[0]:=max;
for i:=1 to max do dat[i]:=dat[i] xor num;
padup:=key;
End;

Var a:String;
Begin;
CryptoBegHasher(c1,alg,'','');
CryptoBegHasher(c2,alg,'','');
a:=padup(key,$36);
CryptoAddHasher(c1,a[1],length(a));
a:=padup(key,$5c);
CryptoAddHasher(c2,a[1],length(a));
End;

Procedure hmacGenerateSum(c1,c2:CryptoContextRecord;var packet;var data;var size:LongInt);
Begin;
CryptoAddHasher(c1,packet,size);
CryptoFinHasher(c1,c1,size);
CryptoAddHasher(c2,c1,size);
CryptoFinHasher(c2,data,size);
End;
