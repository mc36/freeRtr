Const
  modemVersion='v1.0';
  modemProggy='softModem';
Var
  modemConnected:LongInt;       {-5=vtr, -4=vtx, -3=vrx, -2=vts, -1=dial, 0=no, 1=yes, else trying}
  modemBitPerSec:LongInt;
  modemDataBit:LongInt;
  modemParity:LongInt;
  modemFreqOrg:LongInt;
  modemFreqAns:LongInt;
  modemPrecisity:LongInt;
  modemSquareOut:LongInt;
  modemNoCarrPack:LongInt;
  modemRxDat:array[1..1024] of byte;
  modemTxDat:array[1..1024] of byte;
  modemRxSiz:LongInt;
  modemTxSiz:LongInt;
  modemCommand:String;
  modemIgnoreBad:Boolean;
  modemCDforced:Boolean;
  modemDTRignor:Boolean;
  modemCmdEcho:Boolean;
  modemVerbRsp:Boolean;
  movemVoipMode:Boolean;
  modemVtxDLEd:Boolean;
  modemCDchg:LongInt;
  upPipeData:LongInt;
  upPipeCtrl:LongInt;


Procedure modemResetParam;
Begin;
modemBitPerSec:=300;
modemDataBit:=8;
modemParity:=0;
modemFreqOrg:=1080;
modemFreqAns:=1750;
modemPrecisity:=30;
modemSquareOut:=0;
modemConnected:=0;
modemCDchg:=0;
modemCommand:='';
modemRxSiz:=0;
modemTxSiz:=0;
modemIgnoreBad:=False;
modemCDforced:=False;
modemCmdEcho:=True;
modemVerbRsp:=True;
modemDTRignor:=False;
movemVoipMode:=False;
End;


Procedure modemAdd2rx(a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
move(a[1],modemRxDat[modemRxSiz+1],ab0);
inc(modemRxSiz,ab0);
End;

Procedure modemDelTxed(p:LongInt);
Begin;
if (p>modemTxSiz) then p:=modemTxSiz;
dec(modemTxSiz,p);
move(modemTxDat[p+1],modemTxDat[1],modemTxSiz);
End;

Procedure modemAddResp(n:LongInt);
Var a:String;
Begin;
case n of
  0:a:='OK';
  1:a:='CONNECT';
  2:a:='RING';
  3:a:='NO CARRIER';
  4:a:='ERROR';
  6:a:='NO DIALTONE';
  7:a:='BUSY';
  8:a:='NO ANSWER';
  52:a:='CARRIER 300';
  67:a:='COMPRESSION: NONE';
  77:a:='PROTOCOL: FSK';
  100001:a:='VCON';
  end;
if not modemVerbRsp then a:=BStr(n)+#13 else a:=a+#13#10;
modemAdd2rx(a);
End;





Procedure modemFlushRxBuf;
Label f1;
Var
  buf:array[1..1024] of byte;
  i:LongInt;
Begin;
modemRxSiz:=0;
relequish;
f1:
i:=sizeof(buf);
if (pipeLineRecv(upPipeData,buf,i)<>0) then exit;
if (i>0) then goto f1;
End;


Procedure connectStart(called:Boolean);
Var i:LongInt;
Begin;
if called then i:=modemFreqOrg else i:=modemFreqAns;
WriteLn('connecting; carrier='+BStr(i)+' hz...');
fskInitialize(modemBitPerSec,modemPrecisity,i,modemFreqOrg+modemFreqAns-i);
dataBits:=modemDataBit;
parityBit:=modemParity;
encodSqr:=modemSquareOut;
lastDTMFdetection:=999;
modemNoCarrPack:=0;
modemRxSiz:=0;
modemTxSiz:=0;
modemConnected:=2;
modemVtxDLEd:=False;
soundFlush;
soundFedup;
End;


Procedure modemDisconnect;
Begin;
if (modemConnected=0) then exit;
WriteLn('disconnect requested!');
modemRxSiz:=0;
modemTxSiz:=0;
soundFlush;
modemFlushRxBuf;
if (modemConnected=1) then modemAdd2rx(#13#10);
modemConnected:=0;
inc(modemCDchg);
modemCommand:='';
modemAddResp(3);
End;
