Const
  TCPprotocolNum=$06;
  UDPprotocolNum=$11;
Type
  OneAddressRecord=record
    a1:LongInt;
    a2:LongInt;
    a3:LongInt;
    a4:LongInt;
    end;
  OnePacketRecord=record
    a:OneAddressRecord;
    b:OneAddressRecord;
    p:Byte;
    d:Array[1..4*1024] of byte;
    end;
Type
  checkAdjusterRec=record
    pck:Word;                   {packet checksum}
    src:Word;                   {source address}
    trg:Word;                   {target address}
    end;
Var
  ip4addr:OneAddressRecord;
  ip4proc:LongInt;
  ip4pipe:LongInt;
  ip6addr:OneAddressRecord;
  ip6proc:LongInt;
  ip6pipe:LongInt;
  defStck:LongInt;
  lastSent:LongInt;
  uppProc:LongInt;
  uppPipe:LongInt;
  chkAdjI:checkAdjusterRec;
  chkAdjO:checkAdjusterRec;
  addrPri:OneAddressRecord;
  addrSec:OneAddressRecord;

Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

Function equalAddr(var a,b:OneAddressRecord):Boolean;
Begin;
equalAddr:=false;
if (a.a4<>b.a4) then exit;
if (a.a3<>b.a3) then exit;
if (a.a2<>b.a2) then exit;
if (a.a1<>b.a1) then exit;
equalAddr:=true;
End;

Function doOneIPrequest(a:String;closeAfter:Boolean):String;
Label f1;
Var
  pip:LongInt;
  i,o:LongInt;
Begin;
relequish;
if (pipeLineCreate(pip,ip6proc,65536,true)<>0) then begin;
  WriteLn('unabled to create pipeline!');
  Halt(1);
  end;
if (pipeLineSend(pip,a[1],length(a))<>0) then begin;
  WriteLn('unabled to send data over pipeline!');
  Halt(1);
  end;
i:=0;
f1:
inc(i);
if (i>128) then begin;
  WriteLn('unabled to receive data from pipeline!');
  Halt(1);
  end;
relequish;
o:=255;
if (pipeLineRecv(pip,a[1],o)<>0) then o:=0;
if (o=0) then goto f1;
if closeAfter then pipeLineClose(pip) else ip6pipe:=pip;
a[0]:=chr(o);
doOneIPrequest:=a;
End;




Procedure gotLowerPacker(var pck:OnePacketRecord;siz,stk:LongInt);
Label f1;
Var
  p:LongInt;
  b:Boolean;
  a:String;
  w:Word;
Begin;
{$ifdef debug}
case stk of
  1:a:='ip4';
  2:a:='ip6';
  else a:='?';
  end;
WriteLn('got from '+ipAddr2string(pck.a)+' --> '+ipAddr2string(pck.b)+' over '+a);
{$endif}
if (stk=defStck) then goto f1;
if (pck.p=TCPprotocolNum) then p:=17 else
 if (pck.p=UDPprotocolNum) then p:=7 else goto f1;
if not equalAddr(pck.b,addrSec) then goto f1;
move(pck.d[p],chkAdjI,sizeof(w));
w:=CalculateSum(chkAdjI,sizeof(chkAdjI));
move(w,pck.d[p],sizeof(w));
pck.b:=addrPri;
f1:
pipeLineSend(uppPipe,pck,siz);
lastSent:=uppProc;
End;

Procedure gotUpperPacker(var pck:OnePacketRecord;siz:LongInt);
Label f1;
Var
  p:LongInt;
  b:Boolean;
  w:Word;
Begin;
b:=isAddressIPv4mask(pck.a);
if ((defStck<>1) xor b) then goto f1;
if (pck.p=TCPprotocolNum) then p:=17 else
 if (pck.p=UDPprotocolNum) then p:=7 else goto f1;
if not equalAddr(pck.b,addrPri) then goto f1;
move(pck.d[p],chkAdjO,sizeof(w));
w:=CalculateSum(chkAdjO,sizeof(chkAdjO));
move(w,pck.d[p],sizeof(w));
pck.b:=addrSec;
f1:
if b then begin;
  pipeLineSend(ip4pipe,pck,siz);
  lastSent:=ip4proc;
  {$ifdef debug}WriteLn('upper sent '+ipAddr2string(pck.b)+' --> '+ipAddr2string(pck.a)+' over ip4');{$endif}
  end else begin;
  pipeLineSend(ip6pipe,pck,siz);
  lastSent:=ip6proc;
  {$ifdef debug}WriteLn('upper sent '+ipAddr2string(pck.b)+' --> '+ipAddr2string(pck.a)+' over ip6');{$endif}
  end;
End;
