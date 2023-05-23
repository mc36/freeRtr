Type
  OneDCCPheaderStdSngRecord=record
    src:Word;                   {source port}
    trg:Word;                   {target port}
    datOfs:Byte;                {data offset}
    ccCS:byte;                  {ccval, cscov}
    chkSum:Word;                {checksum}
    typ:Byte;                   {type}
    seq:array[1..3] of byte;    {sequence}
    end;
  OneDCCPheaderStdDblRecord=record
    src:Word;                   {source port}
    trg:Word;                   {target port}
    datOfs:Byte;                {data offset}
    ccCS:byte;                  {ccval, cscov}
    chkSum:Word;                {checksum}
    typ:Byte;                   {type}
    seq:array[1..3] of byte;    {sequence}
    res1:Byte;
    ack:array[1..3] of byte;    {acknowledge}
    end;
  OneDCCPheaderExtSngRecord=record
    src:Word;                   {source port}
    trg:Word;                   {target port}
    datOfs:Byte;                {data offset}
    ccCS:byte;                  {ccval, cscov}
    chkSum:Word;                {checksum}
    typ:Byte;                   {type}
    res1:Byte;
    seqHi:Word;                 {sequence high}
    seqLo:LongInt;              {sequence low}
    end;
  OneDCCPheaderExtDblRecord=record
    src:Word;                   {source port}
    trg:Word;                   {target port}
    datOfs:Byte;                {data offset}
    ccCS:byte;                  {ccval, cscov}
    chkSum:Word;                {checksum}
    typ:Byte;                   {type}
    res1:Byte;
    seqHi:Word;                 {sequence high}
    seqLo:LongInt;              {sequence low}
    res2:Word;
    ackHi:Word;                 {acknowledge high}
    ackLo:LongInt;              {acknowledge low}
    end;


Function decodeOneTypeCode(i:LongInt):String;
Var a:String;
Begin;
case i of
  0:a:='request';
  1:a:='response';
  2:a:='data';
  3:a:='ack';
  4:a:='dataack';
  5:a:='closereq';
  6:a:='close';
  7:a:='reset';
  8:a:='sync';
  9:a:='syncack';
  else a:='unknown:'+BStr(i);
  end;
decodeOneTypeCode:=a;
End;

Function decodeResetCode(i:LongInt):String;
Var a:String;
Begin;
case i of
  0:a:='unspecified';
  1:a:='closed';
  2:a:='aborted';
  3:a:='no connection';
  4:a:='packet error';
  5:a:='option error';
  6:a:='mandatory error';
  7:a:='connection refused';
  8:a:='bad service code';
  9:a:='too busy';
  10:a:='bad init cookie';
  11:a:='aggression penalty';
  else a:='unknown:'+BStr(i);
  end;
decodeResetCode:=a;
End;

Function decodeOptionCode(i:LongInt):String;
Var a:String;
Begin;
case i of
   0:a:='padding';
   1:a:='mandatory';
   2:a:='slow receiver';
  32:a:='change l';
  33:a:='confirm l';
  34:a:='change r';
  35:a:='confirm r';
  36:a:='init cookie';
  37:a:='ndp count';
  38:a:='ack vector';
  39:a:='ack vector';
  40:a:='data dropped';
  41:a:='timestamp';
  42:a:='timestamp echo';
  43:a:='elapsed time';
  44:a:='data checksum';
  else a:='unknown:'+BStr(i);
  end;
decodeOptionCode:=a;
End;

Function decodeFeatureCode(i:LongInt):String;
Var a:String;
Begin;
case i of
  1:a:='congestion control id';
  2:a:='allow short seqnos';
  3:a:='sequence window';
  4:a:='ecn incapable';
  5:a:='ack ratio';
  6:a:='send ack vector';
  7:a:='send ndp count';
  8:a:='minimum checksum coverage';
  9:a:='check data checksum';
  else a:='unknown:'+BStr(i);
  end;
decodeFeatureCode:=a;
End;

Function getOneOption(var a:String):String;
Var
  b:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  bb:array[0..1] of byte absolute b;
  bb0:byte absolute b;
Begin;
getOneOption:='';
if (ab0<1) then exit;
bb[1]:=ab[1];
{$ifdef debug}writeLn('  '+decodeOptionCode(bb[1]));{$endif}
a:=copy(a,2,666);
if (bb[1] and $1f=bb[1]) then begin;
  bb0:=1;
  getOneOption:=b;
  exit;
  end;
bb0:=ab[1]-1;
move(ab[2],bb[2],bb0-1);
a:=copy(a,bb0+1,666);
getOneOption:=b;
End;

Procedure putOneOption(var a:String;b:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  bb:array[0..1] of byte absolute b;
  bb0:byte absolute b;
Begin;
{$ifdef debug}writeln('  '+decodeOptionCode(bb[1]));{$endif}
inc(ab0);
ab[ab0]:=bb[1];
if (bb[1] and $1f=bb[1]) then exit;
inc(ab0);
ab[ab0]:=bb0+1;
a:=a+copy(b,2,666);
End;

Procedure sendOneDCCPpacket(var con:OneConnectionRecord;opt:String;var dat;typ,siz:LongInt); {type: $100-ext, $200-ack}
Var
  buf:array[1..1] of byte absolute dat;
  hdrSS:OneDCCPheaderStdSngRecord absolute dat;
  hdrSD:OneDCCPheaderStdDblRecord absolute dat;
  hdrES:OneDCCPheaderExtSngRecord absolute dat;
  hdrED:OneDCCPheaderExtDblRecord absolute dat;
  ab:array[0..1] of byte absolute opt;
  ab0:byte absolute opt;
  i,o,p:LongInt;
Begin;
{$ifdef debug}writeLn('tx '+ipAddr2string(con.addr)+' '+BStr(con.portR)+' '+BStr(con.portL)+' '+decodeOneTypeCode(typ and $ff));{$endif}
case typ and $300 of
  $000:p:=sizeof(hdrSS);
  $200:p:=sizeof(hdrSD);
  $100:p:=sizeof(hdrES);
  $300:p:=sizeof(hdrED);
  else exit;
  end;
while (ab0 and 3<>0) do begin; inc(ab0);ab[ab0]:=0; end;
move(buf,buf[p+ab0+1],siz);
inc(siz,p+ab0);
move(ab[1],buf[p+1],ab0);
fillchar(buf,p,0);
case typ and $300 of
  $000:begin; {standard, single}
    WriteLongMSB(hdrSS.typ,con.seqTa);
    o:=0;
    end;
  $200:begin; {standard, double}
    WriteLongMSB(hdrSD.typ,con.seqTa);
    WriteLongMSB(hdrSD.res1,con.seqRa);
    hdrSD.res1:=0;
    o:=0;
    end;
  $100:begin; {extended, single}
    WriteWordMSB(hdrES.seqHi,con.seqTb);
    WriteLongMSB(hdrES.seqLo,con.seqTa);
    o:=1;
    end;
  $300:begin; {extended, double}
    WriteWordMSB(hdrED.seqHi,con.seqTb);
    WriteLongMSB(hdrED.seqLo,con.seqTa);
    WriteWordMSB(hdrED.ackHi,con.seqRb);
    WriteLongMSB(hdrED.ackLo,con.seqRa);
    o:=1;
    end;
  else exit;
  end;
hdrSS.datOfs:=(p+ab0) shr 2;
hdrSS.typ:=(typ shl 1)+o;
WriteWordMSB(hdrSS.src,con.portL);
WriteWordMSB(hdrSS.trg,con.portR);
hdrSS.ccCS:=0;
hdrSS.chkSum:=0;
hdrSS.chkSum:=not TestPseudoSum(localAddr,con.addr,buf,ipProto,siz);
inc(con.seqTa);
if (con.seqTa=0) then inc(con.seqTb);
UDPsendPacket(localPipe,con.addr,0,buf,siz);
End;
