Type
  OneLDPmessageHeader=record
    ver:Word;                   {version number}
    len:Word;                   {size of message}
    lsr:LongInt;                {router id}
    lsp:Word;                   {label space}
    end;
  OneMessageHeader=record
    typ:Word;                   {type of message}
    len:Word;                   {size of message}
    mid:LongInt;                {message id}
    end;
Const
  labelLocalAddr=0;
  labelConnection=100;


Function decodeOneMessageType(i:LongInt):String;
Var a:String;
Begin;
case i of
  $0001:a:='notification';
  $0100:a:='hello';
  $0200:a:='initialization';
  $0201:a:='keepalive';
  $0300:a:='address';
  $0301:a:='address withdrawal';
  $0400:a:='label mapping';
  $0401:a:='label request';
  $0402:a:='label withdrawal';
  $0403:a:='label release';
  $0404:a:='label abort request';
  $3e00..$3eff:a:='vendor private';
  $3f00..$3fff:a:='experimental message';
  else a:='unknown '+byte2hextype(i shr 8)+byte2hextype(i);
  end;
decodeOneMessageType:=a;
End;

Function decodeOneTLVtype(i:LongInt):String;
Var a:String;
Begin;
case i of
  $0100:a:='fec';
  $0101:a:='address list';
  $0103:a:='hop count';
  $0104:a:='path vector';
  $0200:a:='generic label';
  $0201:a:='atm label';
  $0202:a:='frame label';
  $0203:a:='ft protection';
  $0300:a:='status';
  $0301:a:='extended status';
  $0302:a:='returned pdu';
  $0303:a:='returned message';
  $0400:a:='common hello parms';
  $0401:a:='ipv4 transport address';
  $0402:a:='configuration seqno';
  $0403:a:='ipv6 transport address';
  $0404:a:='mac';
  $0500:a:='common session parms';
  $0501:a:='atm session parms';
  $0502:a:='frame relay session parms';
  $0503:a:='ft session';
  $0504:a:='ft ack';
  $0505:a:='ft cork';
  $0600:a:='label request message id';
  $0800:a:='er';
  $0801:a:='er hop ipv4';
  $0802:a:='er hop ipv6';
  $0803:a:='er hop as';
  $0804:a:='er hop lspid';
  $0810:a:='traffic param';
  $0820:a:='preemption';
  $0821:a:='lspid';
  $0822:a:='resource class';
  $0823:a:='route pinning';
  $0901:a:='diffserv';
  $3E00..$3EFF:a:='vendor private end';
  $3F00..$3FFF:a:='experimental end';
  else a:='unknown '+byte2hextype(i shr 8)+byte2hextype(i);
  end;
decodeOneTLVtype:=a;
End;



Function getOneTLV(var buffer;var ps,typ:LongInt):String;
Const max=255;
Var
  buf:array[1..1] of byte absolute buffer;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
Begin;
typ:=ReadWordMSB(buf[ps+1]);
o:=ReadWordMSB(buf[ps+3]);
if (o>max) then ab0:=max else ab0:=o;
move(buf[ps+5],ab[1],ab0);
inc(ps,o);
inc(ps,4);
getOneTLV:=a;
{$ifdef debug}
Write('tlv '+decodeOneTLVtype(typ));
for i:=1 to ab0 do write(' '+byte2hextype(ab[i]));
WriteLn('');
{$endif}
End;

Procedure addOneTLV(var buffer;var ps:LongInt;t:LongInt;a:String);
Var
  buf:array[1..1] of byte absolute buffer;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
Begin;
{$ifdef debug}
Write('tlv '+decodeOneTLVtype(t));
for i:=1 to ab0 do write(' '+byte2hextype(ab[i]));
WriteLn('');
{$endif}
WriteWordMSB(buf[ps+1],t);
WriteWordMSB(buf[ps+3],ab0);
move(ab[1],buf[ps+5],ab0);
inc(ps,ab0);
inc(ps,4);
End;



Procedure addOneMsgHeader(var buffer;var ps:LongInt;typ,mid:LongInt);
Var
  buf:array[1..1] of byte absolute buffer;
  hdr:OneMessageHeader absolute buffer;
Begin;
{$ifdef debug}
WriteLn('msg '+decodeOneMessageType(typ));
{$endif}
move(buf,buf[sizeof(hdr)+1],ps);
fillchar(hdr,sizeof(hdr),0);
WriteWordMSB(hdr.typ,typ);
WriteWordMSB(hdr.len,ps+4);
WriteLongMSB(hdr.mid,mid);
inc(ps,sizeof(hdr));
End;
