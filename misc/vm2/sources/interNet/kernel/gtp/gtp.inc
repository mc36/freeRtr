Type
  OneGTPpacketHeaderRecord=record
    flg:Byte;                   {flags: vvvt0esn}
    typ:Byte;                   {message type}
    len:Word;                   {message length -8}
    tei:LongInt;                {teid}
    seq:Word;                   {sequence number}
    npd:Byte;                   {npdu number}
    nxt:Byte;                   {next header}
    end;
Const
  TypeLengthValueSizes:array[0..127] of byte=(
{  0}  0,
{  1}  2,  {cause}
{  2}  9,  {international mobile subscriber identity}
{  3}  7,  {routeing area identity}
{  4}  5,  {temporary logical link identity}
{  5}  5,  {packet tmsi}
{  6}  0,
{  7}  0,
{  8}  2,  {reordering required}
{  9}  29, {authentication triplet}
{ 10}  0,
{ 11}  2,  {map cause}
{ 12}  4,  {p-tmsi signature}
{ 13}  2,  {ms validated}
{ 14}  2,  {recovery}
{ 15}  2,  {selection mode}
{ 16}  5,  {tunnel endpoint identifier data i}
{ 17}  5,  {tunnel endpoint identifier control plane}
{ 18}  6,  {tunnel endpoint identifier data ii}
{ 19}  2,  {teardown ind}
{ 20}  2,  {nsapi}
{ 21}  2,  {ranap cause}
{ 22}  10, {rab context}
{ 23}  2,  {radio priority sms}
{ 24}  2,  {radio priority}
{ 25}  3,  {packet flow id}
{ 26}  3,  {charging characteristics}
{ 27}  3,  {trace reference}
{ 28}  3,  {trace type}
{ 29}  2,  {ms not reachable reason}
{ 30}  0,
{ 31}  0,
{ 32}  0,
{ 33}  0,
{ 34}  0,
{ 35}  0,
{ 36}  0,
{ 37}  0,
{ 38}  0,
{ 39}  0,
{ 40}  0,
{ 41}  0,
{ 42}  0,
{ 43}  0,
{ 44}  0,
{ 45}  0,
{ 46}  0,
{ 47}  0,
{ 48}  0,
{ 49}  0,
{ 50}  0,
{ 51}  0,
{ 52}  0,
{ 53}  0,
{ 54}  0,
{ 55}  0,
{ 56}  0,
{ 57}  0,
{ 58}  0,
{ 59}  0,
{ 60}  0,
{ 61}  0,
{ 62}  0,
{ 63}  0,
{ 64}  0,
{ 65}  0,
{ 66}  0,
{ 67}  0,
{ 68}  0,
{ 69}  0,
{ 70}  0,
{ 71}  0,
{ 72}  0,
{ 73}  0,
{ 74}  0,
{ 75}  0,
{ 76}  0,
{ 77}  0,
{ 78}  0,
{ 79}  0,
{ 80}  0,
{ 81}  0,
{ 82}  0,
{ 83}  0,
{ 84}  0,
{ 85}  0,
{ 86}  0,
{ 87}  0,
{ 88}  0,
{ 89}  0,
{ 90}  0,
{ 91}  0,
{ 92}  0,
{ 93}  0,
{ 94}  0,
{ 95}  0,
{ 96}  0,
{ 97}  0,
{ 98}  0,
{ 99}  0,
{100}  0,
{101}  0,
{102}  0,
{103}  0,
{104}  0,
{105}  0,
{106}  0,
{107}  0,
{108}  0,
{109}  0,
{110}  0,
{111}  0,
{112}  0,
{113}  0,
{114}  0,
{115}  0,
{116}  0,
{117}  0,
{118}  0,
{119}  0,
{120}  0,
{121}  0,
{122}  0,
{123}  0,
{124}  0,
{125}  0,
{126}  0,
{127}  5   {charging id}
  );

Function getMessageTypeName(i:LongInt):String;
Var a:String;
Begin;
case i of
  1:a:='echo request';
  2:a:='echo response';
  3:a:='version not supported';
  4:a:='node alive request';
  5:a:='node alive response';
  6:a:='redirection request';
  7:a:='redirection response';
  16:a:='create pdp context request';
  17:a:='create pdp context response';
  18:a:='update pdp context request';
  19:a:='update pdp context response';
  20:a:='delete pdp context request';
  21:a:='delete pdp context response';
  22:a:='initiate pdp context activation request';
  23:a:='initiate pdp context activation response';
  26:a:='error indication';
  27:a:='pdu notification request';
  28:a:='pdu notification response';
  29:a:='pdu notification reject request';
  30:a:='pdu notification reject response';
  31:a:='supported extension headers notification';
  32:a:='send routeing information for gprs request';
  33:a:='send routeing information for gprs response';
  34:a:='failure report request';
  35:a:='failure report response';
  36:a:='note ms gprs present request';
  37:a:='note ms gprs present response';
  48:a:='identification request';
  49:a:='identification response';
  50:a:='sgsn context request';
  51:a:='sgsn context response';
  52:a:='sgsn context acknowledge';
  53:a:='forward relocation request';
  54:a:='forward relocation response';
  55:a:='forward relocation complete';
  56:a:='relocation cancel request';
  57:a:='relocation cancel response';
  58:a:='forward srns context';
  59:a:='forward relocation complete acknowledge';
  60:a:='forward srns context acknowledge';
  70:a:='ran information relay';
  96:a:='mbms notification request';
  97:a:='mbms notification response';
  98:a:='mbms notification reject request';
  99:a:='mbms notification reject response';
  100:a:='create mbms context request';
  101:a:='create mbms context response';
  102:a:='update mbms context request';
  103:a:='update mbms context response';
  104:a:='delete mbms context request';
  105:a:='delete mbms context response';
  112:a:='mbms registration request';
  113:a:='mbms registration response';
  114:a:='mbms de-registration request';
  115:a:='mbms de-registration response';
  116:a:='mbms session start request';
  117:a:='mbms session start response';
  118:a:='mbms session stop request';
  119:a:='mbms session stop response';
  120:a:='mbms session update request';
  121:a:='mbms session update response';
  128:a:='ms info change notification request';
  129:a:='ms info change notification response';
  240:a:='data record transfer request';
  241:a:='data record transfer response';
  255:a:='g-pdu';
  else a:='unknown#'+bstr(i);
  end;
getMessageTypeName:=a;
End;

Function getTypeLengthValueName(i:LongInt):String;
Var a:String;
Begin;
case i of
  1:a:='cause';
  2:a:='international mobile subscriber identity';
  3:a:='routeing area identity';
  4:a:='temporary logical link identity';
  5:a:='packet tmsi';
  8:a:='reordering required';
  9:a:='authentication triplet';
  11:a:='map cause';
  12:a:='p-tmsi signature';
  13:a:='ms validated';
  14:a:='recovery';
  15:a:='selection mode';
  16:a:='tunnel endpoint identifier data i';
  17:a:='tunnel endpoint identifier control plane';
  18:a:='tunnel endpoint identifier data ii';
  19:a:='teardown ind';
  20:a:='nsapi';
  21:a:='ranap cause';
  22:a:='rab context';
  23:a:='radio priority sms';
  24:a:='radio priority';
  25:a:='packet flow id';
  26:a:='charging characteristics';
  27:a:='trace reference';
  28:a:='trace type';
  29:a:='ms not reachable reason';
  127:a:='charging id';
  128:a:='end user address';
  129:a:='mm context';
  130:a:='pdp context';
  131:a:='access point name';
  132:a:='protocol configuration options';
  133:a:='gsn address';
  134:a:='ms international pstn/isdn number';
  135:a:='quality of service profile';
  136:a:='authentication quintuplet';
  137:a:='traffic flow template';
  138:a:='target identification';
  139:a:='utran transparent container';
  140:a:='rab setup information';
  141:a:='extension header type list';
  142:a:='trigger id';
  143:a:='omc identity';
  144:a:='ran transparent container';
  145:a:='pdp context prioritization';
  146:a:='additional rab setup information';
  147:a:='sgsn number';
  148:a:='common flags';
  149:a:='apn restriction';
  150:a:='radio priority lcs';
  151:a:='rat type';
  152:a:='user location information';
  153:a:='ms time zone';
  154:a:='imei';
  155:a:='camel charging information container';
  156:a:='mbms ue context';
  157:a:='temporary mobile group identity';
  158:a:='rim routing address';
  159:a:='mbms protocol configuration options';
  160:a:='mbms service area';
  161:a:='source rnc pdcp context info';
  162:a:='additional trace info';
  163:a:='hop counter';
  164:a:='selected plmn id';
  165:a:='mbms session identifier';
  166:a:='mbms 2g/3g indicator';
  167:a:='enhanced nsapi';
  168:a:='mbms session duration';
  169:a:='additional mbms trace info';
  170:a:='mbms session repetition number';
  171:a:='mbms time to data transfer';
  172:a:='ps handover request context';
  173:a:='bss container';
  174:a:='cell identification';
  175:a:='pdu numbers';
  176:a:='bssgp cause';
  177:a:='required mbms bearer capabilities';
  178:a:='rim routing address discriminator';
  179:a:='list of set-up pfcs';
  180:a:='ps handover xid parameters';
  181:a:='ms info change reporting action';
  182:a:='direct tunnel flags';
  183:a:='correlation-id';
  184:a:='bearer control mode';
  251:a:='charging gateway address';
  255:a:='private extension';
  else a:='unknown#'+bstr(i);
  end;
getTypeLengthValueName:=a;
End;

Function getCauseCodeName(i:LongInt):String;
Var a:String;
Begin;
case i of
  0:a:='request imsi';
  1:a:='request imei';
  2:a:='request imsi and imei';
  3:a:='no identity needed';
  4:a:='ms refuses';
  5:a:='ms is not gprs responding';
  128:a:='request accepted';
  192:a:='non-existent';
  193:a:='invalid message format';
  194:a:='imsi not known';
  195:a:='ms is gprs detached';
  196:a:='ms is not gprs responding';
  197:a:='ms refuses';
  198:a:='version not supported';
  199:a:='no resources available';
  200:a:='service not supported';
  201:a:='mandatory ie incorrect';
  202:a:='mandatory ie missing';
  203:a:='optional ie incorrect';
  204:a:='system failure';
  205:a:='roaming restriction';
  206:a:='p-tmsi signature mismatch';
  207:a:='gprs connection suspended';
  208:a:='authentication failure';
  209:a:='user authentication failed';
  210:a:='context not found';
  211:a:='all dynamic pdp addresses are occupied';
  212:a:='no memory is available';
  213:a:='relocation failure';
  214:a:='unknown mandatory extension header';
  215:a:='semantic error in the tft operation';
  216:a:='syntactic error in the tft operation';
  217:a:='semantic errors in packet filter(s)';
  218:a:='syntactic errors in packet filter(s)';
  219:a:='missing or unknown apn';
  220:a:='unknown pdp address or pdp type';
  221:a:='pdp context without tft already activated';
  222:a:='apn access denied æ no subscription';
  223:a:='apn restriction type incompatibility with currently active pdp contexts';
  224:a:='ms mbms capabilities insufficient';
  225:a:='invalid correlation-id';
  226:a:='mbms bearer context superseded';
  else a:='unknown#'+bstr(i);
  end;
getCauseCodeName:=a;
End;

Procedure dumpOneBuffer(a:String;var buffer;size:LongInt);
Var
  buf:array[1..1] of byte absolute buffer;
  i:LongInt;
Begin;
Write(a+' ('+Bstr(size)+')');
for i:=1 to size do write(' '+byte2hextype(buf[i]));
WriteLn('');
End;



Function getOneTLVdata(var buffer;var ps:LongInt;var a:String):LongInt;
Var
  buf:array[1..1] of byte absolute buffer;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p:LongInt;
Begin;
getOneTLVdata:=-1;
inc(ps);
p:=buf[ps];
if (p and $7f=p) then begin;
  o:=TypeLengthValueSizes[p]-1;
  if (o<0) then exit;
  end else begin;
  o:=ReadWordMSB(buf[ps+1]);
  if (o and $3ff<>o) then exit;
  inc(ps,2);
  end;
ab0:=o;
move(buf[ps+1],ab[1],ab0);
inc(ps,o);
getOneTLVdata:=p;
End;

Procedure putOneTLVdata(var buffer;var ps:LongInt;t:LongInt;a:String);
Var
  buf:array[1..1] of byte absolute buffer;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p:LongInt;
Begin;
inc(ps);
buf[ps]:=t;
if (t and $7f=t) then begin;
  o:=TypeLengthValueSizes[t]-1;
  if (o<0) then exit;
  end else begin;
  o:=ab0;
  WriteWordMSB(buf[ps+1],o);
  inc(ps,2);
  end;
move(ab[1],buf[ps+1],o);
inc(ps,o);
End;

Function decodeOneTelNumber(a:String):String;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  b:String;
  i,o,p:LongInt;
Begin;
b:='';
for p:=1 to ab0 do begin;
  o:=ab[p];
  if (o and $f<10) then b:=b+BStr(o and $f);
  if (o shr 4<10) then b:=b+BStr(o shr 4);
  end;
decodeOneTelNumber:=b;
End;

Function encodeOneTelNumber(a:String):String;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p:LongInt;
Begin;
while (ab0 and 1<>0) do a:=a+'?';
for p:=1 to ab0 shr 1 do begin;
  o:=(ab[p*2] and $f) shl 4;
  inc(o,ab[p*2-1] and $f);
  ab[p]:=o;
  end;
ab0:=ab0 shr 1;
encodeOneTelNumber:=a;
End;


Procedure sendEchoRequest(var addr;port,seq:LongInt);
Var
  buf:array[1..1024] of byte;
  hdr:OneGTPpacketHeaderRecord absolute buf;
Begin;
hdr.flg:=$32;
hdr.typ:=$01;
WriteWordMSB(hdr.len,sizeof(hdr)-8);
hdr.tei:=0;
WriteWordMSB(hdr.seq,seq);
hdr.npd:=0;
hdr.nxt:=0;
UDPsendPacket(ctrlPip,addr,port,buf,sizeof(hdr));
End;
