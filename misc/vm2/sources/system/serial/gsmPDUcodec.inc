Type gsmPDUcodecRecord=record
  d:array[1..4*1024] of byte;
  s:LongInt;
  p:LongInt;
  end;


Const gsmPDUmode1alphabet=
   '@ú$ùäÇóçïÄ'#10'ÌÌ'#13'èÜ_Ë‚‡Í„Á‰È'#255'íë·ê'+
  +' !"#'#9'%&''()*+,-./0123456789:;<=>?'+
  +'≠ABCDEFGHIJKLMNOPQRSTUVWXYZéô•ö'+
  +'®abcdefghijklmnopqrstuvwxyzÑî§ÅÖ';

Const gsmPDUmode2alphabet=
   '                   ^           '+
  +'        {}     \            [~] '+
  +'|                               '+
  +'     e                          ';



FUNCTION gsmPDUcodecDecodeOne(var bin,txt:gsmPDUcodecRecord;Received:Byte):Boolean;
Const bitNames:array[0..1] of string[3]=('no','yes');
Var coding:Byte; {0=7bit, 1=8bit, 2=16bit; bit7=compression}

function getCh:Integer;
begin;
if (bin.p>=bin.s) then begin; getCh:=-1;exit; end;
inc(bin.p);
getCh:=bin.d[bin.p];
end;

procedure putLn(a:string);
begin;
move(a[1],txt.d[txt.s+1],length(a));
inc(txt.s,length(a));
a:=#13#10;
move(a[1],txt.d[txt.s+1],length(a));
inc(txt.s,length(a));
end;

Function GetPhoneNum(scnum:boolean):String;
Var
  siz,typ:Integer;
  a:String;
  i,o:Integer;

procedure x(n:byte);
var c:char;
begin;
case n of
  0..9:c:=chr($30+n);
  10:c:='*';
  11:c:='#';
  12:c:='a';
  13:c:='b';
  14:c:='c';
  15:exit;
  else exit;
  end;
a:=a+c;
end;

Begin;
GetPhoneNum:='';
siz:=getCh;
if (siz<1) then exit;
typ:=getCh;
if (typ<0) then exit;
a:='';
if scnum then dec(siz) else siz:=(siz+1) shr 1;
for o:=1 to siz do begin;
  i:=getCh;
  if (i<0) then exit;
  x(i and 15);
  x(i shr 4);
  end;
i:=typ and 15;
case i of
  0:a:='unknown '+a;
  1:a:='telephone '+a;
  3:a:='data '+a;
  4:a:='telex '+a;
  8:a:='national '+a;
  9:a:='private '+a;
  10:a:='ermes '+a;
  else a:='reserved '+a;
  end;
i:=(typ shr 4) and 7;
case i of
  1:a:='international '+a;
  2:a:='national '+a;
  3:a:='network '+a;
  4:a:='subscriber '+a;
  5:a:='alphanumeric '+a;
  6:a:='abbreviated '+a;
  else a:='reserved '+a;
  end;
GetPhoneNum:=a;
End;

Function GetProtoId:String;
Label f1;
Var
  i,o:Integer;
  a:String;
Begin;
GetProtoId:='';
o:=getCh;
if (o<0) then exit;
a:='?';
i:=o shr 6;
case i of
  0:begin;
    if (o and $20=0) then begin; a:='sme2sme';goto f1; end;
    i:=o and $1f;
    case i of
      0:a:='implicit';
      1:a:='telex';
      2:a:='fax g3';
      3:a:='fax g4';
      4:a:='voice';
      5:a:='ermes';
      6:a:='paging';
      7:a:='video';
      8..12:a:='teletex';
      13:a:='uci';
      16:a:='message';
      17:a:='x400';
      18:a:='email';
      31:a:='mobile';
      else a:='reserved';
      end;
    end;
  1:begin;
    i:=o and $3f;
    case i of
      0:a:='smt0';
      1:a:='smt1';
      2:a:='smt2';
      3:a:='smt3';
      4:a:='smt4';
      5:a:='smt5';
      6:a:='smt6';
      7:a:='smt7';
      31:a:='return-call-message';
      61:a:='data-dl';
      62:a:='de-personalization';
      63:a:='sim-data';
      else a:='reserved';
      end;
    end;
  3:a:='sc-specific';
  else a:='reserved';
  end;
f1:
GetProtoId:=a;
End;

Function GetCodingId:String;
Var
  i,o:Integer;
  a:String;
Begin;
GetCodingId:='';
coding:=0;
o:=getCh;
if (o<0) then exit;
a:='?';
i:=o shr 4;
case i of
  0..3:begin;
    i:=(o shr 2) and 3;
    case i of
      0:begin; coding:=0;a:='7bit'; end;
      1:begin; coding:=1;a:='8bit'; end;
      2:begin; coding:=2;a:='16bit'; end;
      3:a:='reserved';
      end;
    a:='general '+a;
    if (o and $20<>0) then begin;
      a:='compressed '+a;
      coding:=coding or $80;
      end;
    i:=o and 3;
    if (o and $10<>0) then case i of
      0:a:=a+' alert';
      1:a:=a+' me-data';
      2:a:=a+' sim-data';
      3:a:=a+' te-data';
      end;
    end;
  12,13:begin;
    coding:=0;
    if (o and 8=0) then a:='inactive' else a:='active';
    a:='waiting 7bit '+a;
    i:=o and 3;
    case i of
      0:a:=a+' voicemail';
      1:a:=a+' fax message';
      2:a:=a+' email';
      3:a:=a+' other';
      end;
    end;
  14:begin;
    coding:=2;
    if (o and 8=0) then a:='inactive' else a:='active';
    a:='waiting 16bit '+a;
    i:=o and 3;
    case i of
      0:a:=a+' voicemail';
      1:a:=a+' fax message';
      2:a:=a+' email';
      3:a:=a+' other';
      end;
    end;
  15:begin;
    if (o and 4=0) then
     begin; coding:=0;a:='7bit'; end
     else
     begin; coding:=1;a:='8bit'; end;
    a:='message '+a;
    i:=o and 3;
    case i of
      0:a:=a+' alert';
      1:a:=a+' me-data';
      2:a:=a+' sim-data';
      3:a:=a+' te-data';
      end;
    end;
  else a:='reserved';
  end;
GetCodingId:=a;
End;

Function GetTimeStamp:String;
function y(i:byte):byte;begin; y:=(i and 15)*10+(i shr 4); end;
function x(i:byte):string;begin; x:=BStr(i div 10)+BStr(i mod 10); end;
Var
  d:array[1..7] of Byte;
  i,o:Integer;
  a:String;
Begin;
GetTimeStamp:='';
for o:=1 to 7 do begin;
  i:=getCh;
  if (i<0) then exit;
  d[o]:=i;
  end;
i:=y(d[1]);
a:=x(i);
if (i<5) then a:='20'+a else a:='19'+a;
a:=a+'-'+x(y(d[2]));
a:=a+'-'+x(y(d[3]));
a:=a+' '+x(y(d[4]));
a:=a+':'+x(y(d[5]));
a:=a+':'+x(y(d[6]))+' ';
o:=d[7];
if (o and $80=0) then a:=a+'+' else a:=a+'-';
i:=(o and $7f)*15;
a:=a+x(i div 60)+x(i mod 60);
GetTimeStamp:=a;
End;

Function GetUserData:String;
Label f1,f2;
Var
  saw,siz:Integer;
  bufDat:LongInt;
  bufLen:Byte;
  i:Integer;
  a,b:String;
  c:String[7];
Begin;
GetUserData:='';
siz:=getCh;
if (siz<0) then exit;
case coding and $7f of
  0:siz:=siz;
  1:siz:=siz;
  2:siz:=siz shr 1;
  else exit;
  end;
bufLen:=0;
bufDat:=0;
saw:=0;
a:='';
f1:
if (saw>=siz) then goto f2;
case coding and $7f of
  0:if (bufLen>=7) then begin;
    i:=bufDat and $7f;
    a:=a+chr(i);
    bufDat:=bufDat shr 7;
    dec(bufLen,7);
    inc(saw);
    goto f1;
    end;
  1:if (bufLen>=8) then begin;
    a:=a+chr(bufDat);
    bufDat:=bufDat shr 8;
    dec(bufLen,8);
    inc(saw);
    goto f1;
    end;
  2:if (bufLen>=16) then begin;
    a:=a+chr(bufDat)+chr(bufDat shr 8);
    bufDat:=bufDat shr 16;
    dec(bufLen,16);
    inc(saw);
    goto f1;
    end;
  else exit;
  end;
i:=getCh;
if (i<0) then exit;
inc(bufDat,i shl bufLen);
inc(bufLen,8);
goto f1;
f2:
b:='';
case coding and $7f of
  0:while (a<>'') do begin;
    i:=ord(a[1]) and $7f;
    a:=copy(a,2,255);
    c:=copy(gsmPDUmode1alphabet,i+1,1);
    if (i=27) then begin;
      i:=ord(a[1]) and $7f;
      a:=copy(a,2,255);
      c:=copy(gsmPDUmode2alphabet,i+1,1);
      end;
    b:=b+c;
    end;
  1:b:=a;
  2:b:=a;
  else exit;
  end;
GetUserData:=b;
End;

Function GetValidPeriod(i:LongInt):String;
Var a:String;

function decTPVP(i:longint):longint;
begin;
case i of
  0..143:i:=(i+1)*5;
  144..167:i:=(i-143)*30;
  168..196:i:=(i-166)*60*24;
  197..255:i:=(i-192)*60*24*7;
  end;
decTPVP:=i;
end;

function decBCD(i:byte):string;
begin;
decBCD:=BStr(i mod 16)+BStr(i div 16);
end;

Begin;
GetValidPeriod:='';
a:='?';
case i of
  0:a:='absent';
  1:begin;
    i:=getCh;
    if (i<0) then exit;
    i:=decTPVP(i);
    a:=BStr(i)+' minutes';
    end;
  2:begin;
    i:=getCh;
    if (i<0) then exit;
    i:=i and 7;
    case i of
      0:a:='absent';
      1:begin;
        i:=getCh;
        if (i<0) then exit;
        i:=decTPVP(i);
        a:=BStr(i)+' minutes';
        end;
      2:begin;
        i:=getCh;
        if (i<0) then exit;
        a:=BStr(i)+' seconds';
        end;
      3:begin;
        i:=getCh;
        if (i<0) then exit;
        a:='+'+decBCD(i);
        i:=getCh;
        if (i<0) then exit;
        a:=a+':'+decBCD(i);
        i:=getCh;
        if (i<0) then exit;
        a:=a+':'+decBCD(i);
        end;
      else a:='reserved';
      end;
    end;
  3:a:=GetTimeStamp;
  else a:='reserved';
  end;
GetValidPeriod:=a;
End;

Function GetReferenceNum:String;
Var i:Integer;
Begin;
GetReferenceNum:='';
i:=getCh;
if (i<0) then exit;
GetReferenceNum:=BStr(i);
End;

Function GetCommandId:String;
Var
  i:Integer;
  a:String;
Begin;
GetCommandId:='';
i:=getCh;
if (i<0) then exit;
case i of
  0:a:='enquiry';
  1:a:='cancel statusReport';
  2:a:='delete message';
  3:a:='request statusReport';
  224..255:a:='sc-specific';
  else a:='reserved';
  end;
GetCommandId:=a;
End;



Var
  a:String;
  i,o:Integer;
BEGIN;
gsmPDUcodecDecodeOne:=True;
fillchar(txt,sizeof(txt),0);
bin.p:=0;
received:=received and 1;
if (received=0) then a:='ms->sc' else a:='sc->ms';
putLn('direction: '+a);
a:=GetPhoneNum(true);
putLn('serviceCenter: '+a);
o:=getCh;
if (o<0) then exit;
i:=o and 3;
if (received=0) then case i of
  0:a:='deliver-report';
  1:a:='submit';
  2:a:='command';
  else a:='reserved';
  end else case i of
  0:a:='deliver';
  1:a:='submit-report';
  2:a:='status-report';
  else a:='reserved';
  end;
putLn('messageType: '+a);
i:=(received shl 8) or i;
coding:=0;
case i of
  $000:begin; {deliver-report}
    i:=(o shr 6) and 1;
    a:=bitNames[i];
    putLn('userHeader: '+a);
    i:=getCh;
    a:=GetProtoId;
    putLn('protocol: '+a);
    a:=GetCodingId;
    putLn('coding: '+a);
    a:=GetUserData;
    putLn('--- '+BStr(length(a))+' bytes');
    putLn(a);
    end;
  $001:begin; {submit}
    i:=(o shr 2) and 1;
    a:=bitNames[i];
    putLn('rejectDuplicate: '+a);
    i:=(o shr 5) and 1;
    a:=bitNames[i];
    putLn('statusReport: '+a);
    i:=(o shr 6) and 1;
    a:=bitNames[i];
    putLn('userHeader: '+a);
    i:=(o shr 7) and 1;
    a:=bitNames[i];
    putLn('replyPath: '+a);
    o:=(o shr 3) and 3;
    a:=GetReferenceNum;
    putLn('referenceNum: '+a);
    a:=GetPhoneNum(false);
    putLn('phoneNumber: '+a);
    a:=GetProtoId;
    putLn('protocol: '+a);
    a:=GetCodingId;
    putLn('coding: '+a);
    a:=GetValidPeriod(o);
    putLn('validityPeriod: '+a);
    a:=GetUserData;
    putLn('--- '+BStr(length(a))+' bytes');
    putLn(a);
    end;
  $002:begin; {command}
    i:=(o shr 5) and 1;
    a:=bitNames[i];
    putLn('statusReport: '+a);
    i:=(o shr 6) and 1;
    a:=bitNames[i];
    putLn('userHeader: '+a);
    a:=GetReferenceNum;
    putLn('referenceNum2: '+a);
    a:=GetProtoId;
    putLn('protocol: '+a);
    a:=GetCommandId;
    putLn('command: '+a);
    a:=GetReferenceNum;
    putLn('referenceNum: '+a);
    a:=GetPhoneNum(false);
    putLn('phoneNumber: '+a);
    coding:=1;
    a:=GetUserData;
    putLn('--- '+BStr(length(a))+' bytes');
    putLn(a);
    end;
  $100:begin; {deliver}
    i:=(o shr 2) and 1;
    a:=bitNames[i];
    putLn('moreMessages: '+a);
    i:=(o shr 5) and 1;
    a:=bitNames[i];
    putLn('statusReport: '+a);
    i:=(o shr 6) and 1;
    a:=bitNames[i];
    putLn('userHeader: '+a);
    i:=(o shr 7) and 1;
    a:=bitNames[i];
    putLn('replyPath: '+a);
    a:=GetPhoneNum(false);
    putLn('phoneNumber: '+a);
    a:=GetProtoId;
    putLn('protocol: '+a);
    a:=GetCodingId;
    putLn('coding: '+a);
    a:=GetTimeStamp;
    putLn('time: '+a);
    a:=GetUserData;
    putLn('--- '+BStr(length(a))+' bytes');
    putLn(a);
    end;
  $101:begin; {submit-report}
    i:=(o shr 6) and 1;
    a:=bitNames[i];
    putLn('userHeader: '+a);
    i:=getCh;
    a:=GetTimeStamp;
    putLn('time: '+a);
    a:=GetProtoId;
    putLn('protocol: '+a);
    a:=GetCodingId;
    putLn('coding: '+a);
    a:=GetUserData;
    putLn('--- '+BStr(length(a))+' bytes');
    putLn(a);
    end;
  $102:begin; {status-report}
    i:=(o shr 2) and 1;
    a:=bitNames[i];
    putLn('moreMessages: '+a);
    i:=(o shr 5) and 1;
    a:=bitNames[i];
    putLn('statusReport: '+a);
    i:=(o shr 6) and 1;
    a:=bitNames[i];
    putLn('userHeader: '+a);
    a:=GetReferenceNum;
    putLn('referenceNum: '+a);
    a:=GetPhoneNum(false);
    putLn('phoneNumber: '+a);
    a:=GetTimeStamp;
    putLn('time: '+a);
    a:=GetTimeStamp;
    putLn('time2: '+a);
    i:=getCh;
    if (i<0) then exit;
    putLn('status: '+BStr(i));
    i:=getCh;
    a:=GetProtoId;
    putLn('protocol: '+a);
    a:=GetCodingId;
    putLn('coding: '+a);
    a:=GetUserData;
    putLn('--- '+BStr(length(a))+' bytes');
    putLn(a);
    end;
  else ;
  end;
gsmPDUcodecDecodeOne:=False;
END;








FUNCTION gsmPDUcodecEncodeOne(var txt,bin:gsmPDUcodecRecord;Received:Byte):Boolean;
Label f1,f2;
Const charExchg:array[1..23,1..2] of char=(
  ('[','('),('\','/'),(']',')'),('^','"'),('`',''''),
  ('{','('),('|','!'),('}',')'),('~','"'),('É','Ü'),
  ('á','Ä'),('à','ä'),('â','ä'),('ã','ç'),('å','ç'),
  ('ì','î'),('ñ','Å'),('ò','Å'),('õ','Ä'),('†','Ö'),
  ('°','ç'),('¢','ï'),('£','ó')                       );

Function ConvPhone(a:String;scNum:Boolean):String;

function x(a:string):char;
const digs='0123456789*#abc';
var i,o:integer;
begin;
x:=#$ff;
a:=copy(a+'__',1,2);
a:=kicsi(a);
i:=pos(a[1],digs)-1;
o:=pos(a[2],digs)-1;
i:=i and $f;
o:=o and $f;
inc(i,o shl 4);
x:=chr(i);
end;

Var
  int:Boolean;
  digs:Byte;
  b:String;
Begin;
ConvPhone:='';
int:=false;
b:='';
kicserel('telephone','',a);
kicserel('international','+',a);
kicserel('national','',a);
kicserel(' ','',a);
int:=(copy(a,1,1)='+');
kicserel('+','',a);
kicserel('-','',a);
kicserel('(','',a);
kicserel(')','',a);
if (a='') then exit;
digs:=Length(a);
while (a<>'') do begin;
  b:=b+x(a);
  a:=copy(a,3,255);
  end;
if int then b:=#$91+b else b:=#$a1+b;
if scNum then digs:=length(b);
b:=chr(digs)+b;
ConvPhone:=b;
End;

Function ConvText(a:String):String;
Var
  b:String;
  bufDat:LongInt;
  bufSiz:Byte;
Begin;
ConvText:='';
bufDat:=0;
bufSiz:=0;
b:=chr(length(a));
while (a<>'') do begin;
  inc(bufDat,ord(a[1]) shl bufSiz);
  inc(bufSiz,7);
  while (bufSiz>=8) do begin;
    b:=b+chr(bufDat);
    bufDat:=bufDat shr 8;
    dec(bufSiz,8);
    end;
  a:=copy(a,2,255);
  end;
if (bufSiz<>0) then b:=b+chr(bufDat);
ConvText:=b;
End;

Function ConvTime(b:String):String;
Var c:String;

function x(s:char):string;
var i:integer;
begin;
i:=pos(s,b);
if (i=0) then i:=$666;
x:=xLevesz(copy(b,1,i-1));
b:=xLevesz(copy(b,i+1,255));
end;

procedure y(i:byte;swp:boolean);
begin;
if swp then i:=(i shr 4) or ((i and $f) shl 4);
c:=c+chr(i);
end;

Var
  a:String;
  ng:Boolean;
  i,o:Integer;
Begin;
ConvTime:='';
c:='';
a:=x('-');
a:=copy(a,length(a)-2+1,255);
i:=BVal(a);y(i,true);
a:=x('-');i:=BVal(a);y(i,true);
a:=x(' ');i:=BVal(a);y(i,true);
a:=x(':');i:=BVal(a);y(i,true);
a:=x(':');i:=BVal(a);y(i,true);
a:=x(' ');i:=BVal(a);y(i,true);
ng:=(copy(b,1,1)='-');
a:=copy(b,length(b)-4+1,255);
i:=BVal(copy(a,1,2));
o:=BVal(copy(a,3,2));
inc(o,i*60);
o:=(o div 15) and $7f;
if ng then o:=o or $80;
y(o,false);
ConvTime:=c;
End;


Procedure putStr(a:String);
Var i:Byte;
Begin;
move(a[1],bin.d[bin.s+1],length(a));
inc(bin.s,length(a));
End;

Function getLn:String;
Var
  i:LongInt;
  a:String;
Begin;
i:=txt.s-txt.p;
if (i>255) then i:=255;
if (i<1) then i:=0;
move(txt.d[txt.p+1],a[1],i);
a[0]:=chr(i);
i:=pos(#13,a);
if (i<1) then i:=255;
a:=copy(a,1,i-1);
inc(txt.p,i);
if (txt.d[txt.p+1]=10) then inc(txt.p);
Kicserel(#10,'',a);
getLn:=a;
End;


Var
  PhoneSC:String;
  PhoneME:String;
  rejectDup:Boolean;
  statusReport:Boolean;
  referenceNum:Byte;
  TextData:String;
  TimeStamp:String;
  a:String;
  i:Integer;
BEGIN;
gsmPDUcodecEncodeOne:=True;
fillchar(bin,sizeof(bin),0);
txt.p:=0;
received:=received and 1;
PhoneSC:='';
PhoneME:='';
rejectDup:=False;
statusReport:=False;
referenceNum:=0;
TextData:='';
TimeStamp:='';
f1:
if (txt.p>=txt.s) then exit;
a:=kicsi(getLn);
if (copy(a,1,3)='---') then begin; TextData:='';goto f2; end;
i:=pos(':',a);
TextData:=xLevesz(copy(a,1,i-1));
a:=xLevesz(copy(a,i+1,255));
if (textData='servicecenter') then PhoneSC:=a;
if (textData='phonenumber') then PhoneME:=a;
if (textData='rejectduplicate') then rejectDup:=(a='yes');
if (textData='statusreport') then statusReport:=(a='yes');
if (textData='referencenum') then referenceNum:=BVal(a);
if (textData='time') then TimeStamp:=a;
goto f1;
f2:
while (txt.p<txt.s) do textData:=textData+getLn+#10;
if (copy(textData,length(textData),255)=#10) then textData:=copy(textData,1,length(textData)-1);
a:=textData;
kicserel(#0,' ',a);
kicserel(#9,' ',a);
kicserel(#255,' ',a);
for i:=1 to 23 do kicserel(charExchg[i][1],charExchg[i][2],a);
if (copy(a,length(a),1)=#10) then a:=copy(a,1,length(a)-1);
textData:='';
while (a<>'') do begin;
  i:=pos(a[1],gsmPDUmode1alphabet)-1;
  if (i>=0) then textData:=textData+chr(i);
  a:=copy(a,2,255);
  end;
PhoneSC:=ConvPhone(PhoneSC,true);
PhoneME:=ConvPhone(PhoneME,false);
TimeStamp:=ConvTime(TimeStamp);
textData:=ConvText(textData);
if (received=0) then begin;
  {submit}
  if (PhoneSC='') then PhoneSC:=#0;
  if (PhoneME='') then exit;
  putStr(PhoneSC);
  i:=1;
  if rejectDup then i:=i or $04;
  if statusReport then i:=i or $20;
  putStr(chr(i)+chr(referenceNum));
  putStr(PhoneME);
  putStr(#0#0);
  putStr(textData);
  end else begin;
  {deliver}
  if (PhoneSC='') then exit;
  if (PhoneME='') then exit;
  putStr(PhoneSC);
  i:=0;
  if statusReport then i:=i or $20;
  putStr(chr(i));
  putStr(PhoneME);
  putStr(#0#0);
  putStr(TimeStamp);
  putStr(textData);
  end;
gsmPDUcodecEncodeOne:=False;
END;
