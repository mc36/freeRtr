{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
{$sysinc memory.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc random.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_cgi.inc}
{$sysinc inet_tcp.inc}
{$sysinc inet_dns.inc}

Type
  OnePacketRecord=record
    s:LongInt;
    d:array[1..4*1024] of byte;
    end;

{$include \sources\internet\kernel\utils\timer2.inc}
{$include \sources\internet\server\dns\struct.inc}

Const
  ProggyName='DNS Updater v1.0/CGi';
  DnsTypesNum=32;
  DnsTypesDat:array[1..DnsTypesNum] of record
    n:String[15];
    t:LongInt;
    d:String[63];
    end=(
    (n:'A';t:Type_A;d:'a host address'),
    (n:'NS';t:Type_NS;d:'an authoritative name server'),
    (n:'MD';t:Type_MD;d:'a mail destination'),
    (n:'MF';t:Type_MF;d:'a mail forwarder'),
    (n:'CNAME';t:Type_CNAME;d:'the canonical name for an alias'),
    (n:'SOA';t:Type_SOA;d:'marks the start of a zone of authority'),
    (n:'MB';t:Type_MB;d:'a mailbox domain name'),
    (n:'MG';t:Type_MG;d:'a mail group member'),
    (n:'MR';t:Type_MR;d:'a mail rename domain name'),
    (n:'NULL';t:Type_NULL;d:'a null RR'),
    (n:'WKS';t:Type_WKS;d:'a well known service description'),
    (n:'PTR';t:Type_PTR;d:'a domain name pointer'),
    (n:'HINFO';t:Type_HINFO;d:'host information'),
    (n:'MINFO';t:Type_MINFO;d:'mailbox or mail list information'),
    (n:'MX';t:Type_MX;d:'mail exchange'),
    (n:'TXT';t:Type_TXT;d:'text strings'),
    (n:'RP';t:Type_RP;d:'responsible person'),
    (n:'X25';t:Type_X25;d:'x25 phone number'),
    (n:'ISDN';t:Type_ISDN;d:'isdn phone number'),
    (n:'RT';t:Type_RT;d:'route through'),
    (n:'NSAP';t:Type_NSAP;d:'network service access protocol'),
    (n:'NSAPPTR';t:Type_NSAPPTR;d:'a nsap pointer'),
    (n:'PX';t:Type_PX;d:'pointer to x400/rfc822 mapping'),
    (n:'GPOS';t:Type_GPOS;d:'geographical position'),
    (n:'AAAA';t:Type_AAAA;d:'a host address'),
    (n:'LOC';t:Type_LOC;d:'location'),
    (n:'SRV';t:Type_SRV;d:'location of services'),
    (n:'IXFR';t:Type_IXFR;d:'request for incremental zone transfer'),
    (n:'AXFR';t:Type_AXFR;d:'request for a transfer of an entire zone'),
    (n:'MAILB';t:Type_MAILB;d:'request for mailbox-related records'),
    (n:'MAILA';t:Type_MAILA;d:'request for mail agent RRs'),
    (n:'ANY';t:Type_ANY;d:'request for all records')
    );






Procedure createOneQuery(nam:String;typ:LongInt;rec:Boolean;var pck:OnePacketRecord);
Var
  phdr:OneQuestionPacketHeader;
  qhdr:OneQuestionHeader;
  i:LongInt;
Begin;
if not string2ipAddr(nam,pck) then nam:=ConvertIPaddr2ptrName(pck);
nam:=ConvertDomain2protocol(nam);
fillchar(pck,sizeof(pck),0);
i:=Opcode_QUERY;
if rec then i:=i or Flags_RD;
phdr.id:=random($10000);
WriteWordMSB(phdr.flag,i);
WriteWordMSB(phdr.qdc,1);
phdr.anc:=0;
phdr.nsc:=0;
phdr.arc:=0;
WriteWordMSB(qhdr.qtype,typ);
WriteWordMSB(qhdr.class,Class_IN);
i:=0;
move(phdr,pck.d[i+1],sizeof(phdr));inc(i,sizeof(phdr));
move(nam[1],pck.d[i+1],sizeof(nam));inc(i,length(nam));
move(qhdr,pck.d[i+1],sizeof(qhdr));inc(i,sizeof(qhdr));
pck.s:=i;
End;

Procedure dumpOnePacket(a:String;var pck:OnePacketRecord);
Var
  phdr:OneQuestionPacketHeader;
  qhdr:OneQuestionHeader;
  ahdr:OneAnswerHeader;
  nps,ps:LongInt;
  i:LongInt;

Function hw(i:LongInt):String;
Begin;
hw:=byte2hextype(i shr 8)+byte2hextype(i);
End;

Function hl(i:LongInt):String;
Begin;
hl:=hw(i shr 16)+hw(i);
End;

Function yn(i:LongInt):String;
Begin;
if (i=0) then yn:='no' else yn:='yes';
End;

Procedure DisplayOneTable(a:String;max:LongInt);
Var i,o,p:LongInt;
Begin;
WriteLnResponseString('<i>'+BStr(max)+' '+a+':</i><br>');
if (max<1) then exit;
WriteLnResponseString('<table border=1>');
for p:=1 to max do begin;
  WriteLnResponseString('<tr>');
  WriteLnResponseString('  <td>'+GetPckProtocol2domain(pck,ps)+'</td>');
  move(pck.d[ps+1],ahdr,sizeof(ahdr));
  inc(ps,sizeof(ahdr));
  WriteLnResponseString('  <td>'+BStr(ReadLongMSB(ahdr.ttl))+'</td>');
  o:=ReadWordMSB(ahdr.qtype);
  a:=GetQClassName(ReadWordMSB(ahdr.class));
  if (a='INET') then a:='' else a:=a+' ';
  WriteLnResponseString('  <td>'+a+GetQTypeName(o)+'</td>');
  WriteResponseString('  <td>');
  i:=ReadWordMSB(ahdr.len);
  if (i<0) then i:=0;
  if (i>sizeof(pck)) then i:=sizeof(pck);
  nps:=i+ps;
  if (nps>pck.s) then break;
  case o of
    Type_CNAME:WriteResponseString(GetPckProtocol2domain(pck,ps));
    Type_HINFO:begin;
      WriteResponseString('cpu='+GetPckProtocol2string(pck,ps));
      WriteResponseString('<br>');
      WriteResponseString('os='+GetPckProtocol2string(pck,ps));
      end;
    Type_MB:WriteResponseString(GetPckProtocol2domain(pck,ps));
    Type_MD:WriteResponseString(GetPckProtocol2domain(pck,ps));
    Type_MF:WriteResponseString(GetPckProtocol2domain(pck,ps));
    Type_MG:WriteResponseString(GetPckProtocol2domain(pck,ps));
    Type_MINFO:begin;
      WriteResponseString('list='+GetPckProtocol2domain(pck,ps));
      WriteResponseString('<br>');
      WriteResponseString('errors='+GetPckProtocol2domain(pck,ps));
      end;
    Type_MR:WriteResponseString(GetPckProtocol2domain(pck,ps));
    Type_MX:begin;
      WriteResponseString(BStr(ReadWordMSB(pck.d[ps+1])));
      inc(ps,2);
      WriteResponseString(' '+GetPckProtocol2domain(pck,ps));
      end;
    Type_NS:WriteResponseString(GetPckProtocol2domain(pck,ps));
    Type_PTR:WriteResponseString(GetPckProtocol2domain(pck,ps));
    Type_SOA:begin;
      WriteResponseString('ns='+GetPckProtocol2domain(pck,ps));
      WriteResponseString('<br>email='+GetPckProtocol2domain(pck,ps));
      WriteResponseString('<br>serial='+BStr(ReadLongMSB(pck.d[ps+1])));inc(ps,4);
      WriteResponseString('<br>refresh='+BStr(ReadLongMSB(pck.d[ps+1])));inc(ps,4);
      WriteResponseString('<br>retry='+BStr(ReadLongMSB(pck.d[ps+1])));inc(ps,4);
      WriteResponseString('<br>expire='+BStr(ReadLongMSB(pck.d[ps+1])));inc(ps,4);
      WriteResponseString('<br>ttl='+BStr(ReadLongMSB(pck.d[ps+1])));inc(ps,4);
      end;
    Type_TXT:WriteResponseString(GetPckProtocol2string(pck,ps));
    Type_NULL:WriteResponseString(GetPckProtocol2string(pck,ps));
    Type_A:begin;
      a[0]:=#128;
      move(pck.d[ps+1],a[1],128);
      a:=IPv4addressPrefix+a;
      WriteResponseString(ipAddr2string(a[1]));
      end;
    Type_AAAA:begin;
      a[0]:=#128;
      move(pck.d[ps+1],a[1],128);
      WriteResponseString(ipAddr2string(a[1]));
      end;
    else WriteResponseString('???');
    end;
  ps:=nps;
  WriteLnResponseString('  </td>');
  WriteLnResponseString('  </tr>');
  end;
WriteLnResponseString('</table>');
End;

Begin;
WriteLnResponseString('<b><u>'+a+':</u></b><br>');
move(pck.d,phdr,sizeof(phdr));
ps:=sizeof(phdr);
i:=ReadWordMSB(phdr.flag);
case i and Flags_OPCODE of
  Opcode_QUERY:a:='standard query';
  Opcode_IQ:a:='inverse query';
  Opcode_CQM:a:='multiple reply';
  Opcode_CQU:a:='single reply';
  else a:='?';
  end;
WriteLnResponseString('opcode='+a);
case i and Flags_RCODE of
  Respon_OK:a:='okay';
  Respon_FORM:a:='format error';
  Respon_FAIL:a:='server failure';
  Respon_NAME:a:='name doesn''t exist';
  Respon_NOPE:a:='not implemented';
  Respon_NOWAY:a:='server refused';
  else a:='?';
  end;
WriteLnResponseString('response='+a);
WriteLnResponseString('response='+yn(i and Flags_QR));
WriteLnResponseString('authoritative='+yn(i and Flags_AA));
WriteLnResponseString('truncated='+yn(i and Flags_TC));
WriteLnResponseString('<br>');
WriteLnResponseString('recReq='+yn(i and Flags_RD));
WriteLnResponseString('recAvail='+yn(i and Flags_RA));
WriteLnResponseString('authentic='+yn(i and Flags_AD));
WriteLnResponseString('checking='+yn(i and Flags_CD));
WriteLnResponseString('id=0x'+hw(ReadWordMSB(phdr.id)));
WriteLnResponseString('flag=0x'+hw(ReadWordMSB(phdr.flag)));
WriteLnResponseString('<br>');
WriteLnResponseString('<i>'+BStr(ReadWordMSB(phdr.qdc))+' question:</i><br>');
WriteLnResponseString('<table border=1>');
for i:=1 to ReadWordMSB(phdr.qdc) do begin;
  WriteLnResponseString('<tr>');
  WriteLnResponseString('  <td>'+GetPckProtocol2domain(pck,ps)+'</td>');
  move(pck.d[ps+1],qhdr,sizeof(qhdr));
  inc(ps,sizeof(qhdr));
  a:=GetQClassName(ReadWordMSB(qhdr.class));
  if (a='INET') then a:='' else a:=a+' ';
  WriteLnResponseString('  <td>'+a+GetQTypeName(ReadWordMSB(qhdr.qtype))+'</td>');
  WriteLnResponseString('  </tr>');
  end;
WriteLnResponseString('</table>');
DisplayOneTable('answer',ReadWordMSB(phdr.anc));
DisplayOneTable('authority',ReadWordMSB(phdr.nsc));
DisplayOneTable('additional',ReadWordMSB(phdr.arc));
WriteLnResponseString('<i>'+BStr(pck.s)+' bytes:</i><br>');
WriteLnResponseString('<table border=0>');
ps:=0;
while (ps<pck.s) do begin;
  WriteResponseString('<tr>');
  a:='';
  for i:=1 to 24 do begin;
    inc(ps);
    if (ps<=pck.s) then a:=byte2hextype(pck.d[ps]) else a:=' ';
    WriteResponseString('<td>'+a+'</td>');
    end;
  WriteLnResponseString('</tr>');
  end;
WriteLnResponseString('</table>');
End;



Label f1,vege;
Var
  url,ns,nam:String;
  pipe,typ,rec:LongInt;
  pck1:OnePacketRecord;
  pck2:OnePacketRecord;
  i,o,p:LongInt;
  a,b:string;
BEGIN;
WriteLn(ProggyName+', done by Mc at '#%date' '#%time'.');
a:=GetAllParameters;
if OpenRequestFile(a) then Halt(1);
if FindNextRequest(True) then Halt(2);

url:='';
ns:='';
nam:='';
rec:=0;
typ:=Type_ANY;
repeat
  a:=Kicsi(CurrentReqNam);
  Case CurrentReqTyp of
    1:begin;
      if (a='url') then url:='/'+ReadRequestString;
      end;
    2:begin;
      if (a='ns') then ns:=ReadRequestString;
      if (a='name') then nam:=ReadRequestString;
      if (a='type') then typ:=BVal(ReadRequestString);
      if (a='rec') then rec:=BVal(ReadRequestString);
      end;
    3:;
    end;
  until FindNextRequest(False);
Randomize;
TCPfindProcess;
DNSstartResolver;

WriteResponseHeader(3,'html');
WriteLnResponseString('<html><head><title>nslookup</title></head><body>');

WriteLnResponseString('<form action="'+url+'">');
WriteLnResponseString('name: <input type="text" name="name" size="40" value="'+nam+'"><br>');
WriteLnResponseString('server: <input type="text" name="ns" size="40" value="'+ns+'"><br>');
WriteLnResponseString('type: <select name="type" size="1">');
for i:=1 to DnsTypesNum do begin;
  o:=DnsTypesDat[i].t;
  a:=copy(DnsTypesDat[i].n+'                    ',1,15)+' - '+DnsTypesDat[i].d;
  if (o=typ) then b:='selected' else b:='';
  WriteLnResponseString('  <option '+b+' value="'+BStr(o)+'">'+a+'</option>');
  end;
WriteLnResponseString('  </select><br>');
if (rec=0) then a:='' else a:='checked';
WriteLnResponseString('recursive: <input '+a+' type="checkbox" name="rec" value="1"><br>');
WriteLnResponseString('<input type=submit value="Query!">');
WriteLnResponseString('</form><hr>');
if (nam='') then goto vege;
if (ns='') then goto vege;

WriteLnResponseString('quering');
WriteLnResponseString(ns);
WriteLnResponseString('for');
WriteLnResponseString(GetQTypeName(typ));
WriteLnResponseString('of');
WriteLnResponseString(nam);
WriteLnResponseString('...<br>');

DNSresolvePut(1,ns);
while (1=1) do begin;
  i:=DNSresolveGet(a,ns);
  if (i=0) then begin; relequish;continue; end;
  if (i and $80=0) then break;
  WriteLnResponseString('failed to get address of name server!<br>');
  goto vege;
  end;

i:=0;
if UDPlistenOnPort(pipe,4096,a,i) then exit;
createOneQuery(nam,typ,rec<>0,pck1);
UDPsendPacket(pipe,ns,53,pck1.d,pck1.s);
timer2start;
p:=currentTime;
f1:
relequish;
timer2start;
if (GetTimePast(p)>15) then begin;
  WriteLnResponseString('no reply arrived!<br>');
  goto vege;
  end;
pck2.s:=sizeof(pck2);
if UDPreceivePacket(pipe,a,i,pck2.d,pck2.s) then goto f1;
pipeLineClose(pipe);

dumpOnePacket('reply',pck2);
WriteLnResponseString('<hr>');
dumpOnePacket('question',pck1);
WriteLnResponseString('<hr>');

vege:
WriteLnResponseString('<br><i>Created by '+ProggyName+'</i>');
WriteLnResponseString('</body>');
WriteLnResponseString('</html>');
CloseRequestFile;
END.