Type
  OneQuestionPacketHeader=record
    id:word;
    flag:Word;
    qdc:word; {question count}
    anc:word; {answer count}
    nsc:word; {authority count}
    arc:word; {additional count}
    end;
  OneQuestionHeader=record
    qtype:word;
    class:word;
    end;
  OneAnswerHeader=record
    qtype:word;
    class:word;
    ttl:longint;
    len:word;
    end;
Const
{flag masks for the flags field of the DOMAIN header}
  Flags_QR    =$8000;           {0=query, 1=response}
  Flags_OPCODE=$7000;           {opcode}
  Flags_AA    =$0400;           {authoritative answer}
  Flags_TC    =$0200;           {truncation, response was cut off at 512}
  Flags_RD    =$0100;           {recursion desired}
  Flags_RA    =$0080;           {recursion available}
  Flags_AD    =$0020;           {authentic data}
  Flags_CD    =$0010;           {checking disabled}
  Flags_RCODE =$000f;           {response code}
  Flags_COPIED=Flags_OPCODE+Flags_RD;
{opcode values}
  Opcode_QUERY=$0000;           {a standard query}
  Opcode_IQ   =$1000;           {an inverse query}
  Opcode_CQM  =$2000;           {a completion query, multiple reply}
  Opcode_CQU  =$3000;           {a completion query, single reply}
{response values}
  Respon_OK   =$0000;           {okay response}
  Respon_FORM =$0001;           {format error}
  Respon_FAIL =$0002;           {server failure}
  Respon_NAME =$0003;           {we know name doesn't exist}
  Respon_NOPE =$0004;           {not implemented}
  Respon_NOWAY=$0005;           {name server refusing to do request}
{type values}
  Type_A        =  1;           {a host address}
  Type_NS       =  2;           {an authoritative name server}
  Type_MD       =  3;           {a mail destination (OBSOLETE - use MX)}
  Type_MF       =  4;           {a mail forwarder (OBSOLETE - use MX)}
  Type_CNAME    =  5;           {the canonical name for an alias}
  Type_SOA      =  6;           {marks the start of a zone of authority}
  Type_MB       =  7;           {a mailbox domain name (EXPERIMENTAL)}
  Type_MG       =  8;           {a mail group member (EXPERIMENTAL)}
  Type_MR       =  9;           {a mail rename domain name (EXPERIMENTAL)}
  Type_NULL     = 10;           {a null RR (EXPERIMENTAL)}
  Type_WKS      = 11;           {a well known service description}
  Type_PTR      = 12;           {a domain name pointer}
  Type_HINFO    = 13;           {host information}
  Type_MINFO    = 14;           {mailbox or mail list information}
  Type_MX       = 15;           {mail exchange}
  Type_TXT      = 16;           {text strings}
  Type_RP       = 17;           {responsible person}
  Type_X25      = 19;           {x25 phone number}
  Type_ISDN     = 20;           {isdn phone number}
  Type_RT       = 21;           {route through}
  Type_NSAP     = 22;           {network service access protocol}
  Type_NSAPPTR  = 23;           {a nsap pointer}
  Type_SIG      = 24;           {signature}
  Type_KEY      = 25;           {key}
  Type_PX       = 26;           {pointer to x400/rfc822 mapping}
  Type_GPOS     = 27;           {geographical position}
  Type_AAAA     = 28;           {a host address}
  Type_LOC      = 29;           {location}
  Type_NXT      = 30;           {next domain}
  Type_EID      = 31;           {endpoint identifier}
  Type_NIMLOC   = 32;           {nimrod locator}
  Type_SRV      = 33;           {location of services}
  Type_ATMA     = 34;           {atm address}
  Type_NAPTR    = 35;           {naming authority pointer}
  Type_KX       = 36;           {key exchange}
  Type_CERT     = 37;           {certification record}
  Type_A6       = 38;           {ipv6 address}
  Type_DNAME    = 39;           {non-terminal dname}
  Type_SINK     = 40;           {kitchen sink}
  Type_OPT      = 41;           {edns0 option}
  Type_APL      = 42;           {address prefix list}
  Type_DS       = 43;           {delegation signer}
  Type_SSHFP    = 44;           {ssh fp}
  Type_IPSEC    = 45;           {ipsec key}
  Type_RRSIG    = 46;           {rr signature}
  Type_NSEC     = 47;           {name security}
  Type_DNSKEY   = 48;           {dns key}
  Type_SPF      = 99;           {sender policy framework}
  Type_UNSPEC   = 103;          {unspecified}
  Type_TKEY     = 249;          {key}
  Type_TSIG     = 250;          {signature}
  Type_IXFR     = 251;          {incremental zone transfer}
  Type_AXFR     = 252;          {transfer of an entire zone}
  Type_MAILB    = 253;          {mailbox-related records}
  Type_MAILA    = 254;          {mail agent RRs}
  Type_ANY      = 255;          {all records}
{class values}
  Class_IN    =1;               {the Internet}
  Class_CS    =2;               {the CSNET class}
  Class_CH    =3;               {the CHAOS class}
  Class_HS    =4;               {Hesiod / Dyer 87}
  Class_ALL   =255;             {any class}
{domain suffixes}
  ReverseMapSuffix4='IN-ADDR.ARPA';
  ReverseMapSuffix6='IP6.ARPA';


Function GetQTypeName(i:Word):String;
Var a:String;
Begin;
case i of
  Type_A      :a:='A';
  Type_NS     :a:='NS';
  Type_MD     :a:='MD';
  Type_MF     :a:='MF';
  Type_CNAME  :a:='CNAME';
  Type_SOA    :a:='SOA';
  Type_MB     :a:='MB';
  Type_MG     :a:='MG';
  Type_MR     :a:='MR';
  Type_NULL   :a:='NULL';
  Type_WKS    :a:='WKS';
  Type_PTR    :a:='PTR';
  Type_HINFO  :a:='HINFO';
  Type_MINFO  :a:='MINFO';
  Type_MX     :a:='MX';
  Type_TXT    :a:='TXT';
  Type_RP     :a:='RP';
  Type_X25    :a:='X25';
  Type_ISDN   :a:='ISDN';
  Type_RT     :a:='RT';
  Type_NSAP   :a:='NSAP';
  Type_NSAPPTR:a:='NSAP-PTR';
  Type_PX     :a:='PX';
  Type_GPOS   :a:='GPOS';
  Type_AAAA   :a:='AAAA';
  Type_LOC    :a:='LOC';
  Type_NXT    :a:='NXT';
  Type_EID    :a:='EID';
  Type_NIMLOC :a:='NIMLOC';
  Type_SRV    :a:='SRV';
  Type_ATMA   :a:='ATMA';
  Type_NAPTR  :a:='NAPTR';
  Type_KX     :a:='KX';
  Type_CERT   :a:='CERT';
  Type_A6     :a:='A6';
  Type_DNAME  :a:='DNAME';
  Type_SINK   :a:='SINK';
  Type_OPT    :a:='OPT';
  Type_APL    :a:='APL';
  Type_DS     :a:='DS';
  Type_SSHFP  :a:='SSHFP';
  Type_IPSEC  :a:='IPSEC';
  Type_RRSIG  :a:='RRSIG';
  Type_NSEC   :a:='NSEC';
  Type_DNSKEY :a:='DNSKEY';
  Type_SPF    :a:='SPF';
  Type_UNSPEC :a:='UNSPEC';
  Type_TKEY   :a:='TKEY';
  Type_TSIG   :a:='TSIG';
  Type_IXFR   :a:='IXFR';
  Type_AXFR   :a:='AXFR';
  Type_MAILB  :a:='MAILB';
  Type_MAILA  :a:='MAILA';
  Type_ANY    :a:='ANY';
  else a:='$'+byte2hexType(i shr 8)+byte2hexType(i);
  end;
GetQTypeName:=a;
End;

Function GetQClassName(i:LongInt):String;
Var a:String;
Begin;
case i of
  Class_IN    :a:='INET';
  Class_CS    :a:='CSNET';
  Class_CH    :a:='CHAOS';
  Class_HS    :a:='HESIOD';
  Class_ALL   :a:='ALL';
  else a:='$'+byte2hexType(i shr 8)+byte2hexType(i)+' ';
  end;
GetQClassName:=a;
End;

Function ConvertDomain2protocol(s:String):String;
Var
  a:string;
  i:Byte;
Begin;
ConvertDomain2protocol:='';
a:='';
s:=s+'.';
while (s<>'') do begin;
  i:=pos('.',s);
  if (i<2) then Exit;
  a:=a+chr(i-1)+copy(s,1,i-1);
  s:=copy(s,i+1,255);
  end;
ConvertDomain2protocol:=a+#0;
End;

Function GetPckProtocol2domain(var pck:OnePacketRecord;var p:LongInt):String;
Label f1,f2;
Var
  upd:Boolean;
  a,b:String;
  i,o:LongInt;
  ab:byte absolute a;
Begin;
GetPckProtocol2domain:='';
b:='';
upd:=true;
f1:
if (length(b)>252) then exit;
if upd then begin;
  if (p>pck.s) then exit;
  move(pck.d[p+1],a,sizeof(a));
  end else begin;
  if (o>pck.s) then exit;
  move(pck.d[o+1],a,sizeof(a));
  end;
if (ab and $c0=0) then begin;
  if upd then inc(p,length(a)+1) else inc(o,length(a)+1);
  if (a='') then goto f2;
  b:=b+'.'+a;
  goto f1;
  end;
if (ab and $c0=$c0) then begin;
  o:=ReadWordMSB(a) and $3fff;
  if upd then inc(p,sizeof(word));
  upd:=false;
  goto f1;
  end;
exit;
f2:
GetPckProtocol2domain:=copy(b,2,255);
End;

Function GetPckProtocol2string(var pck:OnePacketRecord;var p:LongInt):String;
Label f1,f2;
Var
  a:String;
  ab:byte absolute a;
Begin;
GetPckProtocol2string:='';
inc(p);
move(pck.d[p],a,sizeof(a));
inc(p,ab);
GetPckProtocol2string:=a;
End;

Function ConvertProtocol2domain(Var s:String):String;
Var
  a,b:String;
  i:Byte;
Begin;
ConvertProtocol2domain:='';
b:='';
repeat
  if (length(b)>252) then exit;
  if (s='') then exit;
  i:=Ord(s[1]);
  a:=copy(s,2,i);
  s:=copy(s,i+2,255);
  if (a<>'') then b:=b+'.'+a;
  until (a='');
ConvertProtocol2domain:=copy(b,2,255);
End;

Function ConvertString2protocol(s:String):String;
Begin;
ConvertString2protocol:=s[0]+s;
End;


Function ConvertPtrName2ipAddr(b:String;var buf):Boolean;
Label f1;
Var
  a:String;
  i,o:LongInt;
Begin;
ConvertPtrName2ipAddr:=True;
b:=nagy(b);
a:='';
if (copy(b,length(b)-length(ReverseMapSuffix4)+1,255)=ReverseMapSuffix4) then begin;
  for o:=1 to 4 do begin;
    i:=pos('.',b);
    if (i<1) then exit;
    a:='.'+copy(b,1,i-1)+a;
    b:=copy(b,i+1,255);
    end;
  goto f1;
  end;
if (copy(b,length(b)-length(ReverseMapSuffix6)+1,255)=ReverseMapSuffix6) then begin;
  for o:=1 to 8 do begin;
    i:=pos('.',b);
    if (i<1) then exit;
    a:=copy(b,1,i-1)+a;
    b:=copy(b,i+1,255);
    i:=pos('.',b);
    if (i<1) then exit;
    a:=copy(b,1,i-1)+a;
    b:=copy(b,i+1,255);
    i:=pos('.',b);
    if (i<1) then exit;
    a:=copy(b,1,i-1)+a;
    b:=copy(b,i+1,255);
    i:=pos('.',b);
    if (i<1) then exit;
    a:=':'+copy(b,1,i-1)+a;
    b:=copy(b,i+1,255);
    end;
  goto f1;
  end;
exit;
f1:
ConvertPtrName2ipAddr:=string2ipAddr(copy(a,2,255),buf);
End;

Function ConvertIPaddr2ptrName(var addr):String;
Var
  ip:OneTCPaddressRecord absolute addr;
  a,b:String;
  i:LongInt;
Begin;
b:='';
if isAddressIPv4mask(ip) then begin;
  for i:=sizeof(ip) downto sizeof(ip)-3 do b:=b+BStr(ip[i])+'.';
  ConvertIPaddr2ptrName:=b+ReverseMapSuffix4;
  end else begin;
  for i:=sizeof(ip) downto 1 do begin;
    a:=kicsi(byte2hextype(ip[i]));
    b:=b+a[2]+'.'+a[1]+'.';
    end;
  ConvertIPaddr2ptrName:=b+ReverseMapSuffix6;
  end;
End;

Function GetParentalDomainName(a:String):String;
Var i:LongInt;
Begin;
i:=pos('.',a);
if (i<1) then i:=666;
GetParentalDomainName:=copy(a,i+1,666);
End;
