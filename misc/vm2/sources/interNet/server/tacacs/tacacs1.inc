Procedure encdecTacacsPacket(var buffer);
Label f1,f2;
Var
  hed:OneTacacsHeaderRecord absolute buffer;
  buf:array[1..1] of byte absolute buffer;
  k:String;
  kb:array[0..1] of byte absolute k;
  kb0:byte absolute k;
  m:CryptoContextRecord;
  i,o,p,q:LongInt;
Begin;
k:='';
p:=sizeof(hed);
q:=ReadLongMSB(hed.siz)+sizeof(hed);
f1:
CryptoBegHasher(m,md5algoNum,'','');
CryptoAddHasher(m,hed.ses,sizeof(hed.ses));
CryptoAddHasher(m,sharedSecret[1],length(sharedSecret));
CryptoAddHasher(m,hed.ver,sizeof(hed.ver));
CryptoAddHasher(m,hed.seq,sizeof(hed.seq));
CryptoAddHasher(m,kb[1],kb0);
CryptoFinHasher(m,kb[1],i);
kb0:=i;
o:=0;
f2:
if (p>=q) then exit;
inc(o);
if (o>kb0) then goto f1;
inc(p);
buf[p]:=buf[p] xor kb[o];
goto f2;
End;



Procedure processReq(var con:OneConnectionRecord;var buffer);
Label f1,f2,acc,rej,send;
Var
  head:OneTacacsHeaderRecord absolute buffer;
  buf:array[1..1] of byte absolute buffer;
  hed1i:OneTacacsAuthenReqRecord;
  hed11i:OneTacacsContinueRecord;
  hed1o:OneTacacsAuthenRepRecord;
  hed2i:OneTacacsAuthorReqRecord;
  hed2o:OneTacacsAuthorRepRecord;
  hed3i:OneTacacsAccountReqRecord;
  hed3o:OneTacacsAccountRepRecord;
  a,b,c,d,e:String;
  i,o,p:LongInt;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;

Procedure getString(var a:String;size:LongInt);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
ab0:=size;
move(buf[p+1],ab[1],ab0);
inc(p,size);
End;


Begin;
if not (head.ver in [$c0..$c1]) then begin;
  WriteLn('got bad version number!');
  con.stat:=0;
  exit;
  end;
if (head.flg and 1=0) then encdecTacacsPacket(buf);
inc(head.seq);
head.flg:=head.flg and 1;
p:=sizeof(head);
case head.typ of
  1:begin; {authentication}
    move(buf[p+1],hed1i,sizeof(hed1i));
    inc(p,sizeof(hed1i));
    getString(b,hed1i.len_usr);
    getString(c,hed1i.len_prt);
    getString(d,hed1i.len_adr);
    c:=c+'@'+d;
    getString(d,hed1i.len_dat);
    if (hed1i.act=0) then begin; {ascii continue}
      p:=sizeof(head);
      move(buf[p+1],hed11i,sizeof(hed11i));
      inc(p,sizeof(hed11i));
      getString(d,readWordMSB(hed11i.len_usr));
      getString(c,readWordMSB(hed11i.len_dat));
      if (con.cmdT<>1) then begin;
        con.cmdT:=1;
        con.cmdA:=d;
        a:=passPrompt+' ';
        hed1o.stat:=5;
        hed1o.flag:=1;
        goto f1;
        end;
      c:=con.cmdB;
      b:=con.cmdA;
      if (authenticateCurrentProcess<>'') then begin;
        if (authenticateOneUser(con.cmdA,d,'tacacs at '+ipAddr2string(con.adr)+' '+BStr(con.prt),LoginFlags_accessTac,i)=0) then goto acc;
        goto rej;
        end;
      if CheckOneUser(con.cmdA,e) then goto rej;
      if (e<>d) then goto rej;
      goto acc;
      end;
    if (hed1i.act<>1) then begin;
      a:='authentication action not supported!';
      hed1o.stat:=7;
      hed1o.flag:=0;
      goto f1;
      end;
    con.cmdB:=c;
    case hed1i.typ of
      1:begin; {ascii}
        con.cmdT:=0;
        a:=userPrompt+' ';
        hed1o.stat:=4;
        hed1o.flag:=0;
        goto f1;
        end;
      2:begin; {pap}
        con.cmdT:=1;
        con.cmdA:=b;
        if (authenticateCurrentProcess<>'') then begin;
          if (authenticateOneUser(con.cmdA,d,'tacacs at '+ipAddr2string(con.adr)+' '+BStr(con.prt),LoginFlags_accessTac,i)=0) then goto acc;
          goto rej;
          end;
        if CheckOneUser(b,e) then goto rej;
        if (e<>d) then goto rej;
        goto acc;
        end;
      3:begin; {chap}
        con.cmdT:=1;
        con.cmdA:=b;
        if (authenticateCurrentProcess<>'') then begin;
          WriteLn('got chap, but pap forced!');
          goto rej;
          end;
        if CheckOneUser(b,e) then goto rej;
        a:=copy(d,1,1)+e+copy(d,2,length(d)-17);
        i:=ab0;
        CryptoImmHasher(md5algoNum,'','',ab[1],i);
        ab0:=i;
        if (copy(d,length(d)-15,16)<>a) then goto rej;
        goto acc;
        end;
      else begin;
        a:='authentication method not supported!';
        hed1o.stat:=7;
        hed1o.flag:=0;
        goto f1;
        end;
      end;
    end;
  2:begin; {authorization}
    move(buf[p+1],hed2i,sizeof(hed2i));
    inc(p,sizeof(hed2i));
    inc(p,hed2i.argCnt);
    getString(b,hed2i.len_usr);
    getString(c,hed2i.len_prt);
    getString(d,hed2i.len_adr);
    c:=c+'@'+d;
    d:='';
    o:=sizeof(head)+sizeof(hed2i);
    for i:=1 to hed2i.argCnt do begin;
      inc(o);
      getString(a,buf[o]);
      d:=d+' '+a;
      end;
    append2log('author',b,ipAddr2string(con.adr)+' '+BStr(con.prt)+' '+c,copy(d,2,666));
    hed2o.stat:=1;
    hed2o.argCnt:=0;
    hed2o.len_msg:=0;
    hed2o.len_dat:=0;
    p:=sizeof(head);
    move(hed2o,buf[p+1],sizeof(hed2o));
    inc(p,sizeof(hed2o));
    goto send;
    end;
  3:begin; {accounting}
    move(buf[p+1],hed3i,sizeof(hed3i));
    inc(p,sizeof(hed3i));
    inc(p,hed3i.argCnt);
    getString(b,hed3i.len_usr);
    getString(c,hed3i.len_prt);
    getString(d,hed3i.len_adr);
    c:=c+'@'+d;
    d:='';
    o:=sizeof(head)+sizeof(hed3i);
    for i:=1 to hed3i.argCnt do begin;
      inc(o);
      getString(a,buf[o]);
      d:=d+' '+a;
      end;
    a:='acct';
    if (hed3i.flag and 1<>0) then a:=a+'-none';
    if (hed3i.flag and 2<>0) then a:=a+'-start';
    if (hed3i.flag and 4<>0) then a:=a+'-stop';
    if (hed3i.flag and 8<>0) then a:=a+'-watchdog';
    append2log(a,b,ipAddr2string(con.adr)+' '+BStr(con.prt)+' '+c,copy(d,2,666));
    hed3o.stat:=1;
    hed3o.len_msg:=0;
    hed3o.len_dat:=0;
    p:=sizeof(head);
    move(hed3o,buf[p+1],sizeof(hed3o));
    inc(p,sizeof(hed3o));
    goto send;
    end;
  end;
WriteLn('got unknown type of packet!');
con.stat:=0;
exit;

f1:
WriteWordMSB(hed1o.len_msg,length(a));
hed1o.len_dat:=0;
p:=sizeof(head);
move(hed1o,buf[p+1],sizeof(hed1o));
inc(p,sizeof(hed1o));
move(a[1],buf[p+1],sizeof(a));
inc(p,length(a));
goto send;

acc:
a:=succPrompt;
hed1o.stat:=1;
hed1o.flag:=0;
append2log('authen',b,ipAddr2string(con.adr)+' '+BStr(con.prt),c+' ok!');
goto f1;

rej:
a:=failPrompt;;
hed1o.stat:=2;
hed1o.flag:=0;
append2log('authen',b,ipAddr2string(con.adr)+' '+BStr(con.prt),c+' failed!');
goto f1;

send:
WriteLongMSB(head.siz,p-sizeof(head));
if (head.flg and 1=0) then encdecTacacsPacket(buf);
pipeLineSend(con.pipe,buf,p);
End;
