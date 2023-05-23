Function gotOnePacket(var d:OneRadiusPacketRecord):Boolean;
Label f1,f2,acc,rej,snd;
Var
  authentic:Boolean;
  authProxy:String;
  authName:String;
  authPass:String;
  authChal:String;
  authType:LongInt;
  authCuid:String;
  authRuid:LongInt;
  authSrcN:String;
  authTrgN:String;
  acctType:LongInt;
  acctRxPk:LongInt;
  acctTxPk:LongInt;
  acctRxBt:LongInt;
  acctTxBt:LongInt;
  acctTime:LongInt;
  i,o:LongInt;
  a,b:String;
  ab:array[0..128] of byte absolute a;
  ab0:byte absolute a;

Procedure addAccountingData(d:String;i:LongInt);
Begin;
if (i=0) then exit;
b:=b+d+':'+BStr(i);
End;

Begin;
gotOnePacket:=False;
i:=ReadWordMSB(d.len);
if (i>d.siz) then begin;
  WriteLn('got truncated packet from '+ipAddr2string(d.adr)+' '+BStr(d.prt));
  exit;
  end;
d.siz:=i-OneRadiusPacketHeader;
if (d.siz<0) then begin;
  WriteLn('got too short packet from '+ipAddr2string(d.adr)+' '+BStr(d.prt));
  exit;
  end;
WriteLn('got '+getRadiusPacketCode(d.cod)+' from '+ipAddr2string(d.adr)+' '+BStr(d.prt));
if not (d.cod in [1,4]) then exit;
authentic:=(d.cod=1);
if not authentic then begin;
  fillchar(requestAuth,sizeof(requestAuth),0);
  b:=calcRadiusRespAuth(d);
  ab0:=sizeof(d.auth);
  move(d.auth,ab[1],sizeof(d.auth));
  if (a<>b) then begin;
    WriteLn('got with invalid authenticator field!');
    exit;
    end;
  end;
move(d.auth,requestAuth,sizeof(requestAuth));
{$ifdef debug}dumpOneRadiusPacket(d,not authentic);{$endif}
d.pos:=0;
ab0:=sizeof(requestAuth);
move(requestAuth,a[1],sizeof(requestAuth));
authChal:=a;
authPass:='';
authName:='';
authType:=0;
authSrcN:='';
authTrgN:='';
authProxy:='';
authCuid:='';
acctType:=0;
acctRxPk:=0;
acctTxPk:=0;
acctRxBt:=0;
acctTxBt:=0;
acctTime:=0;
f1:
a:=getRadiusAttrib(d);
if (ab0=0) then goto f2;
i:=ab[1];
a:=copy(a,2,666);
case i of
  1:authName:=a; {user-name}
  2:begin; {user-password}
    authPass:=a;
    authType:=1;
    end;
  3:begin; {chap-password}
    authPass:=a;
    authType:=2;
    end;
  60:begin; {chap-challenge}
    authChal:=a;
    authType:=2;
    end;
  79:begin; {eap-message}
    authPass:=a;
    authType:=3;
    end;
  31:authSrcN:=a; {calling-station-id}
  30:authTrgN:=a; {called-station-id}
  66:authSrcN:=a; {tunnel-client-endpoint}
  67:authTrgN:=a; {tunnel-server-endpoint}
  80:begin; {message-authenticator}
    i:=d.pos-ab0+1;
    fillchar(d.dat[i],ab0,0);
    b:=calcRadiusMsgAuth(d);
    if (a=b) then goto f1;
    WriteLn('message-authenticator is invalid!');
    exit;
    end;
  33:authProxy:=a; {proxy-state}
  40:acctType:=ReadLongMSB(ab[1]); {acct-status-type}
  47:acctRxPk:=ReadLongMSB(ab[1]); {acct-input-packets}
  48:acctTxPk:=ReadLongMSB(ab[1]); {acct-output-packets}
  42:acctRxBt:=ReadLongMSB(ab[1]); {acct-input-octets}
  43:acctTxBt:=ReadLongMSB(ab[1]); {acct-output-octets}
  46:acctTime:=ReadLongMSB(ab[1]); {acct-session-time}
  89:authCuid:=a; {changeable-user-id}
  end;
goto f1;

f2:
d.siz:=0;
if not authentic then begin;
  d.cod:=5;
  case acctType of
    1:a:='start';
    2:a:='stop';
    3:a:='update';
    9:a:='tunStart';
    10:a:='tunStop';
    12:a:='linkStart';
    13:a:='linkStop';
    else goto snd;
    end;
  b:='';
  addAccountingData(' rp',acctRxPk);
  addAccountingData(' tp',acctTxPk);
  addAccountingData(' rb',acctRxBt);
  addAccountingData(' tb',acctTxBt);
  addAccountingData(' tim',acctTime);
  append2log(a,authName,ipAddr2string(d.adr)+' '+BStr(d.prt),b);
  goto snd;
  end;

case authType of
  1:begin; {pap}
    b:=encdecRadiusPasswd(authPass,true);
    i:=pos(#0,b);
    if (i>0) then b:=copy(b,1,i-1);
    if (authenticateCurrentProcess<>'') then begin;
      a:='radius at '+ipAddr2string(d.adr)+' '+BStr(d.prt)+' '+authSrcN+' '+authTrgN;
      if (authenticateOneUser(authName,b,a,LoginFlags_accessRad,authRuid)=0) then goto acc;
      goto rej;
      end;
    if CheckOneUser(authName,a,authRuid) then goto rej;
    if (a=b) then goto acc;
    goto rej;
    end;
  2:begin; {chap}
    if (authenticateCurrentProcess<>'') then begin;
      WriteLn('got chap, but pap forced!');
      goto rej;
      end;
    if CheckOneUser(authName,a,authRuid) then goto rej;
    a:=copy(authPass,1,1)+a+authChal;
    i:=ab0;
    CryptoImmHasher(md5algoNum,'','',ab[1],i);
    ab0:=i;
    if (copy(authPass,2,666)=a) then goto acc;
    goto rej;
    end;
  3:begin; {eap}
    if (authenticateCurrentProcess<>'') then begin;
      WriteLn('got eap, but pap forced!');
      goto rej;
      end;
    a:=authPass;
    if (ab[1]<>2) then goto rej
    i:=ReadWordMSB(ab[3]);
    if (i>255) then i:=255;
    ab0:=i;
    case ab[5] of
      1:begin; {identity}
        ab0:=32;
        for i:=1 to ab0 do ab[i]:=Random($100);
        RandomNum:=(RandomNum+1) and $ff;
        RandomDat[RandomNum]:=a;
        a:=#4+chr(ab0)+a;
        d.cod:=11;
        putRadiusAttrib(d,79,#1+chr(RandomNum)+#0+chr(ab0+4)+a);
        goto snd;
        end;
      4:begin; {md5challenge}
        move(ab[6],authChal,sizeof(authChal));
        authName:=copy(a,length(authChal)+7,666);
        o:=ab[2];
        if CheckOneUser(authName,a,authRuid) then goto rej;
        a:=chr(o)+a+RandomDat[o];
        i:=ab0;
        CryptoImmHasher(md5algoNum,'','',ab[1],i);
        ab0:=i;
        if (authChal=a) then goto acc;
        goto rej;
        end;
      end;
    WriteLn('got unknown eap type!');
    goto rej;
    end;
  else goto rej;
  end;

rej:
append2log('auth',authName,ipAddr2string(d.adr)+' '+BStr(d.prt),authSrcN+' '+authTrgN+' failed!');
d.cod:=3;
if (authType=3) then putRadiusAttrib(d,79,#4+authPass[2]+#0#4);
goto snd;

acc:
append2log('auth',authName,ipAddr2string(d.adr)+' '+BStr(d.prt),authSrcN+' '+authTrgN+' ok!');
d.cod:=2;
d.siz:=attribs2addr.siz;
move(attribs2addr.dat,d.dat,d.siz);
if (authCuid<>'') then putRadiusAttrib(d,89,BStr(authRuid));
if (authType=3) then putRadiusAttrib(d,79,#3+authPass[2]+#0#4);
goto snd;

snd:
if (authProxy<>'') then putRadiusAttrib(d,33,authProxy);
putRadiusAttrib(d,80,#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
WriteWordMSB(d.len,d.siz+OneRadiusPacketHeader);
a:=calcRadiusMsgAuth(d);
move(ab[1],d.dat[d.siz-ab0+1],ab0);
a:=calcRadiusRespAuth(d);
move(ab[1],d.auth,sizeof(d.auth));
WriteLn('sending '+getRadiusPacketCode(d.cod));
{$ifdef debug}dumpOneRadiusPacket(d,not authentic);{$endif}
inc(d.siz,OneRadiusPacketHeader);
gotOnePacket:=True;
Exit;
End;
