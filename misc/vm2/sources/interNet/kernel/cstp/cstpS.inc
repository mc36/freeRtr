Function doConn(var con:OneConnectionRecord):Boolean;

Procedure putLine(a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
{$ifdef debug}writeln('--> "'+a+'"');{$endif}
pipeLineSend(con.pipL,ab[1],ab0);
a:=#13#10;
pipeLineSend(con.pipL,ab[1],ab0);
End;

Function getChar:LongInt;
Var
  b:byte;
  i,o:LongInt;
Begin;
getChar:=-1;
i:=sizeof(b);
pipeLineRecv(con.pipL,b,i);
if (i=sizeof(b)) then begin;
  getChar:=b;
  exit;
  end;
pipeLineStats(con.pipL,o,i,i);
if (o<>0) then exit;
con.stat:=666;
End;

Function getLine:Boolean;
Label f1,f2;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
Begin;
getLine:=True;
a:=con.read;
ab0:=0;
f1:
i:=getChar;
if (i<0) then begin;
  con.read:=a;
  exit;
  end;
if (i=13) then goto f1;
if (i=10) then goto f2;
inc(ab0);
ab[ab0]:=i;
goto f1;
f2:
con.time:=currentTime;
con.read:=a;
{$ifdef debug}writeln('<-- "'+a+'"');{$endif}
getLine:=False;
End;


Label f1;
Var
  buf:array[1..4096] of byte;
  hdr:onePacketHeader absolute buf;
  i,o,p:LongInt;
  a,b:String;
Begin;
doConn:=false;
if (con.stat=0) then exit;
if (GetTimePast(con.time)>5*60) then begin;
  WriteLn('connection timed out with '+ipAddr2string(con.addr)+' '+BStr(con.port));
  putLine('HTTP/1.1 404 error');
  putLine('');
  putLine('connection timed out!');
  doConn:=True;
  exit;
  end;
doConn:=False;
case con.stat of
  100:begin; {connection established}
    o:=sizeof(buf);
    pipeLineRecv(con.pipU,buf,o);
    if (o=0) then begin;
      pipeLineStats(con.pipU,o,i,i);
      if (o=0) then goto f1;
      end else begin;
      move(buf,buf[sizeof(hdr)],o);
      dec(o);
      WriteLongMSB(hdr.id,packetMagicCookie);
      WriteWordMSB(hdr.len,o);
      hdr.typ:=0;
      pipeLineSend(con.pipL,buf,o+sizeof(hdr));
      con.time:=currentTime;
      end;
    pipeLineStats(con.pipL,p,i,o);
    if (p=0) then goto f1;
    if (con.bufS>0) then begin;
      if (i<con.bufS) then exit;
      o:=con.bufS;
      pipeLineRecv(con.pipL,buf[2],o);
      if (o<>con.bufS) then goto f1;
      buf[1]:=$11;
      pipeLineSend(con.pipU,buf,o+1);
      con.bufS:=0;
      con.time:=currentTime;
      exit;
      end;
    if (i<sizeof(hdr)) then exit;
    o:=sizeof(hdr);
    pipeLineRecv(con.pipL,buf,o);
    if (o<>sizeof(hdr)) then goto f1;
    if (ReadLongMSB(hdr.id)<>packetMagicCookie) then goto f1;
    con.bufS:=ReadWordMSB(hdr.len);
    con.time:=currentTime;
    end;
  5:begin; {wait for upper}
    if (con.pipU=0) then exit;
    con.bufS:=0;
    con.stat:=100;
    end;
  4:begin; {read content}
    while (con.bufS>0) do begin;
      if (getChar<0) then exit;
      dec(con.bufS);
      end;
    case con.need of
      1:begin;  {redirect}
        b:='<html><body>moved!</html></body>';
        putLine('HTTP/1.1 303 moved');
        putLine('Server: '+proggyName);
        putLine('Content-Type: text/html');
        putLine('Content-Length: '+BStr(Length(b)+2));
        a:=ipAddr2string(servAddr);
        if not isAddressIPv4mask(servAddr) then a:='['+a+']';
        putLine('Location: https://'+a+':'+BStr(servPort)+myUrl);
        putLine('Set-Cookie: webvpncontext=defctx; path=/');
        putLine('Set-Cookie: webvpn=; expires=Thu, 01 Jan 1970 22:00:00 GMT; path=/');
        putLine('Connection: Keep-Alive');
        putLine('');
        putLine(b);
        end;
      2:begin;  {login screen}
        b:='<html><body><form method=post action='+myUrl+'>login<br>'+
           '<input name=username><br><input type=password name=password><br>'+
           '<input type=submit value=Login name=Login></form></body></html>';
        putLine('HTTP/1.1 200 ok');
        putLine('Server: '+proggyName);
        putLine('Content-Type: text/html');
        putLine('Content-Length: '+BStr(Length(b)+2));
        putLine('Set-Cookie: webvpncontext=defctx; path=/');
        putLine('Set-Cookie: webvpn=; expires=Thu, 01 Jan 1970 22:00:00 GMT; path=/');
        putLine('Connection: Keep-Alive');
        putLine('');
        putLine(b);
        end;
      3:begin;  {authenticated}
        b:='<html><body>authenticated successfully!</body></html>';
        putLine('HTTP/1.1 200 ok');
        putLine('Server: '+proggyName);
        putLine('Content-Type: text/html');
        putLine('Content-Length: '+BStr(Length(b)+2));
        putLine('Set-Cookie: webvpn=defsess@defctx; path=/');
        putLine('Set-Cookie: webvpncontext=; expires=Thu, 01 Jan 1970 22:00:00 GMT; path=/');
        putLine('Connection: Keep-Alive');
        putLine('');
        putLine(b);
        end;
      4:begin;  {connected}
        putLine('HTTP/1.1 200 ok');
        putLine('Server: '+proggyName);
        putLine('X-CSTP-Version: 1');
        putLine('X-CSTP-Address: '+ipAddr2string(con.dele));
        if isAddressIPv4mask(con.dele) then a:='255.255.255.255' else a:='ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff';
        putLine('X-CSTP-Netmask: '+a);
        putLine('X-CSTP-Keep: false');
        putLine('X-CSTP-Lease-Duration: 43200');
        putLine('X-CSTP-DPD: 60');
        putLine('');
        a:=con.cmnd;
        {$ifdef debug}
        WriteLn('starting '+a+'...');
        {$endif}
        i:=pos(' ',a);
        if (i<1) then i:=666;
        if (xExecBgnd(copy(a,1,i-1),copy(a,i+1,255),o)<>0) then o:=0;
        con.stat:=5;
        exit;
        end;
      end;
    con.stat:=2;
    end;
  3:begin; {read headers}
    if getLine then exit;
    b:=con.read;
    con.read:='';
    if (b='') then begin;
      con.stat:=4;
      exit;
      end;
    i:=pos(':',b);
    a:=kicsi(xLevesz(copy(b,1,i-1)));
    b:=xLevesz(copy(b,i+1,666));
    if (a='content-length') then con.bufS:=BVal(b);
    end;
  2:begin; {waiting for command}
    if getLine then exit;
    b:=kicsi(con.read);
    con.read:='';
    con.bufS:=0;
    a:=getWord(b);
    o:=0;
    if (a='get') then begin;
      a:=getWord(b);
      i:=pos('://',a);
      if (i<>0) then begin;
        a:=copy(a,i+3,666);
        a:=copy(a,pos('/',a),666);
        end;
      if (kicsi(a)=myUrl) then o:=2 else o:=1;
      end;
    if (a='post') then o:=3;
    if (a='connect') then o:=4;
    con.need:=o;
    con.stat:=3;
    end;
  1:begin; {init}
    if TCPlookConnected(con.pipL,con.addr,con.port,i) then begin;
      doConn:=(con.pipL=0);
      exit;
      end;
    WriteLn('connection accepted from '+ipAddr2string(con.addr)+' '+BStr(con.port));
    con.time:=currentTime;
    con.read:='';
    con.bufS:=0;
    con.stat:=2;
    end;
  0:;; {disconnect}
  else begin; f1:con.stat:=666;doConn:=True;exit; end;
  end;
End;
