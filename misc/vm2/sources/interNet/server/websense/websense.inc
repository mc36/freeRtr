type
  oneRequestHeaderRecord=record
    pckSiz:Word;
    vers:Word;          {$0420}
    magic1:LongInt;     {$008a0000}
    serial:LongInt;
    magic2:LongInt;     {$00010000}
    srcAdr:LongInt;
    trgAdr:LongInt;
    urlLen:Word;        {length of url}
    end;
  oneResponseHeaderRecord=record
    pckSiz:Word;
    vers:Word;          {$0420}
    magic1:LongInt;     {$008a0000}
    serial:LongInt;
    magic2:LongInt;     {$00000000/$00010001}
    rplLen:LongInt;        {length of reply; 0=allow}
    end;




Function doConn(var con:OneConnectionRecord;conN:LongInt):Boolean;
Const maxlen=255;
Label f1;
Var
  buf:array[1..4*1024] of byte;
  reqHdr:oneRequestHeaderRecord;
  repHdr:oneResponseHeaderRecord;
  i,o,p:LongInt;
  a,b,c:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;

Procedure putCh(i:LongInt);
Begin;
inc(p);
buf[p]:=i;
End;

Procedure putStr(a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
Begin;
for i:=1 to ab0 do putCh(ab[i]);
End;

Begin;
doConn:=False;
if (GetTimePast(con.time)>60*5) then begin;
  con.stat:=0;
  doConn:=True;
  exit;
  end;
case con.stat of
  3:begin; {data}
    pipeLineStats(con.pipe,p,i,o);
    if (i<con.size) then begin;
      if (p=0) then goto f1;
      exit;
      end;
    con.stat:=2;
    con.time:=CurrentTime;
    i:=con.size;
    pipeLineRecv(con.pipe,buf,i);
    if (i<>con.size) then goto f1;
    move(con.buf,reqHdr,sizeof(reqHdr));
    i:=ReadWordMSB(reqHdr.urlLen);
    if (i>maxlen) then i:=maxlen;
    move(buf,a[1],i);
    ab0:=i;
    if (i=0) then p:=1 else p:=testOneUrl(a);
    if (p=1) then b:='permit' else b:='deny';
    b:=b+' '+ipAddr2string(con.addr)+' '+BStr(con.port);
    c:='1234';
    move(reqHdr.srcAdr,c[1],sizeof(reqHdr.srcAdr));
    c:=IPv4addressPrefix+c;
    b:=b+' '+ipAddr2string(c[1]);
    move(reqHdr.trgAdr,c[1],sizeof(reqHdr.trgAdr));
    c:=IPv4addressPrefix+c;
    b:=b+' '+ipAddr2string(c[1]);
    append2log(b,a);
    WriteWordMSB(repHdr.vers,$420);
    WriteLongMSB(repHdr.magic1,$008a0000);
    repHdr.serial:=reqHdr.serial;
    if (p=1) then begin;
      WriteLongMSB(repHdr.magic2,$00000000);
      repHdr.rplLen:=0;
      WriteWordMSB(repHdr.pckSiz,sizeof(repHdr));
      pipeLineSend(con.pipe,repHdr,sizeof(repHdr));
      exit;
      end;
    WriteLongMSB(repHdr.magic2,$00010001);
    p:=sizeof(repHdr);
    putStr('HTTP/1.0 302 moved'#13#10);
    putStr('Location: ');
    putStr(blockUrlBeg);
    if (blockUrlBeg<>'') then for o:=1 to ab0 do begin;
      i:=ab[o];
      case i of
        32:putCh(43);
        0..31,255,58,47,38,43,61,63,35:putStr('%'+byte2hextype(i));
        else putCh(i);
        end;
      end;
    putStr(blockUrlEnd);
    putStr(#13#10);
    putStr('Pragma: no-cache'#13#10);
    putStr('Cache-Control: no-cache'#13#10);
    putStr(#13#10);
    WriteLongMSB(repHdr.rplLen,p-sizeof(repHdr));
    WriteWordMSB(repHdr.pckSiz,p);
    move(repHdr,buf,sizeof(repHdr));
    pipeLineSend(con.pipe,buf,p);
    end;
  2:begin; {header}
    pipeLineStats(con.pipe,p,i,o);
    if (i<sizeof(reqHdr)) then begin;
      if (p=0) then goto f1;
      exit;
      end;
    i:=sizeof(reqHdr);
    pipeLineRecv(con.pipe,con.buf,i);
    if (i<>sizeof(reqHdr)) then goto f1;
    move(con.buf,reqHdr,sizeof(reqHdr));
    i:=readWordMSB(reqHdr.pckSiz);
    if (i>sizeof(buf)) then begin;
      WriteLn('got too big packet!');
      goto f1;
      end;
    if (i<sizeof(reqHdr)) then begin;
      WriteLn('got too short packet!');
      goto f1;
      end;
    con.size:=i-sizeof(reqHdr);
    con.time:=CurrentTime;
    con.stat:=3;
    end;
  1:begin; {init}
    if TCPlookConnected(con.pipe,con.addr,con.port,i) then begin;
      doConn:=(con.pipe=0);
      exit;
      end;
    WriteLn('connection accepted from '+ipAddr2string(con.addr)+' '+BStr(con.port));
    con.stat:=2;
    end;
  0:goto f1; {disconnect}
  else begin; f1: con.stat:=0;doConn:=True;exit; end;
  end;
End;
