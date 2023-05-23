Function doConn(var con:OneConnectionRecord;conN:LongInt):Boolean;
Label f1,f2;
Var buf:array[1..4*1024] of byte;

Procedure sendText(a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
if (ab0=0) then exit;
pipeLineSend(con.pipe,ab[1],ab0);
End;

Procedure sendResp(a:String);
Begin;
sendText(a);
sendText(#13#10);
End;

Procedure sendEntry(typ,prt:LongInt;txt,hst,url:String);
Var s:LongInt;

procedure addT(a:String);
begin;
move(a[1],buf[s+1],255);
inc(s,length(a));
end;

procedure addS;
begin;
inc(s);
buf[s]:=9;
end;

Begin;
if (prt=0) then prt:=serverPort;
if (hst='') then hst:=ipAddr2string(serverAddr);
buf[1]:=typ;s:=1;
addT(txt);addS;
addT(url);addS;
addT(hst);addS;
addT(BStr(prt));
inc(s);buf[s]:=13;
inc(s);buf[s]:=10;
pipeLineSend(con.pipe,buf,s);
End;

Function getNextPart(var a:string):String;
Var i:LongInt;
Begin;
i:=pos('|',a);
if (i<1) then i:=666;
getNextPart:=xLevesz(copy(a,1,i-1));
a:=copy(a,i+1,255);
if (length(a)<128) then a:=a+xtRead(con.text,128);
End;

function convDate(d:xDirEntryDateTimeRec):string;
function x(i:longint):string;var a:string;begin; a:=bstr(i);while (length(a)<2) do a:='0'+a;x:=a; end;
begin;
convDate:=x(d.year)+'-'+x(d.month)+'-'+x(d.day)+' '+x(d.hour)+':'+x(d.minute)+':'+x(d.second);
end;




Var
  visNtry:record
    year:word;month:word;day:Word;
    hour:word;min:word;sec:Word;
    ipaddr:array[1..16] of char;
    agent:String;
    filler:array[1..228] of char;
    end;
  dirNtry:xDirEntryRec;
  bb:Byte;
  bc:Char absolute bb;
  i,o,p:LongInt;
  a,b,c:String;
  f:xFile;
Begin;
if (GetTimePast(con.time)>5*60) then begin;
  WriteLn('connection timed out with '+ipAddr2string(con.adr)+' '+BStr(con.prt));
  sendResp('timeout!');
  doConn:=True;
  exit;
  end;
doConn:=False;
case con.stat of
  5:begin; {binary file}
    pipeLineStats(con.pipe,i,o,p);
    if (i=0) then goto f2;
    if (p>sizeof(buf)) then p:=sizeof(buf);
    p:=p and (-512);
    if (p<512) then exit;
    if (con.pos and $1ff<>0) then p:=512-(con.pos and $1ff);
    o:=con.siz-con.pos;
    if (o<1) then begin; con.stat:=0;exit; end;
    if (o>sizeof(buf)) then o:=sizeof(buf);
    if (p>o) then p:=o;
    if (xBlockRead(con.file,buf,p)<>0) then goto f1;
    pipeLineSend(con.pipe,buf,p);
    inc(con.pos,p);
    con.time:=CurrentTime;
    end;
  6:begin; {text file}
    pipeLineStats(con.pipe,i,o,p);
    if (i=0) then goto f2;
    if (p<1024) then exit;
    if xtEOF(con.text) then begin;
      sendResp('');
      sendResp('.');
      con.stat:=0;
      exit;
      end;
    a:=xtRead(con.text,250);
    sendText(a);
    con.time:=CurrentTime;
    if not xtEOL(con.text) then exit;
    sendResp(xtReadLn(con.text,255));
    a:=xtRead(con.text,120);
    if (copy(a,1,1)='.') then a:='.'+a;
    sendText(a);
    end;
  8:begin; {gopher file}
    pipeLineStats(con.pipe,i,o,p);
    if (i=0) then goto f2;
    if (p<1024) then exit;
    if xtEOF(con.text) then begin;
      sendResp('.');
      con.stat:=0;
      exit;
      end;
    a:=xtRead(con.text,250);
    o:=ord(a[1]);
    if (a='') or (o=$3b) then begin;
      xtReadLn(con.text,255);
      exit;
      end;
    a:=copy(a,2,255);
    c:=getNextPart(a);
    p:=BVal(getNextPart(a));
    b:=getNextPart(a);
    a:=a+xtReadLn(con.text,255);
    while (copy(a,1,1)=' ') do a:=copy(a,2,255);
    sendEntry(o,p,a,c,b);
    con.time:=CurrentTime;
    end;
  7:begin; {directory}
    pipeLineStats(con.pipe,i,o,p);
    if (i=0) then goto f2;
    if (p<1024) then exit;
    if (xDirRead(con.file,dirNtry)<>0) then c:='' else c:=dirNtry.name;
    if (c='') then begin;
      sendResp('.');
      con.stat:=0;
      exit;
      end;
    p:=$39;
    a:=#13+kicsi(copy(xFileName(c,3),2,255))+#13;
    if (pos(a,textExt)<>0) then p:=$30;
    if (pos(a,gophExt)<>0) then p:=$31;
    a:=BStr(dirNtry.size);
    if (dirNtry.rights and xRights_Directory<>0) then begin;
      a:='dir';
      c:=c+'/';
      p:=$31;
      end;
    while (length(a)<10) do a:=' '+a;
    a:=a+' '+convDate(dirNtry.modified)+' '+c;
    b:=con.cmd+c;
    sendEntry(p,0,a,'',b);
    con.time:=CurrentTime;
    end;
  4:begin; {got url}
    con.time:=CurrentTime;
    con.stat:=0;
    c:=con.cmd;
    kicserel('\','/',c);
    if (copy(c,1,1)='/') then c:=copy(c,2,255);
    b:='';
    while (c<>'') do begin;
      i:=pos('/',c);
      if (i=0) then begin;
        if (c='.') or (c='..') then begin; c:=c+'\';continue; end;
        b:=b+c;
        break;
        end;
      a:=copy(c,1,i-1);
      c:=copy(c,i+1,255);
      if (a='') then continue;
      if (a='.') then continue;
      if (a<>'..') then begin; b:=b+a+'\';continue; end;
      o:=0;
      for i:=1 to length(b)-1 do if (b[i]='\') then o:=i;
      b:=copy(b,1,o);
      end;
    con.cmd:=b;
    b:=serverPath+b;
    if (xtOpen(con.text,b,true)=0) then begin;
      con.file:=con.text.CurrentFileRec;
      a:=#13+kicsi(copy(xFileName(b,3),2,255))+#13;
      i:=5;
      if (pos(a,textExt)<>0) then i:=6;
      if (pos(a,gophExt)<>0) then i:=8;
      con.pos:=0;
      con.siz:=xFileSize(con.file);
      con.stat:=i;
      exit;
      end;
    a:=repairPath(con.cmd);
    kicserel('\','/',a);
    con.cmd:=a;
    b:=repairPath(b);
    if (xOpen(f,b+'visitor.data',xGenFilMod_rw)=0) then begin;
      fillchar(visNtry,sizeof(visNtry),0);
      xGetDate(visNtry.year,visNtry.month,visNtry.day);
      xGetTime(visNtry.hour,visNtry.min,visNtry.sec);
      visNtry.agent:='gopher';
      move(con.adr,visNtry.ipaddr,sizeof(visNtry.ipaddr));
      xSeek(f,xFileSize(f));
      xBlockWrite(f,visNtry,sizeof(visNtry));
      xClose(f);
      end;
    if (xtOpen(con.text,b+'autostart.gopher',true)=0) then begin;
      con.file:=con.text.CurrentFileRec;
      con.stat:=8;
      exit;
      end;
    if (xDirOpen(con.file,b)<>0) then goto f1;
    b:=con.cmd;
    b:=copy(b,1,length(b)-1);
    o:=0;
    for i:=1 to length(b) do if (b[i]='/') then o:=i;
    a:=copy(b,1,o);
    if (b<>'') then begin;
      sendEntry($31,0,'root directory','','');
      sendEntry($31,0,'parent directory','',a);
      end;
    con.stat:=7;
    exit;
    f1:
    sendResp('the requested url not found!');
    con.stat:=0;
    exit;
    end;
  3:begin; {parameter}
    i:=sizeof(bb);
    if (pipeLineRecv(con.pipe,bb,i)<>0) then i:=0;
    if (i<>sizeof(bb)) then begin;
      pipeLineStats(con.pipe,i,o,o);
      if (i<>0) then exit;
      goto f2;
      end;
    case bb of
      13:exit;
      10:begin; con.stat:=4;exit; end;
      end;
    con.prm:=con.prm+bc;
    con.time:=CurrentTime;
    end;
  2:begin; {command}
    i:=sizeof(bb);
    if (pipeLineRecv(con.pipe,bb,i)<>0) then i:=0;
    if (i<>sizeof(bb)) then begin;
      pipeLineStats(con.pipe,i,o,o);
      if (i<>0) then exit;
      f2:
      con.stat:=0;
      WriteLn('connection lost with '+ipAddr2string(con.adr)+' '+BStr(con.prt));
      exit;
      end;
    case bb of
      9:begin; con.stat:=3;exit; end;
      13:exit;
      10:begin; con.stat:=4;exit; end;
      end;
    con.cmd:=con.cmd+bc;
    con.time:=CurrentTime;
    end;
  1:begin; {init}
    if TCPlookConnected(con.pipe,con.adr,con.prt,i) then begin;
      doConn:=(con.pipe=0);
      exit;
      end;
    WriteLn('connection accepted from '+ipAddr2string(con.adr)+' '+BStr(con.prt));
    con.stat:=2;
    con.cmd:='';
    con.prm:='';
    end;
  0:begin; doConn:=True;exit; end; {disconnect}
  else begin; doConn:=True;exit; end;
  end;
End;
