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
{$sysinc datetime.inc}
{$sysinc random.inc}
{$sysinc crypto.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$sysinc inet_dns.inc}
{$sysinc inet_cgi.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Const
  ProggyName='webMail';
  ProggyVer='v1.0/CGi';

{$include webmail1.inc}
{$include webmail2.inc}
{$include webmail3.inc}
{$include webmail4.inc}


Label kezd,err,vege;
Var
  md5algorithm:LongInt;
  i,o,p,q:LongInt;
  a,b,c,d:String;
  t:xtText;
BEGIN;
WriteLn(ProggyName+' '+ProggyVer+', done by Mc at '#%date' '#%time'.');
if CryptoStartActions then halt(3);
CryptoGetHasherList(usrD,p);
md5algorithm:=CryptoFindOneAlgo(usrD,p,'md5');
if (md5algorithm<1) then halt(4);
timer2start;
Randomize;
a:=GetAllParameters;
if (a='\\\purge\\\') then begin;
  PurgeTheIDdatabase;
  halt(0);
  end;
if OpenRequestFile(a) then Halt(1);
if FindNextRequest(True) then Halt(2);

url:='';
client:='';
p:=0;
id:='';
data1:='';
data2:='';
data3:='';
repeat
  a:=Kicsi(CurrentReqNam);
  Case CurrentReqTyp of
    1:begin;
      if (a='url') then url:='/'+ReadRequestString;
      if (a='client') then client:=ReadRequestString;
      end;
    2:begin;
      if (a='id') then id:=ReadRequestString;
      if (a='pg') then p:=BVal(ReadRequestString);
      if (a='data1') then data1:=ReadRequestString;
      if (a='data2') then data2:=ReadRequestString;
      if (a='data3') then data3:=ReadRequestString;
      end;
    3:;
    end;
  until FindNextRequest(False);

if (id='') then begin;
  usrN:=0;
  fillChar(usrD,sizeof(usrD),0);
  repeat
    i:=0;
    while (i<1) do i:=Random($7fffffff);
    until FindOneIDvalue(i);
  usrD.id:=i;
  usrD.addr:=client;
  usrD.stat:=1;
  i:=BVal(data3);
  if (i<1) or (i>3) then i:=1;
  usrD.frmt:=i;
  usrD.time:=currentTime;
  UpdateCurrentIDdata;
  id:=BStr(usrN)+'.'+BStr(usrD.id);
  WriteResponseHeader(2,url+'?id='+id+'&pg=0');
  CloseRequestFile;
  end;

i:=pos('.',id);
usrN:=BVal(copy(id,1,i-1));
if FindOneIDvalue(BVal(copy(id,i+1,255))) then begin;
  err:
  WriteResponseHeader(3,'html');
  WriteLnResponseString('<html><head><title>'+proggyName+'</title></head><body>');
  WriteLnResponseString('invalid request arrived (id='+id+')!<br>');
  WriteLnResponseString('hopely your login has expired!<br>');
  WriteLnResponseString('please click <a href="'+url+'?pg=0" target="_parent">here</a> to log in again!<br>');
  WriteLnResponseString('</body></html>');
  CloseRequestFile;
  exit;
  end;
if (usrD.addr<>client) then goto err;
id:=BStr(usrN)+'.'+BStr(usrD.id);

beger:='';
kezd:
if (p=0) then begin; {login screen}
  if (usrD.stat<>1) then goto err;
  a:=GetDateForFiles;
  for i:=1 to 5 do a:=a+BStr(Random($7fffffff));
  usrD.path:=a;
  usrD.time:=currentTime;
  usrD.stat:=1;
  UpdateCurrentIDdata;
  case usrD.frmt of
    1,2:begin;
      WriteResponseHeader(3,'html');
      WriteLnResponseString('<html><head><title>'+proggyName+'</title>');
      WriteLnResponseString('<script language="javascript" type="text/javascript" src="webmail1.js"></script>');
      WriteLnResponseString('<script language="javascript" type="text/javascript" src="webmail2.js"></script>');
      WriteLnResponseString('</head><body onLoad="doStartup();">');
      WriteLnResponseString('<form name="clear" action="'+url+'">');
      WriteLnResponseString('<input type="hidden" name="pg" value="1">');
      WriteLnResponseString('<input type="hidden" name="id" value="'+id+'">');
      WriteLnResponseString('<p align="center"><b><font size="+3">Login</font></b></u><br><br>');
      WriteLnResponseString('username: <input type="text" name="data1" size="40" value=""><br>');
      WriteLnResponseString('password: <input type="password" name="data2" size="40" value=""><br>');
      WriteLnResponseString('mailer style: <select name="data3" size="1">');
      WriteLnResponseString('  <option selected value="1">html with frames</option><option value="2">html without frames</option><option value="3">wml (wap)</option>');
      WriteLnResponseString('  </select><br>');
      WriteLnResponseString('<br>');
      WriteLnResponseString('<input type="submit" name="todo2" value="cleartext login">');
      WriteLnResponseString('<input disabled type="button" name="todo3" value="secure login" onclick="doSubmit();">');
      WriteLnResponseString('<input type="reset" name="todo1" value="cancel">');
      WriteLnResponseString('<input disabled type="button" name="todo4" value="debug info" onclick="doDebug();">');
      WriteLnResponseString('<br></p></form>');
      WriteLnResponseString('<form name="coded" action="bad://bad/bad">');
      WriteLnResponseString('<input type="hidden" name="pg" value="2">');
      WriteLnResponseString('<input type="hidden" name="id" value="'+id+'">');
      WriteLnResponseString('<input type="hidden" name="data1" value="">');
      WriteLnResponseString('<input type="hidden" name="data2" value=""><br>');
      WriteLnResponseString('<input type="hidden" name="data3" value="">');
      WriteLnResponseString('<input type="hidden" name="cookie" value="'+usrD.path+'"><br>');
      WriteLnResponseString('</form>');
      WriteLnResponseString('</body></html>');
      end;
    3:begin;
      WriteResponseHeader(3,'wml');
      WriteLnResponseString('<?xml version="1.0" encoding="ISO-8859-1"?>');
      WriteLnResponseString('<!DOCTYPE wml PUBLIC "-//WAPFORUM//DTD WML 1.1//EN" "http://www.wapforum.org/DTD/wml_1.1.xml">');
      WriteLnResponseString('<wml><card id="wapmail" title="'+proggyName+'"><p>');
      WriteLnResponseString('username: <input type="text" format="*m" name="data1"/><br/>');
      WriteLnResponseString('password: <input type="password" format="*m" name="data2"/><br/>');
      WriteLnResponseString('<anchor>login!<go href="'+url+'">');
      WriteLnResponseString('<postfield name="data1" value=''$(data1)''/>');
      WriteLnResponseString('<postfield name="data2" value=''$(data2)''/>');
      WriteLnResponseString('<postfield name="data3" value=''3''/>');
      WriteLnResponseString('<postfield name="pg" value=''1''/>');
      WriteLnResponseString('<postfield name="id" value='''+id+'''/>');
      WriteLnResponseString('</go></anchor>');
      WriteLnResponseString('<anchor>cancel<refresh>');
      WriteLnResponseString('<setvar name="data1" value=""/>');
      WriteLnResponseString('<setvar name="data2" value=""/>');
      WriteLnResponseString('</refresh></anchor>');
      WriteLnResponseString('</p></card></wml>');
      end;
    else goto err;
    end;
  CloseRequestFile;
  exit;
  end;
if (p=1) then begin; {cleartext login}
  if (usrD.stat<>1) then goto err;
  i:=BVal(data3);
  if (i<1) or (i>3) then i:=1;
  usrD.frmt:=i;
  usrD.time:=currentTime;
  p:=0;
  BugOS_SetOwnerInfo(0);
  if GetOneUserPassword(data1,a,usrD.path,usrD.usrid) then goto kezd;
  if (a<>data2) then goto kezd;
  usrD.stat:=2;
  UpdateCurrentIDdata;
  p:=3;
  end;
if (p=2) then begin; {secure login}
  if (usrD.stat<>1) then goto err;
  i:=BVal(data3);
  if (i<1) or (i>3) then i:=1;
  usrD.frmt:=i;
  usrD.time:=currentTime;
  p:=0;
  b:=usrD.path;
  BugOS_SetOwnerInfo(0);
  if GetOneUserPassword(data1,a,usrD.path,usrD.usrid) then goto kezd;
  a:=b+a+b;
  i:=Length(a);
  CryptoImmHasher(md5algorithm,'','',a[1],i);
  a[0]:=chr(i);
  if (kicsi(data2)<>binary2digest(a)) then goto kezd;
  usrD.stat:=2;
  UpdateCurrentIDdata;
  p:=3;
  end;
if (usrD.stat<>2) then goto err;
BugOS_SetOwnerInfo(usrD.usrid);
case usrD.frmt of
  1,2:begin; {html}
    WriteResponseHeader(3,'html');
    WriteLnResponseString('<html><head><title>'+proggyName+'</title></head><body>');
    end;
  3:begin; {wap}
    WriteResponseHeader(3,'wml');
    WriteLnResponseString('<?xml version="1.0" encoding="ISO-8859-1"?>');
    WriteLnResponseString('<!DOCTYPE wml PUBLIC "-//WAPFORUM//DTD WML 1.1//EN" "http://www.wapforum.org/DTD/wml_1.1.xml">');
    WriteLnResponseString('<wml><card id="wapmail" title="'+proggyName+'"><p>');
    end;
  else goto err;
  end;
case p of
  3:begin; {login succeeded}
    WriteResponseHeader(2,url+'?id='+id+'&pg=10');
    CloseRequestFile;
    exit;
    end;
  10:begin; {frameset}
    case usrD.frmt of
      1:begin; {frames}
        WriteResponseHeader(3,'html');
        WriteLnResponseString('<html><head><title>'+proggyName+'</title></head>');
        WriteLnResponseString('<frameset rows="28%,*">');
        WriteLnResponseString('  <frameset cols="12%,*">');
        WriteLnResponseString('    <frame name="menu" src="'+url+'?id='+id+'&pg=11">');
        WriteLnResponseString('    <frame name="lista" src="'+url+'?id='+id+'&pg=12">');
        WriteLnResponseString('    </frameset>');
        WriteLnResponseString('  <frame name="level" src="'+url+'?id='+id+'&pg=13">');
        WriteLnResponseString('  </frameset>');
        WriteLnResponseString('<noframes><body>');
        WriteLnResponseString('your browser cannot handle frames!<br>');
        WriteLnResponseString('please click <a href="'+url+'?id='+id+'&pg=14">here</a> to get menu!<br>');
        WriteLnResponseString('</body></noframes></html>');
        CloseRequestFile;
        exit;
        end;
      2:begin; {html}
        PutMainMenu(true);
        end;
      3:begin; {wap}
        PutMainMenu(true);
        end;
      else goto err;
      end
    usrD.time:=currentTime;
    UpdateCurrentIDdata;
    end;
  11:begin; {menu}
    PutMainMenu(true);
    end;
  12:begin; {list of messages}
    PutMainMenu(false);
    generateMessageList(usrD.path);
    PutMainMenu(false);
    usrD.time:=currentTime;
    UpdateCurrentIDdata;
    end;
  13:begin; {empty}
    WriteResponseString('select one message!');
    end;
  14:begin; {change to noframes}
    usrD.frmt:=2;
    UpdateCurrentIDdata;
    PutMainMenu(true);
    end;
  15:begin; {quit}
    WriteLnResponseString('logout succeeded!');
    WriteOneChar(#10);
    WriteLnResponseString('click <a href="'+url+'?pg=0&amp;data3='+BStr(usrD.frmt)+'">here</a> to log in again!');
    i:=usrD.frmt;
    fillchar(usrD,sizeof(usrD),0);
    UpdateCurrentIDdata;
    usrD.frmt:=i;
    end;
  20:begin; {read message}
    PutMainMenu(false);
    kicserel('\','',data1);
    kicserel('/','',data1);
    if (xtOpen(t,usrD.path+data1+MessageExtension,true)<>0) then goto err;
    decodeOneHeader(t,a,b,c,d);
    WriteResponseString('from: <b>');WriteOneString(a);WriteLnResponseString('</b>');WriteOneChar(#10);
    WriteResponseString('to: <b>');WriteOneString(b);WriteLnResponseString('</b>');WriteOneChar(#10);
    WriteResponseString('date: <b>');WriteOneString(c);WriteLnResponseString('</b>');WriteOneChar(#10);
    WriteResponseString('subject: <b>');WriteOneString(d);WriteLnResponseString('</b>');WriteOneChar(#10);
    WriteLnResponseString('menu: <a href="'+url+'?id='+id+'&amp;pg=22&amp;data1='+data1+'">reply</a>');
    WriteLnResponseString('<a href="'+url+'?id='+id+'&amp;pg=24&amp;data1='+data1+'">delete</a>');
    decodeOneMessage(t);
    xtClose(t);
    PutMainMenu(false);
    usrD.time:=currentTime;
    UpdateCurrentIDdata;
    end;
  21:begin; {write message}
    PutMainMenu(false);
    PutMessageWriting1('','');
    PutMessageWriting2;
    end;
  22:begin; {reply message}
    PutMainMenu(false);
    if (xtOpen(t,usrD.path+data1+MessageExtension,true)<>0) then goto err;
    decodeOneHeader(t,a,b,c,d);
    d:=GetSubjectForReply(d);
    PutMessageWriting1(a,d);
    case usrD.frmt of
      1,2:begin; {html}
        beger:='> ';
        decodeOneMessage(t);
        beger:='';
        end;
      3:begin; {wap}
        end;
      end;
    xtClose(t);
    PutMessageWriting2;
    end;
  23:begin; {send message}
    if (xtOpen(t,usrD.path+UserConfigureFile,true)<>0) then goto err;
    a:=xtReadLn(t,255);
    if (a<>'') then a:=a+' ';
    c:=a+'<'+xtReadLn(t,255)+'>';
    xtClose(t);
    a:=GetMyFullFileName;
    if (xtOpen(t,xFileName(a,1)+xFileName(a,2)+ConfigurExtension,true)<>0) then goto err;
    xtReadLn(t,255);
    b:=xtReadLn(t,255);
    xtClose(t);
    if FindNextRequest(True) then goto err;
    while (1=1) do begin;
      if (CurrentReqTyp>1) then if (Kicsi(CurrentReqNam)='message') then break;
      if FindNextRequest(False) then goto err;
      end;
    if SendCurrentMessage(b,c,data1,data2) then a:='FAiLED' else a:='successful';
    WriteLnResponseString('message sending '+a+'!');
    PutMainMenu(false);
    usrD.time:=currentTime;
    UpdateCurrentIDdata;
    end;
  24:begin; {delete message}
    kicserel('\','',data1);
    kicserel('/','',data1);
    xErase(usrD.path+data1+MessageExtension);
    WriteLnResponseString('message successfully erased!');
    PutMainMenu(false);
    end;
  30:begin; {settings}
    PutMainMenu(false);
    WriteLnResponseString('<b><u>password:</u></b><br>');
    WriteLnResponseString('<form action="'+url+'" method="post" enctype="multipart/form-data">');
    WriteLnResponseString('<input type="hidden" name="pg" value="31">');
    WriteLnResponseString('<input type="hidden" name="id" value="'+id+'">');
    WriteLnResponseString('password: <input type="password" name="data1" size="40" value=""><br>');
    WriteLnResponseString('password: <input type="password" name="data2" size="40" value=""> (again, for verifycation!)<br>');
    WriteLnResponseString('<input type="submit" name="todo" value="change!">');
    WriteLnResponseString('<br></form><hr>');
    if (xtOpen(t,usrD.path+UserConfigureFile,true)<>0) then begin;
      c:='';
      d:='';
      end else begin;
      c:=xtReadLn(t,255);
      d:=xtReadLn(t,255);
      xtClose(t);
      end;
    WriteLnResponseString('<b><u>identity:</u></b><br>');
    WriteLnResponseString('<form action="'+url+'" method="post" enctype="multipart/form-data">');
    WriteLnResponseString('<input type="hidden" name="pg" value="32">');
    WriteLnResponseString('<input type="hidden" name="id" value="'+id+'">');
    WriteLnResponseString('screen name: <input type="text" name="data1" size="40" value="'+c+'"><br>');
    WriteLnResponseString('email address: <input type="text" name="data2" size="40" value="'+d+'"><br>');
    WriteLnResponseString('<input type="submit" name="todo" value="change!">');
    WriteLnResponseString('<br></form><hr>');
    WriteLnResponseString('<b><u>forwarding:</u></b><br>');
    WriteLnResponseString('<i>if a line contains just the @ sign, then messages will NOT stored in this mailbox.</i><br>');
    WriteLnResponseString('<form action="'+url+'" method="post" enctype="multipart/form-data">');
    WriteLnResponseString('<input type="hidden" name="pg" value="33">');
    WriteLnResponseString('<input type="hidden" name="id" value="'+id+'">');
    WriteResponseString('<textarea name="message" rows="16" cols="80">');
    if (xtOpen(t,usrD.path+ForwardingListFile,true)=0) then begin;
      while not xtEOF(t) do begin;
        a:=xtReadLn(t,255);
        if (a='') then continue;
        WriteOneString(a);
        WriteLnResponseString('');
        end;
      xtClose(t);
      end;
    WriteLnResponseString('</textarea><br>');
    WriteLnResponseString('<input type="submit" name="todo" value="enable!"></form>');
    WriteLnResponseString('<form action="'+url+'" method="post" enctype="multipart/form-data">');
    WriteLnResponseString('<input type="hidden" name="pg" value="34">');
    WriteLnResponseString('<input type="hidden" name="id" value="'+id+'">');
    WriteLnResponseString('<input type="submit" name="todo" value="disable!">');
    WriteLnResponseString('<br></form><hr>');
    WriteLnResponseString('<b><u>automatic reply:</u></b><br>');
    WriteLnResponseString('<i>the message should contain the complete header except to and date fields!</i><br>');
    WriteLnResponseString('<form action="'+url+'" method="post" enctype="multipart/form-data">');
    WriteLnResponseString('<input type="hidden" name="pg" value="35">');
    WriteLnResponseString('<input type="hidden" name="id" value="'+id+'">');
    WriteResponseString('<textarea name="message" rows="16" cols="80">');
    if (xtOpen(t,usrD.path+AutomaticReplyFile,true)=0) then begin;
      while not xtEOF(t) do begin;
        a:=xtReadLn(t,255);
        WriteOneString(a);
        WriteLnResponseString('');
        end;
      xtClose(t);
      end else begin;
      WriteLnResponseString('From: "'+c+'" <'+d+'>');
      WriteLnResponseString('Subject: automatic reply');
      WriteLnResponseString('');
      WriteLnResponseString('hello!');
      WriteLnResponseString('this automatic reply was generated because');
      WriteLnResponseString('i am away, but when i will be back, i will');
      WriteLnResponseString('reply to your message!');
      WriteLnResponseString('best regards: '+c);
      end;
    WriteLnResponseString('</textarea><br>');
    WriteLnResponseString('<input type="submit" name="todo" value="enable!"></form>');
    WriteLnResponseString('<form action="'+url+'" method="post" enctype="multipart/form-data">');
    WriteLnResponseString('<input type="hidden" name="pg" value="36">');
    WriteLnResponseString('<input type="hidden" name="id" value="'+id+'">');
    WriteLnResponseString('<input type="submit" name="todo" value="disable!">');
    WriteLnResponseString('<br></form>');
    PutMainMenu(false);
    end;
  31:begin; {change password}
    if (data1<>data2) then begin;
      WriteLnResponseString('passwords does not match!');
      PutMainMenu(false);
      goto vege;
      end;
    if (data1='') then begin;
      WriteLnResponseString('password must not be empty!');
      PutMainMenu(false);
      goto vege;
      end;
    a:=usrD.path+PasswordConfigFile;
    xCreate(a);
    if (xtOpen(t,a,false)<>0) then goto err;
    xtSetPos(t,0);
    xtWriteLn(t,data1);
    xtTruncate(t);
    xtClose(t);
    WriteLnResponseString('password changed!');
    PutMainMenu(false);
    end;
  32:begin; {change identity}
    a:=usrD.path+UserConfigureFile;
    xCreate(a);
    if (xtOpen(t,a,false)<>0) then goto err;
    xtSetPos(t,0);
    xtWriteLn(t,data1);
    xtWriteLn(t,data2);
    xtTruncate(t);
    xtClose(t);
    WriteLnResponseString('identity changed!');
    PutMainMenu(false);
    end;
  33:begin; {enable forwarding}
    if FindNextRequest(True) then goto err;
    while (1=1) do begin;
      if (CurrentReqTyp>1) then if (Kicsi(CurrentReqNam)='message') then break;
      if FindNextRequest(False) then goto err;
      end;
    a:=usrD.path+ForwardingListFile;
    xCreate(a);
    if (xtOpen(t,a,false)<>0) then goto err;
    xtSetPos(t,0);
    repeat
      i:=ReadRequestData(a[1],255);
      a[0]:=chr(i);
      xtWrite(t,a);
      until (a='');
    xtTruncate(t);
    xtClose(t);
    WriteLnResponseString('forwarding enabled!');
    PutMainMenu(false);
    end;
  34:begin; {disable forwarding}
    xErase(usrD.path+ForwardingListFile);
    WriteLnResponseString('forwarding disabled!');
    PutMainMenu(false);
    end;
  35:begin; {enable autoreply}
    if FindNextRequest(True) then goto err;
    while (1=1) do begin;
      if (CurrentReqTyp>1) then if (Kicsi(CurrentReqNam)='message') then break;
      if FindNextRequest(False) then goto err;
      end;
    a:=usrD.path+AutomaticReplyFile;
    xCreate(a);
    if (xtOpen(t,a,false)<>0) then goto err;
    xtSetPos(t,0);
    repeat
      i:=ReadRequestData(a[1],255);
      a[0]:=chr(i);
      xtWrite(t,a);
      until (a='');
    xtTruncate(t);
    xtClose(t);
    WriteLnResponseString('autoreply enabled!');
    PutMainMenu(false);
    end;
  36:begin; {disable autoreply}
    xErase(usrD.path+AutomaticReplyFile);
    WriteLnResponseString('autoreply disabled!');
    PutMainMenu(false);
    end;
  else goto err;
  end;
vege:
case usrD.frmt of
  1,2:begin; {html}
    WriteLnResponseString('</body></html>');
    end;
  3:begin; {wap}
    WriteLnResponseString('</p></card></wml>');
    end;
  else goto err;
  end;
CloseRequestFile;
END.