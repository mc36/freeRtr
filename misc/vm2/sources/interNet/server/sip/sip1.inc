Procedure appendOneStr(var buffer;var siz:LongInt;a:String);
Var
  buf:array[1..1] of byte absolute buffer;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
move(ab[1],buf[siz+1],ab0);
inc(siz,ab0);
End;

Procedure appendOneLine(var buffer;var siz:LongInt;a:String);
Begin;
appendOneStr(buffer,siz,a);
appendOneStr(buffer,siz,#13#10);
End;

Function generateMyAddr:String;
Var a:String;
Begin;
a:=ipAddr2string(localAddr);
if not isAddressIPv4mask(localAddr) then a:='['+a+']';
generateMyAddr:=a+':'+BStr(localPort);
End;

Function generateServerAddr:String;
Var a:String;
Begin;
a:=ipAddr2string(serverAddr);
if not isAddressIPv4mask(serverAddr) then a:='['+a+']';
generateServerAddr:=a;
End;

Function getPhoneNumber(a:String):String;
Var i:LongInt;
Begin;
getPhoneNumber:='unknown';
i:=pos('sip:',kicsi(a));
if (i<1) then exit;
a:=copy(a,i+4,666);
i:=pos(';',a);
if (i>0) then a:=copy(a,1,i-1);
i:=pos('>',a);
if (i>0) then a:=copy(a,1,i-1);
kicserel(' ','',a);
if (a='') then exit;
getPhoneNumber:=a;
End;

Function generateSDPdata(var con:OneConnectionRecord):String;
Var
  a,b:String;
  i:LongInt;
Begin;
generateSDPdata:='';
con.rtpT:=8;
if isAddressIPv4mask(con.rtpA) then b:='IP4' else b:='IP6';
b:='IN '+b+' '+ipAddr2string(con.rtpA);
b:='v=0'#13#10'o=user '+BStr(currentTime)+' 0 '+b+#13#10's=call'#13#10+
  +'c='+b+#13#10't=0 0'#13#10'm=audio '+BStr(con.rtpP)+' RTP/AVP 8'#13#10+
  +'a=rtpmap:8 PCMA/8000'#13#10;
generateSDPdata:=b;
End;


Procedure addUserAgent(var buffer;var siz:LongInt);
Begin;
appendOneLine(buffer,siz,'User-Agent: '+ProggyName+' for BugOS');
End;

Procedure addAllowed(var buffer;var siz:LongInt);
Begin;
appendOneLine(buffer,siz,'Allow: INVITE, ACK, CANCEL, BYE');
End;

Procedure addDefaultLines(var buffer;var siz:LongInt;var con:OneConnectionRecord);
Begin;
appendOneLine(buffer,siz,'Via: '+con.Svia);
appendOneLine(buffer,siz,'To: '+con.Strg);
appendOneLine(buffer,siz,'From: '+con.Ssrc);
appendOneLine(buffer,siz,'Call-ID: '+con.Scid);
appendOneLine(buffer,siz,'CSeq: '+con.Sseq);
End;

Procedure sendOneSIPmessage(cmd,cnt,dat:String;var con:OneConnectionRecord);
Var
  buf:array[1..1024] of byte;
  i,o:LongInt;
Begin;
if (cnt=#13) then cnt:='<sip:'+myNumber+'@'+generateMyAddr+'>';
o:=0;
appendOneLine(buf,o,cmd);
addDefaultLines(buf,o,con);
addUserAgent(buf,o);
addAllowed(buf,o);
if (cnt<>'') then appendOneLine(buf,o,'Contact: '+cnt);
appendOneLine(buf,o,'Date: '+getDateForHeader);
appendOneLine(buf,o,'Max-Forwards: 70');
if (dat<>'') then appendOneLine(buf,o,'Content-Type: application/sdp');
appendOneLine(buf,o,'Content-Length: '+BStr(length(dat)));
appendOneLine(buf,o,'');
appendOneStr(buf,o,dat);
UDPsendPacket(localPipe,con.addr,con.port,buf,o);
{$ifdef debug}
dumpOneMessage('sent to '+ipAddr2string(con.addr)+' '+BStr(con.port),buf,o);
{$endif}
End;


Procedure sendRegisterMessage;
Var
  con:OneConnectionRecord;
  i,o:LongInt;
  a:String;
Begin;
WriteLn('sending register message...');
lastRegged:=currentTime;
fillchar(con,sizeof(con),0);
a:='<sip:'+myNumber+'@'+generateServerAddr+'>';
con.Svia:='SIP/2.0/UDP '+generateMyAddr;
con.Ssrc:='"'+myNumber+'" '+a+';tag=reg'+BStr(currentTime);
con.Strg:=a;
con.Scid:='reg'+BStr(currentTime)+'@'+ipAddr2string(localAddr);
con.Sseq:=BStr(currentTime)+' REGISTER';
con.addr:=serverAddr;
con.port:=serverPort;
sendOneSIPmessage('REGISTER sip:'+generateServerAddr+' SIP/2.0',a,'',con);
End;
