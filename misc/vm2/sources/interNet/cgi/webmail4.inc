Procedure PutMainMenu(b:boolean);

procedure PutLink(pg:byte;target,txt:string);
begin;
if b then target:='' else begin;
  target:=' target="'+target+'"';
  WriteOneChar(#10);
  end;
WriteResponseString(' <a href="'+url+'?id='+id+'&amp;pg='+BStr(pg)+'"'+target+'>'+txt+'</a>');
end;

Begin;
if (not b) and (usrD.frmt=1) then Exit;
b:=(usrD.frmt<>1);
if b then WriteOneSeparator;
WriteResponseString('<b><i>menu:</i></b>');
PutLink(12,'lista','read');
PutLink(21,'level','write');
if (usrD.frmt<>3) then PutLink(30,'level','settings');
PutLink(15,'_parent','quit');
if b then WriteOneSeparator;
End;







Procedure generateMessageList(path:String);
Label f1,f2,f3,f4;
Var
  f:xFile;
  t:xtText;
  sr:xDirEntryRec;
  a,b,c,d:String;
Begin;
case usrD.frmt of
  1,2:begin; {html}
    WriteLnResponseString('<table border="1" width="100%"><tr>');
    WriteLnResponseString('  <td width="50%"><b><i>source</i></b></td>');
    WriteLnResponseString('  <td width="50%"><b><i>subject</i></b></td>');
    WriteLnResponseString('  </tr>');
    end;
  3:begin; {wap}
    WriteLnResponseString('<b>source subject</b>');
    WriteOneChar(#10);
    end;
  end;
if (xDirOpen(f,path)<>0) then goto f2;
f1:
if (xDirRead(f,sr)<>0) then goto f2;
if (sr.name='') then goto f2;
if (kicsi(xFileName(sr.name,3))<>MessageExtension) then goto f1;
if (xtOpen(t,path+sr.name,true)<>0) then goto f1;
decodeOneHeader(t,a,b,c,d);
xtClose(t);
c:='<a href="'+url+'?id='+id+'&amp;pg=20&amp;data1='+xFileName(sr.name,2)+'"';
if (usrD.frmt=1) then c:=c+' target="level"';
c:=c+'>';
case usrD.frmt of
  1,2:begin; {html}
    WriteResponseString('<tr><td nowrap>'+c);
    WriteOneString(a);
    WriteResponseString(' </a></td><td nowrap>'+c);
    WriteOneString(d);
    WriteLnResponseString(' </a></td></tr>');
    end;
  3:begin; {wap}
    WriteResponseString(c);
    WriteOneString(a);WriteResponseString(' </a>'+c);
    WriteOneString(d);WriteResponseString(' </a>');
    WriteOneChar(#10);
    end;
  end;
goto f1;
f2:
xDirClose(f);
case usrD.frmt of
  1,2:begin; {html}
    WriteLnResponseString('</table>');
    end;
  3:begin; {wap}
    end;
  end;
End;







Procedure PutMessageWriting1(a,b:String);
Begin;
case usrD.frmt of
  1,2:begin; {html}
    WriteLnResponseString('<form action="'+url+'" method="post" enctype="multipart/form-data">');
    WriteLnResponseString('<input type="hidden" name="id" value="'+id+'">');
    WriteLnResponseString('<input type="hidden" name="pg" value="23">');
    WriteResponseString('to: <input type="text" name="data1" size="50" value="');
    WriteOneString(a);
    WriteLnResponseString('">');WriteOneChar(#10);
    WriteResponseString('subject: <input type="text" name="data2" size="50" value="');
    WriteOneString(b);
    WriteLnResponseString('">');WriteOneChar(#10);
    WriteResponseString('<textarea name="message" rows="16" cols="80">');
    end;
  3:begin; {wap}
    WriteResponseString('to: <input type="text" format="*m" name="data1" value="');
    WriteOneString(a);
    WriteLnResponseString('"/>');WriteOneChar(#10);
    WriteResponseString('subj: <input type="text" format="*m" name="data2" value="');
    WriteOneString(b);
    WriteLnResponseString('"/>');WriteOneChar(#10);
    WriteLnResponseString('text: <input type="text" format="*m" name="message"/>');WriteOneChar(#10);
    WriteLnResponseString('<anchor>send!<go href="'+url+'">');
    WriteLnResponseString('<postfield name="id" value='''+id+'''/>');
    WriteLnResponseString('<postfield name="pg" value=''23''/>');
    WriteLnResponseString('<postfield name="data1" value=''$(data1)''/>');
    WriteLnResponseString('<postfield name="data2" value=''$(data2)''/>');
    WriteLnResponseString('<postfield name="message" value=''$(message)''/>');
    WriteLnResponseString('</go></anchor>');
    WriteLnResponseString('<anchor>cancel<refresh>');
    WriteResponseString('<setvar name="data1" value="');
    WriteOneString(a);
    WriteLnResponseString('"/>');
    WriteResponseString('<setvar name="data2" value="');
    WriteOneString(b);
    WriteLnResponseString('"/>');
    WriteLnResponseString('<setvar name="message" value=""/>');
    WriteLnResponseString('</refresh></anchor>');
    end;
  end;
End;

Procedure PutMessageWriting2;
Begin;
case usrD.frmt of
  1,2:begin; {html}
    WriteLnResponseString('</textarea>');WriteOneChar(#10);
    WriteLnResponseString('<input type="submit" name="todo" value="send!">');
    WriteLnResponseString('<input type="reset" name="todo" value="cancel">');
    WriteLnResponseString('</form>');
    end;
  3:begin; {wap}
    end;
  end;
End;






Function SendCurrentMessage(srv,src,trg,sbj:String):Boolean;
var pipe:longint;

procedure PutOutString(a:string);
label f1;
Var i,o:LongInt;
begin;
f1:
if (pipeLineSend(pipe,a[1],length(a))=0) then exit;
relequish;
pipeLineStats(pipe,o,i,i);
if (o<>0) then goto f1;
exit;
end;

Function GetCodeFromLine(s:String):LongInt;
Begin;
s:=copy(s,1,pos(' ',s)-1);
GetCodeFromLine:=BVal(s);
End;

Procedure SendOneCommand(a:String);
Begin;
PutOutString(a);
PutOutString(#13#10);
WriteLn('--> '+a);
End;

Function WaitForStatusLine:String;
Label f1;
Var
  a:String;
  c:Char;
  b:byte absolute c;
  i,o:LongInt;
Begin;
WaitForStatusLine:='';
a:='';
f1:
i:=sizeof(c);
if (pipeLineRecv(pipe,c,i)<>0) then i:=0;
if (i<1) then begin;
   relequish;
  pipeLineStats(pipe,o,i,i);
  if (o<>0) then goto f1;
  exit;
  end;
if (b=13) then goto f1;
if (b<>10) then begin; a:=a+c;goto f1; end;
WriteLn('<-- '+a);
if (copy(a,4,1)='-') then begin; a:='';goto f1; end;
WaitForStatusLine:=a;
End;

Label f1,f2,vege;
Var
  a,b:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p:LongInt;

Procedure fillUpBuffer;
Var i,o,p:LongInt;
Begin;
if (ab0>250) then exit;
i:=ReadRequestData(ab[ab0+1],252-ab0);
if (i<1) then exit;
o:=ab0;
for p:=ab0+1 to ab0+i do begin;
  i:=ab[p];
  if (i=0) then continue;
  if (i=13) then continue;
  inc(o);
  ab[o]:=i;
  end;
ab0:=o;
End;

Begin;
SendCurrentMessage:=True;
if TCPfindProcess then exit;
if DNSstartResolver then exit;

WriteLn('resolving '+srv+'...');
DNSresolvePut(1,srv);
while (1=1) do begin;
  i:=DNSresolveGet(a,b);
  if (i=0) then begin; relequish;continue; end;
  if (i and $80<>0) then exit;
  break;
  end;

WriteLn('connecting to '+ipAddr2string(b)+'...');
TCPbeginConnect(pipe,65536,b,25);
while TCPlookConnected(pipe,a,i,o) do begin;
  relequish;
  if (pipe<>0) then continue;
  exit;
  end;
WriteLn('local side is '+ipAddr2string(a)+' '+BStr(i)+'...');

WaitForStatusLine;
SendOneCommand('EHLO LocalHost');
WaitForStatusLine;
NormalizeOneAddress(src,a,b);
SendOneCommand('MAIL FROM:<'+b+'>');
if (GetCodeFromLine(WaitForStatusLine)<>250) then Goto Vege;
NormalizeOneAddress(trg,a,b);
SendOneCommand('RCPT TO:<'+b+'>');
if (GetCodeFromLine(WaitForStatusLine)<>250) then Goto Vege;

SendOneCommand('DATA');
if (GetCodeFromLine(WaitForStatusLine)<>354) then Goto Vege;
WriteLn('sending body...');
PutOutString('From: ');PutOutString(src);PutOutString(#13#10);
PutOutString('To: ');PutOutString(trg);PutOutString(#13#10);
PutOutString('Subject: ');PutOutString(sbj);PutOutString(#13#10);
PutOutString('Date: '+GetDateForHeader+#13#10);
PutOutString('X-Originating-IP: '+ipAddr2string(client[1])+#13#10);
PutOutString('X-Mailer: '+ProggyName+' '+ProggyVer+#13#10);
PutOutString('MIME-Version: 1.0'#13#10);
PutOutString('Content-Type: text/plain; charset=us-ascii'#13#10);
PutOutString('Content-Transfer-Encoding: 8bit'#13#10);
PutOutString(#13#10);
ab0:=0;
f1:
fillUpBuffer;
if (ab0=0) then goto f2;
p:=0;
for i:=1 to ab0 do if (ab[i]=10) then begin; p:=i;break; end;
if (p<1) then begin;
  PutOutString(a);
  ab0:=0;
  goto f1;
  end;
PutOutString(copy(a,1,p-1)+#13#10);
a:=copy(a,p+1,255);
fillUpBuffer;
if (ab0>0) and (ab[1]=$2e) then PutOutString('.');
goto f1;
f2:
PutOutString(#13#10'.'#13#10);
if (GetCodeFromLine(WaitForStatusLine)<>250) then Goto Vege;
SendOneCommand('QUIT');
if (GetCodeFromLine(WaitForStatusLine)<>221) then Goto Vege;
SendCurrentMessage:=False;
WriteLn('sending successful!');
vege:
pipelineClose(pipe);
End;
