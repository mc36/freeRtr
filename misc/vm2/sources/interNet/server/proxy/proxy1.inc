Var
  processTCP:LongInt;
  processTLS:LongInt;
  processSSH:LongInt;


Procedure sendOneString(pipe:LongInt;a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
pipeLineSend(pipe,ab[1],ab0);
End;


Procedure sendBasicHeader(pipe:LongInt;tp:String);
Begin;
if (tp='') then tp:='text/html';
sendOneString(pipe,'Server: '+proggyName+' for BugOS'#13#10);
sendOneString(pipe,'Content-Type: '+tp+#13#10);
sendOneString(pipe,'Connection: Close'#13#10);
End;

Procedure sendMessage(var con:OneConnectionRecord;hdr,msg:String);
procedure snd(a:string);begin; sendOneString(con.pipe1,a); end;
Begin;
snd('HTTP/1.1 '+hdr+#13#10);
if (msg='') then begin;
  snd(#13#10);
  exit;
  end;
sendBasicHeader(con.pipe1,'');
snd(#13#10);
snd('<html><head><title>error</title></head><body>'#13#10);
snd(msg);
snd(#13#10'</body></html>'#13#10);
End;


Function getOneLine(pipe:LongInt;var line:String):LongInt; {0-nothing, 1-error, 2-ok}
Const max=200;
Var
  i,o:LongInt;
  b:Byte;
  ab:array[0..1] of byte absolute line;
  ab0:byte absolute line;
Begin;
getOneLine:=0;
if (ab0>=2) then if (ab[ab0-1]=13) and (ab[ab0]=10) then begin; dec(ab0,2);getOneLine:=2;exit; end;
i:=1;
pipeLineRecv(pipe,b,i);
if (i<1) then begin;
  pipeLineStats(pipe,o,i,i);
  if (o=0) then getOneLine:=1;
  exit;
  end;
inc(ab0);
ab[ab0]:=b;
if (ab0<max+30) then exit;
line:=copy(line,1,max)+copy(line,max+10,666);
End;

Function getFtpLine(pipe:LongInt;var line:String):LongInt; {0-nothing, 1-error, 2-ok}
Var i:LongInt;
Begin;
i:=getOneLine(pipe,line);
getFtpLine:=i;
if (i<>2) then exit;
if (copy(line,4,1)=' ') then exit;
line:='';
getFtpLine:=0;
End;



Procedure updateResolvers(res:LongInt;nam:String;var val);
Var
  con:OneConnectionRecord;
  i,o,p:LongInt;
Begin;
if (res and $7f<>1) then exit;
for p:=1 to ConnectionNum do begin;
  if (ConnectionDat^[p].stat<>3) then continue;
  if (ConnectionDat^[p].domain<>nam) then continue;
  con:=ConnectionDat^[p];
  if (res<$80) then begin;
    move(val,con.tadr,sizeof(OneTCPaddressRecord));
    con.stat:=4;
    end else begin;
    sendMessage(con,'404 not found','requested domain name ('+con.domain+') not found!');
    con.stat:=0;
    end;
  ConnectionDat^[p]:=con;
  end;
End;



Function decodeBase64(s:String):String;
Label f1,f2;
Const table='ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
Var
  a,b:String;
  i:LongInt;
  BufDat:LongInt;
  BufLen:LongInt;

Procedure AddBits(d,s:LongInt);
Begin;
inc(BufLen,s);
BufDat:=BufDat or (d shl (16-BufLen));
while (BufLen>=8) do begin;
  b:=b+Chr(BufDat shr 8);
  BufDat:=(BufDat and $ff) shl 8;
  dec(BufLen,8);
  end;
End;

Begin;
b:='';
BufLen:=0;
BufDat:=0;
f1:
a:=Copy(s,1,1);
delete(s,1,1);
if (a='') then goto f2;
i:=Pos(a,table);
if (i<1) then goto f1;
AddBits(i-1,6);
goto f1;
f2:
decodeBase64:=b;
End;


Function getParentDir(a:String):String;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
Begin;
o:=0;
for i:=1 to ab0-1 do if (ab[i]=$2f) then o:=i;
getParentDir:=copy(a,1,o);
End;
