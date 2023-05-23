Function FaxClass20waitMsg(msg:String):Byte; {0=msg, 1=error, 2=timeout}
Label f1;
Var
  a,b,c,d:String;
  i:LongInt;

Procedure decSess;
Begin;
case GetNextNum(b) of
  0:a:='98 lpi';
  1:a:='196 lpi';
  else a:='? lpi';
  end;
d:=a;
i:=GetNextNum(b);
if (i<=MaxFaxSpeeds) then a:=BStr(FaxSpeeds[i])+'/'+FaxProtos[i] else a:='? bps';
d:=d+', '+a;
case GetNextNum(b) of
  0:a:='1728 px';
  1:a:='2048 px';
  2:a:='2432 px';
  3:a:='1216 px';
  4:a:='864 px';
  else a:='? px';
  end;
d:=d+', '+a;
case GetNextNum(b) of
  0:a:='297 mm';
  1:a:='364 mm';
  2:a:='any mm';
  else a:='? mm';
  end;
d:=d+', '+a;
case GetNextNum(b) of
  0:a:='1d';
  1:a:='2d';
  2:a:='2d uncmp';
  3:a:='t6';
  else a:='? cmp';
  end;
d:=d+', '+a;
case GetNextNum(b) of
  0:a:='no bft';
  1:a:='bft';
  1:a:='dtm';
  1:a:='edi';
  else a:='? bft';
  end;
d:=d+', '+a;
case GetNextNum(b) of
  0:a:='no ecm';
  1:a:='64 ecm';
  2:a:='256 ecm';
  else a:='? ecm';
  end;
d:=d+', '+a;
d:=d+', '+BStr(GetNextNum(b)*5)+' ms/ln';
End;

Begin;
FaxClass20waitMsg:=2;
pageMessage:=-1;
hangupCode:=-1;
msg:=kicsi(msg);
f1:
if ModemReadCommand(b,60) then exit;
if (ModemTestResponse(b) in [1,3,4,5]) then begin; FaxClass20waitMsg:=1;exit; end;
i:=pos(':',b);
if (i<1) then i:=600;
c:=kicsi(xLevesz(copy(b,1,i-1)));
b:=xLevesz(copy(b,i+1,255));
if (c='+fci') then write2logP('Called Station ID: '+b);
if (c='+fcs') then begin;
  decSess;
  write2logP('Current Session: '+d);
  end;
if (c='+fet') then begin;
  pageMessage:=BVal(b);
  case pageMessage of
    0,8:a:='multipage';
    1,9:a:='Another page next, same document';
    2,10:a:='Another page next';
    3,11:a:='no more pages or documents';
    4,12:a:='Another page, procedure interrupt';
    5,13:a:='Another doc, procedure interrupt';
    6,14:a:='All done, procedure interrupt';
    7,15:a:='Continue to correct';
    else a:='?';
    end;
  write2logP('Post Page Response: '+a);
  end;
if (c='+fhr') then write2logP('hdlc rx: '+b);
if (c='+fhs') then begin;
  hangupCode:=BVal(b);
  write2logP('Call Termination: '+b);
  end;
if (c='+fht') then write2logP('hdlc tx: '+b);
if (c='+fis') then begin;
  decSess;
  write2logP('Requested Session: '+d);
  end;
if (c='+fnc') then write2logP('nsc frame: '+b);
if (c='+fnf') then write2logP('nsf frame: '+b);
if (c='+fns') then write2logP('nss frame: '+b);
if (c='+fpi') then write2logP('Poller Station ID: '+b);
if (c='+fpo') then write2logP('answering may be polled');
if (c='+fps') then begin;
  case GetNextNum(b) of
    0:a:='Partial page errors';
    1:a:='Page Good';
    2:a:='Page bad, retrain requested';
    3:a:='Page good, retrain requested';
    4:a:='Page bad, interrupt requested';
    5:a:='Page good, interrupt requested';
    else a:='?';
    end;
  a:=a+'; '+BStr(GetNextNum(b))+' lines';
  a:=a+' '+BStr(GetNextNum(b))+' bad';
  a:=a+' '+BStr(GetNextNum(b))+' contbad';
  write2logP('Page Transfer Status: '+a);
  end;
if (c='+ftc') then begin;
  decSess;
  write2logP('DTC Session: '+d);
  end;
if (c='+fti') then write2logP('Transmit Station ID: '+b);
if (c='+fdm') then write2logP('transitioning to data mode');
if (c='+fvo') then write2logP('transitioning to voice mode');
if (c<>msg) then goto f1;
FaxClass20waitMsg:=0;
End;

Function FaxClass20init:Boolean;
Var a:String;
Begin;
FaxClass20init:=True;
if ModemDoCmd('AT+FCLASS=2.0;+FNR=1,1,1,1',a) then exit;
if (ModemTestResponse(a)<>0) then exit;
if ModemDoCmd('AT+FLI="'+LocalIdentifier+'"',a) then exit;
if ModemDoCmd('AT+FCC=0,'+BStr(FaxSpeedLimit)+',0,2,0,0,0,0',a) then exit;
FaxClass20init:=False;
End;

Function FaxClass20recv:Boolean;
Label f1,f2;
Var i:LongInt;
Begin;
FaxClass20recv:=True;
write2logS('connecting...');
ModemSendCommand('ATA');
if (FaxClass20waitMsg('+fco')<>0) then exit;
f1:
write2logP('waiting for page...');
if (FaxClass20waitMsg('ok')<>0) then exit;
if (pageMessage=2) then goto f2;
write2logP('starting page...');
ModemSendCommand('AT+FDR');
if (FaxClass20waitMsg('connect')<>0) then exit;
i:=ReceiveUpPage(false,false);
write2logS('page #'+BStr(filPag)+': '+BStr(i)+' bytes received...');
goto f1;
f2:
write2logS('finishing session...');
ModemSendCommand('AT+FDR');
if (FaxClass20waitMsg('ok')<>0) then exit;
FaxClass20recv:=(hangupCode<>0);
End;

Function FaxClass20send:Boolean;
Label f1,f2;
Var
  a:String;
  i:LongInt;
Begin;
FaxClass20send:=True;
write2logS('connecting...');
ModemSendCommand('ATD'+phoneNumber);
if (FaxClass20waitMsg('+fco')<>0) then exit;
f1:
write2logP('starting page...');
if (FaxClass20waitMsg('ok')<>0) then exit;
ModemSendCommand('AT+FDT');
if (FaxClass20waitMsg('connect')<>0) then exit;
i:=SendOutPage(false,false);
write2logS('page #'+BStr(filPag)+': '+BStr(i)+' bytes sent...');
if not isAnotherNextPage then goto f2;
TransmitOneChar(controlCharacterDLE);
TransmitOneChar($2c);
goto f1;
f2:
write2logS('finishing session...');
TransmitOneChar(controlCharacterDLE);
TransmitOneChar($2e);
if (FaxClass20waitMsg('ok')<>0) then exit;
FaxClass20send:=(hangupCode<>0);
End;
