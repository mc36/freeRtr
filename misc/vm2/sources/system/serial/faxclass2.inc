Function FaxClass2waitMsg(msg:String):Byte; {0=msg, 1=error, 2=timeout}
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
FaxClass2waitMsg:=2;
pageMessage:=-1;
hangupCode:=-1;
msg:=kicsi(msg);
f1:
if ModemReadCommand(b,60) then exit;
if (ModemTestResponse(b) in [1,3,4,5]) then begin; FaxClass2waitMsg:=1;exit; end;
i:=pos(':',b);
if (i<1) then i:=600;
c:=kicsi(xLevesz(copy(b,1,i-1)));
b:=xLevesz(copy(b,i+1,255));
if (c='+fdcs') then begin;
  decSess;
  write2logP('Current Session: '+d);
  end;
if (c='+fdis') then begin;
  decSess;
  write2logP('Requested Session: '+d);
  end;
if (c='+fcfr') then write2logP('Confirmation to Receive');
if (c='+ftsi') then write2logP('Transmit Station ID: '+b);
if (c='+fcsi') then write2logP('Called Station ID: '+b);
if (c='+fpts') then begin;
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
if (c='+fhng') then begin;
  hangupCode:=BVal(b);
  case hangupCode of
    0:a:='Normal and proper end of connection';
    1:a:='Ring Detect without successful handshake';
    2:a:='Call aborted, from +FK or AN';
    3:a:='No Loop Current';
    10:a:='Unspecified Phase A error';
    11:a:='No Answer (T.30 T1 timeout)';
    20:a:='Unspecified Transmit Phase B error';
    21:a:='Remote cannot receive or send';
    22:a:='COMREC error in transmit Phase B';
    23:a:='COMREC invalid command received';
    24:a:='RSPEC error';
    25:a:='DCS sent three times without response';
    26:a:='DIS/DTC received 3 times; DCS not recognized';
    27:a:='Failure to train at 2400 bps or +FMINSP value';
    28:a:='RSPREC invalid response received';
    40:a:='Unspecified Transmit Phase C error';
    43:a:='DTE to DCE data underflow';
    50:a:='Unspecified Transmit Phase D error';
    51:a:='RSPREC error';
    52:a:='No response to MPS repeated 3 times';
    53:a:='Invalid response to MPS';
    54:a:='No response to EOP repeated 3 times';
    55:a:='Invalid response to EOM';
    56:a:='No response to EOM repeated 3 times';
    57:a:='Invalid response to EOM';
    58:a:='Unable to continue after PIN or PIP';
    70:a:='Unspecified Receive Phase B error';
    71:a:='RSPREC error';
    72:a:='COMREC error';
    73:a:='T.30 T2 timeout, expected page not received';
    74:a:='T.30 T1 timeout after EOM received';
    90:a:='Unspecified Receive Phase C error';
    91:a:='Missing EOL after 5 seconds';
    92:a:='Unused code';
    93:a:='DCE to DTE buffer overflow';
    94:a:='Bad CRC or frame (ECM or BFT modes)';
    100:a:='Unspecified Receive Phase D errors';
    101:a:='RSPREC invalid response received';
    102:a:='COMREC invalid response received';
    103:a:='Unable to continue after PIN or PIP';
    else a:='?';
    end;
  write2logP('Call Termination: '+a);
  end;
if (c<>msg) then goto f1;
FaxClass2waitMsg:=0;
End;

Function FaxClass2init:Boolean;
Var a:String;
Begin;
FaxClass2init:=True;
if ModemDoCmd('AT+FCLASS=2',a) then exit;
if (ModemTestResponse(a)<>0) then exit;
if ModemDoCmd('AT+FLID="'+LocalIdentifier+'"',a) then exit;
if ModemDoCmd('AT+FDCC=0,'+BStr(FaxSpeedLimit)+',0,2,0,0,0,0',a) then exit;
FaxClass2init:=False;
End;

Function FaxClass2recv:Boolean;
Label f1,f2;
Var i:LongInt;
Begin;
FaxClass2recv:=True;
write2logS('connecting...');
ModemSendCommand('ATA');
if (FaxClass2waitMsg('+fcon')<>0) then exit;
f1:
write2logP('waiting for page...');
if (FaxClass2waitMsg('ok')<>0) then exit;
if (pageMessage=2) then goto f2;
write2logP('starting page...');
ModemSendCommand('AT+FDR');
if (FaxClass2waitMsg('connect')<>0) then exit;
i:=ReceiveUpPage(true,true);
write2logS('page #'+BStr(filPag)+': '+BStr(i)+' bytes received...');
goto f1;
f2:
write2logS('finishing session...');
ModemSendCommand('AT+FDR');
if (FaxClass2waitMsg('ok')<>0) then exit;
FaxClass2recv:=(hangupCode<>0);
End;

Function FaxClass2send:Boolean;
Label f1,f2;
Var
  a:String;
  i:LongInt;
Begin;
FaxClass2send:=True;
write2logS('connecting...');
ModemSendCommand('ATD'+phoneNumber);
if (FaxClass2waitMsg('+fcon')<>0) then exit;
f1:
write2logP('starting page...');
if (FaxClass2waitMsg('ok')<>0) then exit;
ModemSendCommand('AT+FDT');
if (FaxClass2waitMsg('connect')<>0) then exit;
i:=SendOutPage(false,true);
if (FaxClass2waitMsg('ok')<>0) then exit;
write2logS('page #'+BStr(filPag)+': '+BStr(i)+' bytes sent...');
if not isAnotherNextPage then goto f2;
ModemSendCommand('AT+FET=0');
goto f1;
f2:
write2logS('finishing session...');
ModemSendCommand('AT+FET=2');
if (FaxClass2waitMsg('ok')<>0) then exit;
FaxClass2send:=(hangupCode<>0);
End;
