Procedure hciRefuseConn(var adr:oneAddressRecord;err:LongInt);
Var buf:array[1..512] of byte;
Begin;
{$ifdef debug2}writeLn('cmd: refuse connection');{$endif}
byteSwapHCIaddress(adr,buf);
buf[sizeof(adr)+1]:=err;
hciSendCommand(1,$a,sizeof(adr)+1,buf);
End;



Procedure hciAcceptConn(var adr:oneAddressRecord);
Var buf:array[1..512] of byte;
Begin;
{$ifdef debug2}writeLn('cmd: accept connection');{$endif}
byteSwapHCIaddress(adr,buf);
buf[sizeof(adr)+1]:=1;
hciSendCommand(1,$9,sizeof(adr)+1,buf);
End;



Procedure hciCloseConn(hnd,err:LongInt);
Var buf:array[1..512] of byte;
Begin;
{$ifdef debug2}writeLn('cmd: close connection');{$endif}
WriteWordLSB(buf[1],hnd);
buf[3]:=err;
hciSendCommand(1,$6,3,buf);
End;



Procedure hciStartConn(var adr:oneAddressRecord);
Var buf:array[1..512] of byte;
Begin;
{$ifdef debug2}writeLn('cmd: start connection');{$endif}
byteSwapHCIaddress(adr,buf);
writeWordLSB(buf[sizeof(adr)+1],$cc18);
buf[sizeof(adr)+3]:=1;
buf[sizeof(adr)+4]:=0;
writeWordLSB(buf[sizeof(adr)+5],0);
buf[sizeof(adr)+7]:=1;
hciSendCommand(1,$5,sizeof(adr)+7,buf);
End;



Procedure hciRejectKey(var adr:oneAddressRecord);
Var buf:array[1..512] of byte;
Begin;
{$ifdef debug2}writeLn('cmd: reject link key');{$endif}
byteSwapHCIaddress(adr,buf);
hciSendCommand(1,$c,sizeof(adr),buf);
End;



Procedure hciEnterPin(var adr:oneAddressRecord;pin:String);
Var buf:array[1..512] of byte;
Begin;
{$ifdef debug2}writeLn('cmd: accept link pin');{$endif}
byteSwapHCIaddress(adr,buf);
move(pin,buf[sizeof(adr)+1],sizeof(pin));
hciSendCommand(1,$d,sizeof(adr)+1+length(pin),buf);
End;



Procedure hciStartInquiry(tim,max:LongInt);
Var buf:array[1..512] of byte;
Begin;
{$ifdef debug2}writeLn('cmd: inquiry');{$endif}
buf[1]:=$33;
buf[2]:=$8b;
buf[3]:=$9e;
buf[4]:=tim;
buf[5]:=max;
hciSendCommand(1,$1,5,buf);
End;



Procedure hciNameRequest(var adr:oneAddressRecord);
Var buf:array[1..512] of byte;
Begin;
{$ifdef debug2}writeLn('cmd: remote name request');{$endif}
byteSwapHCIaddress(adr,buf);
buf[sizeof(adr)+1]:=1;
buf[sizeof(adr)+2]:=0;
buf[sizeof(adr)+3]:=0;
buf[sizeof(adr)+4]:=0;
hciSendCommand(1,$19,sizeof(adr)+4,buf);
End;








Procedure signCmdRej(var hnd:oneHCIconnectionRecord;id,rsn:LongInt;a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
{$ifdef debug4}writeLn('sigT: command reject');{$endif}
a:='aa'+a;
writeWordLSB(ab[1],rsn);
signSendCommand(hnd,1,id,a);
End;



Procedure signConnReq(var hnd:oneHCIconnectionRecord;id,psm,src:LongInt);
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
{$ifdef debug4}writeLn('sigT: connect request');{$endif}
ab0:=4;
writeWordLSB(ab[1],psm);
writeWordLSB(ab[3],src);
signSendCommand(hnd,2,id,a);
End;



Procedure signConnRep(var hnd:oneHCIconnectionRecord;id,dst,src,res,stt:LongInt);
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
{$ifdef debug4}writeLn('sigT: connect response');{$endif}
ab0:=8;
writeWordLSB(ab[1],dst);
writeWordLSB(ab[3],src);
writeWordLSB(ab[5],res);
writeWordLSB(ab[7],stt);
signSendCommand(hnd,3,id,a);
End;



Procedure signConfReq(var hnd:oneHCIconnectionRecord;id,dst:LongInt;a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
{$ifdef debug4}writeLn('sigT: config request');{$endif}
a:='aaaa'+a;
writeWordLSB(ab[1],dst);
writeWordLSB(ab[3],0);
signSendCommand(hnd,4,id,a);
End;



Procedure signConfRep(var hnd:oneHCIconnectionRecord;id,dst,res:LongInt;a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
{$ifdef debug4}writeLn('sigT: config response');{$endif}
a:='aaaaaa'+a;
writeWordLSB(ab[1],dst);
writeWordLSB(ab[3],0);
writeWordLSB(ab[5],res);
signSendCommand(hnd,5,id,a);
End;



Procedure signDiscReq(var hnd:oneHCIconnectionRecord;id,dst,src:LongInt);
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
{$ifdef debug4}writeLn('sigT: disconnect request');{$endif}
ab0:=4;
writeWordLSB(ab[1],dst);
writeWordLSB(ab[3],src);
signSendCommand(hnd,6,id,a);
End;



Procedure signDiscRep(var hnd:oneHCIconnectionRecord;id,dst,src:LongInt);
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
{$ifdef debug4}writeLn('sigT: disconnect response');{$endif}
ab0:=4;
writeWordLSB(ab[1],dst);
writeWordLSB(ab[3],src);
signSendCommand(hnd,7,id,a);
End;



Procedure signInfoReq(var hnd:oneHCIconnectionRecord;id,typ:LongInt);
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
{$ifdef debug4}writeLn('sigT: information request');{$endif}
ab0:=2;
writeWordLSB(ab[1],typ);
signSendCommand(hnd,10,id,a);
End;



Procedure signInfoRep(var hnd:oneHCIconnectionRecord;id,typ,res:LongInt);
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
{$ifdef debug4}writeLn('sigT: information response');{$endif}
ab0:=4;
writeWordLSB(ab[1],typ);
writeWordLSB(ab[3],res);
signSendCommand(hnd,11,id,a);
End;
