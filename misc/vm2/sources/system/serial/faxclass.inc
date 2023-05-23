Var
  filHdr:xFile;
  filPos:LongInt;
  filSiz:LongInt;
  filPag:LongInt;
Const
  MaxFaxSpeeds=5;
  FaxSpeeds:Array[0..MaxFaxSpeeds] of LongInt=(2400,4800,7200,9600,12000,14400);
  FaxProtos:Array[0..MaxFaxSpeeds] of String[7]=('v27ter','v27ter','v29','v29','v17','v17');
Var
  LocalIdentifier:String;
  FaxSpeedLimit:Byte;
  bitCountTable:Array[0..255] of Byte;
  bitReverseTable:Array[0..255] of Byte;
  pageMessage:LongInt;
  hangupCode:LongInt;
  phoneNumber:String;

Function xLevesz(s:String):String;
Begin;
kicserel(#0,' ',s);
kicserel(#9,' ',s);
kicserel(#255,' ',s);
s:=' '+s+' ';
kicserel('  ',' ',s);
xLevesz:=copy(s,2,length(s)-2);
End;

Function GetNextNum(Var s:String):LongInt;
Var
  i:LongInt;
Begin;
i:=Pos(',',s);
if (i<1) then i:=255;
GetNextNum:=BVal(Copy(s,1,i-1));
s:=Copy(s,i+1,255);
End;

Function ReverseBits(c:byte):byte;
Begin;
ReverseBits:=          ((c        ) shr 7) or ((c and $40) shr 5) or
((c and $20) shr 3) or ((c and $10) shr 1) or ((c and $08) shl 1) or
((c and $04) shl 3) or ((c and $02) shl 5) or ((c        ) shl 7);
End;

Function CountBitsInByte(c:byte):Byte;
Begin;
CountBitsInByte:= ((c shr 7)      )+((c shr 6) and 1)+
((c shr 5) and 1)+((c shr 4) and 1)+((c shr 3) and 1)+
((c shr 2) and 1)+((c shr 1) and 1)+((c      ) and 1);
End;



Function isAnotherNextPage:Boolean;
Begin;
isAnotherNextPage:=(filPos+4<filSiz);
End;

Function ReceiveOneChar:Byte;
Label f1;
Var
  b:Byte;
  i,o:LongInt;
Begin;
f1:
i:=sizeof(b);
pipeLineRecv(serialData,b,i);
if (i>0) then begin;
  ReceiveOneChar:=b;
  exit;
  end;
pipeLineStats(serialData,o,i,i);
if (o=0) then immErr('serial driver closed connection!');
relequish;
goto f1;
End;

Function TestReceivedOne:Boolean;
Var i,o,p:LongInt;
Begin;
pipeLineStats(serialData,o,i,p);
if (o=0) then immErr('serial driver closed connection!');
TestReceivedOne:=(i>0);
End;

Procedure TransmitOneChar(b:Byte);
Begin;
pipeLineSend(serialData,b,sizeof(b));
End;

Procedure HandleXonXoff(already:Boolean);
Label f1,f2;
Var
  b:Byte;
  i:LongInt;
Begin;
Write(' flush');
repeat
  serialBuff[1]:=8;
  serialCmd(1);
  until (serialBuff[2]=0);
write(#8#8#8#8#8'     '#8#8#8#8#8#8);
if already then goto f2;
f1:
if not TestReceivedOne then exit;
b:=ReceiveOneChar;
if (b<>controlCharacterXOFFr) then goto f1;
f2:
Write(' xoff');
b:=controlCharacterXOFFr;
timer2start;
i:=CurrentTime;
while (b<>controlCharacterXONr) do begin;
  relequish;
  timer2start;
  if (GetTimePast(i)>60) then b:=controlCharacterXONr;
  if TestReceivedOne then b:=ReceiveOneChar;
  end;
write(#8#8#8#8'    '#8#8#8#8#8);
goto f1;
End;



Function ReceiveUpPage(revBits,sendXon:Boolean):LongInt;
Var
  buf:array[1..1024] of byte;
  siz:LongInt;
  b:Byte;

procedure flush;
begin;
if (siz<1) then exit;
xBlockWrite(filHdr,buf,siz);
inc(filSiz,siz);
Write(#13+BStr(filSiz));
siz:=0;
end;

procedure putChar(b:Byte);
begin;
if revBits then b:=bitReverseTable[b];
inc(siz);
buf[siz]:=b;
if (siz<sizeof(buf)) then exit;
flush;
end;

Label f1,f2;
Begin;
inc(filPag);
if sendXon then TransmitOneChar(controlCharacterXONs);
filPos:=xFilePos(filHdr);
filSiz:=0;
siz:=0;
xBlockWrite(filHdr,siz,sizeof(siz));
f1:
b:=ReceiveOneChar;
if (b<>controlCharacterDLE) then begin;
  putChar(b);
  goto f1;
  end;
b:=ReceiveOneChar;
if (b=controlCharacterETX) then goto f2;
if (b=controlCharacterCAN) then goto f2;
if (b=controlCharacterDLE) then begin;
  putChar(controlCharacterDLE);
  goto f1;
  end;
if (b=controlCharacterSUB) then begin;
  putChar(controlCharacterDLE);
  putChar(controlCharacterDLE);
  goto f1;
  end;
Write('   bad escaped: '+BStr(b)+#13);
goto f1;
f2:
flush;
filSiz:=xFileSize(filHdr);
siz:=filSiz-filPos-sizeof(siz);
xSeek(filHdr,filPos);
xBlockWrite(filHdr,siz,sizeof(siz));
xSeek(filHdr,filSiz);
filPos:=filSiz;
ClrEol;
ReceiveUpPage:=siz;
End;






Function SendOutPage(revBits,waitXon:Boolean):LongInt;
Label f1;
Var
  buf:array[1..64] of byte;
  siz:LongInt;
  fb:array[1..1024] of byte;
  i,o,p,s:LongInt;
  b:Byte;

procedure flush;
var i,o:LongInt;
begin;
HandleXonXoff(false);
if (siz<1) then exit;
HandleXonXoff(false);
while (pipeLineSend(serialData,buf,siz)<>0) do begin;
  pipeLineStats(serialData,o,i,i);
  if (o=0) then immErr('serial driver closed connection!');
  relequish;
  end;
siz:=0;
end;

procedure putChar(b:byte);
begin;
inc(siz);
buf[siz]:=b;
if (siz<sizeof(buf)) then exit;
flush;
end;

Begin;
inc(filPag);
xBlockRead(filHdr,s,sizeof(s));
i:=filSiz-filPos-sizeof(s);
if (s>i) then s:=i;
if (s<0) then s:=0;
if waitXon then HandleXonXoff(true);
p:=0;
siz:=0;
f1:
Write(#13+BStr(p));
o:=s-p;
if (o>sizeof(fb)) then o:=sizeof(fb);
if (o<1) then begin;
  flush;
  HandleXonXoff(false);
  putChar(controlCharacterDLE);
  putChar(controlCharacterETX);
  flush;
  filPos:=xFilePos(filHdr);
  ClrEol;
  SendOutPage:=s;
  Exit;
  end;
inc(p,o);
xBlockRead(filHdr,fb,o);
HandleXonXoff(false);
for i:=1 to o do begin;
  b:=fb[i];
  if revBits then b:=bitReverseTable[b];
  if (b=controlCharacterDLE) then begin;
    putChar(controlCharacterDLE);
    putChar(controlCharacterDLE);
    continue;
    end;
  putChar(b);
  end;
goto f1;
End;
