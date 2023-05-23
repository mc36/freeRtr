{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc hex.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc memory.inc}

Const
  maxVpiNum=8;
  maxBufSize=4*1024;
  oneCellPayLoad=48;
  oneCellHeader=5;
  oneCellSize=oneCellPayLoad+oneCellHeader;
Var
  devPipe:LongInt;
  myName:String;
  atmConnN:LongInt;
  atmConnI:array[1..maxVpiNum] of LongInt;
  atmConnP:array[1..maxVpiNum] of LongInt;
  atmConnS:array[1..maxVpiNum] of LongInt;
  atmConnB:array[1..maxVpiNum] of array[1..maxBufSize] of Byte;

Procedure ImmErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Procedure dumpPacket(var buffer;siz:LongInt;act:String);
Var
  buf:array[1..1] of byte absolute buffer;
  i:LongInt;
Begin;
Write(act+' '+BStr(siz)+':');
for i:=1 to siz do write(' '+byte2hextype(buf[i]));
WriteLn('');
End;

Procedure openDevice(a:String);
Var
  buf:array[1..1024] of byte;
  i,o,p:LongInt;
Begin;
WriteLn('process: '+a);
o:=BugOS_findProcNam(a);
if (o=0) then immErr('process not found!');
WriteLn('process#: '+BStr(o));
i:=pipeLineCreate(devPipe,o,65536,true);
if (i<>0) then immErr('unabled to create pipeline!');
WriteLn('pipeline#: '+BStr(devPipe));
for i:=1 to 16 do relequish;
i:=sizeof(buf);
if (pipeLineRecv(devPipe,buf,i)<>0) then i:=0;
if (i<1) then immErr('initial packet not received!');
move(buf[1],p,sizeof(p));
WriteLn('address size: '+BStr(p));
move(buf[5],i,sizeof(i));
WriteLn('packet size: '+BStr(i));
o:=17;
Write('station address: ');
for i:=1 to p do begin;
  write(byte2hextype(buf[o])+'-');
  inc(o);
  end;
WriteLn(#8' ');
Write('broadcast address: ');
for i:=1 to p do begin;
  write(byte2hextype(buf[o])+'-');
  inc(o);
  end;
WriteLn(#8' ');
a:='';
while (buf[o]<>0) do begin;
  a:=a+chr(buf[o]);
  inc(o);
  end;
myName:=a;
writeln('device name: "'+a+'"');
End;



Procedure processParams;
Var
  i,o,p,q:LongInt;
  a:String;
Begin;
atmConnN:=0;
for i:=2 to paramCount do begin;
  a:=paramStr(i);
  o:=pos('/',a);
  p:=BVal(copy(a,1,o-1));
  q:=BVal(copy(a,o+1,666));
  if (p and $ff<>p) then continue;
  if (q and $fff<>q) then continue;
  inc(atmConnN);
  {format: gggg pppppppp cccccccccccccccc ttt c hhhhhhhh}
  atmConnI[atmConnN]:=(p shl 16) or q;
  atmConnP[atmConnN]:=0;
  atmConnS[atmConnN]:=0;
  end;
if (atmConnN<1) then immErr('no vpi specified!');
End;



Function calculateHEC(var buffer):LongInt;
Const
  crc8table:array[0..255] of byte=(
    $00,$07,$0e,$09,$1c,$1b,$12,$15,$38,$3f,$36,$31,$24,$23,$2a,$2d,
    $70,$77,$7e,$79,$6c,$6b,$62,$65,$48,$4f,$46,$41,$54,$53,$5a,$5d,
    $e0,$e7,$ee,$e9,$fc,$fb,$f2,$f5,$d8,$df,$d6,$d1,$c4,$c3,$ca,$cd,
    $90,$97,$9e,$99,$8c,$8b,$82,$85,$a8,$af,$a6,$a1,$b4,$b3,$ba,$bd,
    $c7,$c0,$c9,$ce,$db,$dc,$d5,$d2,$ff,$f8,$f1,$f6,$e3,$e4,$ed,$ea,
    $b7,$b0,$b9,$be,$ab,$ac,$a5,$a2,$8f,$88,$81,$86,$93,$94,$9d,$9a,
    $27,$20,$29,$2e,$3b,$3c,$35,$32,$1f,$18,$11,$16,$03,$04,$0d,$0a,
    $57,$50,$59,$5e,$4b,$4c,$45,$42,$6f,$68,$61,$66,$73,$74,$7d,$7a,
    $89,$8e,$87,$80,$95,$92,$9b,$9c,$b1,$b6,$bf,$b8,$ad,$aa,$a3,$a4,
    $f9,$fe,$f7,$f0,$e5,$e2,$eb,$ec,$c1,$c6,$cf,$c8,$dd,$da,$d3,$d4,
    $69,$6e,$67,$60,$75,$72,$7b,$7c,$51,$56,$5f,$58,$4d,$4a,$43,$44,
    $19,$1e,$17,$10,$05,$02,$0b,$0c,$21,$26,$2f,$28,$3d,$3a,$33,$34,
    $4e,$49,$40,$47,$52,$55,$5c,$5b,$76,$71,$78,$7f,$6a,$6d,$64,$63,
    $3e,$39,$30,$37,$22,$25,$2c,$2b,$06,$01,$08,$0f,$1a,$1d,$14,$13,
    $ae,$a9,$a0,$a7,$b2,$b5,$bc,$bb,$96,$91,$98,$9f,$8a,$8d,$84,$83,
    $de,$d9,$d0,$d7,$c2,$c5,$cc,$cb,$e6,$e1,$e8,$ef,$fa,$fd,$f4,$f3);
Var
  buf:array[1..4] of byte absolute buffer;
  i,o:LongInt;
Begin;
o:=0;
for i:=1 to oneCellHeader-1 do o:=crc8table[o xor buf[i]];
calculateHEC:=o xor $55;
End;

Function calculateCRC(var buffer;size:LongInt):LongInt;
Const
  crc32table:array[0..255] of LongWord=(
     $00000000,$04c11db7,$09823b6e,$0d4326d9,$130476dc,$17c56b6b,$1a864db2,$1e475005,
     $2608edb8,$22c9f00f,$2f8ad6d6,$2b4bcb61,$350c9b64,$31cd86d3,$3c8ea00a,$384fbdbd,
     $4c11db70,$48d0c6c7,$4593e01e,$4152fda9,$5f15adac,$5bd4b01b,$569796c2,$52568b75,
     $6a1936c8,$6ed82b7f,$639b0da6,$675a1011,$791d4014,$7ddc5da3,$709f7b7a,$745e66cd,
     $9823b6e0,$9ce2ab57,$91a18d8e,$95609039,$8b27c03c,$8fe6dd8b,$82a5fb52,$8664e6e5,
     $be2b5b58,$baea46ef,$b7a96036,$b3687d81,$ad2f2d84,$a9ee3033,$a4ad16ea,$a06c0b5d,
     $d4326d90,$d0f37027,$ddb056fe,$d9714b49,$c7361b4c,$c3f706fb,$ceb42022,$ca753d95,
     $f23a8028,$f6fb9d9f,$fbb8bb46,$ff79a6f1,$e13ef6f4,$e5ffeb43,$e8bccd9a,$ec7dd02d,
     $34867077,$30476dc0,$3d044b19,$39c556ae,$278206ab,$23431b1c,$2e003dc5,$2ac12072,
     $128e9dcf,$164f8078,$1b0ca6a1,$1fcdbb16,$018aeb13,$054bf6a4,$0808d07d,$0cc9cdca,
     $7897ab07,$7c56b6b0,$71159069,$75d48dde,$6b93dddb,$6f52c06c,$6211e6b5,$66d0fb02,
     $5e9f46bf,$5a5e5b08,$571d7dd1,$53dc6066,$4d9b3063,$495a2dd4,$44190b0d,$40d816ba,
     $aca5c697,$a864db20,$a527fdf9,$a1e6e04e,$bfa1b04b,$bb60adfc,$b6238b25,$b2e29692,
     $8aad2b2f,$8e6c3698,$832f1041,$87ee0df6,$99a95df3,$9d684044,$902b669d,$94ea7b2a,
     $e0b41de7,$e4750050,$e9362689,$edf73b3e,$f3b06b3b,$f771768c,$fa325055,$fef34de2,
     $c6bcf05f,$c27dede8,$cf3ecb31,$cbffd686,$d5b88683,$d1799b34,$dc3abded,$d8fba05a,
     $690ce0ee,$6dcdfd59,$608edb80,$644fc637,$7a089632,$7ec98b85,$738aad5c,$774bb0eb,
     $4f040d56,$4bc510e1,$46863638,$42472b8f,$5c007b8a,$58c1663d,$558240e4,$51435d53,
     $251d3b9e,$21dc2629,$2c9f00f0,$285e1d47,$36194d42,$32d850f5,$3f9b762c,$3b5a6b9b,
     $0315d626,$07d4cb91,$0a97ed48,$0e56f0ff,$1011a0fa,$14d0bd4d,$19939b94,$1d528623,
     $f12f560e,$f5ee4bb9,$f8ad6d60,$fc6c70d7,$e22b20d2,$e6ea3d65,$eba91bbc,$ef68060b,
     $d727bbb6,$d3e6a601,$dea580d8,$da649d6f,$c423cd6a,$c0e2d0dd,$cda1f604,$c960ebb3,
     $bd3e8d7e,$b9ff90c9,$b4bcb610,$b07daba7,$ae3afba2,$aafbe615,$a7b8c0cc,$a379dd7b,
     $9b3660c6,$9ff77d71,$92b45ba8,$9675461f,$8832161a,$8cf30bad,$81b02d74,$857130c3,
     $5d8a9099,$594b8d2e,$5408abf7,$50c9b640,$4e8ee645,$4a4ffbf2,$470cdd2b,$43cdc09c,
     $7b827d21,$7f436096,$7200464f,$76c15bf8,$68860bfd,$6c47164a,$61043093,$65c52d24,
     $119b4be9,$155a565e,$18197087,$1cd86d30,$029f3d35,$065e2082,$0b1d065b,$0fdc1bec,
     $3793a651,$3352bbe6,$3e119d3f,$3ad08088,$2497d08d,$2056cd3a,$2d15ebe3,$29d4f654,
     $c5a92679,$c1683bce,$cc2b1d17,$c8ea00a0,$d6ad50a5,$d26c4d12,$df2f6bcb,$dbee767c,
     $e3a1cbc1,$e760d676,$ea23f0af,$eee2ed18,$f0a5bd1d,$f464a0aa,$f9278673,$fde69bc4,
     $89b8fd09,$8d79e0be,$803ac667,$84fbdbd0,$9abc8bd5,$9e7d9662,$933eb0bb,$97ffad0c,
     $afb010b1,$ab710d06,$a6322bdf,$a2f33668,$bcb4666d,$b8757bda,$b5365d03,$b1f740b4);
Var
  buf:array[1..1] of byte absolute buffer;
  o,p:LongWord;
Begin;
p:=$ffffffff;
for o:=1 to size do p:=crc32table[(p shr 24) xor buf[o]] xor (p shl 8);
calculateCRC:=p xor $ffffffff;
End;



Procedure gotOneCell(var buffer;size:LongInt);
Label f1;
Var
  buf:array[1..1] of byte absolute buffer;
  i,o,p,q:LongInt;
Begin;
if (size<>oneCellSize) then begin;
  WriteLn('got invalid size cell!');
  exit;
  end;
if (calculateHEC(buf)<>buf[5]) then begin;
  WriteLn('got invalid header checksum!');
  exit;
  end;
i:=(ReadLongMSB(buf[1]) shr 4) and $ffffff;
for q:=1 to atmConnN do if (atmConnI[q]=i) then goto f1;
WriteLn('got cell for invalid vpi: '+BStr(i shr 16)+'/'+BStr(i and $ffff));
exit;
f1:
if (buf[4] and 8<>0) then begin;
  WriteLn('got management cell for vpi '+BStr(q));
  exit;
  end;
move(buf[oneCellHeader+1],atmConnB[q][atmConnS[q]+1],oneCellPayLoad);
inc(atmConnS[q],oneCellPayLoad);
if (atmConnS[q]+128>sizeof(atmConnB[1])) then begin;
  WriteLn('too big packet on vpi '+BStr(q));
  atmConnS[q]:=0;
  exit;
  end;
if (buf[4] and 2=0) then exit;
p:=atmConnS[q];
atmConnS[q]:=0;
move(atmConnB[q],buf,p);
if (calculateCRC(atmConnB[q],p-4)<>ReadLongMSB(atmConnB[q][p-3])) then begin;
  WriteLn('got frame with bad crc for vpi '+BStr(q));
  exit;
  end;
o:=ReadWordMSB(atmConnB[q][p-5]);
if (o>p-8) then begin;
  WriteLn('got truncated frame for vpi '+BStr(q));
  exit;
  end;
pipeLineSend(atmConnP[q],atmConnB[q][0],o+1);
End;



Procedure sendOutCells(q:LongInt;var buffer;size:LongInt);
Var
  buf:array[1..1] of byte absolute buffer;
  buf2:array[1..oneCellSize] of byte;
  i,o,p:LongInt;
Begin;
p:=size+8;
i:=p mod oneCellPayLoad;
if (i<>0) then inc(p,oneCellPayLoad-i);
fillChar(buf[size+1],p-size,0);
WriteWordMSB(buf[p-5],size);
WriteLongMSB(buf[p-3],calculateCRC(buf,p-4));
o:=1;
while (o<p) do begin;
  move(buf[o],buf2[oneCellHeader+1],oneCellPayLoad);
  inc(o,oneCellPayLoad);
  WriteLongMSB(buf2[1],atmConnI[q] shl 4);
  if (o>=p) then inc(buf2[oneCellHeader-1],2);
  buf2[oneCellHeader]:=calculateHEC(buf2);
  pipeLineSend(devPipe,buf2,oneCellSize);
  end;
End;



Function doLower:Boolean;
Label f1,f2;
Var
  buf:array[1..4*1024] of byte;
  i,o,p,q:LongInt;
Begin;
doLower:=False;;
p:=sizeof(buf);
if (pipeLineRecv(devPipe,buf,p)<>0) then p:=0;
if (p<1) then begin;
  pipeLineStats(devPipe,p,i,o);
  if (p=0) then immErr('lower level closed connection!');
  exit;
  end;
doLower:=True;
gotOneCell(buf,p);
End;



Function doUpper:Boolean;
Var
  buf:array[1..4*1024] of byte;
  i,o,p,q:LongInt;
  a:String;
Begin;
doUpper:=False;
while (pipeLineGetIncoming(q)=0) do begin;
  p:=0;
  for i:=atmConnN downto 1 do if (atmConnP[i]=0) then p:=i;
  if (p<1) then begin; pipeLineClose(q);continue; end;
  atmConnP[p]:=q;
  i:=atmConnI[p];
  a:=BStr(i shr 16)+'/'+BStr(i and $ffff);
  WriteLn('vci '+a+' logged in!');
  a:='vci '+a+' on '+myName;
  a:='12341234'#0#0#0#0#0#0#0#0+chr(i)+#255+a+#0;
  i:=1;move(i,a[1],sizeof(i));
  i:=1500;move(i,a[5],sizeof(i));
  pipeLineSend(q,a[1],length(a));
  end;
for q:=1 to atmConnN do if (atmConnP[q]<>0) then begin;
  p:=sizeof(buf);
  if (pipeLineRecv(atmConnP[q],buf,p)<>0) then p:=0;
  if (p>0) then begin;
    sendOutCells(q,buf[2],p-1);
    doUpper:=True;
    continue;
    end;
  pipeLineStats(atmConnP[q],p,i,o);
  if (p<>0) then continue;
  WriteLn('vci '+BStr(q)+' logged out!');
  pipeLineClose(atmConnP[q]);
  atmConnP[q]:=0;
  end;

End;



Label f1;
BEGIN;
WriteLn('atm aal5 v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<2) then immErr('using: atm.code <process> <vpi/vci> [vpi/vci]');

processParams;
openDevice(ParamStr(1));

pipeLineBegListen;
BugOS_SignDaemoning;

f1:
relequish;
while doLower do;
while doUpper do;
goto f1;
END.