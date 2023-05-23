Const authBufferMax=512;
Type
  OneConnectionRecord=record
    pipe1:LongInt;              {pipeline id of accepted connection}
    pipe2:LongInt;              {pipeline id of initiated connection}
    time:LongInt;               {time of last action}
    adr:OneTCPaddressRecord;    {remote address}
    prt:LongInt;                {remote port}
    stat:LongInt;               {status: 0=disconn, 1=init, 2=uid, 3=conning, 4=secure, 5=conned}
    line:String;                {received line}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  sourcePort:LongInt;
  sourceAddr:OneTCPaddressRecord;
  targetPort:LongInt;
  targetAddr:OneTCPaddressRecord;
  srcProto:LongInt;             {0=tcp, 1=tls, 2=ssh}
  trgProto:LongInt;             {0=tcp, 1=tls, 2=ssh}
  authBufferDat:array[1..authBufferMax] of Byte;
  authBufferSiz:LongInt;
  magicWord:String;



Function ResizeMem(n:LongInt):Boolean;
Var
  p:Pointer;
  i:LongInt;
Begin;
ResizeMem:=True;
i:=n*sizeof(OneConnectionRecord);
if (ExtendedMemoryResize(p,i)<i) then exit;
ConnectionNum:=n;
ConnectionDat:=p^;
ResizeMem:=False;
End;

Function getWord(var a:String):String;
Var i:LongInt;
Begin;
i:=pos(' ',a);
if (i<1) then i:=666;
getWord:=copy(a,1,i-1);
a:=copy(a,i+1,255);
End;

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Function decodeOneProto(a:String):LongInt;
Var i:LongInt;
Begin;
a:=kicsi(a);
i:=0;
if (a='tcp') then i:=0;
if (a='tls') then i:=1;
if (a='ssh') then i:=2;
decodeOneProto:=i;
End;

Function encodeOneProto(i:LongInt):String;
Var a:String;
Begin;
case i of
  0:a:='tcp';
  1:a:='tls';
  2:a:='ssh';
  else a:='???';
  end;
encodeOneProto:=a;
End;


Procedure askUserAuth(qst:String;msk,add:LongInt);
Label f1,f2;
Var
  siz:LongInt;
  i:LongInt;
Begin;
siz:=0;
Write(qst);
f1:
i:=ReadKey;
if (i and $fe00=0) then begin;{simple key}
  i:=i and $ff;
  if (i in [0,255,13,10,8,9]) then i:=32;
  if (siz>250) then goto f1;
  inc(siz);
  inc(authBufferSiz);
  authBufferDat[authBufferSiz]:=i;
  i:=(i and msk)+add;
  write(chr(i));
  goto f1;
  end;
case i of
  $8003:begin;{backspace}
    if (siz=0) then goto f1;
    Write(#8' '#8);
    dec(siz);
    dec(authBufferSiz);
    goto f1;
    end;
  $8004:goto f2;{enter}
  $8005:begin;{escape}
    WriteLn('');
    dec(authBufferSiz,siz);
    siz:=0;
    Write(qst);
    goto f1;
    end;
  end;
goto f1;
f2:
WriteLn('');
inc(authBufferSiz);
authBufferDat[authBufferSiz]:=13;
End;
