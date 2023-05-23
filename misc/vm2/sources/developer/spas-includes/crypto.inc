Const CryptoContextSize=$1400;
Type CryptoContextRecord=array[1..CryptoContextSize] of byte;
Var
  CryptoProcessId:LongInt;
  CryptoPipelineId:LongInt;



Function CryptoStartActions:Boolean;
Begin;
pipeLineClose(CryptoPipelineId);
CryptoProcessId:=BugOS_findProcNam('crypto.code');
if (pipeLineCreate(CryptoPipelineId,CryptoProcessId,65536,true)<>0) then CryptoPipelineId:=0;
CryptoStartActions:=(CryptoPipelineId=0);
End;

Procedure CryptoStopActions;
Begin;
pipeLineClose(CryptoPipelineId);
CryptoPipelineId:=0;
End;



{format: dd:contextSize; n*( dd:blocksize; asciiz:name )  }
Procedure CryptoGetHasherList(var data;var size:LongInt);
Label f1;
Begin;
size:=1;
pipeLineSend(CryptoPipelineId,size,sizeof(size));
f1:
size:=8*1024;
if (pipeLineRecv(CryptoPipelineId,data,size)<>0) then size:=0;
if (size<1) then begin;
  relequish;
  goto f1;
  end;
End;

{format: dd:contextSize; n*( dd:blocksize; asciiz:name )  }
Procedure CryptoGetCipherList(var data;var size:LongInt);
Label f1;
Begin;
size:=0;
pipeLineSend(CryptoPipelineId,size,sizeof(size));
f1:
size:=8*1024;
if (pipeLineRecv(CryptoPipelineId,data,size)<>0) then size:=0;
if (size<1) then begin;
  relequish;
  goto f1;
  end;
End;

Function CryptoFindOneAlgo(var data;size:LongInt;name:String):LongInt;
Label f1,f2,f3;
Var
  dat2:array[1..1] of byte absolute data;
  i,o,p:LongInt;
  a:String;
Begin;
CryptoFindOneAlgo:=0;
name:=kicsi(name);
o:=0;
p:=4;
f1:
if (p>=size) then exit;
inc(p,4);
inc(o);
a:='';
f2:
inc(p);
i:=dat2[p];
if (i<>0) then begin;
  a:=a+chr(i);
  goto f2;
  end;
a:=kicsi(a);
if (a<>name) then goto f1;
CryptoFindOneAlgo:=o;
End;



Procedure CryptoImmHasher(algo:LongInt;key,iv:String;var data;var size:LongInt);
Label f1;
Var
  datSiz:LongInt;
  i:LongInt;
  datBuf:array[1..8*1024] of byte;
Begin;
dec(algo);
i:=2;
move(i,datBuf[1],sizeof(i));
move(algo,datBuf[5],sizeof(algo));
datSiz:=8;
move(key,datBuf[datSiz+1],sizeof(key));inc(datSiz,length(key)+1);
move(iv,datBuf[datSiz+1],sizeof(iv));inc(datSiz,length(iv)+1);
move(data,datBuf[datSiz+1],size);inc(datSiz,size);
pipeLineSend(CryptoPipelineId,datBuf,datSiz);
f1:
size:=1024;
if (pipeLineRecv(CryptoPipelineId,data,size)<>0) then size:=0;
if (size<1) then begin;
  relequish;
  goto f1;
  end;
End;

Procedure CryptoImmEncrypt(algo:LongInt;key,iv:String;var data;size:LongInt);
Label f1;
Var
  datSiz:LongInt;
  i:LongInt;
  datBuf:array[1..8*1024] of byte;
Begin;
dec(algo);
i:=3;
move(i,datBuf[1],sizeof(i));
move(algo,datBuf[5],sizeof(algo));
datSiz:=8;
move(key,datBuf[datSiz+1],sizeof(key));inc(datSiz,length(key)+1);
move(iv,datBuf[datSiz+1],sizeof(iv));inc(datSiz,length(iv)+1);
move(data,datBuf[datSiz+1],size);inc(datSiz,size);
pipeLineSend(CryptoPipelineId,datBuf,datSiz);
i:=size+1024;
f1:
size:=i;
if (pipeLineRecv(CryptoPipelineId,data,size)<>0) then size:=0;
if (size<1) then begin;
  relequish;
  goto f1;
  end;
End;

Procedure CryptoImmDecrypt(algo:LongInt;key,iv:String;var data;size:LongInt);
Label f1;
Var
  datSiz:LongInt;
  i:LongInt;
  datBuf:array[1..8*1024] of byte;
Begin;
dec(algo);
i:=4;
move(i,datBuf[1],sizeof(i));
move(algo,datBuf[5],sizeof(algo));
datSiz:=8;
move(key,datBuf[datSiz+1],sizeof(key));inc(datSiz,length(key)+1);
move(iv,datBuf[datSiz+1],sizeof(iv));inc(datSiz,length(iv)+1);
move(data,datBuf[datSiz+1],size);inc(datSiz,size);
pipeLineSend(CryptoPipelineId,datBuf,datSiz);
i:=size+1024;
f1:
size:=i;
if (pipeLineRecv(CryptoPipelineId,data,size)<>0) then size:=0;
if (size<1) then begin;
  relequish;
  goto f1;
  end;
End;



Procedure CryptoBegHasher(var ctx:CryptoContextRecord;algo:LongInt;key,iv:String);
Label f1;
Var
  datSiz:LongInt;
  i:LongInt;
  datBuf:array[1..1024] of byte;
Begin;
dec(algo);
i:=5;
move(i,datBuf[1],sizeof(i));
move(algo,datBuf[5],sizeof(algo));
datSiz:=8;
move(key,datBuf[datSiz+1],sizeof(key));inc(datSiz,length(key)+1);
move(iv,datBuf[datSiz+1],sizeof(iv));inc(datSiz,length(iv)+1);
pipeLineSend(CryptoPipelineId,datBuf,datSiz);
f1:
i:=sizeof(ctx);
if (pipeLineRecv(CryptoPipelineId,ctx,i)<>0) then i:=0;
if (i<1) then begin;
  relequish;
  goto f1;
  end;
End;

Procedure CryptoAddHasher(var ctx:CryptoContextRecord;var data;size:LongInt);
Label f1;
Var
  datSiz:LongInt;
  i:LongInt;
  datBuf:array[1..8*1024] of byte;
Begin;
i:=6;
move(i,datBuf[1],sizeof(i));
datSiz:=4;
move(ctx,datBuf[datSiz+1],sizeof(ctx));inc(datSiz,sizeof(ctx));
move(data,datBuf[datSiz+1],size);inc(datSiz,size);
pipeLineSend(CryptoPipelineId,datBuf,datSiz);
f1:
i:=sizeof(ctx);
if (pipeLineRecv(CryptoPipelineId,ctx,i)<>0) then i:=0;
if (i<1) then begin;
  relequish;
  goto f1;
  end;
End;

Procedure CryptoFinHasher(var ctx:CryptoContextRecord;var data;var size:LongInt);
Label f1;
Var
  datSiz:LongInt;
  i:LongInt;
  datBuf:array[1..8*1024] of byte;
Begin;
i:=7;
move(i,datBuf[1],sizeof(i));
datSiz:=4;
move(ctx,datBuf[datSiz+1],sizeof(ctx));inc(datSiz,sizeof(ctx));
pipeLineSend(CryptoPipelineId,datBuf,datSiz);
f1:
size:=1024;
if (pipeLineRecv(CryptoPipelineId,data,size)<>0) then size:=0;
if (size<1) then begin;
  relequish;
  goto f1;
  end;
End;



Procedure CryptoBegCipher(var ctx:CryptoContextRecord;algo:LongInt;key,iv:String);
Label f1;
Var
  datSiz:LongInt;
  i:LongInt;
  datBuf:array[1..1024] of byte;
Begin;
dec(algo);
i:=8;
move(i,datBuf[1],sizeof(i));
move(algo,datBuf[5],sizeof(algo));
datSiz:=8;
move(key,datBuf[datSiz+1],sizeof(key));inc(datSiz,length(key)+1);
move(iv,datBuf[datSiz+1],sizeof(iv));inc(datSiz,length(iv)+1);
pipeLineSend(CryptoPipelineId,datBuf,datSiz);
f1:
i:=sizeof(ctx);
if (pipeLineRecv(CryptoPipelineId,ctx,i)<>0) then i:=0;
if (i<1) then begin;
  relequish;
  goto f1;
  end;
End;

Procedure CryptoAddEncrypt(var ctx:CryptoContextRecord;var data;size:LongInt);
Label f1;
Var
  datSiz:LongInt;
  i:LongInt;
  datBuf:array[1..8*1024] of byte;
Begin;
i:=9;
move(i,datBuf[1],sizeof(i));
datSiz:=4;
move(ctx,datBuf[datSiz+1],sizeof(ctx));inc(datSiz,sizeof(ctx));
move(data,datBuf[datSiz+1],size);inc(datSiz,size);
pipeLineSend(CryptoPipelineId,datBuf,datSiz);
f1:
i:=sizeof(datBuf);
if (pipeLineRecv(CryptoPipelineId,datBuf,i)<>0) then i:=0;
if (i<1) then begin;
  relequish;
  goto f1;
  end;
move(datBuf,ctx,sizeof(ctx));
move(datBuf[sizeof(ctx)+1],data,size);
End;

Procedure CryptoAddDecrypt(var ctx:CryptoContextRecord;var data;size:LongInt);
Label f1;
Var
  datSiz:LongInt;
  i:LongInt;
  datBuf:array[1..8*1024] of byte;
Begin;
i:=10;
move(i,datBuf[1],sizeof(i));
datSiz:=4;
move(ctx,datBuf[datSiz+1],sizeof(ctx));inc(datSiz,sizeof(ctx));
move(data,datBuf[datSiz+1],size);inc(datSiz,size);
pipeLineSend(CryptoPipelineId,datBuf,datSiz);
f1:
i:=sizeof(datBuf);
if (pipeLineRecv(CryptoPipelineId,datBuf,i)<>0) then i:=0;
if (i<1) then begin;
  relequish;
  goto f1;
  end;
move(datBuf,ctx,sizeof(ctx));
move(datBuf[sizeof(ctx)+1],data,size);
End;

Procedure CryptoUpdtCipher(var ctx:CryptoContextRecord;iv:String);
Label f1;
Var
  datSiz:LongInt;
  i:LongInt;
  datBuf:array[1..8*1024] of byte;
Begin;
i:=11;
move(i,datBuf[1],sizeof(i));
datSiz:=4;
move(ctx,datBuf[datSiz+1],sizeof(ctx));inc(datSiz,sizeof(ctx));
move(iv,datBuf[datSiz+1],sizeof(iv));inc(datSiz,length(iv)+1);
pipeLineSend(CryptoPipelineId,datBuf,datSiz);
f1:
i:=sizeof(datBuf);
if (pipeLineRecv(CryptoPipelineId,datBuf,i)<>0) then i:=0;
if (i<1) then begin;
  relequish;
  goto f1;
  end;
move(datBuf,ctx,sizeof(ctx));
End;
