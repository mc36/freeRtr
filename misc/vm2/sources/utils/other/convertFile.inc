Function doConvertOneFile(var f:xFile;b:String;mod,ps,siz:LongInt):String; {1=read, 2=write}
Label f1,f2,ok,err;
Var
  buf:array[1..1024*16] of byte;
  proc,pipe:LongInt;
  i,o,p:LongInt;
  ptr:Pointer;
  a:String;
Begin;
doConvertOneFile:='error starting filter';
proc:=0;
pipe:=0;
pipeLineBegListen;
if (mod=2) then a:='w' else begin;
  fillchar(currentPalD,sizeof(currentPalD),0);
  currentResX:=0;
  currentResY:=0;
  a:='r';
  end;
if (xExecBgnd('\utils\graphic\filter-'+b+'.code',a,proc)<>0) then exit;
f1:
relequish;
if not BugOS_ProcessExists(proc) then begin;
  err:
  pipeLineEndListen;
  pipeLineClose(pipe);
  BugOS_KillProcess(proc);
  exit;
  end;
if (pipeLineGetIncoming(pipe)<>0) then goto f1;
pipeLineStats(pipe,o,i,i);
if (o<>proc) then begin;
  pipeLineClose(pipe);
  pipe:=0;
  goto f1;
  end;
f2:
p:=sizeof(buf);
pipeLineRecv(pipe,buf,p);
if (p=0) then begin;
  pipeLineStats(pipe,o,i,i);
  if (o=proc) then begin; relequish;goto f2; end;
  doConvertOneFile:='error in file format';
  goto err;
  end;
move(buf,i,sizeof(i));
case i of
  1:begin; {size}
    pipeLineSend(pipe,siz,sizeof(siz));
    goto f2;
    end;
  2:begin; {truncate}
    move(buf[5],i,sizeof(i));
    xSeek(f,ps+i);
    if (xTruncate(f)=0) then begin;
      ok:
      i:=0;
      pipeLineSend(pipe,i,sizeof(i));
      goto f2;
      end;
    doConvertOneFile:='error truncating file';
    goto err;
    end;
  3:begin; {write file}
    move(buf[5],i,sizeof(i));
    xSeek(f,ps+i);
    dec(p,8);
    if (p<1) then goto ok;
    if (xBlockWrite(f,buf[9],p)=0) then goto ok;
    doConvertOneFile:='error writing file';
    goto err;
    end;
  4:begin; {read file}
    move(buf[5],o,sizeof(o));
    move(buf[9],i,sizeof(i));
    if (i<1) then goto ok;
    xSeek(f,ps+o);
    if (xBlockRead(f,buf,i)=0) then begin;
      pipeLineSend(pipe,buf,i);
      goto f2;
      end;
    doConvertOneFile:='error reading file';
    goto err;
    end;
  5:begin; {put pal}
    move(buf[5],currentResX,sizeof(currentResX));
    move(buf[9],currentResY,sizeof(currentResY));
    move(buf[13],currentPalD,sizeof(currentPalD));
    o:=currentResX*currentResY+1024;
    if (ExtendedMemoryResize(ptr,o)>=o) then begin;
      currentData:=ptr^;
      goto ok;
      end;
    doConvertOneFile:='error allocating memory';
    goto err;
    end;
  6:begin; {get pal}
    move(currentResX,buf[1],sizeof(currentResX));
    move(currentResY,buf[5],sizeof(currentResY));
    move(currentPalD,buf[9],sizeof(currentPalD));
    pipeLineSend(pipe,buf,776);
    goto f2;
    end;
  7:begin; {write image}
    move(buf[5],i,sizeof(i));
    move(buf[9],o,sizeof(o));
    o:=o*currentResX+i;
    move(buf[13],currentData^[o+1],p-12);
    goto ok;
    end;
  8:begin; {read image}
    move(buf[5],i,sizeof(i));
    move(buf[9],o,sizeof(o));
    move(buf[13],p,sizeof(p));
    o:=o*currentResX+i;
    pipeLineSend(pipe,currentData^[o+1],p);
    goto f2;
    end;
  9:begin; {end}
    doConvertOneFile:='';
    goto err;
    end;
  end;

doConvertOneFile:='filter requested unknown command';
goto err;
End;
