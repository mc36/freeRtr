Const BigNumContextSize=$400;
Type BigNumContextRecord=array[1..BigNumContextSize] of byte;
Var
  BigNumProcessId:LongInt;
  BigNumPipelineId:LongInt;

Function BigNumStartActions:Boolean;
Begin;
pipeLineClose(BigNumPipelineId);
BigNumProcessId:=BugOS_findProcNam('bignum.code');
if (pipeLineCreate(BigNumPipelineId,BigNumProcessId,65536,true)<>0) then BigNumPipelineId:=0;
BigNumStartActions:=(BigNumPipelineId=0);
End;

Procedure BigNumStopActions;
Begin;
pipeLineClose(BigNumPipelineId);
BigNumPipelineId:=0;
End;



Procedure BigNumConvBN2LSB(var num:BigNumContextRecord;var size:LongInt;var res);
Label f1;
Var
  rec1:record
    cmd:LongInt;
    num:BigNumContextRecord;
    end;
Begin;
rec1.cmd:=19;
rec1.num:=num;
pipeLineSend(BigNumPipelineId,rec1,sizeof(rec1));
f1:
size:=4096;
if (pipeLineRecv(BigNumPipelineId,res,size)<>0) then size:=0;
if (size<1) then begin;
  relequish;
  goto f1;
  end;
End;

Procedure BigNumConvBN2MSB(var num:BigNumContextRecord;var size:LongInt;var res);
Label f1;
Var
  rec1:record
    cmd:LongInt;
    num:BigNumContextRecord;
    end;
Begin;
rec1.cmd:=20;
rec1.num:=num;
pipeLineSend(BigNumPipelineId,rec1,sizeof(rec1));
f1:
size:=4096;
if (pipeLineRecv(BigNumPipelineId,res,size)<>0) then size:=0;
if (size<1) then begin;
  relequish;
  goto f1;
  end;
End;

Procedure BigNumConvLSB2BN(size:LongInt;var num;var res:BigNumContextRecord);
Label f1;
Var
  rec1:record
    cmd:LongInt;
    num:BigNumContextRecord;
    end;
  i:LongInt;
Begin;
rec1.cmd:=21;
move(num,rec1.num,size);
pipeLineSend(BigNumPipelineId,rec1,4+size);
f1:
i:=sizeof(res);
if (pipeLineRecv(BigNumPipelineId,res,i)<>0) then i:=0;
if (i<1) then begin;
  relequish;
  goto f1;
  end;
End;

Procedure BigNumConvMSB2BN(size:LongInt;var num;var res:BigNumContextRecord);
Label f1;
Var
  rec1:record
    cmd:LongInt;
    num:BigNumContextRecord;
    end;
  i:LongInt;
Begin;
rec1.cmd:=22;
move(num,rec1.num,size);
pipeLineSend(BigNumPipelineId,rec1,4+size);
f1:
i:=sizeof(res);
if (pipeLineRecv(BigNumPipelineId,res,i)<>0) then i:=0;
if (i<1) then begin;
  relequish;
  goto f1;
  end;
End;




Procedure BigNumStoreInt(num:LongInt;var res:BigNumContextRecord);
Label f1;
Var
  rec1:record
    cmd:LongInt;
    num:LongInt;
    end;
  i:LongInt;
Begin;
rec1.cmd:=1;
rec1.num:=num;
pipeLineSend(BigNumPipelineId,rec1,sizeof(rec1));
f1:
i:=sizeof(res);
if (pipeLineRecv(BigNumPipelineId,res,i)<>0) then i:=0;
if (i<1) then begin;
  relequish;
  goto f1;
  end;
End;

Procedure BigNumAbs(var num,res:BigNumContextRecord);
Label f1;
Var
  rec1:record
    cmd:LongInt;
    num:BigNumContextRecord;
    end;
  i:LongInt;
Begin;
rec1.cmd:=2;
rec1.num:=num;
pipeLineSend(BigNumPipelineId,rec1,sizeof(rec1));
f1:
i:=sizeof(res);
if (pipeLineRecv(BigNumPipelineId,res,i)<>0) then i:=0;
if (i<1) then begin;
  relequish;
  goto f1;
  end;
End;

Procedure BigNumNeg(var num,res:BigNumContextRecord);
Label f1;
Var
  rec1:record
    cmd:LongInt;
    num:BigNumContextRecord;
    end;
  i:LongInt;
Begin;
rec1.cmd:=3;
rec1.num:=num;
pipeLineSend(BigNumPipelineId,rec1,sizeof(rec1));
f1:
i:=sizeof(res);
if (pipeLineRecv(BigNumPipelineId,res,i)<>0) then i:=0;
if (i<1) then begin;
  relequish;
  goto f1;
  end;
End;



Procedure BigNumClearPadding(var num:BigNumContextRecord);
Label f1;
Var
  rec1:record
    cmd:LongInt;
    num:BigNumContextRecord;
    end;
  i:LongInt;
Begin;
rec1.cmd:=25;
rec1.num:=num;
pipeLineSend(BigNumPipelineId,rec1,sizeof(rec1));
f1:
i:=sizeof(num);
if (pipeLineRecv(BigNumPipelineId,num,i)<>0) then i:=0;
if (i<1) then begin;
  relequish;
  goto f1;
  end;
End;

Function BigNumSizeInBits(var num:BigNumContextRecord):LongInt;
Label f1;
Var
  rec1:record
    cmd:LongInt;
    num:BigNumContextRecord;
    end;
  i,o:LongInt;
Begin;
rec1.cmd:=23;
rec1.num:=num;
pipeLineSend(BigNumPipelineId,rec1,sizeof(rec1));
f1:
i:=sizeof(o);
if (pipeLineRecv(BigNumPipelineId,o,i)<>0) then i:=0;
if (i<1) then begin;
  relequish;
  goto f1;
  end;
BigNumSizeInBits:=o;
End;

Function BigNumSetedBits(var num:BigNumContextRecord):LongInt;
Label f1;
Var
  rec1:record
    cmd:LongInt;
    num:BigNumContextRecord;
    end;
  i,o:LongInt;
Begin;
rec1.cmd:=24;
rec1.num:=num;
pipeLineSend(BigNumPipelineId,rec1,sizeof(rec1));
f1:
i:=sizeof(o);
if (pipeLineRecv(BigNumPipelineId,o,i)<>0) then i:=0;
if (i<1) then begin;
  relequish;
  goto f1;
  end;
BigNumSetedBits:=o;
End;

Function BigNumComp(var n1,n2:BigNumContextRecord):LongInt;
Label f1;
Var
  rec1:record
    cmd:LongInt;
    n1:BigNumContextRecord;
    n2:BigNumContextRecord;
    end;
  i,o:LongInt;
Begin;
rec1.cmd:=4;
rec1.n1:=n1;
rec1.n2:=n2;
pipeLineSend(BigNumPipelineId,rec1,sizeof(rec1));
f1:
i:=sizeof(o);
if (pipeLineRecv(BigNumPipelineId,o,i)<>0) then i:=0;
if (i<1) then begin;
  relequish;
  goto f1;
  end;
BigNumComp:=o;
End;




Procedure BigNumShl(cnt:LongInt;var num,res:BigNumContextRecord);
Label f1;
Var
  rec1:record
    cmd:LongInt;
    cnt:LongInt;
    num:BigNumContextRecord;
    end;
  i:LongInt;
Begin;
rec1.cmd:=5;
rec1.cnt:=cnt;
rec1.num:=num;
pipeLineSend(BigNumPipelineId,rec1,sizeof(rec1));
f1:
i:=sizeof(res);
if (pipeLineRecv(BigNumPipelineId,res,i)<>0) then i:=0;
if (i<1) then begin;
  relequish;
  goto f1;
  end;
End;

Procedure BigNumShr(cnt:LongInt;var num,res:BigNumContextRecord);
Label f1;
Var
  rec1:record
    cmd:LongInt;
    cnt:LongInt;
    num:BigNumContextRecord;
    end;
  i:LongInt;
Begin;
rec1.cmd:=6;
rec1.cnt:=cnt;
rec1.num:=num;
pipeLineSend(BigNumPipelineId,rec1,sizeof(rec1));
f1:
i:=sizeof(res);
if (pipeLineRecv(BigNumPipelineId,res,i)<>0) then i:=0;
if (i<1) then begin;
  relequish;
  goto f1;
  end;
End;

Procedure BigNumNot(var num,res:BigNumContextRecord);
Label f1;
Var
  rec1:record
    cmd:LongInt;
    num:BigNumContextRecord;
    end;
  i:LongInt;
Begin;
rec1.cmd:=7;
rec1.num:=num;
pipeLineSend(BigNumPipelineId,rec1,sizeof(rec1));
f1:
i:=sizeof(res);
if (pipeLineRecv(BigNumPipelineId,res,i)<>0) then i:=0;
if (i<1) then begin;
  relequish;
  goto f1;
  end;
End;

Procedure BigNumOr(var n1,n2,res:BigNumContextRecord);
Label f1;
Var
  rec1:record
    cmd:LongInt;
    n1:BigNumContextRecord;
    n2:BigNumContextRecord;
    end;
  i:LongInt;
Begin;
rec1.cmd:=8;
rec1.n1:=n1;
rec1.n2:=n2;
pipeLineSend(BigNumPipelineId,rec1,sizeof(rec1));
f1:
i:=sizeof(res);
if (pipeLineRecv(BigNumPipelineId,res,i)<>0) then i:=0;
if (i<1) then begin;
  relequish;
  goto f1;
  end;
End;

Procedure BigNumXor(var n1,n2,res:BigNumContextRecord);
Label f1;
Var
  rec1:record
    cmd:LongInt;
    n1:BigNumContextRecord;
    n2:BigNumContextRecord;
    end;
  i:LongInt;
Begin;
rec1.cmd:=9;
rec1.n1:=n1;
rec1.n2:=n2;
pipeLineSend(BigNumPipelineId,rec1,sizeof(rec1));
f1:
i:=sizeof(res);
if (pipeLineRecv(BigNumPipelineId,res,i)<>0) then i:=0;
if (i<1) then begin;
  relequish;
  goto f1;
  end;
End;

Procedure BigNumAnd(var n1,n2,res:BigNumContextRecord);
Label f1;
Var
  rec1:record
    cmd:LongInt;
    n1:BigNumContextRecord;
    n2:BigNumContextRecord;
    end;
  i:LongInt;
Begin;
rec1.cmd:=10;
rec1.n1:=n1;
rec1.n2:=n2;
pipeLineSend(BigNumPipelineId,rec1,sizeof(rec1));
f1:
i:=sizeof(res);
if (pipeLineRecv(BigNumPipelineId,res,i)<>0) then i:=0;
if (i<1) then begin;
  relequish;
  goto f1;
  end;
End;




Procedure BigNumAdd(var n1,n2,res:BigNumContextRecord);
Label f1;
Var
  rec1:record
    cmd:LongInt;
    n1:BigNumContextRecord;
    n2:BigNumContextRecord;
    end;
  i:LongInt;
Begin;
rec1.cmd:=11;
rec1.n1:=n1;
rec1.n2:=n2;
pipeLineSend(BigNumPipelineId,rec1,sizeof(rec1));
f1:
i:=sizeof(res);
if (pipeLineRecv(BigNumPipelineId,res,i)<>0) then i:=0;
if (i<1) then begin;
  relequish;
  goto f1;
  end;
End;

Procedure BigNumSub(var n1,n2,res:BigNumContextRecord);
Label f1;
Var
  rec1:record
    cmd:LongInt;
    n1:BigNumContextRecord;
    n2:BigNumContextRecord;
    end;
  i:LongInt;
Begin;
rec1.cmd:=12;
rec1.n1:=n1;
rec1.n2:=n2;
pipeLineSend(BigNumPipelineId,rec1,sizeof(rec1));
f1:
i:=sizeof(res);
if (pipeLineRecv(BigNumPipelineId,res,i)<>0) then i:=0;
if (i<1) then begin;
  relequish;
  goto f1;
  end;
End;

Procedure BigNumMul(var n1,n2,res:BigNumContextRecord);
Label f1;
Var
  rec1:record
    cmd:LongInt;
    n1:BigNumContextRecord;
    n2:BigNumContextRecord;
    end;
  i:LongInt;
Begin;
rec1.cmd:=13;
rec1.n1:=n1;
rec1.n2:=n2;
pipeLineSend(BigNumPipelineId,rec1,sizeof(rec1));
f1:
i:=sizeof(res);
if (pipeLineRecv(BigNumPipelineId,res,i)<>0) then i:=0;
if (i<1) then begin;
  relequish;
  goto f1;
  end;
End;

Procedure BigNumDiv(var n1,n2,res,rmd:BigNumContextRecord);
Label f1;
Var
  rec1:record
    cmd:LongInt;
    n1:BigNumContextRecord;
    n2:BigNumContextRecord;
    end;
  rec2:record
    n1:BigNumContextRecord;
    n2:BigNumContextRecord;
    end;
  i:LongInt;
Begin;
rec1.cmd:=14;
rec1.n1:=n1;
rec1.n2:=n2;
pipeLineSend(BigNumPipelineId,rec1,sizeof(rec1));
f1:
i:=sizeof(rec2);
if (pipeLineRecv(BigNumPipelineId,rec2,i)<>0) then i:=0;
if (i<1) then begin;
  relequish;
  goto f1;
  end;
res:=rec2.n1;
rmd:=rec2.n2;
End;




Procedure BigNumPower(var n1,n2,res:BigNumContextRecord);
Label f1;
Var
  rec1:record
    cmd:LongInt;
    n1:BigNumContextRecord;
    n2:BigNumContextRecord;
    end;
  i:LongInt;
Begin;
rec1.cmd:=15;
rec1.n1:=n1;
rec1.n2:=n2;
pipeLineSend(BigNumPipelineId,rec1,sizeof(rec1));
f1:
i:=sizeof(res);
if (pipeLineRecv(BigNumPipelineId,res,i)<>0) then i:=0;
if (i<1) then begin;
  relequish;
  goto f1;
  end;
End;

Procedure BigNumRoot(var n1,n2,res:BigNumContextRecord);
Label f1;
Var
  rec1:record
    cmd:LongInt;
    n1:BigNumContextRecord;
    n2:BigNumContextRecord;
    end;
  i:LongInt;
Begin;
rec1.cmd:=16;
rec1.n1:=n1;
rec1.n2:=n2;
pipeLineSend(BigNumPipelineId,rec1,sizeof(rec1));
f1:
i:=sizeof(res);
if (pipeLineRecv(BigNumPipelineId,res,i)<>0) then i:=0;
if (i<1) then begin;
  relequish;
  goto f1;
  end;
End;

Procedure BigNumPowerMod(var n1,n2,n3,res:BigNumContextRecord);
Label f1;
Var
  rec1:record
    cmd:LongInt;
    n1:BigNumContextRecord;
    n2:BigNumContextRecord;
    n3:BigNumContextRecord;
    end;
  i:LongInt;
Begin;
rec1.cmd:=17;
rec1.n1:=n1;
rec1.n2:=n2;
rec1.n3:=n3;
pipeLineSend(BigNumPipelineId,rec1,sizeof(rec1));
f1:
i:=sizeof(res);
if (pipeLineRecv(BigNumPipelineId,res,i)<>0) then i:=0;
if (i<1) then begin;
  relequish;
  goto f1;
  end;
End;

Procedure BigNumInverseMod(var n1,n2,res:BigNumContextRecord);
Label f1;
Var
  rec1:record
    cmd:LongInt;
    n1:BigNumContextRecord;
    n2:BigNumContextRecord;
    end;
  i:LongInt;
Begin;
rec1.cmd:=18;
rec1.n1:=n1;
rec1.n2:=n2;
pipeLineSend(BigNumPipelineId,rec1,sizeof(rec1));
f1:
i:=sizeof(res);
if (pipeLineRecv(BigNumPipelineId,res,i)<>0) then i:=0;
if (i<1) then begin;
  relequish;
  goto f1;
  end;
End;

Procedure BigNumGreatestCommonDivisor(var n1,n2,res:BigNumContextRecord);
Label f1;
Var
  rec1:record
    cmd:LongInt;
    n1:BigNumContextRecord;
    n2:BigNumContextRecord;
    end;
  i:LongInt;
Begin;
rec1.cmd:=26;
rec1.n1:=n1;
rec1.n2:=n2;
pipeLineSend(BigNumPipelineId,rec1,sizeof(rec1));
f1:
i:=sizeof(res);
if (pipeLineRecv(BigNumPipelineId,res,i)<>0) then i:=0;
if (i<1) then begin;
  relequish;
  goto f1;
  end;
End;
