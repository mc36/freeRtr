Var
  DNSprocessId:LongInt;
  DNSpipelineId:LongInt;

Function DNSstartResolver:Boolean;
Begin;
pipeLineClose(DNSpipelineId);
DNSprocessId:=BugOS_findProcNam('dns.code');
if (pipeLineCreate(DNSpipelineId,DNSprocessId,1024,true)<>0) then DNSpipelineId:=0;
DNSstartResolver:=(DNSpipelineId=0);
End;

Procedure DNSstopResolver;
Begin;
pipeLineClose(DNSpipelineId);
DNSpipelineId:=0;
End;

{type: 1=name-->addr, 2=addr-->name, 3=name-->mailer}
Procedure DNSresolvePut(typ:Byte;var dat);
Var rec:record
  typ:LongInt;
  dat:string;
  end;
Begin;
rec.typ:=typ;
move(dat,rec.dat,256);
pipeLineSend(DNSpipelineId,rec,sizeof(rec));
End;

{type: $80=failed, &$7f=original type}
Function DNSresolveGet(var nam,dat):LongInt;
Label f1;
Var rec:record
  typ:LongInt;
  nam:string;
  dat:string;
  end;
  i:LongInt;
Begin;
DNSresolveGet:=0;
i:=sizeof(rec);
if (pipeLineRecv(DNSpipelineId,rec,i)<>0) then exit;
if (i<>sizeof(rec)) then exit;
move(rec.nam,nam,256);
move(rec.dat,dat,256);
DNSresolveGet:=rec.typ;
End;
