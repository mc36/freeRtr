{$heap 7k}
{$stack 1k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc memory.inc}
{$sysinc filesys.inc}

Type
  OneELFheaderRecord=record
    id:array[1..16] of byte;    {e_ident}
    typ:word;                   {e_type}
    mch:word;                   {e_machine}
    ver:longint;                {e_version}
    ntry:longint;               {e_entry}
    phof:longint;               {e_phoff}
    shof:longint;               {e_shoff}
    flg:longint;                {e_flags}
    ehs:word;                   {e_ehsize}
    phs:word;                   {e_phentsize}
    phn:word;                   {e_phnum}
    shs:word;                   {e_shentsize}
    shn:word;                   {e_shnum}
    sht:word;                   {e_shtrndx}
    end;
  OnePRGheaderRecord=record
    typ:longint;                {p_type}
    fof:longint;                {p_offset}
    vad:longint;                {p_vaddr}
    pad:longint;                {p_paddr}
    fsz:longint;                {p_filesz}
    msz:longint;                {p_memsz}
    flg:longint;                {p_flags}
    alg:longint;                {p_align}
    end;
  OneSECheaderRecord=record
    nam:LongInt;                {sh_name}
    typ:LongInt;                {sh_type}
    flg:LongInt;                {sh_flags}
    adr:LongInt;                {sh_addr}
    ofs:LongInt;                {sh_offset}
    siz:LongInt;                {sh_size}
    lnk:LongInt;                {sh_link}
    nfo:LongInt;                {sh_info}
    alg:LongInt;                {sh_addralign}
    ens:LongInt;                {sh_entsize}
    end;
Var
  msb:Boolean;
  data:longInt;
  cpu:LongInt;
  entry:LongInt;


Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Procedure putW(var buf;dat:Word);
Begin;
if msb then writeWordMSB(buf,dat) else writeWordLSB(buf,dat);
End;

Procedure putL(var buf;dat:longInt);
Begin;
if msb then writeLongMSB(buf,dat) else writeLongLSB(buf,dat);
End;

Var
  eh:OneELFheaderRecord;
  ph:OnePRGheaderRecord;
  sh:OneSECheaderRecord;
  f:xFile;
  i,o:LongInt;
  a:String;
BEGIN;
WriteLn('elf header creator v1.0, done by Mc at '#%date' '#%time'.');
a:=paramStr(1);
if (a='') then immErr('using: mkelf <code> <msb/lsb> <entry> <cpu> <data>');
if (paramStr(2)='msb') then msb:=true else msb:=false;
entry:=BVal(paramStr(3));
cpu:=BVal(paramStr(4));
data:=BVal(paramStr(5));
if (xOpen(f,a,xGenFilMod_rw)<>0) then immErr('error opening file!');
fillchar(eh,sizeof(eh),0);
fillchar(ph,sizeof(ph),0);
fillchar(sh,sizeof(sh),0);
eh.id[1]:=$7f;
eh.id[2]:=$45;
eh.id[3]:=$4c;
eh.id[4]:=$46;
eh.id[5]:=1;
if msb then i:=2 else i:=1;
eh.id[6]:=i;
eh.id[7]:=1;
putW(eh.typ,2);
putW(eh.mch,cpu);
putL(eh.ver,1);
putL(eh.ntry,entry);
putL(eh.phof,sizeof(eh));
putL(eh.shof,sizeof(eh)+sizeof(ph));
putL(eh.flg,$20000001);
putW(eh.ehs,sizeof(eh));
putW(eh.phs,sizeof(ph));
putW(eh.phn,1);
putW(eh.shs,sizeof(sh));
putW(eh.shn,1);
putW(eh.sht,0);

putL(ph.typ,1);
putL(ph.fof,sizeof(eh)+sizeof(ph)+sizeof(sh));
putL(ph.vad,entry);
putL(ph.pad,entry);
putL(ph.fsz,xFileSize(f)-sizeof(eh)-sizeof(ph)-sizeof(sh));
putL(ph.msz,xfileSize(f)+data);
putL(ph.flg,7);
putL(ph.alg,$1000);

putL(sh.nam,0);
putL(sh.typ,1);
putL(sh.flg,7);
putL(sh.adr,entry);
putL(sh.ofs,sizeof(eh)+sizeof(ph)+sizeof(sh));
putL(sh.siz,xFileSize(f)-sizeof(eh)-sizeof(ph)-sizeof(sh));
putL(sh.lnk,0);
putL(sh.nfo,0);
putL(sh.alg,$1000);
putL(sh.ens,0);

xBlockWrite(f,eh,sizeof(eh));
xBlockWrite(f,ph,sizeof(ph));
xBlockWrite(f,sh,sizeof(sh));
xClose(f);
WriteLn('successful!');
END.