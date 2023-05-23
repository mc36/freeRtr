import java.io.*;

class oneVMemulator {

private static int reg_a=1;
private static int reg_b=2;
private static int reg_c=3;
private static int reg_d=4;
private static int reg_src=5;
private static int reg_trg=6;
private static int reg_cip=7;
public static int terminationFlag=0x40000000;
private int[] regs;
private int flags;                //1=above, 2=below, 4=equal
private String nam;
private String par;
private byte[] stackD;
private int stackS;
private int stackP;
private byte[] codeD;
private int codeS;
private byte[] dataD;
private int dataS;
private int dataP;
private int[] procD;
private int procS;


public void emulator() {}
public String toString() {  return "emulating "+nam;  }


private int getMovementSize(int siz) {
siz&=15;
switch (siz) {
  case 1: return 1;
  case 2: return 2;
  case 3: return 4;
  }
return 0;
}


private int readOne(byte[] data,int ofs,int siz,int msb) throws Exception {
siz&=15;
switch (siz) {
  case 1: return ((int)(data[ofs]))&0xff;
  case 2:
    switch (msb) {
      case 1:
      case 3:
        return ((((int)data[ofs+1])&0xff)<<8) | (((int)data[ofs])&0xff);
      case 2:
        return ((((int)data[ofs])&0xff)<<8) | (((int)data[ofs+1])&0xff);
      }
    break;
  case 3:
    switch (msb) {
      case 1:
      case 3:
        return ((((int)data[ofs+3])&0xff)<<24) | ((((int)data[ofs+2])&0xff)<<16) | ((((int)data[ofs+1])&0xff)<<8) | (((int)data[ofs])&0xff);
      case 2:
        return ((((int)data[ofs])&0xff)<<24) | ((((int)data[ofs+1])&0xff)<<16) | ((((int)data[ofs+2])&0xff)<<8) | (((int)data[ofs+3])&0xff);
      }
    break;
  }
throw new Exception();
}


private void writeOne(byte[] data,int ofs,int siz,int msb,int val) throws Exception {
siz&=15;
switch (siz) {
  case 1:
    data[ofs]=(byte)(val&0xff);
    return;
  case 2:
    switch (msb) {
      case 1:
      case 3:
        data[ofs]=(byte)(val&0xff);
        data[ofs+1]=(byte)((val>>8)&0xff);
        return;
      case 2:
        data[ofs+1]=(byte)(val&0xff);
        data[ofs]=(byte)((val>>8)&0xff);
        return;
      }
    break;
  case 3:
    switch (msb) {
      case 1:
      case 3:
        data[ofs]=(byte)(val&0xff);
        data[ofs+1]=(byte)((val>>8)&0xff);
        data[ofs+2]=(byte)((val>>16)&0xff);
        data[ofs+3]=(byte)((val>>24)&0xff);
        return;
      case 2:
        data[ofs+3]=(byte)(val&0xff);
        data[ofs+2]=(byte)((val>>8)&0xff);
        data[ofs+1]=(byte)((val>>16)&0xff);
        data[ofs]=(byte)((val>>24)&0xff);
        return;
      }
    break;
  }
throw new Exception();
}


private int getConstant(int siz) throws Exception {
int i=readOne(codeD,regs[reg_cip],siz,1);
regs[reg_cip]+=getMovementSize(siz);
return i;
}


private void pushOne(int val,int siz) throws Exception {
writeOne(stackD,stackP,siz,1,val);
stackP+=getMovementSize(siz);
}


private int popOne(int siz) throws Exception {
stackP-=getMovementSize(siz);
return readOne(stackD,stackP,siz,1);
}


private int convType(int d,int sig,int siz,int after) {
sig&=1;
siz&=15;
switch (siz) {
  case 1: d&=0xff; break;
  case 2: d&=0xffff; break;
  case 3: d&=0xffffffff; break;
  }
if (sig!=0) switch (siz) {
  case 1: d=(byte)d; break;
  case 2: d=(short)d; break;
  case 3: d=(int)d; break;
  }
if (after!=0) switch (siz) {
  case 1: d&=0xff; break;
  case 2: d&=0xffff; break;
  case 3: d&=0xffffffff; break;
  }
return d;
}


private int getMemory() throws Exception {
int i,o,p;
i=getConstant(1);
p=i&0x80;
i=i&15;
o=getConstant(3);
i=regs[i];
if (p==0) o+=i; else o-=i;
return o;
}


public void load(String name,String param) throws Exception {
RandomAccessFile f=new RandomAccessFile(name,"r");
regs=new int[8];
nam=name;
par=param;
codeS=(int)f.length();
codeD=new byte[codeS];
f.read(codeD,0,codeS);
f.close();
if (getConstant(3)!=0x30314d56) throw new Exception();
int i=getConstant(3);
stackS=getConstant(3);
dataS=getConstant(3);
procS=getConstant(3);
regs[reg_cip]=getConstant(3);
procD=new int [procS];
stackD=new byte[stackS];
dataD=new byte[dataS];
}


public void dump() {
System.out.println("nam='"+nam+"'  par='"+par+"'");
System.out.println("a="+regs[reg_a]+"  b="+regs[reg_b]+"  c="+regs[reg_c]+"  d="+regs[reg_d]);
System.out.println("src="+regs[reg_src]+"  trg="+regs[reg_trg]+"  cip="+regs[reg_cip]);
}


public String getResult(int i) {
String a;
switch (i) {
  case 0:
    a="running";
    break;
  case 1:
    a="invalid syscall code";
    break;
  case 2:
    a="invalid instruction code";
    break;
  case 3:
    a="invalid stack reference";
    break;
  case 4:
    a="division by zero";
    break;
  case 5:
    a="invalid memory reference";
    break;
  case 6:
    a="invalid subroutine reference";
    break;
  case 7:
    a="invalid code reference";
    break;
  case 8:
    a="invalid memory reference";
    break;
  default:
    a="unknown error happened";
    break;
  }
a=i+"-"+a;
if ((i&terminationFlag)!=0) a="0-successful, exitcode="+(i&0xffff);
return a;
}


public int oneOpcode() throws Exception {
int form;
int siz1,siz2;
int reg1,reg2;
int val1,val2;

siz1=getConstant(1);
switch (siz1) {
  case 1:  //add
    siz1=getConstant(1);
    reg1=getConstant(1);
    if ((siz1&0x80)!=0) val1=getConstant(3); else val1=regs[getConstant(1)];
    regs[reg1]+=val1;
    return 0;
  case 2:  //sub
    siz1=getConstant(1);
    reg1=getConstant(1);
    if ((siz1&0x80)!=0) val1=getConstant(3); else val1=regs[getConstant(1)];
    regs[reg1]-=val1;
    return 0;
  case 3:  //mul
    siz1=getConstant(1);
    siz2=siz1>>6;
    reg1=getConstant(1);
    if ((siz1&0x80)!=0) val1=getConstant(3); else val1=regs[getConstant(1)];
    val1=convType(val1,siz2,siz1,0);
    val2=convType(regs[reg1],siz2,siz1,0);
    regs[reg1]=val2*val1;
    return 0;
  case 4:  //div
    siz1=getConstant(1);
    siz2=siz1>>6;
    reg1=getConstant(1);
    if ((siz1&0x80)!=0) val1=getConstant(3); else val1=regs[getConstant(1)];
    val1=convType(val1,siz2,siz1,0);
    val2=convType(regs[reg1],siz2,siz1,0);
    if (val1==0) return 4;
    regs[reg1]=val2/val1;
    return 0;
  case 5:  //mod
    siz1=getConstant(1);
    siz2=siz1>>6;
    reg1=getConstant(1);
    if ((siz1&0x80)!=0) val1=getConstant(3); else val1=regs[getConstant(1)];
    val1=convType(val1,siz2,siz1,0);
    val2=convType(regs[reg1],siz2,siz1,0);
    if (val1==0) return 4;
    regs[reg1]=val2%val1;
    return 0;
  case 6:  //or
    siz1=getConstant(1);
    reg1=getConstant(1);
    if ((siz1&0x80)!=0) val1=getConstant(3); else val1=regs[getConstant(1)];
    regs[reg1]|=val1;
    return 0;
  case 7:  //xor
    siz1=getConstant(1);
    reg1=getConstant(1);
    if ((siz1&0x80)!=0) val1=getConstant(3); else val1=regs[getConstant(1)];
    regs[reg1]^=val1;
    return 0;
  case 8:  //and
    siz1=getConstant(1);
    reg1=getConstant(1);
    if ((siz1&0x80)!=0) val1=getConstant(3); else val1=regs[getConstant(1)];
    regs[reg1]&=val1;
    return 0;
  case 9:  //not
    siz1=getConstant(1);
    reg1=getConstant(1);
    regs[reg1]=-1-regs[reg1];
    return 0;
  case 10:  //neg
    siz1=getConstant(1);
    reg1=getConstant(1);
    regs[reg1]=-regs[reg1];
    return 0;
  case 11:  //shl
    siz1=getConstant(1);
    reg1=getConstant(1);
    if ((siz1&0x80)!=0) val1=getConstant(3); else val1=regs[getConstant(1)];
    regs[reg1]<<=val1;
    return 0;
  case 12:  //shr
    siz1=getConstant(1);
    reg1=getConstant(1);
    if ((siz1&0x80)!=0) val1=getConstant(3); else val1=regs[getConstant(1)];
    regs[reg1]>>=val1;
    return 0;
  case 13:  //push
    siz1=getConstant(1);
    reg1=getConstant(1);
    val1=regs[reg1];
    pushOne(val1,siz1);
    return 0;
  case 14:  //pop
    siz1=getConstant(1);
    reg1=getConstant(1);
    regs[reg1]=popOne(siz1);
    return 0;
  case 15:  //comp
    siz1=getConstant(1);
    siz2=siz1>>6;
    reg1=getConstant(1);
    if ((siz1&0x80)!=0) val1=getConstant(3); else val1=regs[getConstant(1)];
    val1=convType(val1,siz2,siz1,0);
    val2=convType(regs[reg1],siz2,siz1,0);
    reg1=0;
    if (val1<val2) reg1|=1;
    if (val1>val2) reg1|=2;
    if (val1==val2) reg1|=4;
    flags&=0xffffff8;
    flags|=reg1;
    return 0;
  case 16:  //move
    siz1=getConstant(1);
    siz2=getConstant(1);
    reg1=getConstant(1);
    if ((siz2&0x80)!=0) val1=getConstant(3); else val1=regs[getConstant(1)];
    val1=convType(val1,siz2>>6,siz2,0);
    val1=convType(val1,siz1>>6,siz1,1);
    regs[reg1]=val1;
    return 0;
  case 17:  //movr
    form=getConstant(1);
    siz1=getConstant(1);
    siz2=getConstant(1);
    val2=getMemory();
    reg1=getConstant(1);
    val1=readOne(dataD,val2,siz2,form);
    val1=convType(val1,siz2>>6,siz2,0);
    val1=convType(val1,siz1>>6,siz1,1);
    regs[reg1]=val1;
    return 0;
  case 18:  //movw
    form=getConstant(1);
    siz1=getConstant(1);
    siz2=getConstant(1);
    val2=getMemory();
    reg1=getConstant(1);
    val1=regs[reg1];
    val1=convType(val1,siz2>>6,siz2,0);
    val1=convType(val1,siz1>>6,siz1,1);
    writeOne(dataD,val2,siz1,form,val1);
    return 0;
  case 19:  //call
    val1=getConstant(3);
    pushOne(regs[reg_cip],3);
    regs[reg_cip]=val1;
    return 0;
  case 20:  //ret
    regs[reg_cip]=popOne(3);
    return 0;
  case 21:  //jump
    regs[reg_cip]=getConstant(3);
    return 0;
  case 22:  //jmpc
    reg1=getConstant(1);
    val1=getConstant(3);
    if ((flags&reg1)!=0) regs[reg_cip]=val1;
    return 0;
  case 23:  //addrLod
    val2=getMemory();
    reg1=getConstant(1);
    regs[reg1]=readOne(dataD,val2,3,1);
    return 0;
  case 24:  //addrSav
    val2=getMemory();
    reg1=getConstant(1);
    writeOne(dataD,val2,3,1,regs[reg1]);
    return 0;
  case 25:  //procAddr
    reg1=getConstant(1);
    val1=getConstant(3);
    if (val1==-1) {
      val2=popOne(3);
      pushOne(val2,3);
      } else val2=procD[val1];
    regs[reg1]=val2;
    return 0;
  case 26:  //procAllocBeg
    val1=getConstant(3);
    val2=procD[val1];
    pushOne(val2,3);
    val2=dataP;
    val1=getConstant(3);
    dataP+=val1;
    pushOne(val2,3);
    return 0;
  case 31:  //procAllocEnd
    val2=popOne(3);
    val1=getConstant(3);
    procD[val1]=val2;
    getConstant(3);
    return 0;
  case 27:  //procFree
    val1=getConstant(3);
    val2=popOne(3);
    procD[val1]=val2;
    val2=getConstant(3);
    dataP-=val2;
    return 0;
  case 28:  //codeOfs
    reg1=getConstant(1);
    regs[reg1]=getConstant(3);
    return 0;
  case 29:  //xchg
    siz1=getConstant(1);
    val2=getMemory();
    reg1=getConstant(1);
    val1=regs[reg1];
    regs[reg1]=readOne(dataD,val2,siz1,1);
    writeOne(dataD,val2,siz1,1,val1);
    return 0;
  case 30:  //setc
    reg2=getConstant(1);
    siz1=getConstant(1);
    reg1=getConstant(1);
    if ((flags&reg2)!=0) val1=1; else val1=0;
    regs[reg1]=val1;
    return 0;
  case 32:  //syscall
    siz1=getConstant(1);
    switch (siz1) {
      case 1:  //sleep
        return 0;
      case 2:  //memcopy
        for (val1=0;val1<regs[reg_c&0xffff];val1++) dataD[regs[reg_trg]+val1]=dataD[regs[reg_src]+val1];
        return 0;
      case 3:  //codecopy
        for (val1=0;val1<regs[reg_c&0xffff];val1++) dataD[regs[reg_trg]+val1]=codeD[regs[reg_src]+val1];
        return 0;
      case 4:  //terminate
        return (regs[reg_a]&0xffff)|terminationFlag;
      case 5:  //console.write
        val1=regs[reg_src];
        for (val2=0;val2<regs[reg_c];val2++) System.out.write((int)dataD[val1++]);
        return 0;
      case 44:  //memFillByte
        for (val1=0;val1<regs[reg_c&0xffff];val1++) dataD[regs[reg_trg]+val1]=(byte)(regs[reg_a]&0xff);
        return 0;
      default:
        regs[reg_cip]-=2;
        return 1;
      }
  case 33:  //cllr
    reg1=getConstant(1);
    pushOne(regs[reg_cip],3);
    regs[reg_cip]=regs[reg1];
    return 0;
  case 34:  //jmpr
    reg1=getConstant(1);
    regs[reg_cip]=regs[reg1];
    return 0;
  default:
    regs[reg_cip]-=1;
    return 2;
  }
}


}













public class emulator{
public static void main(String[] args) throws Exception {
System.out.println("Virtual Machine Emulator v1.0, done by Mc in 2002.");
oneVMemulator emu=new oneVMemulator();
String a="";
int i;
for (i=1;i<args.length;i++) a=a+" "+args[i];
if (a.length()>0) a=a.substring(1);
if (args.length<1) {
  System.out.println("parameters: <binary-file> [parameters]");
  return;
  }
System.out.println("executable: "+args[0]);
System.out.println("parameters: "+a);
emu.load(args[0],a);
do {
  i=emu.oneOpcode();
  } while (i==0);
System.out.println("");
a=emu.getResult(i);
System.out.println("finished: "+a);
if ((i&emu.terminationFlag)==0) emu.dump();
}
}
