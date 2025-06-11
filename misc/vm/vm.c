#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define maxHandlers 16
#define terminationFlag 0x40000000
#define int8 char
#define int16 short int
#define int32 int
#define reg_a 1
#define reg_b 2
#define reg_c 3
#define reg_d 4
#define reg_src 5
#define reg_trg 6
#define reg_cip 7

typedef struct {
    int32 regs[8];
    int32 flags;                //1=above, 2=below, 4=equal
    char nam[255];
    char par[255];
    char *stackD;
    int32 stackS;
    int32 stackP;
    char *codeD;
    int32 codeS;
    char *dataD;
    int32 dataS;
    int32 dataP;
    int32 *procD;
    int32 procS;
    int32 handleD[maxHandlers];
    int32 handleN;
} oneEmulatorRecord;



int32 getConstant(oneEmulatorRecord *d,int32 n) {
    int32 i;
    i=0;
    memcpy(&i,d->codeD+d->regs[reg_cip],n);
    d->regs[reg_cip]+=n;
    return i;
}



void emuClear(oneEmulatorRecord *d) {
    memset((char*)d,0,sizeof(*d));
}



void emuFinish(oneEmulatorRecord *d) {
    if (d->dataD!=0) free(d->dataD);
    if (d->stackD!=0) free(d->stackD);
    if (d->procD!=0) free(d->procD);
    if (d->codeD!=0) free(d->codeD);
    emuClear(d);
}



void emuDump(oneEmulatorRecord *d) {
    printf("nam='%s'  par='%s'\n",d->nam,d->par);
    printf("a=%08x  b=%08x  c=%08x  d=%08x\n",d->regs[reg_a],d->regs[reg_b],d->regs[reg_c],d->regs[reg_d]);
    printf("src=%08x  trg=%08x  cip=%08x\n",d->regs[reg_src],d->regs[reg_trg],d->regs[reg_cip]);
}



void emuGetResult(oneEmulatorRecord *d,int32 i,char *b) {
    switch (i) {
    case 0:
        sprintf(b,"%i-running",i);
        break;
    case 1:
        sprintf(b,"%i-invalid syscall code",i);
        break;
    case 2:
        sprintf(b,"%i-invalid instruction code",i);
        break;
    case 3:
        sprintf(b,"%i-invalid stack reference",i);
        break;
    case 4:
        sprintf(b,"%i-division by zero",i);
        break;
    case 5:
        sprintf(b,"%i-invalid memory reference",i);
        break;
    case 6:
        sprintf(b,"%i-invalid subroutine reference",i);
        break;
    case 7:
        sprintf(b,"%i-invalid code reference",i);
        break;
    case 8:
        sprintf(b,"%i-invalid memory reference",i);
        break;
    default:
        sprintf(b,"%i-unknown error happened",i);
        break;
    }
    if (i&terminationFlag) sprintf(b,"0-successful, exitcode=%i",i&0xffff);
}



int32 emuStart(oneEmulatorRecord *d,char *n,char *p) {
    FILE *f;
    int32 i;

    emuClear(d);
    strcpy(d->nam,n);
    strcpy(d->par,p);
    f=fopen(d->nam,"rb");
    if (f==0) return 1;
    fseek(f,0,SEEK_END);
    d->codeS=ftell(f);
    fseek(f,0,SEEK_SET);
    if ((d->codeD=malloc(d->codeS))==0) return 1;
    if (fread(d->codeD,1,d->codeS,f)!=(unsigned)d->codeS) return 1;
    fclose(f);
    if (getConstant(d,4)!=0x30314d56) return 1;
    i=getConstant(d,4);
    d->stackS=getConstant(d,4);
    d->dataS=getConstant(d,4);
    d->procS=getConstant(d,4);
    d->regs[reg_cip]=getConstant(d,4);

    i=d->procS*4;
    if ((d->procD=malloc(i))==0) return 1;
    memset(d->procD,0x7f,i);
    if ((d->stackD=malloc(d->stackS))==0) return 1;
    if ((d->dataD=malloc(d->dataS))==0) return 1;

    return 0;
}



void emuExec_push(oneEmulatorRecord *d,int32 v,int32 s) {
    memcpy(d->stackD+d->stackP,&v,s);
    d->stackP+=s;
}



int32 emuExec_pop(oneEmulatorRecord *d,int32 s) {
    int32 v;
    v=0;
    d->stackP-=s;
    memcpy(&v,d->stackD+d->stackP,s);
    return v;
}



int32 emuExec_convType(int32 d,int32 sig,int32 siz,int32 after) {
    sig&=1;
    siz&=15;
    switch (siz) {
    case 1:
        d&=0xff;
        break;
    case 2:
        d&=0xffff;
        break;
    case 3:
        d&=0xffffffff;
        break;
    }
    if (sig) switch (siz) {
        case 1:
            d=(int8)d;
            break;
        case 2:
            d=(int16)d;
            break;
        case 3:
            d=(int32)d;
            break;
        }
    if (after) switch (siz) {
        case 1:
            d&=0xff;
            break;
        case 2:
            d&=0xffff;
            break;
        case 3:
            d&=0xffffffff;
            break;
        }
    return d;
}



int32 emuExec_getMemory(oneEmulatorRecord *d) {
    int32 i,o,p;
    i=getConstant(d,1);
    p=i&0x80;
    i=i&15;
    o=getConstant(d,4);
    i=d->regs[i];
    if (p==0) o+=i;
    else o-=i;
    return o;
}



int32 emuExec_convForm(int32 d,int32 frm,int32 siz) {
    frm&=15;
    siz&=15;
    if (frm==2) switch (siz) {
        case 1:
            d&=0xff;
            break;
        case 2:
            d=((d>>8)&0xff)|((d<<8)&0xff00);
            break;
        case 3:
            d=((d>>24)&0xff)|((d>>8)&0xff00)|((d<<8)&0xff0000)|(d<<24);
            break;
        }
    return d;
}



int32 emuExec_GetMovementSize(int32 siz) {
    siz&=15;
    switch (siz) {
    case 1:
        return 1;
        break;
    case 2:
        return 2;
        break;
    case 3:
        return 4;
        break;
    }
    return 0;
}



int32 emuExec_oneOpcode(oneEmulatorRecord *d) {
    int32 form;
    int32 siz1,siz2;
    int32 reg1,reg2;
    int32 val1,val2;

    siz1=getConstant(d,1);

    switch (siz1) {
    case 1:  //add
        siz1=getConstant(d,1);
        reg1=getConstant(d,1);
        if (siz1&0x80) val1=getConstant(d,4);
        else val1=d->regs[getConstant(d,1)];
        d->regs[reg1]+=val1;
        return 0;
        break;
    case 2:  //sub
        siz1=getConstant(d,1);
        reg1=getConstant(d,1);
        if (siz1&0x80) val1=getConstant(d,4);
        else val1=d->regs[getConstant(d,1)];
        d->regs[reg1]-=val1;
        return 0;
        break;
    case 3:  //mul
        siz1=getConstant(d,1);
        siz2=siz1>>6;
        reg1=getConstant(d,1);
        if (siz1&0x80) val1=getConstant(d,4);
        else val1=d->regs[getConstant(d,1)];
        val1=emuExec_convType(val1,siz2,siz1,0);
        val2=emuExec_convType(d->regs[reg1],siz2,siz1,0);
        d->regs[reg1]=val2*val1;
        return 0;
        break;
    case 4:  //div
        siz1=getConstant(d,1);
        siz2=siz1>>6;
        reg1=getConstant(d,1);
        if (siz1&0x80) val1=getConstant(d,4);
        else val1=d->regs[getConstant(d,1)];
        val1=emuExec_convType(val1,siz2,siz1,0);
        val2=emuExec_convType(d->regs[reg1],siz2,siz1,0);
        if (val1==0) return 4;
        d->regs[reg1]=val2/val1;
        return 0;
        break;
    case 5:  //mod
        siz1=getConstant(d,1);
        siz2=siz1>>6;
        reg1=getConstant(d,1);
        if (siz1&0x80) val1=getConstant(d,4);
        else val1=d->regs[getConstant(d,1)];
        val1=emuExec_convType(val1,siz2,siz1,0);
        val2=emuExec_convType(d->regs[reg1],siz2,siz1,0);
        if (val1==0) return 4;
        d->regs[reg1]=val2%val1;
        return 0;
        break;
    case 6:  //or
        siz1=getConstant(d,1);
        reg1=getConstant(d,1);
        if (siz1&0x80) val1=getConstant(d,4);
        else val1=d->regs[getConstant(d,1)];
        d->regs[reg1]|=val1;
        return 0;
        break;
    case 7:  //xor
        siz1=getConstant(d,1);
        reg1=getConstant(d,1);
        if (siz1&0x80) val1=getConstant(d,4);
        else val1=d->regs[getConstant(d,1)];
        d->regs[reg1]^=val1;
        return 0;
        break;
    case 8:  //and
        siz1=getConstant(d,1);
        reg1=getConstant(d,1);
        if (siz1&0x80) val1=getConstant(d,4);
        else val1=d->regs[getConstant(d,1)];
        d->regs[reg1]&=val1;
        return 0;
        break;
    case 9:  //not
        siz1=getConstant(d,1);
        reg1=getConstant(d,1);
        d->regs[reg1]=!d->regs[reg1];
        return 0;
        break;
    case 10:  //neg
        siz1=getConstant(d,1);
        reg1=getConstant(d,1);
        d->regs[reg1]=-d->regs[reg1];
        return 0;
        break;
    case 11:  //shl
        siz1=getConstant(d,1);
        reg1=getConstant(d,1);
        if (siz1&0x80) val1=getConstant(d,4);
        else val1=d->regs[getConstant(d,1)];
        d->regs[reg1]<<=val1;
        return 0;
        break;
    case 12:  //shr
        siz1=getConstant(d,1);
        reg1=getConstant(d,1);
        if (siz1&0x80) val1=getConstant(d,4);
        else val1=d->regs[getConstant(d,1)];
        d->regs[reg1]>>=val1;
        return 0;
        break;
    case 13:  //push
        siz1=getConstant(d,1);
        reg1=getConstant(d,1);
        siz1=emuExec_GetMovementSize(siz1);
        val1=d->regs[reg1];
        emuExec_push(d,val1,siz1);
        return 0;
        break;
    case 14:  //pop
        siz1=getConstant(d,1);
        reg1=getConstant(d,1);
        siz1=emuExec_GetMovementSize(siz1);
        d->regs[reg1]=emuExec_pop(d,siz1);
        return 0;
        break;
    case 15:  //comp
        siz1=getConstant(d,1);
        siz2=siz1>>6;
        reg1=getConstant(d,1);
        if (siz1&0x80) val1=getConstant(d,4);
        else val1=d->regs[getConstant(d,1)];
        val1=emuExec_convType(val1,siz2,siz1,0);
        val2=emuExec_convType(d->regs[reg1],siz2,siz1,0);
        reg1=0;
        if (val1<val2) reg1|=1;
        if (val1>val2) reg1|=2;
        if (val1==val2) reg1|=4;
        d->flags&=0xffffff8;
        d->flags|=reg1;
        return 0;
        break;
    case 16:  //move
        siz1=getConstant(d,1);
        siz2=getConstant(d,1);
        reg1=getConstant(d,1);
        if (siz2&0x80) val1=getConstant(d,4);
        else val1=d->regs[getConstant(d,1)];
        val1=emuExec_convType(val1,siz2>>6,siz2,0);
        val1=emuExec_convType(val1,siz1>>6,siz1,1);
        d->regs[reg1]=val1;
        return 0;
        break;
    case 17:  //movr
        form=getConstant(d,1);
        siz1=getConstant(d,1);
        siz2=getConstant(d,1);
        val2=emuExec_getMemory(d);
        reg1=getConstant(d,1);
        memcpy(&val1,d->dataD+val2,4);
        val1=emuExec_convForm(val1,form,siz2);
        val1=emuExec_convType(val1,siz2>>6,siz2,0);
        val1=emuExec_convType(val1,siz1>>6,siz1,1);
        d->regs[reg1]=val1;
        return 0;
        break;
    case 18:  //movw
        form=getConstant(d,1);
        siz1=getConstant(d,1);
        siz2=getConstant(d,1);
        val2=emuExec_getMemory(d);
        reg1=getConstant(d,1);
        val1=d->regs[reg1];
        val1=emuExec_convType(val1,siz2>>6,siz2,0);
        val1=emuExec_convType(val1,siz1>>6,siz1,1);
        val1=emuExec_convForm(val1,form,siz1);
        siz2=emuExec_GetMovementSize(siz1);
        memcpy(d->dataD+val2,&val1,siz2);
        return 0;
        break;
    case 19:  //call
        val1=getConstant(d,4);
        emuExec_push(d,d->regs[reg_cip],4);
        d->regs[reg_cip]=val1;
        return 0;
        break;
    case 20:  //ret
        d->regs[reg_cip]=emuExec_pop(d,4);
        return 0;
        break;
    case 21:  //jump
        d->regs[reg_cip]=getConstant(d,4);
        return 0;
        break;
    case 22:  //jmpc
        reg1=getConstant(d,1);
        val1=getConstant(d,4);
        if (d->flags&reg1) d->regs[reg_cip]=val1;
        return 0;
        break;
    case 23:  //addrLod
        val2=emuExec_getMemory(d);
        reg1=getConstant(d,1);
        memcpy(&val1,d->dataD+val2,4);
        d->regs[reg1]=val1;
        return 0;
        break;
    case 24:  //addrSav
        val2=emuExec_getMemory(d);
        reg1=getConstant(d,1);
        val1=d->regs[reg1];
        memcpy(d->dataD+val2,&val1,4);
        return 0;
        break;
    case 25:  //procAddr
        reg1=getConstant(d,1);
        val1=getConstant(d,4);
        if (val1==-1) {
            val2=emuExec_pop(d,4);
            emuExec_push(d,val2,4);
        } else val2=d->procD[val1];
        d->regs[reg1]=val2;
        return 0;
        break;
    case 26:  //procAllocBeg
        val1=getConstant(d,4);
        val2=d->procD[val1];
        emuExec_push(d,val2,4);
        val2=d->dataP;
        val1=getConstant(d,4);
        d->dataP+=val1;
        emuExec_push(d,val2,4);
        return 0;
        break;
    case 31:  //procAllocEnd
        val2=emuExec_pop(d,4);
        val1=getConstant(d,4);
        d->procD[val1]=val2;
        getConstant(d,4);
        return 0;
        break;
    case 27:  //procFree
        val1=getConstant(d,4);
        val2=emuExec_pop(d,4);
        d->procD[val1]=val2;
        val2=getConstant(d,4);
        d->dataP-=val2;
        return 0;
        break;
    case 28:  //codeOfs
        reg1=getConstant(d,1);
        d->regs[reg1]=getConstant(d,4);
        return 0;
        break;
    case 29:  //xchg
        siz1=getConstant(d,1);
        val2=emuExec_getMemory(d);
        reg1=getConstant(d,1);
        siz2=emuExec_GetMovementSize(siz1);
        val1=d->regs[reg1];
        memcpy(d->regs+reg1,d->dataD+val2,siz2);
        memcpy(d->dataD+val2,&val1,siz2);
        return 0;
        break;
    case 30:  //setc
        reg2=getConstant(d,1);
        siz1=getConstant(d,1);
        reg1=getConstant(d,1);
        if (d->flags&reg2) val1=1;
        else val1=0;
        d->regs[reg1]=val1;
        return 0;
        break;
    case 32:  //syscall
        siz1=getConstant(d,1);
        goto syscall;
        break;
    case 33:  //cllr
        reg1=getConstant(d,1);
        emuExec_push(d,d->regs[reg_cip],4);
        d->regs[reg_cip]=d->regs[reg1];
        return 0;
        break;
    case 34:  //jmpr
        reg1=getConstant(d,1);
        d->regs[reg_cip]=d->regs[reg1];
        return 0;
        break;
    default:
        d->regs[reg_cip]-=1;
        return 2;
        break;
    }

syscall:
    switch (siz1) {
    case 1:  //sleep
        return 0;
        break;
    case 2:  //memcopy
        memmove(d->dataD+d->regs[reg_trg],d->dataD+d->regs[reg_src],d->regs[reg_c]&0xffff);
        return 0;
        break;
    case 3:  //codecopy
        memcpy(d->dataD+d->regs[reg_trg],d->codeD+d->regs[reg_src],d->regs[reg_c]&0xffff);
        return 0;
        break;
    case 4:  //terminate
        return (d->regs[reg_a]&0xffff)|terminationFlag;
        break;
    case 5:  //console.write
        val1=d->regs[reg_src];
        for (val2=0; val2<d->regs[reg_c]; val2++) putchar(d->dataD[val1++]);
        return 0;
        break;
    case 44:  //memFillByte
        memset(d->dataD+d->regs[reg_trg],d->regs[reg_a],d->regs[reg_c]&0xffff);
        return 0;
        break;
    default:
        d->regs[reg_cip]-=2;
        return 1;
        break;
    }

}






int32 main(int32 argc,char *argv[]) {
    oneEmulatorRecord d;
    char buf[1024];
    int32 i;

    printf("Virtual Machine Emulator v1.0, done by Mc in 2002.\n");

    if (argc<2) {
        printf("parameters: <binary-file> [parameters]\n");
        return 1;
    }

    memset(buf,0,sizeof(buf));
    for (i=2; i<argc; i++) {
        strcat(buf," ");
        strcat(buf,argv[i]);
    }
    printf("executable: %s\n",argv[1]);
    printf("parameters: %s\n",&buf[1]);

    if (emuStart(&d,argv[1],&buf[1])!=0) {
        printf("error reading executable!\n");
        return 2;
    }

f1:
    i=emuExec_oneOpcode(&d);
    if (i==0) goto f1;
    printf("\n");
    emuGetResult(&d,i,buf);
    printf("finished: %s\n",buf);
    if (!(i&terminationFlag)) {
        emuDump(&d);
        emuFinish(&d);
        return 3;
    }
    emuFinish(&d);
    return 0;
}
