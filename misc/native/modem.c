#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <pthread.h>
#include <termios.h>
#include <fcntl.h>
#include <sys/ioctl.h>

char *ttyName;
int addrTty;
int debug;
int delay;
int retry;
char* readDat;
int readLen;
int readStat; // 0=reading, 1=finished
pthread_t threadRead;

void err(char*buf) {
    printf("%s\n", buf);
    exit(1);
}

void doReadLoop() {
    char buf[1];
    int i;
    for (;;) {
        if (readStat != 0) {
            usleep(10000);
            continue;
        }
        readLen = 0;
        for (;;) {
            if (readLen > 768) break;
            i = read(addrTty, buf, 1);
            if (i != 1) err("read thread exited");
            i = buf[0] & 0xff;
            if (i == 13) break;
            if (i == 10) break;
            readDat[readLen] = i;
            readLen++;
            readDat[readLen] = 0;
        }
        if (readLen < 1) continue;
        readStat = 1;
    }
}

int wait4line() {
    int sec = delay;
    for (;;) {
        if (readStat != 0) return 1;
        if (sec < 0) return 0;
        sec--;
        usleep(1000);
    }
}

int doOneCmd(char *cmdFul) {
    if (debug) printf("cmd: %s\n", cmdFul);
    int cmdNum = 0;
    char* cmdPar[32];
    char cmdEmp[1];
    int i, o;
    cmdEmp[0] = 0;
    for (i = 0; i < 32; i++) cmdPar[i] = (char*) &cmdEmp;
    for (i = 0;; i++) {
        if (cmdFul[i] == 0) break;
        if (cmdFul[i] != 32) continue;
        cmdFul[i] = 0;
        cmdPar[cmdNum] = cmdFul + i + 1;
        cmdNum++;
    }
    if (cmdFul[0] == '!') return 0;
    if (strcmp(cmdFul, "sleep") == 0) {
        i = atoi(cmdPar[0]);
        if (i < 1) i = 1;
        sleep(i);
        return 0;
    }
    if (strcmp(cmdFul, "debug") == 0) {
        debug = atoi(cmdPar[0]) & 1;
        return 0;
    }
    if (strcmp(cmdFul, "delay") == 0) {
        delay = atoi(cmdPar[0]);
        return 0;
    }
    if (strcmp(cmdFul, "retry") == 0) {
        retry = atoi(cmdPar[0]);
        return 0;
    }
    if (strcmp(cmdFul, "echo") == 0) {
        for (i = 0; i < cmdNum; i++) printf("%s ", cmdPar[i]);
        printf("\n");
        return 0;
    }
    if (strcmp(cmdFul, "script") == 0) {
        FILE *fr;
        fr = fopen(cmdPar[0], "r");
        if (fr == NULL) return 1;
        char ln[512];
        o = 0;
        for (;;) {
            if (fgets((char*) &ln, 512, fr) == NULL) break;
            i = strlen(ln);
            if (i < 2) continue;
            ln[i - 1] = 0;
            o |= doOneCmd(ln);
            if (o != 0) break;
        }
        fclose(fr);
        return o;
    }
    if (strcmp(cmdFul, "ctrlget") == 0) {
        ioctl(addrTty, TIOCMGET, &i);
        printf("dtr=%s ", i & TIOCM_DTR ? "1" : "0");
        printf("dsr=%s ", i & TIOCM_DSR ? "1" : "0");
        printf("rts=%s ", i & TIOCM_RTS ? "1" : "0");
        printf("cts=%s ", i & TIOCM_CTS ? "1" : "0");
        printf("cd=%s ", i & TIOCM_CD ? "1" : "0");
        printf("ri=%s ", i & TIOCM_RI ? "1" : "0");
        printf("\n");
        return 0;
    }
    if (strcmp(cmdFul, "ctrlset") == 0) {
        o = atoi(cmdPar[0]);
        ioctl(addrTty, TIOCMGET, &i);
        i &= ~(TIOCM_DTR | TIOCM_RTS);
        if (o & 1) i |= TIOCM_DTR;
        if (o & 2) i |= TIOCM_RTS;
        ioctl(addrTty, TIOCMSET, &i);
        return 0;
    }
    if (strcmp(cmdFul, "speedset") == 0) {
        o = atoi(cmdPar[0]);
        i = 0;
        if (o == 1200) i = B1200;
        if (o == 2400) i = B2400;
        if (o == 4800) i = B4800;
        if (o == 9600) i = B9600;
        if (o == 19200) i = B19200;
        if (o == 38400) i = B38400;
        if (o == 57600) i = B57600;
        if (o == 115200) i = B115200;
        if (i == 0) return 1;
        struct termios term;
        memset(&term, 0, sizeof (term));
        term.c_cflag = CREAD | CS8 | CLOCAL;
        term.c_cc[VMIN] = 1;
        cfsetispeed(&term, i);
        cfsetospeed(&term, i);
        tcsetattr(addrTty, TCSANOW, &term);
        return 0;
    }
    if (strcmp(cmdFul, "sendline") == 0) {
        char buf[1];
        buf[0] = 32;
        for (i = 0; i < cmdNum; i++) {
            if (i > 0) write(addrTty, buf, 1);
            write(addrTty, cmdPar[i], strlen(cmdPar[i]));
        }
        buf[0] = 13;
        write(addrTty, buf, 1);
        return 0;
    }
    if (strcmp(cmdFul, "flush") == 0) {
        o = atoi(cmdPar[0]);
        for (;;) {
            i = wait4line();
            if (i == 0) return 0;
            printf("%s\n", readDat);
            readStat = 0;
        }
        return 0;
    }
    if (strcmp(cmdFul, "readline") == 0) {
        o = atoi(cmdPar[0]);
        for (; o > 0; o--) {
            i = wait4line();
            if (i == 0) return 1;
            printf("%s\n", readDat);
            readStat = 0;
        }
        return 0;
    }
    if (strcmp(cmdFul, "readuntil") == 0) {
        for (;;) {
            i = wait4line();
            if (i == 0) return 1;
            printf("%s\n", readDat);
            o = 0;
            for (i = 0; i < cmdNum; i++) {
                if (strncmp(readDat, cmdPar[i], strlen(cmdPar[i])) == 0) o = 1;
            }
            readStat = 0;
            if (o == 1) return 0;
        }
    }

    return 1;
}

int main(int argc, char **argv) {

    if (argc < 3) {
        printf("using: %s <tty> <command> [command]\n", argv[0]);
        printf("commands: sleep x     - wait x seconds\n");
        printf("          delay x     - set read delay to x ms\n");
        printf("          debug x     - set debug 0=off, 1=on\n");
        printf("          retry x     - set retry 0=off, 1=on\n");
        printf("          echo x      - print string x\n");
        printf("          script x    - execute commands from file x\n");
        printf("          ctrlget     - get control lines\n");
        printf("          ctrlset x   - set control lines bit0=dtr, bit1=rts\n");
        printf("          speedset x  - set speed to x\n");
        printf("          sendline x  - send line x\n");
        printf("          flush       - wait for silence\n");
        printf("          readline x  - read x line(s)\n");
        printf("          readuntil x - read until x received\n");
        exit(1);
    }

    ttyName = malloc(1024);
    strcpy(ttyName, argv[1]);
    printf("opening tty %s.\n", ttyName);
    if ((addrTty = open(ttyName, O_RDWR)) < 0) err("unable to open file");

    debug = 0;
    retry = 0;
    delay = 1000;
    readDat = malloc(1024);
    readLen = 0;
    readStat = 0;
    if (pthread_create(&threadRead, NULL, (void*) & doReadLoop, NULL)) err("error creating read thread");

    int i, o;
    for (i = 2; i < argc; i++) {
        char buf[1024];
        strcpy(buf, argv[i]);
        o = doOneCmd(buf);
        if (o == 0) continue;
        retry--;
        if (retry < 0) err("error!");
        printf("\nerror, %i tries left.\n\n", retry);
        i = 2;
    }

    printf("successfully finished.\n");
    exit(0);
}
