#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <termios.h>
#include <fcntl.h>
#include <sys/ioctl.h>

#define BANG_CK (TIOCM_DTR | TIOCM_RTS)

char *ttyName;
int addrTty;
int spedRx;
int spedTx;

void err(char*buf) {
    printf("%s\n", buf);
    _exit(1);
}

void doTxLoop() {
    int bit = 0;
    int cur;
    if (ioctl(addrTty, TIOCMSET, &cur) < 0) err("error getting state");
    for (;;) {
        usleep(spedRx);
        bit ^= 1;
        if (bit == 0) {
            cur |= BANG_CK;
        } else {
            cur &= ~BANG_CK;
        }
        if (ioctl(addrTty, TIOCMSET, &cur) < 0) err("error setting state");
    }
}

int main(int argc, char **argv) {

    if (argc < 3) {
        printf("using: %s <tty> <speed>\n", argv[0]);
        _exit(1);
    }

    spedTx = atoi(argv[2]);
    if (spedTx < 1) err("invalid speed");
    spedTx = 1000000 / spedTx;
    spedRx = spedTx / 2;
    if (spedRx < 1) err("bad speed");

    ttyName = malloc(strlen(argv[1]) + 1);
    if (ttyName == NULL) err("error allocating memory");
    strcpy(ttyName, argv[1]);
    printf("opening tty %s.\n", ttyName);

    if ((addrTty = open(ttyName, O_RDWR)) < 0) err("unable to open file");

    setgid(1);
    setuid(1);
    printf("serving others\n");

    doTxLoop();
}
