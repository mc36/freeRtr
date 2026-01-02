#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <asm/ioctls.h>
#include <asm/termios.h>
#include <sys/mman.h>

unsigned char *sharedMem = NULL;

int myVal;

#define BANG_CK TIOCM_CD
#define BANG_RX TIOCM_RI
#define BANG_TX TIOCM_DTR

char*nams[] = {"clock", "iface1", "iface2", "unknown"};
int sets[] = {0x01, 0x02, 0x04, 0x00};
int clrs[] = {0xfe, 0xfd, 0xfb, 0xff};
int gets[] = {0x00, 0x04, 0x02, 0x00};

int ioctl(int fd, unsigned long op, unsigned int*par) {
    if (!sharedMem) {
        ftruncate(fd, 2);
        sharedMem =  mmap(NULL, 2, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
        if (sharedMem == NULL) return -1;
        myVal = (sharedMem[1]++) & 3;
        printf("syncemu initialized in slot %i - %s\n",myVal, nams[myVal]);
    }
    switch (op) {
    case TIOCMSET:
        if ((*par & BANG_TX) == 0) {
            sharedMem[0] &= clrs[myVal];
        } else {
            sharedMem[0] |= sets[myVal];
        }
        return 0;
    case TIOCMGET:
        *par = 0;
        if ((sharedMem[0] & 1) != 0) *par |= BANG_CK;
        if ((sharedMem[0] & gets[myVal]) != 0) *par |= BANG_RX;
        return 0;
    default:
        return -1;
    }
}
