#ifndef HAVE_NOCRYPTO

#include <openssl/conf.h>
#if OPENSSL_VERSION_NUMBER >= 0x30000000L
#include <openssl/provider.h>
#endif
#include <openssl/evp.h>
#include <openssl/rand.h>

#endif

struct packetContext {
    int sgt;
    unsigned char *bufA;
    unsigned char *bufB;
    unsigned char *bufC;
    unsigned char *bufD;
    unsigned char *bufH;
#ifndef HAVE_NOCRYPTO
    EVP_CIPHER_CTX *encr;
    EVP_MD_CTX *dgst;
#endif
};

#define maxPorts 128
#define preBuff 512
#define minBuff 128
#define maxBuff 1024
#define totBuff 16384

extern int dataPorts;
extern int cpuPort;
extern char *ifaceName[maxPorts];

void err(char*buf);
void sendPack(unsigned char *bufD, int bufS, int port);
void setMtu(int port, int mtu);
void setState(int port, int sta);
int getState(int port);
void getStats(int port, unsigned char*buf, unsigned char*pre, int*len);

extern void initIface(int port, char *name);
extern int initTables();
extern int hashDataPacket(unsigned char *bufP);
extern void processDataPacket(struct packetContext *ctx, int bufS, int port, int prt);
extern void processCpuPack(struct packetContext *ctx, int bufS);
extern int doOneCommand(struct packetContext *ctx, unsigned char* buf);
extern void doStatRound(FILE *commands, int round);
extern int doConsoleCommand(unsigned char*buf);
extern int initContext(struct packetContext *ctx);
extern char* getCapas();

#define platformBase "p4emu/"
