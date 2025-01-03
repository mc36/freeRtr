#ifndef HAVE_NOCRYPTO

#include <openssl/conf.h>
#if OPENSSL_VERSION_NUMBER >= 0x30000000L
#include <openssl/provider.h>
#endif
#include <openssl/evp.h>
#include <openssl/rand.h>

#endif

struct ifaceStat_entry {
    long int byteRx;
    long int packRx;
    long int byteTx;
    long int packTx;
    long int byteDr;
    long int packDr;
    long int byteMpls;
    long int packMpls;
    long int byteVlan;
    long int packVlan;
    long int byteIpv4;
    long int packIpv4;
    long int byteIpv6;
    long int packIpv6;
    long int bytePppoe;
    long int packPppoe;
    long int byteBridge;
    long int packBridge;
    long int bytePolka;
    long int packPolka;
    long int byteNsh;
    long int packNsh;
};

struct packetContext {
    int port;
    struct ifaceStat_entry *stat;
    int hash;
    int sgt;
    unsigned char *bufB3;
    unsigned char *bufB2;
    unsigned char *bufB1;
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

extern int commandSock;
extern int dataPorts;
extern int cpuPort;
extern char *ifaceName[maxPorts];
extern struct ifaceStat_entry *ifaceStat[maxPorts];

void err(char*buf);
void sendPack(unsigned char *bufD, int bufS, int port);
void setMtu(int port, int mtu);
void setState(int port, int sta);
int getState(int port);
void getStats(int port, unsigned char*buf, unsigned char*pre, int*len);

extern void initIface(int port, char *name);
extern int initTables();
extern int hashDataPacket(unsigned char *bufP);
extern void processDataPacket(struct packetContext *ctx, int bufS, int prt);
extern void processCpuPack(struct packetContext *ctx, int bufS);
extern int doOneCommand(struct packetContext *ctx, unsigned char* buf);
extern int initContext(struct packetContext *ctx);
extern void doNegotiate(char*name);
extern void doSockLoop();
extern void doStatLoop();
extern void doMainLoop();
