#ifndef HAVE_NOCRYPTO

#include <openssl/conf.h>
#if OPENSSL_VERSION_NUMBER >= 0x30000000L
#include <openssl/provider.h>
#endif
#include <openssl/evp.h>
#include <openssl/rand.h>

#endif


int punts = 0;
int dataPorts;
int cpuPort;
char *ifaceName[maxPorts];
struct ifaceStat_entry *ifaceStat[maxPorts];




struct polkaIdx_entry {
    int index;
    int nexthop;
    long pack;
    long byte;
};


struct polkaPoly_entry {
    int port;
    long pack;
    long byte;
    int tab[256];
};

struct hasht_head polkaPoly_table;

struct hasht_head mpolkaPoly_table;



void crc16mktab(int *tab, int poly) {
    for (int o=0; o < 256; o++) {
        int v = o << 8;
        for (int i = 0; i < 8; i++) {
            v <<= 1;
            if ((v & 0x10000) != 0) {
                v ^= poly;
            }
        }
        tab[o] = v & 0xffff;
    }
}

#define crc16calc(tmp, tab, buf, ofs, len)                                      \
    tmp = 0;                                                                    \
    for (int i = 0; i < len; i++) {                                             \
        tmp = ((tmp << 8) & 0xffff) ^ tab[(tmp >> 8) ^ (buf[ofs + i] & 0xff)];  \
    }


struct nsh_entry {
    int sp;
    int si;
    int command;    // 1=fwd, 2=vrf, 3=nei
    int port;
    int vrf;
    int trg;
    unsigned char macs[12];
    long pack;
    long byte;
};

struct hasht_head nsh_table;



struct mpls_entry {
    int label;
    int command;    // 1=vrf, 2=pop, 3=swap, 4=xconn, 5=vpls, 6=punt, 7=dup, 8=bier, 9=push, 10=pwhe
    long pack;
    long byte;
    int nexthop;
    int port;
    int vrf;
    int ver;
    int swap;
    int push;
    int bier[8];
    struct table_head flood;
};

struct hasht_head mpls_table;



#define mpls_init hasht_addinited(&mpls_table, &mpls_ntry, &mpls_ntry.flood, sizeof(struct flood_entry), 3);

struct port2vrf_entry {
    int port;
    int command;    // 1=vrf, 2=bridge, 3=xconn, 4=loconnifc, 5=loconnnei, 6=nshconn
    int vrf;
    int bridge;
    int nexthop;
    int label1;
    int label2;
    int tcpmss4;
    int tcpmss6;
    int verify4;
    int verify6;
    int pmtud4;
    int pmtud6;
    int nflw4;
    int nflw6;
    int pttl4;
    int pttl6;
    int rateIn;
    int rateOut;
    int mpls;
    int nsh;
    int monTarget;
    int monSample;
    int monTruncate;
    int monPackets;
    int sgtSet;
    int sgtTag;
    struct hasht_head insp4;
    struct hasht_head insp6;
    struct table_head inacl4;
    struct table_head inacl6;
    struct table_head outacl4;
    struct table_head outacl6;
    struct table_head inqos4;
    struct table_head inqos6;
    struct table_head outqos4;
    struct table_head outqos6;
#ifndef HAVE_NOCRYPTO
    int mcscEthtyp;
    int mcscCrTxKeyLen;
    int mcscCrRxKeyLen;
    int mcscDgTxKeyLen;
    int mcscDgRxKeyLen;
    int mcscIvTxKeyLen;
    int mcscIvRxKeyLen;
    int mcscEncrBlkLen;
    int mcscEncrTagLen;
    int mcscHashBlkLen;
    int mcscNeedMacs;
    int mcscNeedAead;
    int mcscSeqTx;
    int mcscSeqRx;
    long mcscPackRx;
    long mcscByteRx;
    long mcscPackOk;
    long mcscByteOk;
    long mcscPackTx;
    long mcscByteTx;
    const EVP_CIPHER *mcscEncrAlg;
    const EVP_MD *mcscHashAlg;
    unsigned char mcscCrTxKeyDat[maxKeys];
    unsigned char mcscCrRxKeyDat[maxKeys];
    unsigned char mcscDgTxKeyDat[maxKeys];
    unsigned char mcscDgRxKeyDat[maxKeys];
    unsigned char mcscIvTxKeyDat[maxKeys];
    unsigned char mcscIvRxKeyDat[maxKeys];
#endif
};

struct hasht_head port2vrf_table;


struct port2vrf_entry* port2vrf_init(struct port2vrf_entry *ntry) {
    struct port2vrf_entry *res = hasht_find(&port2vrf_table, ntry);
    if (res != NULL) return res;
    res = hasht_add(&port2vrf_table, ntry);
    res->monTarget = -1;
    res->sgtSet = -1;
    return res;
}


void port2vrf_deinit(struct port2vrf_entry *ntry) {
    struct port2vrf_entry *res = hasht_find(&port2vrf_table, ntry);
    if (res == NULL) return;
    hasht_deinit(&res->insp4);
    hasht_deinit(&res->insp6);
    table_deinit(&res->inacl4);
    table_deinit(&res->inacl6);
    table_deinit(&res->outacl4);
    table_deinit(&res->outacl6);
    table_deinit(&res->inqos4);
    table_deinit(&res->inqos6);
    table_deinit(&res->outqos4);
    table_deinit(&res->outqos6);
    hasht_del(&port2vrf_table, ntry);
}


struct vrf2rib_entry {
    int vrf;
    int samp;
    long pack;
    long byte;
    struct tree_head rou;
    struct hasht_head natT;
    struct hasht_head tun;
    struct hasht_head mcst;
    struct table_head plk;
    struct table_head copp;
    struct table_head flws;
    struct table_head pbr;
    struct table_head natC;
};


struct hasht_head vrf2rib4_table;

struct hasht_head vrf2rib6_table;


struct vrf2rib_entry* vrf2rib_init(struct hasht_head *tab, struct vrf2rib_entry *ntry, int reclen1, int reclen2, int reclen3, int reclen4, int natter, int tunner, int mcaster) {
    struct vrf2rib_entry* res = hasht_find(tab, ntry);
    if (res != NULL) return res;
    tree_init(&ntry->rou, reclen1);
    hasht_init(&ntry->natT, reclen2, natter);
    hasht_init(&ntry->tun, reclen3, tunner);
    hasht_init(&ntry->mcst, reclen4, mcaster);
    table_init(&ntry->plk, sizeof(struct polkaIdx_entry), 1);
    res = hasht_add(tab, ntry);
    return res;
}

#define vrf2rib_init4 vrf2rib_init(&vrf2rib4_table, &vrf2rib_ntry, sizeof(struct route4_entry), sizeof(struct nat4_entry), sizeof(struct tun4_entry), sizeof(struct mroute4_entry), 5, 5, 2)
#define vrf2rib_init6 vrf2rib_init(&vrf2rib6_table, &vrf2rib_ntry, sizeof(struct route6_entry), sizeof(struct nat6_entry), sizeof(struct tun6_entry), sizeof(struct mroute6_entry), 11, 11, 8)


struct route4_entry {
    int mask;
    int addr[1];
    int command;    // 1=route, 2=punt, 3=mpls1, 4=mpls2, 5=srv6, 6=mysrv4, 7=mysrv6, 8=brsrv, 9=polka, 10=mpolka, 11=drop
    long packTx;
    long byteTx;
    long packRx;
    long byteRx;
    int nexthop;
    int label1;
    int label2;
    int srv1;
    int srv2;
    int srv3;
    int srv4;
    unsigned char polka[16];
};


struct route6_entry {
    int mask;
    int addr[4];
    int command;    // 1=route, 2=punt, 3=mpls1, 4=mpls2, 5=srv6, 6=mysrv4, 7=mysrv6, 8=brsrv, 9=polka, 10=mpolka, 11=drop
    long packTx;
    long byteTx;
    long packRx;
    long byteRx;
    int nexthop;
    int label1;
    int label2;
    int srv1;
    int srv2;
    int srv3;
    int srv4;
    unsigned char polka[16];
};


struct neigh_entry {
    int id;
    int vrf;
    int command;    // 1=rawip, 2=pppoe, 3=gre, 4=l2tp, 5=ipip, 6=esp, 7=ovpn, 13=wg4. 14=wg6, 15=amt4, 16=amt6, 17=gtp4, 18=gtp6, 19=l3tp4, 20=l3tp6, 21=tmux4, 22=tmux6, 23=pwhe, 24=labels
    int layer3;     // 1=ipv4, 2=ipv6, 3=udp4, 4=udp6
    long pack;
    long byte;
    int port;
    int aclport;
    int tid;
    unsigned char macs[12];
    unsigned char mac2[12];
    int sip1;
    int sip2;
    int sip3;
    int sip4;
    int dip1;
    int dip2;
    int dip3;
    int dip4;
    int sprt;
    int dprt;
#ifndef HAVE_NOCRYPTO
    int seq;
    unsigned char encrKeyDat[maxKeys];
    unsigned char hashKeyDat[maxKeys];
    int encrKeyLen;
    int hashKeyLen;
    int encrBlkLen;
    int encrTagLen;
    int hashBlkLen;
    const EVP_CIPHER *encrAlg;
    const EVP_MD *hashAlg;
#endif
};

struct hasht_head neigh_table;


struct bridge_entry {
    int id;
    int mac2;
    int mac1;
    int command;    // 1=port, 2=vpls, 3=route, 4=vxlan4, 5=vxlan6, 6=pckoudp4, 7=pckoudp6, 8=srv4, 9=srv6, 10=etherip4, 11=etherip6, 12=eoip4, 13=eoip6
    long packRx;
    long byteRx;
    long packTx;
    long byteTx;
    int port;
    int nexthop;
    int label1;
    int label2;
    int srcAddr1;
    int srcAddr2;
    int srcAddr3;
    int srcAddr4;
    int trgAddr1;
    int trgAddr2;
    int trgAddr3;
    int trgAddr4;
    int srcPort;
    int trgPort;
    int instance;
};

struct hasht_head bridge_table;


struct vlanin_entry {
    int vlan;
    int port;
    int id;
    long pack;
    long byte;
};

struct hasht_head vlanin_table;


struct vlanout_entry {
    int id;
    int vlan;
    int vlan2;
    int port;
    int port2;
    long pack;
    long byte;
};

struct hasht_head vlanout_table;



struct acl4_entry {
    int pri;
    int act;
    long pack;
    long byte;
    int cmd;
    int vrf;
    int nexthop;
    int label;
    int srcAddr;
    int srcMask;
    int trgAddr;
    int trgMask;
    int protV;
    int protM;
    int srcPortV;
    int srcPortM;
    int trgPortV;
    int trgPortM;
    int tosV;
    int tosM;
    int flowV;
    int flowM;
    int sgtV;
    int sgtM;
};


int acl4_matcher(void *ptr1, void *ptr2) {
    struct acl4_entry *ntry1 = ptr1;
    struct acl4_entry *ntry2 = ptr2;
    if ((ntry1->protV & ntry2->protM) != ntry2->protV) return 1;
    if ((ntry1->srcPortV & ntry2->srcPortM) != ntry2->srcPortV) return 1;
    if ((ntry1->trgPortV & ntry2->trgPortM) != ntry2->trgPortV) return 1;
    if ((ntry1->tosV & ntry2->tosM) != ntry2->tosV) return 1;
    if ((ntry1->flowV & ntry2->flowM) != ntry2->flowV) return 1;
    if ((ntry1->sgtV & ntry2->sgtM) != ntry2->sgtV) return 1;
    if ((ntry1->srcAddr & ntry2->srcMask) != ntry2->srcAddr) return 1;
    if ((ntry1->trgAddr & ntry2->trgMask) != ntry2->trgAddr) return 1;
    return 0;
}


struct acl6_entry {
    int pri;
    int act;
    long pack;
    long byte;
    int cmd;
    int vrf;
    int nexthop;
    int label;
    int srcAddr1;
    int srcAddr2;
    int srcAddr3;
    int srcAddr4;
    int srcMask1;
    int srcMask2;
    int srcMask3;
    int srcMask4;
    int trgAddr1;
    int trgAddr2;
    int trgAddr3;
    int trgAddr4;
    int trgMask1;
    int trgMask2;
    int trgMask3;
    int trgMask4;
    int protV;
    int protM;
    int srcPortV;
    int srcPortM;
    int trgPortV;
    int trgPortM;
    int tosV;
    int tosM;
    int flowV;
    int flowM;
    int sgtV;
    int sgtM;
};


int acl6_matcher(void *ptr1, void *ptr2) {
    struct acl6_entry *ntry1 = ptr1;
    struct acl6_entry *ntry2 = ptr2;
    if ((ntry1->protV & ntry2->protM) != ntry2->protV) return 1;
    if ((ntry1->srcPortV & ntry2->srcPortM) != ntry2->srcPortV) return 1;
    if ((ntry1->trgPortV & ntry2->trgPortM) != ntry2->trgPortV) return 1;
    if ((ntry1->tosV & ntry2->tosM) != ntry2->tosV) return 1;
    if ((ntry1->flowV & ntry2->flowM) != ntry2->flowV) return 1;
    if ((ntry1->sgtV & ntry2->sgtM) != ntry2->sgtV) return 1;
    if ((ntry1->srcAddr1 & ntry2->srcMask1) != ntry2->srcAddr1) return 1;
    if ((ntry1->srcAddr2 & ntry2->srcMask2) != ntry2->srcAddr2) return 1;
    if ((ntry1->srcAddr3 & ntry2->srcMask3) != ntry2->srcAddr3) return 1;
    if ((ntry1->srcAddr4 & ntry2->srcMask4) != ntry2->srcAddr4) return 1;
    if ((ntry1->trgAddr1 & ntry2->trgMask1) != ntry2->trgAddr1) return 1;
    if ((ntry1->trgAddr2 & ntry2->trgMask2) != ntry2->trgAddr2) return 1;
    if ((ntry1->trgAddr3 & ntry2->trgMask3) != ntry2->trgAddr3) return 1;
    if ((ntry1->trgAddr4 & ntry2->trgMask4) != ntry2->trgAddr4) return 1;
    return 0;
}


struct aclH_entry {
    int pri;
    int act;        // 0=permit, 1=deny, 2=punt
    long pack;
    long byte;
    int cmd;        // 1=normal, 2=setvrf, 3=sethop, 4=setlab
    int vrf;
    int nexthop;
    int label;
};

struct aclH_entry* search_ace(struct table_head *tab, void *ntry, int matcher(void *, void *),int siz) {
    for (int i=0; i<tab->size; i++) {
        struct aclH_entry *res = table_get(tab, i);
        if (matcher(ntry, res) != 0) continue;
        res->pack++;
        res->byte += siz;
        return res;
    }
    return NULL;
}

int apply_acl(struct table_head *tab, void *ntry, int matcher(void *, void *),int siz) {
    struct aclH_entry *res = search_ace(tab, ntry, matcher, siz);
    if (res == NULL) return 1;
    return res->act;
}

void acl4init(struct table_head *tab) {
    if (!table_nonexist(tab)) return;
    table_init(tab, sizeof(struct acl4_entry), 1);
}

void acl6init(struct table_head *tab) {
    if (!table_nonexist(tab)) return;
    table_init(tab, sizeof(struct acl6_entry), 1);
}



struct insp4_entry {
    int srcPort;
    int trgPort;
    int srcAddr;
    int trgAddr;
    int prot;
    long packRx;
    long byteRx;
    long packTx;
    long byteTx;
};

struct nat4_entry {
    int oSrcPort;
    int oTrgPort;
    int oSrcAddr;
    int oTrgAddr;
    int prot;
    int nSrcAddr;
    int nTrgAddr;
    int nSrcPort;
    int nTrgPort;
    int sum3;
    int sum4;
    long pack;
    long byte;
};



struct insp6_entry {
    int srcPort;
    int trgPort;
    int srcAddr1;
    int srcAddr2;
    int srcAddr3;
    int srcAddr4;
    int trgAddr1;
    int trgAddr2;
    int trgAddr3;
    int trgAddr4;
    int prot;
    long packRx;
    long byteRx;
    long packTx;
    long byteTx;
};

struct nat6_entry {
    int oSrcPort;
    int oTrgPort;
    int oSrcAddr1;
    int oSrcAddr2;
    int oSrcAddr3;
    int oSrcAddr4;
    int oTrgAddr1;
    int oTrgAddr2;
    int oTrgAddr3;
    int oTrgAddr4;
    int prot;
    int nSrcAddr1;
    int nSrcAddr2;
    int nSrcAddr3;
    int nSrcAddr4;
    int nTrgAddr1;
    int nTrgAddr2;
    int nTrgAddr3;
    int nTrgAddr4;
    int nSrcPort;
    int nTrgPort;
    int sum3;
    int sum4;
    long pack;
    long byte;
};


struct bundle_entry {
    int id;
    int command;    // 1=port, 2=hairpin
    long pack;
    long byte;
    int out[16];
};

struct hasht_head bundle_table;


struct pppoe_entry {
    int port;
    int session;
    int aclport;
    int neigh;
    long pack;
    long byte;
};

struct hasht_head pppoe_table;


struct tun4_entry {
    int srcPort;
    int trgPort;
    int srcAddr;
    int trgAddr;
    int prot;
    int command;    // 1=gre, 2=l2tp, 3=vxlan, 4=ipip, 5=etherip, 6=pckoudp, 7=esp, 8=openvpn, 9=wireguard, 10=amt, 11=gtp, 12=l3tp, 13=tmux, 14=eoip
    long pack;
    long byte;
    int aclport;
    int neigh;
#ifndef HAVE_NOCRYPTO
    unsigned char encrKeyDat[maxKeys];
    unsigned char hashKeyDat[maxKeys];
    int encrKeyLen;
    int hashKeyLen;
    int encrBlkLen;
    int encrTagLen;
    int hashBlkLen;
    const EVP_CIPHER *encrAlg;
    const EVP_MD *hashAlg;
#endif
};


struct tun6_entry {
    int srcPort;
    int trgPort;
    int srcAddr1;
    int srcAddr2;
    int srcAddr3;
    int srcAddr4;
    int trgAddr1;
    int trgAddr2;
    int trgAddr3;
    int trgAddr4;
    int prot;
    int command;    // 1=gre, 2=l2tp, 3=vxlan, 4=ipip, 5=etherip, 6=pckoudp, 7=esp, 8=openvpn, 9=wireguard, 10=amt, 11=gtp, 12=l3tp, 13=tmux, 14=eoip
    long pack;
    long byte;
    int aclport;
    int neigh;
#ifndef HAVE_NOCRYPTO
    unsigned char encrKeyDat[maxKeys];
    unsigned char hashKeyDat[maxKeys];
    int encrKeyLen;
    int hashKeyLen;
    int encrBlkLen;
    int encrTagLen;
    int hashBlkLen;
    const EVP_CIPHER *encrAlg;
    const EVP_MD *hashAlg;
#endif
};


struct policer_entry {
    int vrf;
    int dir; // 1=ifcin, 2=ifcout, 3=flwspc4, 4=flwspc6, 5=ratein, 6=rateout
    int meter;
    long allow;
    long avail;
};

struct hasht_head policer_table;


struct flood_entry {
    int command; // 1=iface, 2=mpls, 3=biermsk, 4=bierset, 5=neigh, 6=biervpn
    int trg;
    int id;
    int lab;
    int lab2;
    int src;
    unsigned char macs[12];
    int bier[8];
};


struct mroute4_entry {
    int grp;
    int src;
    int ingr;
    int local;
    long pack;
    long byte;
    struct table_head flood;
};



struct mroute6_entry {
    int grp1;
    int grp2;
    int grp3;
    int grp4;
    int src1;
    int src2;
    int src3;
    int src4;
    int ingr;
    int local;
    long pack;
    long byte;
    struct table_head flood;
};



#define mcast_init4 hasht_addinited(&vrf2rib_res->mcst, &mroute4_ntry, &mroute4_ntry.flood, sizeof(struct flood_entry), 3);
#define mcast_init6 hasht_addinited(&vrf2rib_res->mcst, &mroute6_ntry, &mroute6_ntry.flood, sizeof(struct flood_entry), 3);







void initIface(int port, char *name) {
    ifaceName[port] = malloc(strlen(name)+1);
    if (ifaceName[port] == NULL) err("error allocating memory");
    strcpy(ifaceName[port], name);
    ifaceStat[port] = malloc(sizeof(struct ifaceStat_entry));
    if (ifaceStat[port] == NULL) err("error allocating memory");
    memset(ifaceStat[port], 0, sizeof(struct ifaceStat_entry));
}


int initTables() {
    hasht_init(&polkaPoly_table, sizeof(struct polkaPoly_entry), 1);
    hasht_init(&mpolkaPoly_table, sizeof(struct polkaPoly_entry), 1);
    hasht_init(&nsh_table, sizeof(struct nsh_entry), 2);
    hasht_init(&mpls_table, sizeof(struct mpls_entry), 1);
    hasht_init(&port2vrf_table, sizeof(struct port2vrf_entry), 1);
    hasht_init(&vrf2rib4_table, sizeof(struct vrf2rib_entry), 1);
    hasht_init(&vrf2rib6_table, sizeof(struct vrf2rib_entry), 1);
    hasht_init(&neigh_table, sizeof(struct neigh_entry), 1);
    hasht_init(&vlanin_table, sizeof(struct vlanin_entry), 2);
    hasht_init(&vlanout_table, sizeof(struct vlanout_entry), 1);
    hasht_init(&bridge_table, sizeof(struct bridge_entry), 3);
    hasht_init(&bundle_table, sizeof(struct bundle_entry), 1);
    hasht_init(&pppoe_table, sizeof(struct pppoe_entry), 2);
    hasht_init(&policer_table, sizeof(struct policer_entry), 3);
#ifndef HAVE_NOCRYPTO
    printf("openssl version: %s\n", OpenSSL_version(OPENSSL_VERSION));
#if OPENSSL_VERSION_NUMBER >= 0x30000000L
    if (OSSL_PROVIDER_load(NULL, "legacy") == NULL) return 1;
    if (OSSL_PROVIDER_load(NULL, "default") == NULL) return 1;
#endif
    OpenSSL_add_all_ciphers();
    OpenSSL_add_all_digests();
    OpenSSL_add_all_algorithms();
    RAND_poll();
#endif
    return 0;
}


int initContext(struct packetContext *ctx) {
#ifndef HAVE_NOCRYPTO
    ctx->encr = EVP_CIPHER_CTX_new();
    if (ctx->encr == NULL) return 1;
    ctx->dgst = EVP_MD_CTX_new();
    if (ctx->dgst == NULL) return 1;
#endif
    ctx->bufB3 = malloc(totBuff);
    if (ctx->bufB3 == NULL) return 1;
    ctx->bufB2 = malloc(totBuff);
    if (ctx->bufB2 == NULL) return 1;
    ctx->bufB1 = malloc(totBuff);
    if (ctx->bufB1 == NULL) return 1;
    ctx->bufC = malloc(totBuff);
    if (ctx->bufC == NULL) return 1;
    ctx->bufD = malloc(totBuff);
    if (ctx->bufD == NULL) return 1;
    ctx->bufH = malloc(preBuff);
    if (ctx->bufH == NULL) return 1;
    return 0;
}

int shiftContext(struct packetContext *trg, struct packetContext *src, unsigned char *bufD) {
    trg->sgt = src->sgt;
    trg->hash = src->hash;
    trg->stat = src->stat;
    trg->bufH = src->bufH;
    trg->bufD = bufD;
    trg->bufC = src->bufB1;
    trg->bufB1 = src->bufB2;
    trg->bufB2 = src->bufB3;
    trg->bufB3 = NULL;
#ifndef HAVE_NOCRYPTO
    trg->encr = src->encr;
    trg->dgst = src->dgst;
#endif
    return trg->bufC == NULL;
}


#ifndef HAVE_NOCRYPTO
void myHmacSetup(const EVP_MD *alg, unsigned char *key, int *len) {
    int blk = EVP_MD_block_size(alg);
    for (int i=*len; i<blk; i++) key[i]=0;
    memcpy(&key[blk], &key[0], blk);
    for (int i=0; i<blk; i++) {
        key[i] ^= 0x36;
        key[blk+i] ^= 0x5c;
    }
    *len = blk;
}


int myHmacInit(EVP_MD_CTX *ctx, const EVP_MD *alg, unsigned char *key, int len) {
    if (EVP_MD_CTX_reset(ctx) != 1) return 0;
    if (EVP_DigestInit_ex(ctx, alg, NULL) != 1) return 0;
    if (EVP_DigestUpdate(ctx, key, len) != 1) return 0;
    return 1;
}


int myHmacEnd(EVP_MD_CTX *ctx, const EVP_MD *alg, unsigned char *key, int len, unsigned char *dst) {
    unsigned int siz = preBuff;
    if (EVP_DigestFinal(ctx, dst, &siz) != 1) return 0;
    if (EVP_MD_CTX_reset(ctx) != 1) return 0;
    if (EVP_DigestInit_ex(ctx, alg, NULL) != 1) return 0;
    if (EVP_DigestUpdate(ctx, &key[len], len) != 1) return 0;
    if (EVP_DigestUpdate(ctx, dst, siz) != 1) return 0;
    siz = preBuff;
    if (EVP_DigestFinal(ctx, dst, &siz) != 1) return 0;
    return 1;
}
#endif
