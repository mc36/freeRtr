void processDataPacket(unsigned char *bufA, unsigned char *bufB, unsigned char *bufC, unsigned char *bufD, int bufS, int port, int prt, EVP_CIPHER_CTX *encrCtx, EVP_MD_CTX *hashCtx);


#ifdef debugging

int dropStat[4096];

#define doDropper {dropStat[__LINE__]++;goto drop;}
#define doPunting {dropStat[__LINE__]++;goto punt;}
#define doCpuing {dropStat[__LINE__]++;goto cpu;}

#else

#define doDropper goto drop;
#define doPunting goto punt;
#define doCpuing goto cpu;

#endif


void send2port(unsigned char *bufD, int bufS, int port) {
    if (port < 0) return;
    if (port >= ports) return;
    sendPack(bufD, bufS, port);
    packTx[port]++;
    byteTx[port] += bufS;
}



void send2cpu(unsigned char *bufD, int bufP, int bufS, int port) {
    bufP -= 12;
    memmove(&bufD[bufP], &bufD[preBuff], 12);
    bufP -= 2;
    put16msb(bufD, bufP, port);
    send2port(&bufD[bufP], bufS - bufP + preBuff, cpuport);
}





#ifdef basicLoop


int hashDataPacket(unsigned char *bufP) {
    return 0;
}


void processDataPacket(unsigned char *bufA, unsigned char *bufB, unsigned char *bufC, unsigned char *bufD, int bufS, int port, int prt, EVP_CIPHER_CTX *encrCtx, EVP_MD_CTX *hashCtx) {
    packRx[port]++;
    byteRx[port] += bufS;
    send2cpu(bufD, preBuff, bufS, port);
}


void processCpuPack(unsigned char *bufA, unsigned char *bufB, unsigned char *bufC, unsigned char* bufD, int bufS, EVP_CIPHER_CTX *encrCtx, EVP_MD_CTX *hashCtx) {
    packRx[cpuport]++;
    byteRx[cpuport] += bufS;
    int prt = get16msb(bufD, preBuff);
    send2port(&bufD[preBuff + 2], bufS - 2, prt);
}


#else



int hashDataPacket(unsigned char *bufP) {
    int hash = get32msb(bufP, 0);
    hash ^= get32msb(bufP, 4);
    hash ^= get32msb(bufP, 8);
    int ethtyp = get16msb(bufP, 12);
    bufP += 14;
    if (ethtyp == ETHERTYPE_VLAN) { // dot1q
        hash ^= get16msb(bufP, 0) & 0xfff; // vlan
        ethtyp = get16msb(bufP, 2);
        bufP += 4;
    }
    if (ethtyp == ETHERTYPE_SGT) { // sgt
        ethtyp = get16msb(bufP, 6);
        bufP += 8;
    }
    switch (ethtyp) {
    case ETHERTYPE_MPLS_UCAST: // mpls
        hash ^= (get32msb(bufP, 0) >> 12) & 0xfffff; // label
        break;
    case ETHERTYPE_IPV4: // ipv4
        hash ^= get32msb(bufP, 12); // src
        hash ^= get32msb(bufP, 16); // dst
        break;
    case ETHERTYPE_IPV6: // ipv6
        hash ^= get32msb(bufP, 8); // src
        hash ^= get32msb(bufP, 12);
        hash ^= get32msb(bufP, 16);
        hash ^= get32msb(bufP, 20);
        hash ^= get32msb(bufP, 24); // dst
        hash ^= get32msb(bufP, 28);
        hash ^= get32msb(bufP, 32);
        hash ^= get32msb(bufP, 36);
        break;
    case ETHERTYPE_PPPOE_DATA: // pppoe
        hash ^= get16msb(bufP, 2); // session
        break;
    }
    hash = ((hash >> 16) ^ hash) & 0xffff;
    hash = ((hash >> 8) ^ hash) & 0xff;
    return hash;
}





#define update_chksum(ofs, val)                                 \
    sum = get16lsb(bufD, ofs);                                  \
    sum -= val;                                                 \
    sum = (sum & 0xffff) + (sum >> 16);                         \
    put16lsb(bufD, ofs, sum);





int calcIPsum(unsigned char *buf, int pos, int len, int sum) {
    while (len > 1)  {
        sum += get16lsb(buf, pos);
        len -= 2;
        pos += 2;
    }
    if (len > 0) sum += buf[pos];
    sum = (sum >> 16) + (sum & 0xffff);
    sum += (sum >> 16);
    return sum;
}





void adjustMss(unsigned char *bufD, int bufT, int mss) {
    int bufE = ((bufD[bufT + 12] & 0xf0) >> 2) + bufT;
    int bufO = -1;
    for (int bufP = bufT + 20; bufP < bufE ; ) {
        int opt = bufD[bufP + 0];
        int siz = bufD[bufP + 1];
        if (siz < 2) siz = 2;
        switch (opt) {
        case 0: // end of options
            siz = 1024;
            break;
        case 1: // noop
            siz = 1;
            break;
        case 2: // mss
            bufO = bufP + 2;
            break;
        }
        bufP += siz;
    }
    if (bufO < 1) return;
    int old = get16msb(bufD, bufO);
    if (old <= mss) return;
    int sum2 = calcIPsum(bufD, bufT, bufE - bufT, 0);
    put16msb(bufD, bufO, mss);
    sum2 = calcIPsum(bufD, bufT, bufE - bufT, 0) - sum2;
    int sum;
    update_chksum(bufT + 16, sum2);
}


#define extract_layer4(ntry, mss)                               \
    switch (ntry.protV) {                                       \
        case 6:                                                 \
            ntry.srcPortV = get16msb(bufD, bufT + 0);           \
            ntry.trgPortV = get16msb(bufD, bufT + 2);           \
            if (mss < 1) break;                                 \
            if ((bufD[bufT + 13] & 2) == 0) break;              \
            adjustMss(bufD, bufT, mss);                         \
            break;                                              \
        case 17:                                                \
            ntry.srcPortV = get16msb(bufD, bufT + 0);           \
            ntry.trgPortV = get16msb(bufD, bufT + 2);           \
            break;                                              \
        default:                                                \
            ntry.srcPortV = 0;                                  \
            ntry.trgPortV = 0;                                  \
            break;                                              \
    }                                                           \
    hash ^= ntry.protV ^ ntry.srcPortV ^ ntry.trgPortV;




#define update_layer4(ntry)                                     \
    switch (ntry->prot) {                                       \
        case 6:                                                 \
            put16msb(bufD, bufT + 0, ntry->nSrcPort);           \
            put16msb(bufD, bufT + 2, ntry->nTrgPort);           \
            update_chksum(bufT + 16, ntry->sum4);               \
            break;                                              \
        case 17:                                                \
            put16msb(bufD, bufT + 0, ntry->nSrcPort);           \
            put16msb(bufD, bufT + 2, ntry->nTrgPort);           \
            if (get16msb(bufD, bufT + 6) == 0) break;           \
            update_chksum(bufT + 6, ntry->sum4);                \
            break;                                              \
    }



#define ethtyp2ppptyp(bufP, ethtyp)                             \
    switch (ethtyp) {                                           \
    case ETHERTYPE_MPLS_UCAST:                                  \
        ethtyp = PPPTYPE_MPLS_UCAST;                            \
        break;                                                  \
    case ETHERTYPE_IPV4:                                        \
        ethtyp = PPPTYPE_IPV4;                                  \
        break;                                                  \
    case ETHERTYPE_IPV6:                                        \
        ethtyp = PPPTYPE_IPV6;                                  \
        break;                                                  \
    case ETHERTYPE_MACSEC:                                      \
        ethtyp = PPPTYPE_MACSEC;                                \
        break;                                                  \
    case ETHERTYPE_SGT:                                         \
        ethtyp = PPPTYPE_SGT;                                   \
        break;                                                  \
    case ETHERTYPE_ROUTEDMAC:                                   \
        bufP -= 2;                                              \
        put16msb(bufD, bufP, 1);                                \
        ethtyp = PPPTYPE_ROUTEDMAC;                             \
        break;                                                  \
    default:                                                    \
        doDropper;                                              \
    }




#define ppptyp2ethtyp                                           \
    switch (ethtyp) {                                           \
    case PPPTYPE_MPLS_UCAST:                                    \
        ethtyp = ETHERTYPE_MPLS_UCAST;                          \
        break;                                                  \
    case PPPTYPE_IPV4:                                          \
        ethtyp = ETHERTYPE_IPV4;                                \
        break;                                                  \
    case PPPTYPE_IPV6:                                          \
        ethtyp = ETHERTYPE_IPV6;                                \
        break;                                                  \
    case PPPTYPE_MACSEC:                                        \
        ethtyp = ETHERTYPE_MACSEC;                              \
        break;                                                  \
    case PPPTYPE_SGT:                                           \
        ethtyp = ETHERTYPE_SGT;                                 \
        break;                                                  \
    case PPPTYPE_ROUTEDMAC:                                     \
        ethtyp = ETHERTYPE_ROUTEDMAC;                           \
        bufP += 2;                                              \
        break;                                                  \
    default:                                                    \
        doDropper;                                              \
    }


#define putPppoeHeader                                          \
    put16msb(bufD, *bufP, *ethtyp);                             \
    tmp = *bufS - *bufP + preBuff;                              \
    *bufP -= 6;                                                 \
    put16msb(bufD, *bufP + 0, 0x1100);                          \
    put16msb(bufD, *bufP + 2, neigh_res->session);              \
    put16msb(bufD, *bufP + 4, tmp);                             \
    *ethtyp = ETHERTYPE_PPPOE_DATA;                             \
    *bufP -= 2;                                                 \
    put16msb(bufD, *bufP, *ethtyp);






#define putIpv4header(bufP, bufS, ethtyp, proto, sip, dip)      \
    bufP -= 20;                                                 \
    put16msb(bufD, bufP + 0, 0x4500);                           \
    ethtyp = bufS - bufP + preBuff;                             \
    put16msb(bufD, bufP + 2, ethtyp);                           \
    put16msb(bufD, bufP + 4, 0);                                \
    put16msb(bufD, bufP + 6, 0);                                \
    bufD[bufP + 8] = 0xff;                                      \
    bufD[bufP + 9] = proto;                                     \
    ethtyp += 0x4500 + 0xff00 + proto;                          \
    ethtyp += ((sip >> 16) & 0xffff) + (sip & 0xffff);          \
    ethtyp += ((dip >> 16) & 0xffff) + (dip & 0xffff);          \
    ethtyp = (ethtyp >> 16) + (ethtyp & 0xffff);                \
    ethtyp += (ethtyp >> 16);                                   \
    ethtyp = 0xffff - ethtyp;                                   \
    put16msb(bufD, bufP + 10, ethtyp);                          \
    put32msb(bufD, bufP + 12, sip);                             \
    put32msb(bufD, bufP + 16, dip);                             \
    ethtyp = ETHERTYPE_IPV4;                                    \
    bufP -= 2;                                                  \
    put16msb(bufD, bufP, ethtyp);



#define putIpv6header(bufP, bufS, ethtyp, proto, sip1, sip2, sip3, sip4, dip1, dip2, dip3, dip4)    \
    bufP -= 40;                                                 \
    put16msb(bufD, bufP + 0, 0x6000);                           \
    put16msb(bufD, bufP + 2, 0);                                \
    ethtyp = bufS - bufP + preBuff - 40;                        \
    put16msb(bufD, bufP + 4, ethtyp);                           \
    bufD[bufP + 6] = proto;                                     \
    bufD[bufP + 7] = 0xff;                                      \
    put32msb(bufD, bufP + 8, sip1);                             \
    put32msb(bufD, bufP + 12, sip2);                            \
    put32msb(bufD, bufP + 16, sip3);                            \
    put32msb(bufD, bufP + 20, sip4);                            \
    put32msb(bufD, bufP + 24, dip1);                            \
    put32msb(bufD, bufP + 28, dip2);                            \
    put32msb(bufD, bufP + 32, dip3);                            \
    put32msb(bufD, bufP + 36, dip4);                            \
    ethtyp = ETHERTYPE_IPV6;                                    \
    bufP -= 2;                                                  \
    put16msb(bufD, bufP, ethtyp);



#define putGreHeader(bufP)                                      \
    bufP -= 2;                                                  \
    put16msb(bufD, bufP, 0x0000);



#define putIpipHeader(bufP, ethtyp, res)                        \
    switch (ethtyp) {                                           \
    case ETHERTYPE_IPV4:                                        \
        res = 4;                                                \
        break;                                                  \
    case ETHERTYPE_IPV6:                                        \
        res = 41;                                               \
        break;                                                  \
    default:                                                    \
        doDropper;                                              \
    }                                                           \
    bufP += 2;




#define putUdpHeader(bufP, bufS, sprt, dprt, sip1, sip2, sip3, sip4, dip1, dip2, dip3, dip4)    \
    bufP -= 8;                                                  \
    put16msb(bufD, bufP + 0, sprt);                             \
    put16msb(bufD, bufP + 2, dprt);                             \
    tmp = bufS - bufP + preBuff;                                \
    put16msb(bufD, bufP + 4, tmp);                              \
    put16msb(bufD, bufP + 6, 0);



#define putL2tpHeader(bufP, ethtyp)                             \
    put16msb(bufD, bufP, ethtyp);                               \
    bufP -= 10;                                                 \
    put16msb(bufD, bufP + 0, 0x0202);                           \
    put32msb(bufD, bufP + 2, neigh_res->tid);                   \
    put16msb(bufD, bufP + 6, 0);                                \
    put16msb(bufD, bufP + 8, 0xff03);


#define putVxlanHeader                                          \
    bufP -= 12;                                                 \
    memmove(&bufD[bufP], &bufH[0], 12);                         \
    bufP -= 8;                                                  \
    put16msb(bufD, bufP + 0, 0x800);                            \
    put16msb(bufD, bufP + 2, 0);                                \
    tmp = bridge_res->instance << 8;                            \
    put32msb(bufD, bufP + 4, tmp);


#define putPckoudpHeader                                        \
    bufP -= 12;                                                 \
    memmove(&bufD[bufP], &bufH[0], 12);



#define guessEthtyp                                             \
    switch (bufD[bufP] & 0xf0) {                                \
        case 0x40:                                              \
            ethtyp = ETHERTYPE_IPV4;                            \
            break;                                              \
        case 0x60:                                              \
            ethtyp = ETHERTYPE_IPV6;                            \
            break;                                              \
    default:                                                    \
        doDropper;                                              \
    }






#define putGtpHeader                                            \
    *bufP -= 6;                                                 \
    put16msb(bufD, *bufP + 0, 0x30ff);                          \
    tmp = *bufS - *bufP + preBuff - 8;                          \
    put16msb(bufD, *bufP + 2, tmp);                             \
    put32msb(bufD, *bufP + 4, neigh_res->tid);



int putWireguardHeader(struct neigh_entry *neigh_res, EVP_CIPHER_CTX *encrCtx, EVP_MD_CTX *hashCtx, unsigned char *bufD, int *bufP, int *bufS) {
    int seq = neigh_res->seq;
    neigh_res->seq++;
    *bufP += 2;
    int tmp = *bufS - *bufP + preBuff;
    int tmp2 = 16 - (tmp % 16);
    for (int i=0; i<tmp2; i++) {
        bufD[*bufP + tmp + i] = 0;
    }
    tmp += tmp2;
    *bufS += tmp2;
    put32lsb(bufD, 0, 0);
    put32lsb(bufD, 4, seq);
    put32lsb(bufD, 8, 0);
    if (EVP_CIPHER_CTX_reset(encrCtx) != 1) return 1;
    if (EVP_EncryptInit_ex(encrCtx, EVP_chacha20_poly1305(), NULL, neigh_res->encrKeyDat, &bufD[0]) != 1) return 1;
    if (EVP_CIPHER_CTX_set_padding(encrCtx, 0) != 1) return 1;
    if (EVP_EncryptUpdate(encrCtx, &bufD[*bufP], &tmp2, &bufD[*bufP], tmp) != 1) return 1;
    if (EVP_EncryptFinal_ex(encrCtx, &bufD[*bufP + tmp], &tmp2) != 1) return 1;
    if (EVP_CIPHER_CTX_ctrl(encrCtx, EVP_CTRL_AEAD_GET_TAG, 16, &bufD[*bufP + tmp]) != 1) return 1;
    tmp += 16;
    *bufS += 16;
    *bufP -= 16;
    put32lsb(bufD, *bufP + 0, 4);
    put32msb(bufD, *bufP + 4, neigh_res->tid);
    put32lsb(bufD, *bufP + 8, seq);
    put32lsb(bufD, *bufP + 12, 0);
    return 0;
}


int putOpenvpnHeader(struct neigh_entry *neigh_res, EVP_CIPHER_CTX *encrCtx, EVP_MD_CTX *hashCtx, unsigned char *bufD, int *bufP, int *bufS) {
    int seq = neigh_res->seq;
    neigh_res->seq++;
    *bufP += 2;
    *bufP -= 8;
    put32msb(bufD, *bufP + 0, seq);
    put32msb(bufD, *bufP + 4, neigh_res->tid);
    int tmp = *bufS - *bufP + preBuff;
    int tmp2 = neigh_res->encrBlkLen - (tmp % neigh_res->encrBlkLen);
    for (int i=0; i<tmp2; i++) {
        bufD[*bufP + tmp + i] = tmp2;
    }
    tmp += tmp2;
    *bufS += tmp2;
    *bufP -= neigh_res->encrBlkLen;
    RAND_bytes(&bufD[*bufP], neigh_res->encrBlkLen);
    tmp += neigh_res->encrBlkLen;
    if (EVP_CIPHER_CTX_reset(encrCtx) != 1) return 1;
    if (EVP_EncryptInit_ex(encrCtx, neigh_res->encrAlg, NULL, neigh_res->encrKeyDat, neigh_res->hashKeyDat) != 1) return 1;
    if (EVP_CIPHER_CTX_set_padding(encrCtx, 0) != 1) return 1;
    if (EVP_EncryptUpdate(encrCtx, &bufD[*bufP], &tmp2, &bufD[*bufP], tmp) != 1) return 1;
    if (neigh_res->hashBlkLen < 1) return 0;
    if (EVP_MD_CTX_reset(hashCtx) != 1) return 1;
    if (EVP_DigestSignInit(hashCtx, NULL, neigh_res->hashAlg, NULL, neigh_res->hashPkey) != 1) return 1;
    if (EVP_DigestSignUpdate(hashCtx, &bufD[*bufP], tmp) != 1) return 1;
    *bufP -= neigh_res->hashBlkLen;
    size_t sizt = preBuff;
    if (EVP_DigestSignFinal(hashCtx, &bufD[*bufP], &sizt) != 1) return 1;
    return 0;
}


int putEspHeader(struct neigh_entry *neigh_res, EVP_CIPHER_CTX *encrCtx, EVP_MD_CTX *hashCtx, unsigned char *bufD, int *bufP, int *bufS, int ethtyp) {
    int seq = neigh_res->seq;
    neigh_res->seq++;
    int tmp = *bufS - *bufP + preBuff;
    int tmp2 = neigh_res->encrBlkLen - ((tmp + 2) % neigh_res->encrBlkLen);
    for (int i=0; i<tmp2; i++) {
        bufD[*bufP + tmp + i] = i+1;
    }
    tmp += tmp2;
    *bufS += tmp2;
    bufD[*bufP + tmp + 0] = tmp2;
    bufD[*bufP + tmp + 1] = ethtyp;
    tmp += 2;
    *bufS += 2;
    *bufP -= neigh_res->encrBlkLen;
    RAND_bytes(&bufD[*bufP], neigh_res->encrBlkLen);
    tmp += neigh_res->encrBlkLen;
    if (EVP_CIPHER_CTX_reset(encrCtx) != 1) return 1;
    if (EVP_EncryptInit_ex(encrCtx, neigh_res->encrAlg, NULL, neigh_res->encrKeyDat, neigh_res->hashKeyDat) != 1) return 1;
    if (EVP_CIPHER_CTX_set_padding(encrCtx, 0) != 1) return 1;
    if (EVP_EncryptUpdate(encrCtx, &bufD[*bufP], &tmp2, &bufD[*bufP], tmp) != 1) return 1;
    *bufP -= 8;
    put32msb(bufD, *bufP + 0, neigh_res->spi);
    put32msb(bufD, *bufP + 4, seq);
    if (neigh_res->hashBlkLen < 1) return 0;
    tmp += 8;
    if (EVP_MD_CTX_reset(hashCtx) != 1) return 1;
    if (EVP_DigestSignInit(hashCtx, NULL, neigh_res->hashAlg, NULL, neigh_res->hashPkey) != 1) return 1;
    if (EVP_DigestSignUpdate(hashCtx, &bufD[*bufP], tmp) != 1) return 1;
    size_t sizt = preBuff;
    if (EVP_DigestSignFinal(hashCtx, &bufD[*bufP + tmp], &sizt) != 1) return 1;
    *bufS += neigh_res->hashBlkLen;
    return 0;
}


int macsec_apply(int prt, EVP_CIPHER_CTX *encrCtx, EVP_MD_CTX *hashCtx, unsigned char *bufD, int *bufP, int *bufS, unsigned char *bufH, int *ethtyp, int sgt) {
    struct port2vrf_entry port2vrf_ntry;
    struct port2vrf_entry *port2vrf_res;
    size_t sizt;
    port2vrf_ntry.port = prt;
    int index = table_find(&port2vrf_table, &port2vrf_ntry);
    if (index < 0) return 0;
    port2vrf_res = table_get(&port2vrf_table, index);
    if ((sgt >= 0) && (port2vrf_res->sgtTag != 0)) {
        *bufP -= 8;
        put16msb(bufD, *bufP + 2, 0x0101);
        put16msb(bufD, *bufP + 4, 0x0001);
        put16msb(bufD, *bufP + 6, sgt);
        *ethtyp = ETHERTYPE_SGT;
        put16msb(bufD, *bufP + 0, *ethtyp);
    }
    if (encrCtx == NULL) return 0;
    if (port2vrf_res->mcscEthtyp == 0) return 0;
    port2vrf_res->mcscPackTx++;
    port2vrf_res->mcscByteTx += *bufS;
    int seq = port2vrf_res->mcscSeqTx++;
    int tmp = *bufS - *bufP + preBuff;
    int tmp2 = tmp % port2vrf_res->mcscEncrBlkLen;
    if (tmp2 != 0) {
        tmp2 = port2vrf_res->mcscEncrBlkLen - tmp2;
        RAND_bytes(&bufD[*bufP + tmp], tmp2);
        *bufS += tmp2;
    }
    *bufP -= port2vrf_res->mcscEncrBlkLen;
    RAND_bytes(&bufD[*bufP], port2vrf_res->mcscEncrBlkLen);
    tmp = *bufS - *bufP + preBuff;
    if (EVP_CIPHER_CTX_reset(encrCtx) != 1) return 1;
    if (port2vrf_res->mcscNeedAead != 0) {
        unsigned char mac[12];
        put32msb(mac, 0, seq);
        put32msb(mac, 4, 0);
        put32msb(mac, 8, 0);
        if (EVP_EncryptInit_ex(encrCtx, port2vrf_res->mcscEncrAlg, NULL, port2vrf_res->mcscEncrKeyDat, mac) != 1) return 1;
        if (EVP_CIPHER_CTX_set_padding(encrCtx, 0) != 1) return 1;
        if (port2vrf_res->mcscNeedMacs != 0) {
            if (EVP_EncryptUpdate(encrCtx, NULL, &tmp2, &bufH[6], 6) != 1) return 1;
            if (EVP_EncryptUpdate(encrCtx, NULL, &tmp2, &bufH[0], 6) != 1) return 1;
        }
        if (EVP_EncryptUpdate(encrCtx, &bufD[*bufP], &tmp2, &bufD[*bufP], tmp) != 1) return 1;
        if (EVP_EncryptFinal_ex(encrCtx, &bufD[*bufP + tmp], &tmp2) != 1) return 1;
        if (EVP_CIPHER_CTX_ctrl(encrCtx, EVP_CTRL_GCM_GET_TAG, port2vrf_res->mcscEncrBlkLen, &bufD[*bufP + tmp]) != 1) return 1;
        tmp += port2vrf_res->mcscEncrBlkLen;
        *bufS += port2vrf_res->mcscEncrBlkLen;
    } else {
        if (EVP_EncryptInit_ex(encrCtx, port2vrf_res->mcscEncrAlg, NULL, port2vrf_res->mcscEncrKeyDat, port2vrf_res->mcscHashKeyDat) != 1) return 1;
        if (EVP_CIPHER_CTX_set_padding(encrCtx, 0) != 1) return 1;
        if (EVP_EncryptUpdate(encrCtx, &bufD[*bufP], &tmp2, &bufD[*bufP], tmp) != 1) return 1;
    }
    if (port2vrf_res->mcscHashBlkLen > 0) {
        if (EVP_MD_CTX_reset(hashCtx) != 1) return 1;
        if (EVP_DigestSignInit(hashCtx, NULL, port2vrf_res->mcscHashAlg, NULL, port2vrf_res->mcscHashPkey) != 1) return 1;
        if (port2vrf_res->mcscNeedMacs != 0) {
            if (EVP_DigestSignUpdate(hashCtx, &bufH[6], 6) != 1) return 1;
            if (EVP_DigestSignUpdate(hashCtx, &bufH[0], 6) != 1) return 1;
        }
        if (EVP_DigestSignUpdate(hashCtx, &bufD[*bufP], tmp) != 1) return 1;
        sizt = preBuff;
        if (EVP_DigestSignFinal(hashCtx, &bufD[*bufP + tmp], &sizt) != 1) return 1;
        *bufS += port2vrf_res->mcscHashBlkLen;
    }
    *bufP -= 8;
    *ethtyp = port2vrf_res->mcscEthtyp;
    put16msb(bufD, *bufP + 0, port2vrf_res->mcscEthtyp);
    bufD[*bufP + 2] = 0x08; // tci=v,e
    bufD[*bufP + 3] = 0; // sl
    put32msb(bufD, *bufP + 4, seq);
    return 0;
}


#define putMacAddr                                  \
    *bufP -= 12;                                    \
    memmove(&bufD[*bufP], &bufH[0], 12);



int send2subif(int prt, EVP_CIPHER_CTX *encrCtx, EVP_MD_CTX *hashCtx, int hash, unsigned char *bufD, int *bufP, int *bufS, unsigned char *bufH, int *ethtyp, int sgt) {
    if (macsec_apply(prt, encrCtx, hashCtx, bufD, &*bufP, &*bufS, bufH, &*ethtyp, sgt) != 0) return -1;
    struct vlan_entry vlan_ntry;
    struct bundle_entry bundle_ntry;
    struct vlan_entry *vlan_res;
    struct bundle_entry *bundle_res;
    vlan_ntry.id = prt;
    int index = table_find(&vlanout_table, &vlan_ntry);
    if (index >= 0) {
        vlan_res = table_get(&vlanout_table, index);
        hash ^= vlan_res->vlan;
        *bufP -= 2;
        put16msb(bufD, *bufP, vlan_res->vlan);
        *bufP -= 2;
        put16msb(bufD, *bufP, ETHERTYPE_VLAN);
        prt = vlan_res->port;
        vlan_res->pack++;
        vlan_res->byte += *bufS;
        *ethtyp = ETHERTYPE_VLAN;
        if (macsec_apply(prt, encrCtx, hashCtx, bufD, &*bufP, &*bufS, bufH, &*ethtyp, sgt) != 0) return -1;
    }
    bundle_ntry.id = prt;
    index = table_find(&bundle_table, &bundle_ntry);
    if (index < 0) {
        putMacAddr;
        send2port(&bufD[*bufP], *bufS - *bufP + preBuff, prt);
        return -1;
    }
    bundle_res = table_get(&bundle_table, index);
    hash = ((hash >> 16) ^ hash) & 0xffff;
    hash = ((hash >> 8) ^ hash) & 0xff;
    hash = ((hash >> 4) ^ hash) & 0xf;
    prt = bundle_res->out[hash];
    bundle_res->pack++;
    bundle_res->byte += *bufS;
    if (bundle_res->command == 2) {
        putMacAddr;
        *bufS = *bufS - *bufP + preBuff;
        memmove(&bufD[preBuff], &bufD[*bufP], *bufS);
        *bufP = preBuff + 12;
        return prt;
    }
    if (macsec_apply(prt, encrCtx, hashCtx, bufD, &*bufP, &*bufS, bufH, &*ethtyp, sgt) != 0) return -1;
    putMacAddr;
    send2port(&bufD[*bufP], *bufS - *bufP + preBuff, prt);
    return -1;
}



int send2neigh(struct neigh_entry *neigh_res, EVP_CIPHER_CTX *encrCtx, EVP_MD_CTX *hashCtx, int hash, unsigned char *bufD, int *bufP, int *bufS, unsigned char *bufH, int *ethtyp, int sgt) {
    int tmp;
    neigh_res->pack++;
    neigh_res->byte += *bufS;
    int prt = neigh_res->port;
    memmove(&bufH[0], &neigh_res->dmac, 6);
    memmove(&bufH[6], &neigh_res->smac, 6);
    if (neigh_res->aclport != prt) {
        if (macsec_apply(neigh_res->aclport, encrCtx, hashCtx, bufD, &*bufP, &*bufS, bufH, &*ethtyp, sgt) != 0) doDropper;
    }
    switch (neigh_res->command) {
    case 1: // raw ip
        break;
    case 2: // pppoe
        ethtyp2ppptyp(*bufP, *ethtyp);
        putPppoeHeader;
        break;
    case 3: // gre4
        putGreHeader(*bufP);
        putIpv4header(*bufP, *bufS, *ethtyp, 47, neigh_res->sip1, neigh_res->dip1);
        break;
    case 4: // gre6
        putGreHeader(*bufP);
        putIpv6header(*bufP, *bufS, *ethtyp, 47, neigh_res->sip1, neigh_res->sip2, neigh_res->sip3, neigh_res->sip4, neigh_res->dip1, neigh_res->dip2, neigh_res->dip3, neigh_res->dip4);
        break;
    case 5: // l2tp4
        ethtyp2ppptyp(*bufP, *ethtyp);
        putL2tpHeader(*bufP, *ethtyp);
        putUdpHeader(*bufP, *bufS, neigh_res->sprt, neigh_res->dprt, neigh_res->sip1, 0, 0, 0, neigh_res->dip1, 0, 0, 0);
        putIpv4header(*bufP, *bufS, *ethtyp, 17, neigh_res->sip1, neigh_res->dip1);
        break;
    case 6: // l2tp6
        ethtyp2ppptyp(*bufP, *ethtyp);
        putL2tpHeader(*bufP, *ethtyp);
        putUdpHeader(*bufP, *bufS, neigh_res->sprt, neigh_res->dprt, neigh_res->sip1, neigh_res->sip2, neigh_res->sip3, neigh_res->sip4, neigh_res->dip1, neigh_res->dip2, neigh_res->dip3, neigh_res->dip4);
        putIpv6header(*bufP, *bufS, *ethtyp, 17, neigh_res->sip1, neigh_res->sip2, neigh_res->sip3, neigh_res->sip4, neigh_res->dip1, neigh_res->dip2, neigh_res->dip3, neigh_res->dip4);
        break;
    case 7: // ipip4
        putIpipHeader(*bufP, *ethtyp, tmp);
        putIpv4header(*bufP, *bufS, *ethtyp, tmp, neigh_res->sip1, neigh_res->dip1);
        break;
    case 8: // ipip6
        putIpipHeader(*bufP, *ethtyp, tmp);
        putIpv6header(*bufP, *bufS, *ethtyp, tmp, neigh_res->sip1, neigh_res->sip2, neigh_res->sip3, neigh_res->sip4, neigh_res->dip1, neigh_res->dip2, neigh_res->dip3, neigh_res->dip4);
        break;
    case 9: // esp4
        putIpipHeader(*bufP, *ethtyp, *ethtyp);
        if (putEspHeader(neigh_res, encrCtx, hashCtx, bufD, &*bufP, &*bufS, *ethtyp) != 0) doDropper;
        putIpv4header(*bufP, *bufS, *ethtyp, 50, neigh_res->sip1, neigh_res->dip1);
        break;
    case 10: // esp6
        putIpipHeader(*bufP, *ethtyp, *ethtyp);
        if (putEspHeader(neigh_res, encrCtx, hashCtx, bufD, &*bufP, &*bufS, *ethtyp) != 0) doDropper;
        putIpv6header(*bufP, *bufS, *ethtyp, 50, neigh_res->sip1, neigh_res->sip2, neigh_res->sip3, neigh_res->sip4, neigh_res->dip1, neigh_res->dip2, neigh_res->dip3, neigh_res->dip4);
        break;
    case 11: // openvpn4
        if (putOpenvpnHeader(neigh_res, encrCtx, hashCtx, bufD, &*bufP, &*bufS) != 0) doDropper;
        putUdpHeader(*bufP, *bufS, neigh_res->sprt, neigh_res->dprt, neigh_res->sip1, 0, 0, 0, neigh_res->dip1, 0, 0, 0);
        putIpv4header(*bufP, *bufS, *ethtyp, 17, neigh_res->sip1, neigh_res->dip1);
        break;
    case 12: // openvpn6
        if (putOpenvpnHeader(neigh_res, encrCtx, hashCtx, bufD, &*bufP, &*bufS) != 0) doDropper;
        putUdpHeader(*bufP, *bufS, neigh_res->sprt, neigh_res->dprt, neigh_res->sip1, neigh_res->sip2, neigh_res->sip3, neigh_res->sip4, neigh_res->dip1, neigh_res->dip2, neigh_res->dip3, neigh_res->dip4);
        putIpv6header(*bufP, *bufS, *ethtyp, 17, neigh_res->sip1, neigh_res->sip2, neigh_res->sip3, neigh_res->sip4, neigh_res->dip1, neigh_res->dip2, neigh_res->dip3, neigh_res->dip4);
        break;
    case 13: // wireguard4
        if (putWireguardHeader(neigh_res, encrCtx, hashCtx, bufD, &*bufP, &*bufS) != 0) doDropper;
        putUdpHeader(*bufP, *bufS, neigh_res->sprt, neigh_res->dprt, neigh_res->sip1, 0, 0, 0, neigh_res->dip1, 0, 0, 0);
        putIpv4header(*bufP, *bufS, *ethtyp, 17, neigh_res->sip1, neigh_res->dip1);
        break;
    case 14: // wireguard6
        if (putWireguardHeader(neigh_res, encrCtx, hashCtx, bufD, &*bufP, &*bufS) != 0) doDropper;
        putUdpHeader(*bufP, *bufS, neigh_res->sprt, neigh_res->dprt, neigh_res->sip1, neigh_res->sip2, neigh_res->sip3, neigh_res->sip4, neigh_res->dip1, neigh_res->dip2, neigh_res->dip3, neigh_res->dip4);
        putIpv6header(*bufP, *bufS, *ethtyp, 17, neigh_res->sip1, neigh_res->sip2, neigh_res->sip3, neigh_res->sip4, neigh_res->dip1, neigh_res->dip2, neigh_res->dip3, neigh_res->dip4);
        break;
    case 15: // amt4
        put16msb(bufD, *bufP, 0x600);
        putUdpHeader(*bufP, *bufS, neigh_res->sprt, neigh_res->dprt, neigh_res->sip1, 0, 0, 0, neigh_res->dip1, 0, 0, 0);
        putIpv4header(*bufP, *bufS, *ethtyp, 17, neigh_res->sip1, neigh_res->dip1);
        break;
    case 16: // amt6
        put16msb(bufD, *bufP, 0x600);
        putUdpHeader(*bufP, *bufS, neigh_res->sprt, neigh_res->dprt, neigh_res->sip1, neigh_res->sip2, neigh_res->sip3, neigh_res->sip4, neigh_res->dip1, neigh_res->dip2, neigh_res->dip3, neigh_res->dip4);
        putIpv6header(*bufP, *bufS, *ethtyp, 17, neigh_res->sip1, neigh_res->sip2, neigh_res->sip3, neigh_res->sip4, neigh_res->dip1, neigh_res->dip2, neigh_res->dip3, neigh_res->dip4);
        break;
    case 17: // gtp4
        putGtpHeader;
        putUdpHeader(*bufP, *bufS, neigh_res->sprt, neigh_res->dprt, neigh_res->sip1, 0, 0, 0, neigh_res->dip1, 0, 0, 0);
        putIpv4header(*bufP, *bufS, *ethtyp, 17, neigh_res->sip1, neigh_res->dip1);
        break;
    case 18: // gtp6
        putGtpHeader;
        putUdpHeader(*bufP, *bufS, neigh_res->sprt, neigh_res->dprt, neigh_res->sip1, neigh_res->sip2, neigh_res->sip3, neigh_res->sip4, neigh_res->dip1, neigh_res->dip2, neigh_res->dip3, neigh_res->dip4);
        putIpv6header(*bufP, *bufS, *ethtyp, 17, neigh_res->sip1, neigh_res->sip2, neigh_res->sip3, neigh_res->sip4, neigh_res->dip1, neigh_res->dip2, neigh_res->dip3, neigh_res->dip4);
        break;
    default:
        doDropper;
    }
    return send2subif(prt, encrCtx, hashCtx, hash, &*bufD, &*bufP, &*bufS, bufH, &*ethtyp, sgt);
drop:
    return -1;
}



#define bierAnd(bufC, bufP, bier, tmp, res)                         \
    tmp = get32msb(bufC, bufP + 0) & bier[0];                       \
    put32msb(bufC, bufP + 0, tmp);                                  \
    res = tmp;                                                      \
    tmp = get32msb(bufC, bufP + 4) & bier[1];                       \
    put32msb(bufC, bufP + 4, tmp);                                  \
    res |= tmp;                                                     \
    tmp = get32msb(bufC, bufP + 8) & bier[2];                       \
    put32msb(bufC, bufP + 8, tmp);                                  \
    res |= tmp;                                                     \
    tmp = get32msb(bufC, bufP + 12) & bier[3];                      \
    put32msb(bufC, bufP + 12, tmp);                                 \
    res |= tmp;                                                     \
    tmp = get32msb(bufC, bufP + 16) & bier[4];                      \
    put32msb(bufC, bufP + 16, tmp);                                 \
    res |= tmp;                                                     \
    tmp = get32msb(bufC, bufP + 20) & bier[5];                      \
    put32msb(bufC, bufP + 20, tmp);                                 \
    res |= tmp;                                                     \
    tmp = get32msb(bufC, bufP + 24) & bier[6];                      \
    put32msb(bufC, bufP + 24, tmp);                                 \
    res |= tmp;                                                     \
    tmp = get32msb(bufC, bufP + 28) & bier[7];                      \
    put32msb(bufC, bufP + 28, tmp);                                 \
    res |= tmp;



void doFlood(struct table_head flood, EVP_CIPHER_CTX *encrCtx, EVP_MD_CTX *hashCtx, int hash, unsigned char *bufA, unsigned char *bufB, unsigned char *bufC, unsigned char *bufD, int bufP, int bufS, unsigned char *bufH, int ethtyp, int sgt, int label, int port) {
    struct neigh_entry neigh_ntry;
    struct neigh_entry *neigh_res;
    struct flood_entry *flood_res;
    for (int i = 0; i < flood.size; i++) {
        flood_res = table_get(&flood, i);
        int tmpP = preBuff;
        int tmpE;
        int tmp = -1;
        int tmp2;
        int index;
        switch (flood_res->command) {
        case 1: // raw ip
            tmpE = ethtyp;
            tmp2 = bufS - bufP + preBuff + 2;
            put16msb(bufC, preBuff, tmpE);
            memmove(&bufC[preBuff + 2], &bufD[bufP], tmp2);
            memmove(&bufH[0], &flood_res->dmac, 6);
            memmove(&bufH[6], &flood_res->smac, 6);
            tmp = send2subif(flood_res->trg, encrCtx, hashCtx, hash, bufC, &tmpP, &tmp2, bufH, &tmpE, sgt);
            break;
        case 2: // mpls
            tmpE = ETHERTYPE_MPLS_UCAST;
            tmp2 = bufS - bufP + preBuff + 6;
            int tmpL = label | (flood_res->lab << 12);
            put16msb(bufC, preBuff, tmpE);
            put32msb(bufC, preBuff + 2, tmpL);
            memmove(&bufC[preBuff + 6], &bufD[bufP], tmp2);
            neigh_ntry.id = flood_res->trg;
            index = table_find(&neigh_table, &neigh_ntry);
            if (index < 0) continue;
            neigh_res = table_get(&neigh_table, index);
            tmp = send2neigh(neigh_res, encrCtx, hashCtx, hash, bufC, &tmpP, &tmp2, bufH, &tmpE, sgt);
            break;
        case 3: // bier mask
            tmpE = ETHERTYPE_MPLS_UCAST;
            tmp2 = bufS - bufP + preBuff + 6;
            tmpL = label | (flood_res->lab << 12);
            put16msb(bufC, preBuff, tmpE);
            put32msb(bufC, preBuff + 2, tmpL);
            memmove(&bufC[preBuff + 6], &bufD[bufP], tmp2);
            int o;
            int p;
            bierAnd(bufC, preBuff + 14, flood_res->bier, o, p);
            if (p == 0) continue;
            neigh_ntry.id = flood_res->trg;
            index = table_find(&neigh_table, &neigh_ntry);
            if (index < 0) continue;
            neigh_res = table_get(&neigh_table, index);
            tmp = send2neigh(neigh_res, encrCtx, hashCtx, hash, bufC, &tmpP, &tmp2, bufH, &tmpE, sgt);
            break;
        case 4: // bier set
            tmpE = ETHERTYPE_MPLS_UCAST;
            tmp2 = bufS - bufP + preBuff + 46;
            tmpL = label | (flood_res->lab << 12);
            put16msb(bufC, preBuff, tmpE);
            put32msb(bufC, preBuff + 2, tmpL);
            bufC[preBuff + 6] = 0x50; // ver
            bufC[preBuff + 7] = 0x30; // bsl
            bufC[preBuff + 8] = 0; // entropy
            bufC[preBuff + 9] = 0; // entropy
            bufC[preBuff + 10] = 0; // oam
            switch (ethtyp) {
            case ETHERTYPE_IPV4:
                bufC[preBuff + 11] = 4; // proto
                break;
            case ETHERTYPE_IPV6:
                bufC[preBuff + 11] = 6; // proto
                break;
            default:
                continue;
            }
            put16msb(bufC, preBuff + 12, flood_res->src); // bfir
            put32msb(bufC, preBuff + 14, flood_res->bier[0]);
            put32msb(bufC, preBuff + 18, flood_res->bier[1]);
            put32msb(bufC, preBuff + 22, flood_res->bier[2]);
            put32msb(bufC, preBuff + 26, flood_res->bier[3]);
            put32msb(bufC, preBuff + 30, flood_res->bier[4]);
            put32msb(bufC, preBuff + 34, flood_res->bier[5]);
            put32msb(bufC, preBuff + 38, flood_res->bier[6]);
            put32msb(bufC, preBuff + 42, flood_res->bier[7]);
            memmove(&bufC[preBuff + 46], &bufD[bufP], tmp2);
            neigh_ntry.id = flood_res->trg;
            index = table_find(&neigh_table, &neigh_ntry);
            if (index < 0) continue;
            neigh_res = table_get(&neigh_table, index);
            tmp = send2neigh(neigh_res, encrCtx, hashCtx, hash, bufC, &tmpP, &tmp2, bufH, &tmpE, sgt);
            break;
        default:
            continue;
        }
        if (tmp < 0) continue;
        if (bufB == NULL) continue;
        processDataPacket(NULL, bufA, bufB, bufC, tmp2, port, tmp, encrCtx, hashCtx);
    }
}


#define routeMpls()                                                 \
    vrf2rib_ntry.vrf = mpls_res->vrf;                               \
    switch (mpls_res->ver) {                                        \
    case 4:                                                         \
        ethtyp = ETHERTYPE_IPV4;                                    \
        goto ipv4_rx;                                               \
    case 6:                                                         \
        ethtyp = ETHERTYPE_IPV6;                                    \
        goto ipv6_rx;                                               \
    default:                                                        \
        ethtyp = 0;                                                 \
        break;                                                      \
    }                                                               \
    doDropper;


#define doTunneled(tun_res)                                         \
    switch (tun_res->command) {                                     \
    case 1:                                                         \
        bufP = bufT + 2;                                            \
        break;                                                      \
    case 2:                                                         \
        bufP = bufT + 8;                                            \
        if ((get16msb(bufD, bufP) & 0x8000) != 0) doCpuing;         \
        bufP += 8;                                                  \
        bufP += 2;                                                  \
        ethtyp = get16msb(bufD, bufP);                              \
        if ((ethtyp & 0x8000) != 0) doCpuing;                       \
        ppptyp2ethtyp;                                              \
        put16msb(bufD, bufP, ethtyp);                               \
        break;                                                      \
    case 3:                                                         \
        bufP = bufT + 8;                                            \
        bufP += 8;                                                  \
        bufP -= 2;                                                  \
        put16msb(bufD, bufP, ETHERTYPE_ROUTEDMAC);                  \
        break;                                                      \
    case 4:                                                         \
        bufP = bufT - 2;                                            \
        put16msb(bufD, bufP, ETHERTYPE_IPV4);                       \
        break;                                                      \
    case 5:                                                         \
        bufP = bufT - 2;                                            \
        put16msb(bufD, bufP, ETHERTYPE_IPV6);                       \
        break;                                                      \
    case 6:                                                         \
        bufP = bufT;                                                \
        if (get32msb(bufD, bufP + 0) != tun_res->spi) doDropper;    \
        tun_res->seq = get32msb(bufD, bufP + 4);                    \
        tmp = bufS - bufP + preBuff - tun_res->hashBlkLen;          \
        if (tmp < 1) doDropper;                                     \
        if (tun_res->hashBlkLen > 0) {                              \
            if (EVP_MD_CTX_reset(hashCtx) != 1) doDropper;          \
            if (EVP_DigestSignInit(hashCtx, NULL, tun_res->hashAlg, NULL, tun_res->hashPkey) != 1) doDropper; \
            if (EVP_DigestSignUpdate(hashCtx, &bufD[bufP], tmp) != 1) doDropper;    \
            sizt = preBuff;                                         \
            if (EVP_DigestSignFinal(hashCtx, &bufH[0], &sizt) != 1) doDropper;      \
            if (memcmp(&bufH[0], &bufD[bufP + tmp], tun_res->hashBlkLen) !=0) doDropper;   \
            bufS -= tun_res->hashBlkLen;                            \
        }                                                           \
        bufP += 8;                                                  \
        tmp -= 8;                                                   \
        if (EVP_CIPHER_CTX_reset(encrCtx) != 1) doDropper;          \
        if (EVP_DecryptInit_ex(encrCtx, tun_res->encrAlg, NULL, tun_res->encrKeyDat, tun_res->hashKeyDat) != 1) doDropper;   \
        if (EVP_CIPHER_CTX_set_padding(encrCtx, 0) != 1) doDropper; \
        if (EVP_DecryptUpdate(encrCtx, &bufD[bufP], &tmp2, &bufD[bufP], tmp) != 1) doDropper;   \
        bufP += tun_res->encrBlkLen;                                \
        tmp -= tun_res->encrBlkLen;                                 \
        ethtyp = bufD[bufP + tmp - 1];                              \
        bufS -= bufD[bufP + tmp - 2] + 2;                           \
        switch (ethtyp) {                                           \
            case 4:                                                 \
                ethtyp = ETHERTYPE_IPV4;                            \
                break;                                              \
            case 41:                                                \
                ethtyp = ETHERTYPE_IPV6;                            \
                break;                                              \
        default:                                                    \
            doDropper;                                              \
        }                                                           \
        bufP -= 2;                                                  \
        put16msb(bufD, bufP, ethtyp);                               \
        break;                                                      \
    case 7:                                                         \
        bufP = bufT + 8;                                            \
        bufP -= 2;                                                  \
        put16msb(bufD, bufP, ETHERTYPE_ROUTEDMAC);                  \
        break;                                                      \
    case 8:                                                         \
        bufP = bufT + 8;                                            \
        bufP += tun_res->hashBlkLen;                                \
        tmp = bufS - bufP + preBuff;                                \
        if (tmp < 1) doDropper;                                     \
        if (tun_res->hashBlkLen > 0) {                              \
            if (EVP_MD_CTX_reset(hashCtx) != 1) doDropper;          \
            if (EVP_DigestSignInit(hashCtx, NULL, tun_res->hashAlg, NULL, tun_res->hashPkey) != 1) doDropper; \
            if (EVP_DigestSignUpdate(hashCtx, &bufD[bufP], tmp) != 1) doDropper;    \
            sizt = preBuff;                                         \
            if (EVP_DigestSignFinal(hashCtx, &bufH[0], &sizt) != 1) doDropper;      \
            if (memcmp(&bufH[0], &bufD[bufP - tun_res->hashBlkLen], tun_res->hashBlkLen) !=0) doDropper;    \
        }                                                           \
        if (EVP_CIPHER_CTX_reset(encrCtx) != 1) doDropper;          \
        if (EVP_DecryptInit_ex(encrCtx, tun_res->encrAlg, NULL, tun_res->encrKeyDat, tun_res->hashKeyDat) != 1) doDropper;   \
        if (EVP_CIPHER_CTX_set_padding(encrCtx, 0) != 1) doDropper; \
        if (EVP_DecryptUpdate(encrCtx, &bufD[bufP], &tmp2, &bufD[bufP], tmp) != 1) doDropper;   \
        bufP += tun_res->encrBlkLen;                                \
        tmp -= tun_res->encrBlkLen;                                 \
        bufP += 8;                                                  \
        guessEthtyp;                                                \
        bufP -= 2;                                                  \
        put16msb(bufD, bufP, ethtyp);                               \
        break;                                                      \
    case 9:                                                         \
        bufP = bufT + 8;                                            \
        if (get32lsb(bufD, bufP) != 4) doCpuing;                    \
        tmp = bufS - bufP + preBuff;                                \
        if (tmp < 32) doDropper;                                    \
        put32msb(bufD, bufP + 4, 0);                                \
        bufP += 16;                                                 \
        bufS -= 16;                                                 \
        tmp -= 32;                                                  \
        if (EVP_CIPHER_CTX_reset(encrCtx) != 1) doDropper;          \
        if (EVP_DecryptInit_ex(encrCtx, EVP_chacha20_poly1305(), NULL, tun_res->encrKeyDat, &bufD[bufP - 12]) != 1) doDropper;  \
        if (EVP_CIPHER_CTX_set_padding(encrCtx, 0) != 1) doDropper; \
        if (EVP_CIPHER_CTX_ctrl(encrCtx, EVP_CTRL_AEAD_SET_TAG, 16, &bufD[bufP + tmp]) != 1) doDropper; \
        if (EVP_DecryptUpdate(encrCtx, &bufD[bufP], &tmp2, &bufD[bufP], tmp) != 1) doDropper;   \
        if (EVP_DecryptFinal_ex(encrCtx, &bufD[bufP + tmp], &tmp2) != 1) doDropper; \
        guessEthtyp;                                                \
        bufP -= 2;                                                  \
        put16msb(bufD, bufP, ethtyp);                               \
        break;                                                      \
    case 10:                                                        \
        bufP = bufT + 8;                                            \
        if (bufD[bufP] != 6) doCpuing;                              \
        bufP += 2;                                                  \
        guessEthtyp;                                                \
        bufP -= 2;                                                  \
        put16msb(bufD, bufP, ethtyp);                               \
        break;                                                      \
    case 11:                                                        \
        bufP = bufT + 8;                                            \
        bufP += 8;                                                  \
        guessEthtyp;                                                \
        bufP -= 2;                                                  \
        put16msb(bufD, bufP, ethtyp);                               \
        break;                                                      \
    default:                                                        \
        doDropper;                                                  \
    }                                                               \
    tun_res->pack++;                                                \
    tun_res->byte += bufS;                                          \
    prt = tun_res->aclport;                                         \
    goto ethtyp_rx;                                                 \


#define doRouted(route_res, proto)                                  \
    case 3:                                                         \
        ethtyp = ETHERTYPE_MPLS_UCAST;                              \
        bufP -= 4;                                                  \
        label = 0x100 | ttl | (route_res->label1 << 12);            \
        put32msb(bufD, bufP, label);                                \
        neigh_ntry.id = route_res->nexthop;                         \
        goto ethtyp_tx;                                             \
    case 4:                                                         \
        ethtyp = ETHERTYPE_MPLS_UCAST;                              \
        bufP -= 4;                                                  \
        label = 0x100 | ttl | (route_res->label2 << 12);            \
        put32msb(bufD, bufP, label);                                \
        bufP -= 4;                                                  \
        label = ttl | (route_res->label1 << 12);                    \
        put32msb(bufD, bufP, label);                                \
        neigh_ntry.id = route_res->nexthop;                         \
        goto ethtyp_tx;                                             \
    case 5:                                                         \
        putIpv6header(bufP, bufS, ethtyp, proto, route_res->srv1, route_res->srv2, route_res->srv3, route_res->srv4, route_res->srv1, route_res->srv2, route_res->srv3, route_res->srv4);   \
        neigh_ntry.id = route_res->nexthop;                         \
        goto nethtyp_tx;                                            \
    case 6:                                                         \
        vrf2rib_ntry.vrf = route_res->srv1;                         \
        bufP = bufT;                                                \
        ethtyp = ETHERTYPE_IPV4;                                    \
        goto ipv4_rx;                                               \
    case 7:                                                         \
        vrf2rib_ntry.vrf = route_res->srv1;                         \
        bufP = bufT;                                                \
        ethtyp = ETHERTYPE_IPV6;                                    \
        goto ipv6_rx;                                               \
    case 8:                                                         \
        bridge_ntry.id = route_res->srv1;                           \
        bufP = bufT;                                                \
        memmove(&bufH[0], &bufD[bufP], 12);                         \
        bufP += 12;                                                 \
        bufP += 2;                                                  \
        goto bridgevpls_rx;                                         \
    case 9:                                                         \
        bufP -= 20;                                                 \
        put16msb(bufD, bufP + 0, ttl);                              \
        put16msb(bufD, bufP + 2, ethtyp);                           \
        memmove(&bufD[bufP + 4], route_res->polka, 16);             \
        neigh_ntry.id = route_res->nexthop;                         \
        ethtyp = ETHERTYPE_POLKA;                                   \
        goto ethtyp_tx;                                             \
    case 10:                                                        \
        bufP -= 20;                                                 \
        put16msb(bufD, bufP + 0, ttl);                              \
        put16msb(bufD, bufP + 2, ethtyp);                           \
        memmove(&bufD[bufP + 4], route_res->polka, 16);             \
        neigh_ntry.id = route_res->nexthop;                         \
        ethtyp = ETHERTYPE_MPOLKA;                                  \
        goto ethtyp_tx;                                             \




void processDataPacket(unsigned char *bufA, unsigned char *bufB, unsigned char *bufC, unsigned char *bufD, int bufS, int port, int prt, EVP_CIPHER_CTX *encrCtx, EVP_MD_CTX *hashCtx) {
    packRx[port]++;
    byteRx[port] += bufS;
    unsigned char bufH[preBuff];
    struct nsh_entry nsh_ntry;
    struct polkaPoly_entry polkaPoly_ntry;
    struct polkaIdx_entry polkaIdx_ntry;
    struct mpls_entry mpls_ntry;
    struct port2vrf_entry port2vrf_ntry;
    struct vrf2rib_entry vrf2rib_ntry;
    struct route4_entry route4_ntry;
    struct route6_entry route6_ntry;
    struct route4_entry sroute4_ntry;
    struct route6_entry sroute6_ntry;
    struct neigh_entry neigh_ntry;
    struct vlan_entry vlan_ntry;
    struct bridge_entry bridge_ntry;
    struct acls_entry acls_ntry;
    struct acl4_entry acl4_ntry;
    struct acl6_entry acl6_ntry;
    struct insp4_entry insp4_ntry;
    struct insp6_entry insp6_ntry;
    struct nat4_entry nat4_ntry;
    struct nat6_entry nat6_ntry;
    struct pppoe_entry pppoe_ntry;
    struct tun4_entry tun4_ntry;
    struct tun6_entry tun6_ntry;
    struct policer_entry policer_ntry;
    struct mroute4_entry mroute4_ntry;
    struct mroute6_entry mroute6_ntry;
    struct polkaPoly_entry *polkaPoly_res = NULL;
    struct polkaIdx_entry *polkaIdx_res = NULL;
    struct nsh_entry *nsh_res = NULL;
    struct mpls_entry *mpls_res = NULL;
    struct port2vrf_entry *port2vrf_res = NULL;
    struct vrf2rib_entry *vrf2rib_res = NULL;
    struct route4_entry *route4_res = NULL;
    struct route6_entry *route6_res = NULL;
    struct neigh_entry *neigh_res = NULL;
    struct vlan_entry *vlan_res = NULL;
    struct bridge_entry *bridge_res = NULL;
    struct acls_entry *acls_res = NULL;
    struct aclH_entry *aceh_res = NULL;
    struct insp4_entry *insp4_res = NULL;
    struct insp6_entry *insp6_res = NULL;
    struct nat4_entry *nat4_res = NULL;
    struct nat6_entry *nat6_res = NULL;
    struct pppoe_entry *pppoe_res = NULL;
    struct tun4_entry *tun4_res = NULL;
    struct tun6_entry *tun6_res = NULL;
    struct policer_entry *policer_res = NULL;
    struct mroute4_entry *mroute4_res = NULL;
    struct mroute6_entry *mroute6_res = NULL;
    int sgt = 0;
    int index = 0;
    int label = 0;
    int sum = 0;
    int ttl = 0;
    int hash = 0;
    int bufP = 0;
    int bufE = 0;
    int bufT = 0;
    int frag = 0;
    int ethtyp = 0;
    int tmp = 0;
    int tmp2 = 0;
    int i = 0;
    size_t sizt = 0;
    bufP = preBuff;
    bufP += 6 * 2; // dmac, smac
ethtyp_rx:
    bufE = bufP;
    ethtyp = get16msb(bufD, bufP);
    bufP += 2;
    port2vrf_ntry.port = prt;
    index = table_find(&port2vrf_table, &port2vrf_ntry);
    if (index < 0) {
        port2vrf_res = NULL;
        goto etyped_rx;
    }
    port2vrf_res = table_get(&port2vrf_table, index);
    if (port2vrf_res->mcscEthtyp != 0) {
        port2vrf_res->mcscPackRx++;
        port2vrf_res->mcscByteRx += bufS;
        if (ethtyp != port2vrf_res->mcscEthtyp) doDropper;
        if (bufD[bufP] != 0x08) doCpuing;
        int seq = port2vrf_res->mcscSeqRx = get32msb(bufD, bufP + 2);
        bufP += 6;
        tmp = bufS - bufP + preBuff - port2vrf_res->mcscHashBlkLen;
        if (tmp < 1) doDropper;
        if ((tmp % port2vrf_res->mcscEncrBlkLen) != 0) doDropper;
        if (port2vrf_res->mcscHashBlkLen > 0) {
            if (EVP_MD_CTX_reset(hashCtx) != 1) doDropper;
            if (EVP_DigestSignInit(hashCtx, NULL, port2vrf_res->mcscHashAlg, NULL, port2vrf_res->mcscHashPkey) != 1) doDropper;
            if (port2vrf_res->mcscNeedMacs != 0) {
                if (EVP_DigestSignUpdate(hashCtx, &bufD[preBuff + 6], 6) != 1) doDropper;
                if (EVP_DigestSignUpdate(hashCtx, &bufD[preBuff + 0], 6) != 1) doDropper;
            }
            if (EVP_DigestSignUpdate(hashCtx, &bufD[bufP], tmp) != 1) doDropper;
            sizt = preBuff;
            if (EVP_DigestSignFinal(hashCtx, &bufH[0], &sizt) != 1) doDropper;
            if (memcmp(&bufH[0], &bufD[bufP + tmp], port2vrf_res->mcscHashBlkLen) !=0) doDropper;
            bufS -= port2vrf_res->mcscHashBlkLen;
        }
        if (EVP_CIPHER_CTX_reset(encrCtx) != 1) doDropper;
        if (port2vrf_res->mcscNeedAead != 0) {
            unsigned char mac[12];
            put32msb(mac, 0, seq);
            put32msb(mac, 4, 0);
            put32msb(mac, 8, 0);
            if (EVP_DecryptInit_ex(encrCtx, port2vrf_res->mcscEncrAlg, NULL, port2vrf_res->mcscEncrKeyDat, mac) != 1) doDropper;
            if (EVP_CIPHER_CTX_set_padding(encrCtx, 0) != 1) doDropper;
            if (port2vrf_res->mcscNeedMacs != 0) {
                if (EVP_DecryptUpdate(encrCtx, NULL, &tmp2, &bufD[preBuff + 6], 6) != 1) doDropper;
                if (EVP_DecryptUpdate(encrCtx, NULL, &tmp2, &bufD[preBuff + 0], 6) != 1) doDropper;
            }
            tmp -= port2vrf_res->mcscEncrBlkLen;
            if (EVP_DecryptUpdate(encrCtx, &bufD[bufP], &tmp2, &bufD[bufP], tmp) != 1) doDropper;
            if (EVP_CIPHER_CTX_ctrl(encrCtx, EVP_CTRL_GCM_SET_TAG, port2vrf_res->mcscEncrBlkLen, &bufD[bufP + tmp]) != 1) doDropper;
            if (EVP_DecryptFinal_ex(encrCtx, &bufD[bufP + tmp], &tmp2) != 1) doDropper;
        } else {
            if (EVP_DecryptInit_ex(encrCtx, port2vrf_res->mcscEncrAlg, NULL, port2vrf_res->mcscEncrKeyDat, port2vrf_res->mcscHashKeyDat) != 1) doDropper;
            if (EVP_CIPHER_CTX_set_padding(encrCtx, 0) != 1) doDropper;
            if (EVP_DecryptUpdate(encrCtx, &bufD[bufP], &tmp2, &bufD[bufP], tmp) != 1) doDropper;
        }
        bufP += port2vrf_res->mcscEncrBlkLen;
        tmp -= port2vrf_res->mcscEncrBlkLen;
        bufS = tmp + bufP - preBuff;
        bufE = bufP;
        ethtyp = get16msb(bufD, bufP);
        bufP += 2;
        port2vrf_res->mcscPackOk++;
        port2vrf_res->mcscByteOk += bufS;
    }
    if (port2vrf_res->sgtTag != 0) {
        if (ethtyp != ETHERTYPE_SGT) doDropper;
        if (get32msb(bufD, bufP + 0) != 0x01010001) doDropper;
        sgt = get16msb(bufD, bufP + 4);
        ethtyp = get16msb(bufD, bufP + 6);
        bufP += 8;
    }
    if (port2vrf_res->sgtSet >= 0) {
        sgt = port2vrf_res->sgtSet;
    }
    if (port2vrf_res->monTarget >= 0) {
        if ((port2vrf_res->monPackets++%port2vrf_res->monSample) == 0) {
            int tmpS = bufS - bufP + preBuff + 2;
            if (tmpS > port2vrf_res->monTruncate) tmpS = port2vrf_res->monTruncate;
            memmove(&bufC[preBuff], &bufD[bufP - 2], tmpS);
            memmove(&bufH[0], &bufD[preBuff], 12);
            int tmpP = preBuff;
            int tmpE = ethtyp;
            send2subif(port2vrf_res->monTarget, encrCtx, hashCtx, hash, bufC, &tmpP, &tmpS, bufH, &tmpE, sgt);
        }
    }
    if (ethtyp != ETHERTYPE_ROUTEDMAC) switch (port2vrf_res->command) {
        case 2:
            goto bridge_rx;
        case 3:
            goto xconn_rx;
        }
etyped_rx:
    if ((bufP < minBuff) || (bufP > maxBuff)) doDropper;
    switch (ethtyp) {
    case ETHERTYPE_MPLS_UCAST: // mpls
        if (port2vrf_res == NULL) doDropper;
        if (port2vrf_res->mpls == 0) doDropper;
        packMpls[port]++;
        byteMpls[port] += bufS;
mpls_rx:
        label = get32msb(bufD, bufP);
        ttl = (label & 0xff) - 1;
        if (ttl <= 1) doPunting;
        bufP += 4;
        mpls_ntry.label = (label >> 12) & 0xfffff;
        hash ^= mpls_ntry.label;
        switch (mpls_ntry.label) {
        case 0: // explicit ipv4
            if ((label & 0x100) == 0) {
                bufD[bufP + 3] = ttl + 1;
                goto mpls_rx;
            }
            ethtyp = ETHERTYPE_IPV4;
            vrf2rib_ntry.vrf = port2vrf_res->vrf;
            goto ipv4_rx;
        case 2: // explicit ipv6
            if ((label & 0x100) == 0) {
                bufD[bufP + 3] = ttl + 1;
                goto mpls_rx;
            }
            ethtyp = ETHERTYPE_IPV6;
            vrf2rib_ntry.vrf = port2vrf_res->vrf;
            goto ipv6_rx;
        }
        index = table_find(&mpls_table, &mpls_ntry);
        if (index < 0) doDropper;
        mpls_res = table_get(&mpls_table, index);
        mpls_res->pack++;
        mpls_res->byte += bufS;
        switch (mpls_res->command) {
        case 1: // route
mpls_rou:
            if ((label & 0x100) == 0) {
                bufD[bufP + 3] = ttl + 1;
                goto mpls_rx;
            }
            routeMpls();
        case 2: // pop
            neigh_ntry.id = mpls_res->nexthop;
            if ((label & 0x100) == 0) {
                bufD[bufP + 3] = ttl;
                goto ethtyp_tx;
            }
            switch (mpls_res->ver) {
            case 4:
                ethtyp = ETHERTYPE_IPV4;
                goto ethtyp_tx;
            case 6:
                ethtyp = ETHERTYPE_IPV6;
                goto ethtyp_tx;
            default:
                ethtyp = 0;
                break;
            }
            doDropper;
        case 3: // swap
            bufP -= 4;
            label = (label & 0xf00) | ttl | (mpls_res->swap << 12);
            put32msb(bufD, bufP, label);
            neigh_ntry.id = mpls_res->nexthop;
ethtyp_tx:
            bufP -= 2;
            put16msb(bufD, bufP, ethtyp);
nethtyp_tx:
            index = table_find(&neigh_table, &neigh_ntry);
            if (index < 0) doDropper;
            neigh_res = table_get(&neigh_table, index);
neigh_tx:
            prt = send2neigh(neigh_res, encrCtx, hashCtx, hash, bufD, &bufP, &bufS, bufH, &ethtyp, sgt);
            if (prt >= 0) goto ethtyp_rx;
            return;
        case 4: // xconn
            memmove(&bufH[0], &bufD[bufP], 12);
            bufP += 12;
            prt = mpls_res->port;
            prt = send2subif(prt, encrCtx, hashCtx, hash, bufD, &bufP, &bufS, bufH, &ethtyp, sgt);
            if (prt >= 0) goto ethtyp_rx;
            return;
        case 5: // vpls
            memmove(&bufH[0], &bufD[bufP], 12);
            bufP += 12;
            bufP += 2;
            bridge_ntry.id = mpls_res->bridge;
            goto bridgevpls_rx;
        case 6: // punt
            doCpuing;
        case 7: // dup
            doFlood(mpls_res->flood, encrCtx, hashCtx, hash, bufA, bufB, bufC, bufD, bufP, bufS, bufH, ethtyp, sgt, (label & 0xf00) | ttl, port);
            if (mpls_res->swap != 0) goto mpls_rou;
            return;
        case 8: // bier
            if ((label & 0x100) == 0) doDropper;
            if (bufD[bufP] != 0x50) doDropper;
            doFlood(mpls_res->flood, encrCtx, hashCtx, hash, bufA, bufB, bufC, bufD, bufP, bufS, bufH, ethtyp, sgt, (label & 0xf00) | ttl, port);
            bierAnd(bufD, bufP + 8, mpls_res->bier, tmp, tmp2);
            if (tmp2 == 0) return;
            bufP += 8;
            bufP += 32;
            routeMpls();
        default:
            return;
        }
        return;
    case ETHERTYPE_VLAN: // dot1q
        packVlan[port]++;
        byteVlan[port] += bufS;
        vlan_ntry.port = prt;
        vlan_ntry.vlan = get16msb(bufD, bufP) & 0xfff;
        bufP += 2;
        index = table_find(&vlanin_table, &vlan_ntry);
        if (index < 0) doDropper;
        vlan_res = table_get(&vlanin_table, index);
        prt = vlan_res->id;
        vlan_res->pack++;
        vlan_res->byte += bufS;
        goto ethtyp_rx;
    case ETHERTYPE_IPV4: // ipv4
        if (port2vrf_res == NULL) doDropper;
        if (port2vrf_res->command != 1) doDropper;
        packIpv4[port]++;
        byteIpv4[port] += bufS;
        vrf2rib_ntry.vrf = port2vrf_res->vrf;
ipv4_rx:
        index = table_find(&vrf2rib4_table, &vrf2rib_ntry);
        if (index < 0) doDropper;
        vrf2rib_res = table_get(&vrf2rib4_table, index);
        if ((bufD[bufP + 0] & 0xf0) != 0x40) doDropper;
        bufT = bufD[bufP + 0] & 0xf;
        if (bufT < 5) doDropper;
        ttl = get16msb(bufD, bufP + 2) + bufP - preBuff;
        if (ttl > bufS) doDropper;
        bufS = ttl;
        bufT = bufP + (bufT << 2);
        frag = get16msb(bufD, bufP + 6) & 0x3fff;
        acl4_ntry.sgtV = sgt;
        acl4_ntry.protV = bufD[bufP + 9];
        acl4_ntry.tosV = bufD[bufP + 1];
        acl4_ntry.flowV = get16msb(bufD, bufP + 4);
        acl4_ntry.srcAddr = get32msb(bufD, bufP + 12);
        route4_ntry.addr = acl4_ntry.trgAddr = get32msb(bufD, bufP + 16);
        route4_ntry.mask = 32;
        hash ^= acl4_ntry.srcAddr ^ acl4_ntry.trgAddr;
        ttl = bufD[bufP + 8] - 1;
        if (ttl <= 1) doPunting;
        bufD[bufP + 8] = ttl;
        update_chksum(bufP + 10, -1);
        extract_layer4(acl4_ntry, port2vrf_res->tcpmss4);
        if (port2vrf_res->verify4 > 0) {
            sroute4_ntry.addr = acl4_ntry.srcAddr;
            sroute4_ntry.mask = 32;
            route4_res = tree_lpm(&vrf2rib_res->rou, &sroute4_ntry);
            if (route4_res == NULL) doPunting;
            route4_res->packRx++;
            route4_res->byteRx += bufS;
            if (port2vrf_res->verify4 > 1) {
                neigh_ntry.id = route4_res->nexthop;
                index = table_find(&neigh_table, &neigh_ntry);
                if (index < 0) doPunting;
                neigh_res = table_get(&neigh_table, index);
                if (neigh_res->aclport != prt) doPunting;
            }
        }
        acls_ntry.dir = 1;
        acls_ntry.port = prt;
        index = table_find(&acls4_table, &acls_ntry);
        if (index >= 0) {
            if (frag != 0) doPunting;
            acls_res = table_get(&acls4_table, index);
            insp4_ntry.prot = acl4_ntry.protV;
            insp4_ntry.srcAddr = acl4_ntry.srcAddr;
            insp4_ntry.trgAddr = acl4_ntry.trgAddr;
            insp4_ntry.srcPort = acl4_ntry.srcPortV;
            insp4_ntry.trgPort = acl4_ntry.trgPortV;
            index = table_find(acls_res->insp, &insp4_ntry);
            if (index < 0) {
                tmp = apply_acl(&acls_res->aces, &acl4_ntry, &acl4_matcher, bufS - bufP + preBuff);
                if (tmp == 2) doCpuing;
                if (tmp != 0) doPunting;
            } else {
                insp4_res = table_get(acls_res->insp, index);
                insp4_res->packRx++;
                insp4_res->byteRx += bufS;
            }
        }
        acls_ntry.dir = 6;
        index = table_find(&acls4_table, &acls_ntry);
        if (index >= 0) {
            if (frag != 0) doPunting;
            acls_res = table_get(&acls4_table, index);
            aceh_res = search_ace(&acls_res->aces, &acl4_ntry, &acl4_matcher, bufS - bufP + preBuff);
            if (aceh_res == NULL) goto ipv4_qosed;
            if (aceh_res->act != 0) goto ipv4_qosed;
            policer_ntry.vrf = 0;
            policer_ntry.meter = aceh_res->nexthop;
            policer_ntry.dir = 1;
            index = table_find(&policer_table, &policer_ntry);
            if (index < 0) doDropper;
            policer_res = table_get(&policer_table, index);
            if (policer_res->avail < 1) doDropper;
            policer_res->avail -= bufS - bufP + preBuff;
        }
ipv4_qosed:
        acls_ntry.dir = 8;
        acls_ntry.port = vrf2rib_ntry.vrf;
        index = table_find(&acls4_table, &acls_ntry);
        if (index >= 0) {
            acls_res = table_get(&acls4_table, index);
            aceh_res = search_ace(&acls_res->aces, &acl4_ntry, &acl4_matcher, bufS - bufP + preBuff);
            if (aceh_res == NULL) goto ipv4_flwed;
            if (aceh_res->act != 0) goto ipv4_flwed;
            policer_ntry.vrf = vrf2rib_ntry.vrf;
            policer_ntry.meter = aceh_res->pri;
            policer_ntry.dir = 3;
            index = table_find(&policer_table, &policer_ntry);
            if (index < 0) doDropper;
            policer_res = table_get(&policer_table, index);
            if (policer_res->avail < 1) doDropper;
            policer_res->avail -= bufS - bufP + preBuff;
        }
ipv4_flwed:
        nat4_ntry.prot = acl4_ntry.protV;
        nat4_ntry.oSrcAddr = acl4_ntry.srcAddr;
        nat4_ntry.oTrgAddr = acl4_ntry.trgAddr;
        nat4_ntry.oSrcPort = acl4_ntry.srcPortV;
        nat4_ntry.oTrgPort = acl4_ntry.trgPortV;
        index = table_find(&vrf2rib_res->nat, &nat4_ntry);
        if (index >= 0) {
            if (frag != 0) goto ipv4_natted;
            nat4_res = table_get(&vrf2rib_res->nat, index);
            nat4_res->pack++;
            nat4_res->byte += bufS;
            acl4_ntry.srcAddr = nat4_res->nSrcAddr;
            acl4_ntry.trgAddr = route4_ntry.addr = nat4_res->nTrgAddr;
            acl4_ntry.srcPortV = nat4_res->nSrcPort;
            acl4_ntry.trgPortV = nat4_res->nTrgPort;
            put32msb(bufD, bufP + 12, acl4_ntry.srcAddr);
            put32msb(bufD, bufP + 16, acl4_ntry.trgAddr);
            update_chksum(bufP + 10, nat4_res->sum3);
            update_layer4(nat4_res);
        } else {
            acls_ntry.dir = 3;
            acls_ntry.port = vrf2rib_ntry.vrf;
            index = table_find(&acls4_table, &acls_ntry);
            if (index < 0) goto ipv4_natted;
            if (frag != 0) goto ipv4_natted;
            acls_res = table_get(&acls4_table, index);
            if (apply_acl(&acls_res->aces, &acl4_ntry, &acl4_matcher, bufS - bufP + preBuff) == 0) doCpuing;
        }
ipv4_natted:
        acls_ntry.dir = 5;
        acls_ntry.port = vrf2rib_ntry.vrf;
        index = table_find(&acls4_table, &acls_ntry);
        if (index >= 0) {
            if (frag != 0) doPunting;
            acls_res = table_get(&acls4_table, index);
            aceh_res = search_ace(&acls_res->aces, &acl4_ntry, &acl4_matcher, bufS - bufP + preBuff);
            if (aceh_res == NULL) goto ipv4_pbred;
            if (aceh_res->act != 0) goto ipv4_pbred;
            switch (aceh_res->cmd) {
            case 1: // normal
                break;
            case 2: // setvrf
                vrf2rib_ntry.vrf = aceh_res->vrf;
                break;
            case 3: // sethop
                vrf2rib_ntry.vrf = aceh_res->vrf;
                neigh_ntry.id = aceh_res->nexthop;
                index = table_find(&neigh_table, &neigh_ntry);
                if (index < 0) doDropper;
                neigh_res = table_get(&neigh_table, index);
                goto ipv4_tx;
            case 4: // setlab
                ethtyp = ETHERTYPE_MPLS_UCAST;
                bufP -= 4;
                label = 0x100 | ttl | (aceh_res->label << 12);
                put32msb(bufD, bufP, label);
                vrf2rib_ntry.vrf = aceh_res->vrf;
                neigh_ntry.id = aceh_res->nexthop;
                goto ethtyp_tx;
            default:
                doDropper;
            }
            index = table_find(&vrf2rib4_table, &vrf2rib_ntry);
            if (index < 0) doDropper;
            vrf2rib_res = table_get(&vrf2rib4_table, index);
        }
ipv4_pbred:
        if (acl4_ntry.protV == 46) doCpuing;
        vrf2rib_res->pack++;
        vrf2rib_res->byte += bufS;
        route4_res = tree_lpm(&vrf2rib_res->rou, &route4_ntry);
        if (route4_res == NULL) doPunting;
        route4_res->packTx++;
        route4_res->byteTx += bufS;
        switch (route4_res->command) {
        case 1: // route
            neigh_ntry.id = route4_res->nexthop;
            index = table_find(&neigh_table, &neigh_ntry);
            if (index < 0) doDropper;
            neigh_res = table_get(&neigh_table, index);
ipv4_tx:
            acls_ntry.dir = 2;
            acls_ntry.port = neigh_res->aclport;
            index = table_find(&acls4_table, &acls_ntry);
            if (index >= 0) {
                if (frag != 0) doPunting;
                acls_res = table_get(&acls4_table, index);
                insp4_ntry.prot = acl4_ntry.protV;
                insp4_ntry.trgAddr = acl4_ntry.srcAddr;
                insp4_ntry.srcAddr = acl4_ntry.trgAddr;
                insp4_ntry.trgPort = acl4_ntry.srcPortV;
                insp4_ntry.srcPort = acl4_ntry.trgPortV;
                index = table_find(acls_res->insp, &insp4_ntry);
                if (index < 0) {
                    tmp = apply_acl(&acls_res->aces, &acl4_ntry, &acl4_matcher, bufS - bufP + preBuff);
                    if (tmp == 2) doCpuing;
                    if (tmp != 0) doPunting;
                } else {
                    insp4_res = table_get(acls_res->insp, index);
                    insp4_res->packTx++;
                    insp4_res->byteTx += bufS;
                }
            }
            bufP -= 2;
            put16msb(bufD, bufP, ethtyp);
            acls_ntry.dir = 7;
            index = table_find(&acls4_table, &acls_ntry);
            if (index < 0) goto neigh_tx;
            if (frag != 0) doPunting;
            acls_res = table_get(&acls4_table, index);
            aceh_res = search_ace(&acls_res->aces, &acl4_ntry, &acl4_matcher, bufS - bufP + preBuff);
            if (aceh_res == NULL) goto neigh_tx;
            if (aceh_res->act != 0) goto neigh_tx;
            policer_ntry.vrf = 0;
            policer_ntry.meter = aceh_res->nexthop;
            policer_ntry.dir = 2;
            index = table_find(&policer_table, &policer_ntry);
            if (index < 0) doDropper;
            policer_res = table_get(&policer_table, index);
            if (policer_res->avail < 1) doDropper;
            policer_res->avail -= bufS - bufP + preBuff;
            goto neigh_tx;
        case 2: // punt
            tun4_ntry.srcAddr = mroute4_ntry.src = acl4_ntry.srcAddr;
            tun4_ntry.trgAddr = mroute4_ntry.grp = acl4_ntry.trgAddr;
            index = table_find(&vrf2rib_res->mcst, &mroute4_ntry);
            if (index >= 0) {
                mroute4_res = table_get(&vrf2rib_res->mcst, index);
                if (mroute4_res->ingr != prt) doDropper;
                mroute4_res->pack++;
                mroute4_res->byte += bufS;
                doFlood(mroute4_res->flood, encrCtx, hashCtx, hash, bufA, bufB, bufC, bufD, bufP, bufS, bufH, ethtyp, sgt, 0x100 | ttl, port);
                if (mroute4_res->local != 0) doCpuing;
                return;
            }
            tun4_ntry.prot = acl4_ntry.protV;
            tun4_ntry.srcPort = acl4_ntry.srcPortV;
            tun4_ntry.trgPort = acl4_ntry.trgPortV;
            index = table_find(&vrf2rib_res->tun, &tun4_ntry);
            if (index >= 0) {
                if (frag != 0) doPunting;
                tun4_res = table_get(&vrf2rib_res->tun, index);
                doTunneled(tun4_res);
            }
            acls_ntry.dir = 4;
            acls_ntry.port = 0;
            index = table_find(&acls4_table, &acls_ntry);
            if (index >= 0) {
                if (frag != 0) doPunting;
                acls_res = table_get(&acls4_table, index);
                if (apply_acl(&acls_res->aces, &acl4_ntry, &acl4_matcher, bufS - bufP + preBuff) != 0) doDropper;
            }
            doCpuing;
            doRouted(route4_res, 4);
        }
    case ETHERTYPE_IPV6: // ipv6
        if (port2vrf_res == NULL) doDropper;
        if (port2vrf_res->command != 1) doDropper;
        packIpv6[port]++;
        byteIpv6[port] += bufS;
        vrf2rib_ntry.vrf = port2vrf_res->vrf;
ipv6_rx:
        index = table_find(&vrf2rib6_table, &vrf2rib_ntry);
        if (index < 0) doDropper;
        vrf2rib_res = table_get(&vrf2rib6_table, index);
        if ((bufD[bufP + 0] & 0xf0) != 0x60) doDropper;
        ttl = get16msb(bufD, bufP + 4) + 40 + bufP - preBuff;
        if (ttl > bufS) doDropper;
        bufS = ttl;
        bufT = bufP + 40;
        acl6_ntry.sgtV = sgt;
        acl6_ntry.protV = bufD[bufP + 6];
        acl6_ntry.tosV = (get16msb(bufD, bufP + 0) >> 4) & 0xff;
        acl6_ntry.flowV = get32msb(bufD, bufP + 0) & 0xfffff;
        if (acl6_ntry.protV == 44) {
            acl6_ntry.protV = bufD[bufT + 0];
            bufT += 8;
            frag = 1;
        } else {
            frag = 0;
        }
        acl6_ntry.srcAddr1 = get32msb(bufD, bufP + 8);
        acl6_ntry.srcAddr2 = get32msb(bufD, bufP + 12);
        acl6_ntry.srcAddr3 = get32msb(bufD, bufP + 16);
        acl6_ntry.srcAddr4 = get32msb(bufD, bufP + 20);
        route6_ntry.addr1 = acl6_ntry.trgAddr1 = get32msb(bufD, bufP + 24);
        route6_ntry.addr2 = acl6_ntry.trgAddr2 = get32msb(bufD, bufP + 28);
        route6_ntry.addr3 = acl6_ntry.trgAddr3 = get32msb(bufD, bufP + 32);
        route6_ntry.addr4 = acl6_ntry.trgAddr4 = get32msb(bufD, bufP + 36);
        route6_ntry.mask = 128;
        hash ^= acl6_ntry.srcAddr1 ^ acl6_ntry.trgAddr1;
        hash ^= acl6_ntry.srcAddr2 ^ acl6_ntry.trgAddr2;
        hash ^= acl6_ntry.srcAddr3 ^ acl6_ntry.trgAddr3;
        hash ^= acl6_ntry.srcAddr4 ^ acl6_ntry.trgAddr4;
        ttl = bufD[bufP + 7] - 1;
        if (ttl <= 1) doPunting;
        bufD[bufP + 7] = ttl;
        extract_layer4(acl6_ntry, port2vrf_res->tcpmss6);
        if (port2vrf_res->verify6 > 0) {
            sroute6_ntry.addr1 = acl6_ntry.srcAddr1;
            sroute6_ntry.addr2 = acl6_ntry.srcAddr2;
            sroute6_ntry.addr3 = acl6_ntry.srcAddr3;
            sroute6_ntry.addr4 = acl6_ntry.srcAddr4;
            sroute6_ntry.mask = 128;
            route6_res = tree_lpm(&vrf2rib_res->rou, &sroute6_ntry);
            if (route6_res == NULL) doPunting;
            route6_res->packRx++;
            route6_res->byteRx += bufS;
            if (port2vrf_res->verify6 > 1) {
                neigh_ntry.id = route6_res->nexthop;
                index = table_find(&neigh_table, &neigh_ntry);
                if (index < 0) doPunting;
                neigh_res = table_get(&neigh_table, index);
                if (neigh_res->aclport != prt) doPunting;
            }
        }
        acls_ntry.dir = 1;
        acls_ntry.port = prt;
        index = table_find(&acls6_table, &acls_ntry);
        if (index >= 0) {
            if (frag != 0) doPunting;
            acls_res = table_get(&acls6_table, index);
            insp6_ntry.prot = acl6_ntry.protV;
            insp6_ntry.srcAddr1 = acl6_ntry.srcAddr1;
            insp6_ntry.srcAddr2 = acl6_ntry.srcAddr2;
            insp6_ntry.srcAddr3 = acl6_ntry.srcAddr3;
            insp6_ntry.srcAddr4 = acl6_ntry.srcAddr4;
            insp6_ntry.trgAddr1 = acl6_ntry.trgAddr1;
            insp6_ntry.trgAddr2 = acl6_ntry.trgAddr2;
            insp6_ntry.trgAddr3 = acl6_ntry.trgAddr3;
            insp6_ntry.trgAddr4 = acl6_ntry.trgAddr4;
            insp6_ntry.srcPort = acl6_ntry.srcPortV;
            insp6_ntry.trgPort = acl6_ntry.trgPortV;
            index = table_find(acls_res->insp, &insp6_ntry);
            if (index < 0) {
                tmp = apply_acl(&acls_res->aces, &acl6_ntry, &acl6_matcher, bufS - bufP + preBuff);
                if (tmp == 2) doCpuing;
                if (tmp != 0) doPunting;
            } else {
                insp6_res = table_get(acls_res->insp, index);
                insp6_res->packRx++;
                insp6_res->byteRx += bufS;
            }
        }
        acls_ntry.dir = 6;
        index = table_find(&acls6_table, &acls_ntry);
        if (index >= 0) {
            if (frag != 0) doPunting;
            acls_res = table_get(&acls6_table, index);
            aceh_res = search_ace(&acls_res->aces, &acl6_ntry, &acl6_matcher, bufS - bufP + preBuff);
            if (aceh_res == NULL) goto ipv6_qosed;
            if (aceh_res->act != 0) goto ipv6_qosed;
            policer_ntry.vrf = 0;
            policer_ntry.meter = aceh_res->nexthop;
            policer_ntry.dir = 1;
            index = table_find(&policer_table, &policer_ntry);
            if (index < 0) doDropper;
            policer_res = table_get(&policer_table, index);
            if (policer_res->avail < 1) doDropper;
            policer_res->avail -= bufS - bufP + preBuff;
        }
ipv6_qosed:
        acls_ntry.dir = 8;
        acls_ntry.port = vrf2rib_ntry.vrf;
        index = table_find(&acls6_table, &acls_ntry);
        if (index >= 0) {
            acls_res = table_get(&acls6_table, index);
            aceh_res = search_ace(&acls_res->aces, &acl6_ntry, &acl6_matcher, bufS - bufP + preBuff);
            if (aceh_res == NULL) goto ipv6_flwed;
            if (aceh_res->act != 0) goto ipv6_flwed;
            policer_ntry.vrf = vrf2rib_ntry.vrf;
            policer_ntry.meter = aceh_res->pri;
            policer_ntry.dir = 4;
            index = table_find(&policer_table, &policer_ntry);
            if (index < 0) doDropper;
            policer_res = table_get(&policer_table, index);
            if (policer_res->avail < 1) doDropper;
            policer_res->avail -= bufS - bufP + preBuff;
        }
ipv6_flwed:
        nat6_ntry.prot = acl6_ntry.protV;
        nat6_ntry.oSrcAddr1 = acl6_ntry.srcAddr1;
        nat6_ntry.oSrcAddr2 = acl6_ntry.srcAddr2;
        nat6_ntry.oSrcAddr3 = acl6_ntry.srcAddr3;
        nat6_ntry.oSrcAddr4 = acl6_ntry.srcAddr4;
        nat6_ntry.oTrgAddr1 = acl6_ntry.trgAddr1;
        nat6_ntry.oTrgAddr2 = acl6_ntry.trgAddr2;
        nat6_ntry.oTrgAddr3 = acl6_ntry.trgAddr3;
        nat6_ntry.oTrgAddr4 = acl6_ntry.trgAddr4;
        nat6_ntry.oSrcPort = acl6_ntry.srcPortV;
        nat6_ntry.oTrgPort = acl6_ntry.trgPortV;
        index = table_find(&vrf2rib_res->nat, &nat6_ntry);
        if (index >= 0) {
            if (frag != 0) goto ipv6_natted;
            nat6_res = table_get(&vrf2rib_res->nat, index);
            nat6_res->pack++;
            nat6_res->byte += bufS;
            acl6_ntry.srcAddr1 = nat6_res->nSrcAddr1;
            acl6_ntry.srcAddr2 = nat6_res->nSrcAddr2;
            acl6_ntry.srcAddr3 = nat6_res->nSrcAddr3;
            acl6_ntry.srcAddr4 = nat6_res->nSrcAddr4;
            acl6_ntry.trgAddr1 = route6_ntry.addr1 = nat6_res->nTrgAddr1;
            acl6_ntry.trgAddr2 = route6_ntry.addr2 = nat6_res->nTrgAddr2;
            acl6_ntry.trgAddr3 = route6_ntry.addr3 = nat6_res->nTrgAddr3;
            acl6_ntry.trgAddr4 = route6_ntry.addr4 = nat6_res->nTrgAddr4;
            acl6_ntry.srcPortV = nat6_res->nSrcPort;
            acl6_ntry.trgPortV = nat6_res->nTrgPort;
            put32msb(bufD, bufP + 8, acl6_ntry.srcAddr1);
            put32msb(bufD, bufP + 12, acl6_ntry.srcAddr2);
            put32msb(bufD, bufP + 16, acl6_ntry.srcAddr3);
            put32msb(bufD, bufP + 20, acl6_ntry.srcAddr4);
            put32msb(bufD, bufP + 24, acl6_ntry.trgAddr1);
            put32msb(bufD, bufP + 28, acl6_ntry.trgAddr2);
            put32msb(bufD, bufP + 32, acl6_ntry.trgAddr3);
            put32msb(bufD, bufP + 36, acl6_ntry.trgAddr4);
            update_layer4(nat6_res);
        } else {
            acls_ntry.dir = 3;
            acls_ntry.port = vrf2rib_ntry.vrf;
            index = table_find(&acls6_table, &acls_ntry);
            if (index < 0) goto ipv6_natted;
            if (frag != 0) goto ipv6_natted;
            acls_res = table_get(&acls6_table, index);
            if (apply_acl(&acls_res->aces, &acl6_ntry, &acl6_matcher, bufS - bufP + preBuff) == 0) doCpuing;
        }
ipv6_natted:
        acls_ntry.dir = 5;
        acls_ntry.port = vrf2rib_ntry.vrf;
        index = table_find(&acls6_table, &acls_ntry);
        if (index >= 0) {
            if (frag != 0) doPunting;
            acls_res = table_get(&acls6_table, index);
            aceh_res = search_ace(&acls_res->aces, &acl6_ntry, &acl6_matcher, bufS - bufP + preBuff);
            if (aceh_res == NULL) goto ipv6_pbred;
            if (aceh_res->act != 0) goto ipv6_pbred;
            switch (aceh_res->cmd) {
            case 1: // normal
                break;
            case 2: // setvrf
                vrf2rib_ntry.vrf = aceh_res->vrf;
                break;
            case 3: // sethop
                vrf2rib_ntry.vrf = aceh_res->vrf;
                neigh_ntry.id = aceh_res->nexthop;
                index = table_find(&neigh_table, &neigh_ntry);
                if (index < 0) doDropper;
                neigh_res = table_get(&neigh_table, index);
                goto ipv6_tx;
            case 4: // setlab
                ethtyp = ETHERTYPE_MPLS_UCAST;
                bufP -= 4;
                label = 0x100 | ttl | (aceh_res->label << 12);
                put32msb(bufD, bufP, label);
                vrf2rib_ntry.vrf = aceh_res->vrf;
                neigh_ntry.id = aceh_res->nexthop;
                goto ethtyp_tx;
            default:
                doDropper;
            }
            index = table_find(&vrf2rib6_table, &vrf2rib_ntry);
            if (index < 0) doDropper;
            vrf2rib_res = table_get(&vrf2rib6_table, index);
        }
ipv6_pbred:
        if (acl6_ntry.protV == 0) doCpuing;
        vrf2rib_res->pack++;
        vrf2rib_res->byte += bufS;
        route6_res = tree_lpm(&vrf2rib_res->rou, &route6_ntry);
        if (route6_res == NULL) doPunting;
        route6_res->packTx++;
        route6_res->byteTx += bufS;
        switch (route6_res->command) {
        case 1: // route
            neigh_ntry.id = route6_res->nexthop;
            index = table_find(&neigh_table, &neigh_ntry);
            if (index < 0) doDropper;
            neigh_res = table_get(&neigh_table, index);
ipv6_tx:
            acls_ntry.dir = 2;
            acls_ntry.port = neigh_res->aclport;
            index = table_find(&acls6_table, &acls_ntry);
            if (index >= 0) {
                if (frag != 0) doPunting;
                acls_res = table_get(&acls6_table, index);
                insp6_ntry.prot = acl6_ntry.protV;
                insp6_ntry.trgAddr1 = acl6_ntry.srcAddr1;
                insp6_ntry.trgAddr2 = acl6_ntry.srcAddr2;
                insp6_ntry.trgAddr3 = acl6_ntry.srcAddr3;
                insp6_ntry.trgAddr4 = acl6_ntry.srcAddr4;
                insp6_ntry.srcAddr1 = acl6_ntry.trgAddr1;
                insp6_ntry.srcAddr2 = acl6_ntry.trgAddr2;
                insp6_ntry.srcAddr3 = acl6_ntry.trgAddr3;
                insp6_ntry.srcAddr4 = acl6_ntry.trgAddr4;
                insp6_ntry.trgPort = acl6_ntry.srcPortV;
                insp6_ntry.srcPort = acl6_ntry.trgPortV;
                index = table_find(acls_res->insp, &insp6_ntry);
                if (index < 0) {
                    tmp = apply_acl(&acls_res->aces, &acl6_ntry, &acl6_matcher, bufS - bufP + preBuff);
                    if (tmp == 2) doCpuing;
                    if (tmp != 0) doPunting;
                } else {
                    insp6_res = table_get(acls_res->insp, index);
                    insp6_res->packTx++;
                    insp6_res->byteTx += bufS;
                }
            }
            bufP -= 2;
            put16msb(bufD, bufP, ethtyp);
            acls_ntry.dir = 7;
            index = table_find(&acls6_table, &acls_ntry);
            if (index < 0) goto neigh_tx;
            if (frag != 0) doPunting;
            acls_res = table_get(&acls6_table, index);
            aceh_res = search_ace(&acls_res->aces, &acl6_ntry, &acl6_matcher, bufS - bufP + preBuff);
            if (aceh_res == NULL) goto neigh_tx;
            if (aceh_res->act != 0) goto neigh_tx;
            policer_ntry.vrf = 0;
            policer_ntry.meter = aceh_res->nexthop;
            policer_ntry.dir = 2;
            index = table_find(&policer_table, &policer_ntry);
            if (index < 0) doDropper;
            policer_res = table_get(&policer_table, index);
            if (policer_res->avail < 1) doDropper;
            policer_res->avail -= bufS - bufP + preBuff;
            goto neigh_tx;
        case 2: // punt
            tun6_ntry.srcAddr1 = mroute6_ntry.src1 = acl6_ntry.srcAddr1;
            tun6_ntry.srcAddr2 = mroute6_ntry.src2 = acl6_ntry.srcAddr2;
            tun6_ntry.srcAddr3 = mroute6_ntry.src3 = acl6_ntry.srcAddr3;
            tun6_ntry.srcAddr4 = mroute6_ntry.src4 = acl6_ntry.srcAddr4;
            tun6_ntry.trgAddr1 = mroute6_ntry.grp1 = acl6_ntry.trgAddr1;
            tun6_ntry.trgAddr2 = mroute6_ntry.grp2 = acl6_ntry.trgAddr2;
            tun6_ntry.trgAddr3 = mroute6_ntry.grp3 = acl6_ntry.trgAddr3;
            tun6_ntry.trgAddr4 = mroute6_ntry.grp4 = acl6_ntry.trgAddr4;
            index = table_find(&vrf2rib_res->mcst, &mroute6_ntry);
            if (index >= 0) {
                mroute6_res = table_get(&vrf2rib_res->mcst, index);
                if (mroute6_res->ingr != prt) doDropper;
                mroute6_res->pack++;
                mroute6_res->byte += bufS;
                doFlood(mroute6_res->flood, encrCtx, hashCtx, hash, bufA, bufB, bufC, bufD, bufP, bufS, bufH, ethtyp, sgt, 0x100 | ttl, port);
                if (mroute6_res->local != 0) doCpuing;
                return;
            }
            tun6_ntry.prot = acl6_ntry.protV;
            tun6_ntry.srcPort = acl6_ntry.srcPortV;
            tun6_ntry.trgPort = acl6_ntry.trgPortV;
            index = table_find(&vrf2rib_res->tun, &tun6_ntry);
            if (index >= 0) {
                if (frag != 0) doPunting;
                tun6_res = table_get(&vrf2rib_res->tun, index);
                doTunneled(tun6_res);
            }
            acls_ntry.dir = 4;
            acls_ntry.port = 0;
            index = table_find(&acls6_table, &acls_ntry);
            if (index >= 0) {
                if (frag != 0) doPunting;
                acls_res = table_get(&acls6_table, index);
                if (apply_acl(&acls_res->aces, &acl6_ntry, &acl6_matcher, bufS - bufP + preBuff) != 0) doDropper;
            }
            doCpuing;
            doRouted(route6_res, 41);
        }
    case ETHERTYPE_PPPOE_DATA: // pppoe
        packPppoe[port]++;
        bytePppoe[port] += bufS;
        pppoe_ntry.port = prt;
        pppoe_ntry.session = get16msb(bufD, bufP + 2);
        hash ^= pppoe_ntry.session;
        index = table_find(&pppoe_table, &pppoe_ntry);
        if (index < 0) doDropper;
        pppoe_res = table_get(&pppoe_table, index);
        pppoe_res->pack++;
        pppoe_res->byte += bufS;
        bufP += 6;
        ethtyp = get16msb(bufD, bufP);
        if ((ethtyp & 0x8000) != 0) doCpuing;
        ppptyp2ethtyp;
        put16msb(bufD, bufP, ethtyp);
        prt = pppoe_res->aclport;
        goto ethtyp_rx;
    case ETHERTYPE_ROUTEDMAC: // routed bridge
        if (port2vrf_res == NULL) doDropper;
        packBridge[port]++;
        byteBridge[port] += bufS;
        if (port2vrf_res->command != 2) doDropper;
        bridge_ntry.id = port2vrf_res->bridge;
        memmove(&bufH[0], &bufD[bufP], 12);
        bufP += 12;
        bufP += 2;
        goto bridgevpls_rx;
    case ETHERTYPE_POLKA: // polka
        if (port2vrf_res == NULL) doDropper;
        packPolka[port]++;
        bytePolka[port] += bufS;
        index = table_find(&vrf2rib4_table, &vrf2rib_ntry);
        if (index < 0) doDropper;
        vrf2rib_res = table_get(&vrf2rib4_table, index);
        ttl = get16msb(bufD, bufP + 0);
        if ((ttl & 0xff00) != 0) doDropper;
        if ((ttl & 0xff) <= 1) doPunting;
        ttl--;
        bufD[bufP + 1] = ttl;
        polkaPoly_ntry.port = prt;
        index = table_find(&polkaPoly_table, &polkaPoly_ntry);
        if (index < 0) doDropper;
        polkaPoly_res = table_get(&polkaPoly_table, index);
        polkaPoly_res->pack++;
        polkaPoly_res->byte += bufS;
        crc16calc(tmp, polkaPoly_res->tab, bufD, bufP + 4, 14);
        tmp ^= get16msb(bufD, bufP + 18);
        if (tmp == 0) {
            ethtyp = get16msb(bufD, bufP + 2);
            bufP += 20;
            goto etyped_rx;
        }
        polkaIdx_ntry.index = tmp;
        index = table_find(&vrf2rib_res->plk, &polkaIdx_ntry);
        if (index < 0) doDropper;
        polkaIdx_res = table_get(&vrf2rib_res->plk, index);
        polkaIdx_res->pack++;
        polkaIdx_res->byte += bufS;
        neigh_ntry.id = polkaIdx_res->nexthop;
        goto ethtyp_tx;
    case ETHERTYPE_MPOLKA: // mpolka
        if (port2vrf_res == NULL) doDropper;
        packMpolka[port]++;
        byteMpolka[port] += bufS;
        index = table_find(&vrf2rib6_table, &vrf2rib_ntry);
        if (index < 0) doDropper;
        vrf2rib_res = table_get(&vrf2rib6_table, index);
        ttl = get16msb(bufD, bufP + 0);
        if ((ttl & 0xff00) != 0) doDropper;
        if ((ttl & 0xff) <= 1) doPunting;
        ttl--;
        bufD[bufP + 1] = ttl;
        polkaPoly_ntry.port = prt;
        index = table_find(&mpolkaPoly_table, &polkaPoly_ntry);
        if (index < 0) doDropper;
        polkaPoly_res = table_get(&mpolkaPoly_table, index);
        polkaPoly_res->pack++;
        polkaPoly_res->byte += bufS;
        crc16calc(tmp, polkaPoly_res->tab, bufD, bufP + 4, 14);
        tmp ^= get16msb(bufD, bufP + 18);
        for (i = 0; i < vrf2rib_res->plk.size; i++) {
            if ((tmp & bitVals[30 - i]) == 0) continue;
            polkaIdx_res = table_get(&vrf2rib_res->plk, i);
            polkaIdx_res->pack++;
            polkaIdx_res->byte += bufS;
            neigh_ntry.id = polkaIdx_res->nexthop;
            index = table_find(&neigh_table, &neigh_ntry);
            if (index < 0) continue;
            neigh_res = table_get(&neigh_table, index);
            int tmpP = preBuff;
            int tmpS = bufS - bufP + preBuff + 2;
            int tmpE = ethtyp;
            put16msb(bufC, preBuff, tmpE);
            memmove(&bufC[preBuff + 2], &bufD[bufP], tmpS);
            send2neigh(neigh_res, encrCtx, hashCtx, hash, bufC, &tmpP, &tmpS, bufH, &tmpE, sgt);
        }
        if ((tmp & 1) == 0) return;
        ethtyp = get16msb(bufD, bufP + 2);
        bufP += 20;
        goto etyped_rx;
    case ETHERTYPE_NSH: // nsh
        if (port2vrf_res == NULL) doDropper;
        if (port2vrf_res->nsh == 0) doDropper;
        packNsh[port]++;
        byteNsh[port] += bufS;
        ttl = get16msb(bufD, bufP + 0);
        if ((ttl & 0xe000) != 0) doDropper;
        if (((ttl >> 6) & 0x3f) <= 1) doPunting;
        tmp = get32msb(bufD, bufP + 4);
        ethtyp = bufD[bufP + 3];
        nsh_ntry.sp = tmp >> 8;
        nsh_ntry.si = tmp & 0xff;
        hash ^= tmp;
        index = table_find(&nsh_table, &nsh_ntry);
        if (index < 0) doDropper;
        nsh_res = table_get(&nsh_table, index);
        nsh_res->pack++;
        nsh_res->byte += bufS;
        switch (nsh_res->command) {
        case 1: // fwd
            ttl = ttl - 0x40;
            put16msb(bufD, bufP + 0, ttl);
            put32msb(bufD, bufP + 4, nsh_res->trg);
            ethtyp = ETHERTYPE_NSH;
            bufP -= 2;
            put16msb(bufD, bufP, ethtyp);
            memmove(&bufH[0], &nsh_res->dmac, 6);
            memmove(&bufH[6], &nsh_res->smac, 6);
            prt = send2subif(nsh_res->port, encrCtx, hashCtx, hash, bufD, &bufP, &bufS, bufH, &ethtyp, sgt);
            if (prt >= 0) goto ethtyp_rx;
            return;
        case 2: // vrf
            bufP += ((ttl & 0x3f) * 4);
            vrf2rib_ntry.vrf = nsh_res->vrf;
            switch (ethtyp) {
            case 5:
                ethtyp = ETHERTYPE_MPLS_UCAST;
                goto mpls_rx;
            case 1:
                ethtyp = ETHERTYPE_IPV4;
                goto ipv4_rx;
            case 2:
                ethtyp = ETHERTYPE_IPV6;
                goto ipv6_rx;
            }
            return;
        }
        return;
xconn_rx:
        memmove(&bufH[0], &bufD[preBuff], 12);
        bufP -= 2;
        bufP -= 12;
        memmove(&bufD[bufP], &bufH[0], 12);
        ethtyp = ETHERTYPE_MPLS_UCAST;
        bufP -= 4;
        label = 0x1ff | (port2vrf_res->label2 << 12);
        put32msb(bufD, bufP, label);
        bufP -= 4;
        label = 0xff | (port2vrf_res->label1 << 12);
        put32msb(bufD, bufP, label);
        neigh_ntry.id = port2vrf_res->nexthop;
        goto ethtyp_tx;
bridge_rx:
        bridge_ntry.id = port2vrf_res->bridge;
        memmove(&bufH[0], &bufD[preBuff], 12);
        goto bridgevpls_rx;
bridgevpls_rx:
        bridge_ntry.mac1 = get16msb(bufH, 6);
        bridge_ntry.mac2 = get32msb(bufH, 8);
        hash ^= bridge_ntry.mac1 ^ bridge_ntry.mac2;
        index = table_find(&bridge_table, &bridge_ntry);
        if (index < 0) doCpuing;
        bridge_res = table_get(&bridge_table, index);
        bridge_res->packRx++;
        bridge_res->byteRx += bufS;
        bridge_ntry.mac1 = get16msb(bufH, 0);
        bridge_ntry.mac2 = get32msb(bufH, 2);
        hash ^= bridge_ntry.mac1 ^ bridge_ntry.mac2;
        index = table_find(&bridge_table, &bridge_ntry);
        if (index < 0) doCpuing;
        bridge_res = table_get(&bridge_table, index);
        bridge_res->packTx++;
        bridge_res->byteTx += bufS;
        bufP -= 2;
        switch (bridge_res->command) {
        case 1: // port
            break;
        case 2: // vpls
            bufP -= 12;
            memmove(&bufD[bufP], &bufH[0], 12);
            ethtyp = ETHERTYPE_MPLS_UCAST;
            bufP -= 4;
            label = 0x1ff | (bridge_res->label2 << 12);
            put32msb(bufD, bufP, label);
            bufP -= 4;
            label = 0xff | (bridge_res->label1 << 12);
            put32msb(bufD, bufP, label);
            neigh_ntry.id = bridge_res->nexthop;
            goto ethtyp_tx;
        case 3: // routed
            bufP -= 12;
            memmove(&bufD[bufP], &bufH[0], 12);
            ethtyp = ETHERTYPE_ROUTEDMAC;
            neigh_ntry.id = bridge_res->nexthop;
            goto ethtyp_tx;
        case 4: // vxlan4
            putVxlanHeader;
            putUdpHeader(bufP, bufS, 4789, 4789, bridge_res->srcAddr1, 0, 0, 0, bridge_res->trgAddr1, 0, 0, 0);
            putIpv4header(bufP, bufS, ethtyp, 17, bridge_res->srcAddr1, bridge_res->trgAddr1);
            neigh_ntry.id = bridge_res->nexthop;
            goto nethtyp_tx;
        case 5: // vxlan6
            putVxlanHeader;
            putUdpHeader(bufP, bufS, 4789, 4789, bridge_res->srcAddr1, bridge_res->srcAddr2, bridge_res->srcAddr3, bridge_res->srcAddr4, bridge_res->trgAddr1, bridge_res->trgAddr2, bridge_res->trgAddr3, bridge_res->trgAddr4);
            putIpv6header(bufP, bufS, ethtyp, 17, bridge_res->srcAddr1, bridge_res->srcAddr2, bridge_res->srcAddr3, bridge_res->srcAddr4, bridge_res->trgAddr1, bridge_res->trgAddr2, bridge_res->trgAddr3, bridge_res->trgAddr4);
            neigh_ntry.id = bridge_res->nexthop;
            goto nethtyp_tx;
        case 6: // pckoudp4
            putPckoudpHeader;
            putUdpHeader(bufP, bufS, bridge_res->srcPort, bridge_res->trgPort, bridge_res->srcAddr1, 0, 0, 0, bridge_res->trgAddr1, 0, 0, 0);
            putIpv4header(bufP, bufS, ethtyp, 17, bridge_res->srcAddr1, bridge_res->trgAddr1);
            neigh_ntry.id = bridge_res->nexthop;
            goto nethtyp_tx;
        case 7: // pckoudp6
            putPckoudpHeader;
            putUdpHeader(bufP, bufS, bridge_res->srcPort, bridge_res->trgPort, bridge_res->srcAddr1, bridge_res->srcAddr2, bridge_res->srcAddr3, bridge_res->srcAddr4, bridge_res->trgAddr1, bridge_res->trgAddr2, bridge_res->trgAddr3, bridge_res->trgAddr4);
            putIpv6header(bufP, bufS, ethtyp, 17, bridge_res->srcAddr1, bridge_res->srcAddr2, bridge_res->srcAddr3, bridge_res->srcAddr4, bridge_res->trgAddr1, bridge_res->trgAddr2, bridge_res->trgAddr3, bridge_res->trgAddr4);
            neigh_ntry.id = bridge_res->nexthop;
            goto nethtyp_tx;
        case 8: // srv4
            putPckoudpHeader;
            putIpv4header(bufP, bufS, ethtyp, 143, bridge_res->srcAddr1, bridge_res->trgAddr1);
            neigh_ntry.id = bridge_res->nexthop;
            goto nethtyp_tx;
        case 9: // srv6
            putPckoudpHeader;
            putIpv6header(bufP, bufS, ethtyp, 143, bridge_res->srcAddr1, bridge_res->srcAddr2, bridge_res->srcAddr3, bridge_res->srcAddr4, bridge_res->trgAddr1, bridge_res->trgAddr2, bridge_res->trgAddr3, bridge_res->trgAddr4);
            neigh_ntry.id = bridge_res->nexthop;
            goto nethtyp_tx;
        default:
            return;
        }
        prt = bridge_res->port;
        prt = send2subif(prt, encrCtx, hashCtx, hash, bufD, &bufP, &bufS, bufH, &ethtyp, sgt);
        if (prt >= 0) goto ethtyp_rx;
        return;
    case ETHERTYPE_ARP: // arp
        doCpuing;
    case ETHERTYPE_PPPOE_CTRL: // pppoe ctrl
        doCpuing;
    case ETHERTYPE_MACSEC: // macsec
        doCpuing;
    case ETHERTYPE_LACP: // lacp
        doCpuing;
    case ETHERTYPE_LLDP: // lldp
        doCpuing;
    default:
        if (ethtyp < 1500) doCpuing;
punt:
        if (punts < 0) {
drop:
            packDr[port]++;
            byteDr[port] += bufS - bufP + preBuff;
            return;
        }
        punts--;
cpu:
        send2cpu(bufD, bufE, bufS, prt);
        return;
    }
}


void processCpuPack(unsigned char *bufA, unsigned char *bufB, unsigned char *bufC, unsigned char* bufD, int bufS, EVP_CIPHER_CTX *encrCtx, EVP_MD_CTX *hashCtx) {
    packRx[cpuport]++;
    byteRx[cpuport] += bufS;
    int prt = get16msb(bufD, preBuff + 0);
    int hash = get32msb(bufD, preBuff + 2) ^ get32msb(bufD, preBuff + 6) ^ get32msb(bufD, preBuff + 10);
    int ethtyp = get16msb(bufD, preBuff + 14);
    int bufP = preBuff + 14;
    memmove(&bufC[0], &bufD[preBuff + 2], 12);
    prt = send2subif(prt, NULL, NULL, hash, bufD, &bufP, &bufS, bufC, &ethtyp, -1);
    if (prt < 0) return;
    processDataPacket(bufA, bufB, bufC, bufD, bufS, cpuport, prt, encrCtx, hashCtx);
}


#endif
