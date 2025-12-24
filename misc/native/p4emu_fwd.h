#ifdef HAVE_DEBUG

int dropStat[4096];

#define doDropper {dropStat[__LINE__]++;goto drop;}
#define doPunting {dropStat[__LINE__]++;goto punt;}
#define doCpuing {dropStat[__LINE__]++;goto cpu;}

#else

#define doDropper goto drop;
#define doPunting goto punt;
#define doCpuing goto cpu;

#endif




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




#define send2port(prt)                                          \
    if (prt < 0) return;                                        \
    if (prt >= dataPorts) return;                               \
    ifaceStat[prt]->packTx++;                                   \
    ifaceStat[prt]->byteTx += bufS - bufP + preBuff;            \
    sendPack(&bufD[bufP], bufS - bufP + preBuff, prt);






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


#define extract_layer4(acl)                                     \
    switch (acl.protV) {                                        \
        case IP_PROTOCOL_TCP:                                   \
            acl.srcPortV = get16msb(bufD, bufT + 0);            \
            acl.trgPortV = get16msb(bufD, bufT + 2);            \
            break;                                              \
        case IP_PROTOCOL_UDP:                                   \
            acl.srcPortV = get16msb(bufD, bufT + 0);            \
            acl.trgPortV = get16msb(bufD, bufT + 2);            \
            break;                                              \
        default:                                                \
            acl.srcPortV = 0;                                   \
            acl.trgPortV = 0;                                   \
            break;                                              \
    }                                                           \
    ctx->hash ^= acl.protV ^ acl.srcPortV ^ acl.trgPortV;


#define update_tcpMss(ntry, mss)                                \
    if (ntry.protV == 6) if (mss > 0) if ((bufD[bufT + 13] & 2) != 0) adjustMss(bufD, bufT, mss);




#define update_layer4(ntry)                                     \
    switch (ntry->prot) {                                       \
        case IP_PROTOCOL_TCP:                                   \
            put16msb(bufD, bufT + 0, ntry->nSrcPort);           \
            put16msb(bufD, bufT + 2, ntry->nTrgPort);           \
            update_chksum(bufT + 16, ntry->sum4);               \
            break;                                              \
        case IP_PROTOCOL_UDP:                                   \
            put16msb(bufD, bufT + 0, ntry->nSrcPort);           \
            put16msb(bufD, bufT + 2, ntry->nTrgPort);           \
            if (get16msb(bufD, bufT + 6) == 0) break;           \
            update_chksum(bufT + 6, ntry->sum4);                \
            break;                                              \
    }



#define ethtyp2ppptyp                                           \
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
    case ETHERTYPE_NSH:                                         \
        ethtyp = PPPTYPE_NSH;                                   \
        break;                                                  \
    case ETHERTYPE_ROUTEDMAC:                                   \
        bufP -= 2;                                              \
        put16msb(bufD, bufP, 1);                                \
        ethtyp = PPPTYPE_ROUTEDMAC;                             \
        break;                                                  \
    case ETHERTYPE_POLKA:                                       \
        ethtyp = PPPTYPE_POLKA;                                 \
        break;                                                  \
    default:                                                    \
        doDropper;                                              \
    }




#define ppptyp2ethtyp(bufP)                                     \
    if ((ethtyp & 0x8000) != 0) doCpuing;                       \
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
    case PPPTYPE_NSH:                                           \
        ethtyp = ETHERTYPE_NSH;                                 \
        break;                                                  \
    case PPPTYPE_ROUTEDMAC:                                     \
        ethtyp = ETHERTYPE_ROUTEDMAC;                           \
        bufP += 2;                                              \
        break;                                                  \
    case PPPTYPE_POLKA:                                         \
        ethtyp = ETHERTYPE_POLKA;                               \
        break;                                                  \
    default:                                                    \
        doDropper;                                              \
    }



#define ethtyp2iproto                                           \
    switch (ethtyp) {                                           \
    case ETHERTYPE_MPLS_UCAST:                                  \
        tmp = IP_PROTOCOL_MPLS;                                 \
        break;                                                  \
    case ETHERTYPE_IPV4:                                        \
        tmp = IP_PROTOCOL_IPV4;                                 \
        break;                                                  \
    case ETHERTYPE_IPV6:                                        \
        tmp = IP_PROTOCOL_IPV6;                                 \
        break;                                                  \
    case ETHERTYPE_MACSEC:                                      \
        tmp = IP_PROTOCOL_SWIPE;                                \
        break;                                                  \
    case ETHERTYPE_SGT:                                         \
        tmp = IP_PROTOCOL_SKIP;                                 \
        break;                                                  \
    case ETHERTYPE_NSH:                                         \
        tmp = IP_PROTOCOL_NSH;                                  \
        break;                                                  \
    case ETHERTYPE_ROUTEDMAC:                                   \
        tmp = IP_PROTOCOL_SRL2;                                 \
        break;                                                  \
    default:                                                    \
        doDropper;                                              \
    }                                                           \
    bufP += 2;



#define iproto2ethtyp                                           \
    switch (ethtyp) {                                           \
    case IP_PROTOCOL_MPLS:                                      \
        ethtyp = ETHERTYPE_MPLS_UCAST;                          \
        break;                                                  \
    case IP_PROTOCOL_IPV4:                                      \
        ethtyp = ETHERTYPE_IPV4;                                \
        break;                                                  \
    case IP_PROTOCOL_IPV6:                                      \
        ethtyp = ETHERTYPE_IPV6;                                \
        break;                                                  \
    case IP_PROTOCOL_SWIPE:                                     \
        ethtyp = ETHERTYPE_MACSEC;                              \
        break;                                                  \
    case IP_PROTOCOL_SKIP:                                      \
        ethtyp = ETHERTYPE_SGT;                                 \
        break;                                                  \
    case IP_PROTOCOL_NSH:                                       \
        ethtyp = ETHERTYPE_NSH;                                 \
        break;                                                  \
    case IP_PROTOCOL_SRL2:                                      \
        ethtyp = ETHERTYPE_ROUTEDMAC;                           \
        break;                                                  \
    default:                                                    \
        doDropper;                                              \
    }



#define putPppoeHeader                                          \
    put16msb(bufD, bufP, ethtyp);                               \
    tmp = bufS - bufP + preBuff;                                \
    bufP -= 6;                                                  \
    put16msb(bufD, bufP + 0, 0x1100);                           \
    put16msb(bufD, bufP + 2, neigh_res->tid);                   \
    put16msb(bufD, bufP + 4, tmp);                              \
    ethtyp = ETHERTYPE_PPPOE_DATA;                              \
    bufP -= 2;                                                  \
    put16msb(bufD, bufP, ethtyp);






#define putIpv4header(proto, sip, dip)                          \
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
    ethtyp = 0xffff & (~ethtyp);                                \
    put16msb(bufD, bufP + 10, ethtyp);                          \
    put32msb(bufD, bufP + 12, sip);                             \
    put32msb(bufD, bufP + 16, dip);                             \
    ethtyp = ETHERTYPE_IPV4;                                    \
    bufP -= 2;                                                  \
    put16msb(bufD, bufP, ethtyp);



#define putIpv6header(proto, sip1, sip2, sip3, sip4, dip1, dip2, dip3, dip4)    \
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



#define putGreHeader                                            \
    bufP -= 2;                                                  \
    put16msb(bufD, bufP, 0x0000);



#define putTmuxHeader                                           \
    bufP -= 4;                                                  \
    ethtyp = bufS - bufP + preBuff;                             \
    put16msb(bufD, bufP, ethtyp);                               \
    bufD[bufP + 2] = tmp;                                       \
    bufD[bufP + 3] = tmp ^ (ethtyp & 0xff) ^ (ethtyp >> 8);




#define putUdpHeader(sprt, dprt)                                \
    bufP -= 8;                                                  \
    put16msb(bufD, bufP + 0, sprt);                             \
    put16msb(bufD, bufP + 2, dprt);                             \
    tmp = bufS - bufP + preBuff;                                \
    put16msb(bufD, bufP + 4, tmp);                              \
    put16msb(bufD, bufP + 6, 0);



#define putL2tpHeader                                           \
    put16msb(bufD, bufP, ethtyp);                               \
    bufP -= 10;                                                 \
    put16msb(bufD, bufP + 0, 0x0202);                           \
    put32msb(bufD, bufP + 2, neigh_res->tid);                   \
    put16msb(bufD, bufP + 6, 0);                                \
    put16msb(bufD, bufP + 8, 0xff03);


#define putL3tpHeader                                           \
    put16msb(bufD, bufP, ethtyp);                               \
    bufP -= 4;                                                  \
    put32msb(bufD, bufP + 0, neigh_res->tid);


#define putVxlanHeader                                          \
    bufP -= 12;                                                 \
    memcpy(&bufD[bufP], &bufH[0], 12);                          \
    bufP -= 8;                                                  \
    put16msb(bufD, bufP + 0, 0x800);                            \
    put16msb(bufD, bufP + 2, 0);                                \
    tmp = bridge_res->instance << 8;                            \
    put32msb(bufD, bufP + 4, tmp);


#define putEtheripHeader                                        \
    bufP -= 12;                                                 \
    memcpy(&bufD[bufP], &bufH[0], 12);                          \
    bufP -= 2;                                                  \
    put16msb(bufD, bufP + 0, 0x300);


#define putEoipHeader                                           \
    bufP -= 12;                                                 \
    memcpy(&bufD[bufP], &bufH[0], 12);                          \
    tmp = bufS - bufP + preBuff;                                \
    bufP -= 8;                                                  \
    put16msb(bufD, bufP + 0, 0x2001);                           \
    put16msb(bufD, bufP + 2, 0x6400);                           \
    put16msb(bufD, bufP + 4, tmp);                              \
    put16lsb(bufD, bufP + 6, bridge_res->instance);


#define putPckoudpHeader                                        \
    bufP -= 12;                                                 \
    memcpy(&bufD[bufP], &bufH[0], 12);



#define guessEthtyp                                             \
    switch (bufD[*bufP] & 0xf0) {                               \
        case 0x40:                                              \
            ethtyp = ETHERTYPE_IPV4;                            \
            break;                                              \
        case 0x60:                                              \
            ethtyp = ETHERTYPE_IPV6;                            \
            break;                                              \
    default:                                                    \
        doDropper;                                              \
    }



#define putAmtHeader                                            \
    put16msb(bufD, bufP, 0x600);



#define putGtpHeader                                            \
    bufP -= 6;                                                  \
    put16msb(bufD, bufP + 0, 0x30ff);                           \
    tmp = bufS - bufP + preBuff - 8;                            \
    put16msb(bufD, bufP + 2, tmp);                              \
    put32msb(bufD, bufP + 4, neigh_res->tid);




#ifndef HAVE_NOCRYPTO


int putWireguardHeader(struct packetContext *ctx, struct neigh_entry *neigh_res, int *bufP, int *bufS) {
    unsigned char *bufD = ctx->bufD;
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
    if (EVP_CIPHER_CTX_reset(ctx->encr) != 1) doDropper;
    if (EVP_EncryptInit_ex(ctx->encr, EVP_chacha20_poly1305(), NULL, neigh_res->encrKeyDat, &bufD[0]) != 1) doDropper;
    if (EVP_CIPHER_CTX_set_padding(ctx->encr, 0) != 1) doDropper;
    if (EVP_EncryptUpdate(ctx->encr, &bufD[*bufP], &tmp2, &bufD[*bufP], tmp) != 1) doDropper;
    if (EVP_EncryptFinal_ex(ctx->encr, &bufD[*bufP + tmp], &tmp2) != 1) doDropper;
    if (EVP_CIPHER_CTX_ctrl(ctx->encr, EVP_CTRL_AEAD_GET_TAG, 16, &bufD[*bufP + tmp]) != 1) doDropper;
    *bufS += 16;
    *bufP -= 16;
    put32lsb(bufD, *bufP + 0, 4);
    put32msb(bufD, *bufP + 4, neigh_res->tid);
    put32lsb(bufD, *bufP + 8, seq);
    put32lsb(bufD, *bufP + 12, 0);
    return 0;
drop:
    return 1;
}


int putOpenvpnHeader(struct packetContext *ctx, struct neigh_entry *neigh_res, int *bufP, int *bufS) {
    unsigned char *bufD = ctx->bufD;
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
    RAND_bytes(&bufD[*bufP - neigh_res->encrBlkLen], neigh_res->encrBlkLen);
    if (EVP_CIPHER_CTX_reset(ctx->encr) != 1) doDropper;
    if (EVP_EncryptInit_ex(ctx->encr, neigh_res->encrAlg, NULL, neigh_res->encrKeyDat, &bufD[*bufP - neigh_res->encrBlkLen]) != 1) doDropper;
    if (EVP_CIPHER_CTX_set_padding(ctx->encr, 0) != 1) doDropper;
    if (EVP_EncryptUpdate(ctx->encr, &bufD[*bufP], &tmp2, &bufD[*bufP], tmp) != 1) doDropper;
    *bufP -= neigh_res->encrBlkLen;
    tmp += neigh_res->encrBlkLen;
    if (neigh_res->hashBlkLen < 1) return 0;
    if (myHmacInit(ctx->dgst, neigh_res->hashAlg, neigh_res->hashKeyDat, neigh_res->hashKeyLen) != 1) doDropper;
    if (EVP_DigestUpdate(ctx->dgst, &bufD[*bufP], tmp) != 1) doDropper;
    *bufP -= neigh_res->hashBlkLen;
    if (myHmacEnd(ctx->dgst, neigh_res->hashAlg, neigh_res->hashKeyDat, neigh_res->hashKeyLen, &bufD[*bufP]) != 1) doDropper;
    return 0;
drop:
    return 1;
}



int putEspHeader(struct packetContext *ctx, struct neigh_entry *neigh_res, int *bufP, int *bufS, int ethtyp) {
    unsigned char *bufD = ctx->bufD;
    int seq = neigh_res->seq;
    neigh_res->seq++;
    int tmp = *bufS - *bufP + preBuff;
    int tmp2;
    if (neigh_res->encrTagLen > 0) {
        tmp2 = 4 - ((tmp + 2) & 3);
    } else {
        tmp2 = neigh_res->encrBlkLen - ((tmp + 2) % neigh_res->encrBlkLen);
    }
    for (int i=0; i<tmp2; i++) {
        bufD[*bufP + tmp + i] = i+1;
    }
    tmp += tmp2;
    *bufS += tmp2;
    bufD[*bufP + tmp + 0] = tmp2;
    bufD[*bufP + tmp + 1] = ethtyp;
    tmp += 2;
    *bufS += 2;
    if (neigh_res->encrTagLen > 0) {
        memcpy(&bufD[0], neigh_res->hashKeyDat, 4);
        RAND_bytes(&bufD[4], 8);
        put32msb(bufD, *bufP - 16, neigh_res->tid);
        put32msb(bufD, *bufP - 12, seq);
        memcpy(&bufD[*bufP - 8], &bufD[4], 8);
        if (EVP_CIPHER_CTX_reset(ctx->encr) != 1) doDropper;
        if (EVP_EncryptInit_ex(ctx->encr, neigh_res->encrAlg, NULL, neigh_res->encrKeyDat, bufD) != 1) doDropper;
        if (EVP_CIPHER_CTX_set_padding(ctx->encr, 0) != 1) doDropper;
        if (EVP_EncryptUpdate(ctx->encr, NULL, &tmp2, &bufD[*bufP - 16], 8) != 1) doDropper;
        if (EVP_EncryptUpdate(ctx->encr, &bufD[*bufP], &tmp2, &bufD[*bufP], tmp) != 1) doDropper;
        if (EVP_EncryptFinal_ex(ctx->encr, &bufD[*bufP + tmp], &tmp2) != 1) doDropper;
        if (EVP_CIPHER_CTX_ctrl(ctx->encr, EVP_CTRL_GCM_GET_TAG, neigh_res->encrTagLen, &bufD[*bufP + tmp]) != 1) doDropper;
        *bufS += neigh_res->encrTagLen;
        *bufP -= 16;
        return 0;
    }
    RAND_bytes(&bufD[*bufP - neigh_res->encrBlkLen], neigh_res->encrBlkLen);
    if (EVP_CIPHER_CTX_reset(ctx->encr) != 1) doDropper;
    if (EVP_EncryptInit_ex(ctx->encr, neigh_res->encrAlg, NULL, neigh_res->encrKeyDat, &bufD[*bufP - neigh_res->encrBlkLen]) != 1) doDropper;
    if (EVP_CIPHER_CTX_set_padding(ctx->encr, 0) != 1) doDropper;
    if (EVP_EncryptUpdate(ctx->encr, &bufD[*bufP], &tmp2, &bufD[*bufP], tmp) != 1) doDropper;
    tmp += neigh_res->encrBlkLen;
    *bufP -= neigh_res->encrBlkLen;
    *bufP -= 8;
    put32msb(bufD, *bufP + 0, neigh_res->tid);
    put32msb(bufD, *bufP + 4, seq);
    if (neigh_res->hashBlkLen < 1) return 0;
    tmp += 8;
    if (myHmacInit(ctx->dgst, neigh_res->hashAlg, neigh_res->hashKeyDat, neigh_res->hashKeyLen) != 1) doDropper;
    if (EVP_DigestUpdate(ctx->dgst, &bufD[*bufP], tmp) != 1) doDropper;
    if (myHmacEnd(ctx->dgst, neigh_res->hashAlg, neigh_res->hashKeyDat, neigh_res->hashKeyLen, &bufD[*bufP + tmp]) != 1) doDropper;
    *bufS += neigh_res->hashBlkLen;
    return 0;
drop:
    return 1;
}


#endif



int macsec_apply(struct packetContext *ctx, int prt, int *bufP, int *bufS, int *ethtyp) {
    if (ctx->sgt < 0) return 0;
    unsigned char *bufD = ctx->bufD;
    struct port2vrf_entry port2vrf_ntry;
    struct policer_entry policer_ntry;
    struct port2vrf_entry *port2vrf_res;
    struct policer_entry *policer_res;
    port2vrf_ntry.port = prt;
    port2vrf_res = hasht_find(&port2vrf_table, &port2vrf_ntry);
    if (port2vrf_res == NULL) return 0;
    if (port2vrf_res->rateOut != 0) {
        policer_ntry.vrf = 0;
        policer_ntry.meter = port2vrf_res->rateOut;
        policer_ntry.dir = 6;
        policer_res = hasht_find(&policer_table, &policer_ntry);
        if (policer_res == NULL) doDropper;
        if (policer_res->avail < 1) doDropper;
        policer_res->avail -= *bufS - *bufP + preBuff;
    }
    if (port2vrf_res->sgtTag != 0) {
        *bufP -= 8;
        put16msb(bufD, *bufP + 2, 0x0101);
        put16msb(bufD, *bufP + 4, 0x0001);
        put16msb(bufD, *bufP + 6, ctx->sgt);
        *ethtyp = ETHERTYPE_SGT;
        put16msb(bufD, *bufP + 0, *ethtyp);
    }
#ifndef HAVE_NOCRYPTO
    if (port2vrf_res->mcscEthtyp == 0) return 0;
    unsigned char *bufH = ctx->bufH;
    port2vrf_res->mcscPackTx++;
    port2vrf_res->mcscByteTx += *bufS;
    int seq = port2vrf_res->mcscSeqTx++;
    int tmp = *bufS - *bufP + preBuff;
    int tmp2 = tmp % port2vrf_res->mcscEncrBlkLen;
    if (tmp2 > 0) {
        tmp2 = port2vrf_res->mcscEncrBlkLen - tmp2;
        memset(&bufD[*bufP + tmp], 0, tmp2);
        *bufS += tmp2;
        tmp += tmp2;
    }
    if (EVP_CIPHER_CTX_reset(ctx->encr) != 1) doDropper;
    memcpy(&bufD[0], port2vrf_res->mcscIvTxKeyDat, port2vrf_res->mcscIvTxKeyLen);
    put32msb(bufD, port2vrf_res->mcscIvTxKeyLen, seq);
    if (EVP_EncryptInit_ex(ctx->encr, port2vrf_res->mcscEncrAlg, NULL, port2vrf_res->mcscCrTxKeyDat, bufD) != 1) doDropper;
    if (EVP_CIPHER_CTX_set_padding(ctx->encr, 0) != 1) doDropper;
    tmp2 = 0;
    if (tmp < 48) {
        tmp2 = tmp;
    }
    put16msb(bufD, 0, port2vrf_res->mcscEthtyp);
    bufD[2] = 0x0c; // tci
    bufD[3] = tmp2; // sl
    put32msb(bufD, 4, seq);
    if (port2vrf_res->mcscNeedAead != 0) {
        if (port2vrf_res->mcscNeedMacs != 0) {
            if (EVP_EncryptUpdate(ctx->encr, NULL, &tmp2, &bufH[0], 12) != 1) doDropper;
        }
        if (EVP_EncryptUpdate(ctx->encr, NULL, &tmp2, &bufD[0], 8) != 1) doDropper;
        if (EVP_EncryptUpdate(ctx->encr, &bufD[*bufP], &tmp2, &bufD[*bufP], tmp) != 1) doDropper;
        if (EVP_EncryptFinal_ex(ctx->encr, &bufD[*bufP + tmp], &tmp2) != 1) doDropper;
        if (EVP_CIPHER_CTX_ctrl(ctx->encr, EVP_CTRL_GCM_GET_TAG, port2vrf_res->mcscEncrTagLen, &bufD[*bufP + tmp]) != 1) doDropper;
        tmp += port2vrf_res->mcscEncrTagLen;
        *bufS += port2vrf_res->mcscEncrTagLen;
    } else {
        if (EVP_EncryptUpdate(ctx->encr, &bufD[*bufP], &tmp2, &bufD[*bufP], tmp) != 1) doDropper;
    }
    if (port2vrf_res->mcscHashBlkLen > 0) {
        if (myHmacInit(ctx->dgst, port2vrf_res->mcscHashAlg, port2vrf_res->mcscDgTxKeyDat, port2vrf_res->mcscDgTxKeyLen) != 1) doDropper;
        if (port2vrf_res->mcscNeedMacs != 0) {
            if (EVP_DigestUpdate(ctx->dgst, &bufH[0], 12) != 1) doDropper;
        }
        if (EVP_DigestUpdate(ctx->dgst, &bufD[0], 8) != 1) doDropper;
        if (EVP_DigestUpdate(ctx->dgst, &bufD[*bufP], tmp) != 1) doDropper;
        if (myHmacEnd(ctx->dgst, port2vrf_res->mcscHashAlg, port2vrf_res->mcscDgTxKeyDat, port2vrf_res->mcscDgTxKeyLen, &bufD[*bufP + tmp]) != 1) doDropper;
        *bufS += port2vrf_res->mcscHashBlkLen;
    }
    *bufP -= 8;
    *ethtyp = port2vrf_res->mcscEthtyp;
    memcpy(&bufD[*bufP], &bufD[0], 8);
#endif
    return 0;
drop:
    return 1;
}


#define putMacAddr                                  \
    bufP -= 12;                                     \
    memcpy(&bufD[bufP], &bufH[0], 12);



void send2subif(struct packetContext *ctx, int prt, int bufP, int bufS, int ethtyp) {
    if (macsec_apply(ctx, prt, &bufP, &bufS, &ethtyp) != 0) return;
    unsigned char *bufD = ctx->bufD;
    unsigned char *bufH = ctx->bufH;
    struct vlanout_entry vlanout_ntry;
    struct bundle_entry bundle_ntry;
    struct vlanout_entry *vlanout_res;
    struct bundle_entry *bundle_res;
    vlanout_ntry.id = prt;
    vlanout_res = hasht_find(&vlanout_table, &vlanout_ntry);
    if (vlanout_res != NULL) {
        ctx->hash ^= vlanout_res->vlan;
        bufP -= 2;
        put16msb(bufD, bufP, vlanout_res->vlan);
        bufP -= 2;
        put16msb(bufD, bufP, ETHERTYPE_VLAN);
        if (vlanout_res->vlan2 > 0) {
            prt = vlanout_res->port2;
            ethtyp = ETHERTYPE_VLAN;
            if (macsec_apply(ctx, prt, &bufP, &bufS, &ethtyp) != 0) return;
            ctx->hash ^= vlanout_res->vlan2;
            bufP -= 2;
            put16msb(bufD, bufP, vlanout_res->vlan2);
            bufP -= 2;
            put16msb(bufD, bufP, ETHERTYPE_VLAN);
        }
        prt = vlanout_res->port;
        vlanout_res->pack++;
        vlanout_res->byte += bufS;
        ethtyp = ETHERTYPE_VLAN;
        if (macsec_apply(ctx, prt, &bufP, &bufS, &ethtyp) != 0) return;
    }
    bundle_ntry.id = prt;
    bundle_res = hasht_find(&bundle_table, &bundle_ntry);
    if (bundle_res == NULL) {
        putMacAddr;
        send2port(prt);
        return;
    }
    int hash = ctx->hash;
    hash = ((hash >> 16) ^ hash) & 0xffff;
    hash = ((hash >> 8) ^ hash) & 0xff;
    hash = ((hash >> 4) ^ hash) & 0xf;
    prt = bundle_res->out[hash];
    bundle_res->pack++;
    bundle_res->byte += bufS;
    if (bundle_res->command == 2) {
        putMacAddr;
        bufS = bufS - bufP + preBuff;
        struct packetContext ctx2;
        unsigned char *bufC = ctx->bufC;
        memcpy(&bufC[preBuff], &bufD[bufP], bufS);
        if (shiftContext(&ctx2, ctx, bufC) != 0) return;
        processDataPacket(&ctx2, bufS, prt);
        return;
    }
    if (macsec_apply(ctx, prt, &bufP, &bufS, &ethtyp) != 0) return;
    putMacAddr;
    send2port(prt);
}



void send2neigh(struct packetContext *ctx, struct neigh_entry *neigh_res, int bufP, int bufS, int ethtyp) {
    unsigned char *bufD = ctx->bufD;
    unsigned char *bufH = ctx->bufH;
    int tmp;
    neigh_res->pack++;
    neigh_res->byte += bufS;
    memcpy(&bufH[0], &neigh_res->macs, 12);
    if (neigh_res->aclport != neigh_res->port) {
        if (macsec_apply(ctx, neigh_res->aclport, &bufP, &bufS, &ethtyp) != 0) doDropper;
    }
    switch (neigh_res->command) {
    case 1: // raw ip
        goto send;
    case 2: // pppoe
        ethtyp2ppptyp;
        putPppoeHeader;
        goto send;
    case 3: // gre
        putGreHeader;
        tmp = IP_PROTOCOL_GRE;
        goto layer3;
    case 4: // l2tp
        ethtyp2ppptyp;
        putL2tpHeader;
        goto layer3;
    case 5: // ipip
        ethtyp2iproto;
        goto layer3;
#ifndef HAVE_NOCRYPTO
    case 6: // esp
        ethtyp2iproto;
        if (putEspHeader(ctx, neigh_res, &bufP, &bufS, tmp) != 0) doDropper;
        tmp = IP_PROTOCOL_ESP;
        goto layer3;
    case 7: // openvpn
        if (putOpenvpnHeader(ctx, neigh_res, &bufP, &bufS) != 0) doDropper;
        goto layer3;
    case 8: // wireguard
        if (putWireguardHeader(ctx, neigh_res, &bufP, &bufS) != 0) doDropper;
        goto layer3;
#endif
    case 9: // amt
        putAmtHeader;
        goto layer3;
    case 10: // gtp
        putGtpHeader;
        goto layer3;
    case 11: // l3tp
        ethtyp2ppptyp;
        putL3tpHeader;
        tmp = IP_PROTOCOL_L2TP;
        goto layer3;
    case 12: // tmux4
        ethtyp2iproto;
        putTmuxHeader;
        tmp = IP_PROTOCOL_TMUX;
        goto layer3;
    case 13: // pwhe
        bufP -= 12;
        memcpy(&bufD[bufP], &bufH[0], 12);
        bufP -= 4;
        tmp = 0x1ff | (neigh_res->dprt << 12);
        put32msb(bufD, bufP, tmp);
        bufP -= 4;
        tmp = 0xff | (neigh_res->sprt << 12);
        put32msb(bufD, bufP, tmp);
        ethtyp = ETHERTYPE_MPLS_UCAST;
        bufP -= 2;
        put16msb(bufD, bufP, ethtyp);
        memcpy(&bufH[0], &neigh_res->mac2, 12);
        goto send;
    case 14: // labels
        bufP += 2;
        if (neigh_res->tid >= 1) {
            ethtyp = ETHERTYPE_MPLS_UCAST;
            bufP -= 4;
            tmp = 0x1ff | (neigh_res->dprt << 12);
            put32msb(bufD, bufP, tmp);
        }
        if (neigh_res->tid >= 2) {
            bufP -= 4;
            tmp = 0xff | (neigh_res->sprt << 12);
            put32msb(bufD, bufP, tmp);
        }
        if (neigh_res->tid >= 3) {
            bufP -= 4;
            tmp = 0xff | (neigh_res->sip1 << 12);
            put32msb(bufD, bufP, tmp);
        }
        if (neigh_res->tid >= 4) {
            bufP -= 4;
            tmp = 0xff | (neigh_res->sip2 << 12);
            put32msb(bufD, bufP, tmp);
        }
        if (neigh_res->tid >= 5) {
            bufP -= 4;
            tmp = 0xff | (neigh_res->sip3 << 12);
            put32msb(bufD, bufP, tmp);
        }
        if (neigh_res->tid >= 6) {
            bufP -= 4;
            tmp = 0xff | (neigh_res->sip4 << 12);
            put32msb(bufD, bufP, tmp);
        }
        if (neigh_res->tid >= 7) {
            bufP -= 4;
            tmp = 0xff | (neigh_res->dip1 << 12);
            put32msb(bufD, bufP, tmp);
        }
        if (neigh_res->tid >= 8) {
            bufP -= 4;
            tmp = 0xff | (neigh_res->dip2 << 12);
            put32msb(bufD, bufP, tmp);
        }
        if (neigh_res->tid >= 9) {
            bufP -= 4;
            tmp = 0xff | (neigh_res->dip3 << 12);
            put32msb(bufD, bufP, tmp);
        }
        if (neigh_res->tid >= 10) {
            bufP -= 4;
            tmp = 0xff | (neigh_res->dip4 << 12);
            put32msb(bufD, bufP, tmp);
        }
        bufP -= 2;
        put16msb(bufD, bufP, ethtyp);
        goto send;
    default:
        doDropper;
    }


send:
    send2subif(ctx, neigh_res->port, bufP, bufS, ethtyp);
drop:
    return;
layer3:
    switch (neigh_res->layer3) {
    case 1: // ipv4
        putIpv4header(tmp, neigh_res->sip1, neigh_res->dip1);
        goto send;
    case 2: // ipv6
        putIpv6header(tmp, neigh_res->sip1, neigh_res->sip2, neigh_res->sip3, neigh_res->sip4, neigh_res->dip1, neigh_res->dip2, neigh_res->dip3, neigh_res->dip4);
        goto send;
    case 3: // udp4
        putUdpHeader(neigh_res->sprt, neigh_res->dprt);
        putIpv4header(IP_PROTOCOL_UDP, neigh_res->sip1, neigh_res->dip1);
        goto send;
    case 4: // udp6
        putUdpHeader(neigh_res->sprt, neigh_res->dprt);
        putIpv6header(IP_PROTOCOL_UDP, neigh_res->sip1, neigh_res->sip2, neigh_res->sip3, neigh_res->sip4, neigh_res->dip1, neigh_res->dip2, neigh_res->dip3, neigh_res->dip4);
        goto send;
    default:
        doDropper;
    }
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



void doFlood(struct packetContext *ctx, struct table_head *flood, int bufP, int bufS, int ethtyp, int label) {
    struct neigh_entry neigh_ntry;
    struct neigh_entry *neigh_res;
    struct flood_entry *flood_res;
    struct packetContext ctx2;
    unsigned char *bufD = ctx->bufD;
    unsigned char *bufH = ctx->bufH;
    unsigned char *bufC = ctx->bufC;
    for (int i = 0; i < flood->size; i++) {
        if (shiftContext(&ctx2, ctx, bufC) != 0) break;
        flood_res = table_get(flood, i);
        int tmpP = preBuff;
        int tmpE;
        int tmpS;
        switch (flood_res->command) {
        case 1: // raw ip
            tmpE = ethtyp;
            tmpS = bufS - bufP + preBuff + 2;
            put16msb(bufC, preBuff, tmpE);
            memcpy(&bufC[preBuff + 2], &bufD[bufP], tmpS);
            memcpy(&bufH[0], &flood_res->macs, 12);
            send2subif(&ctx2, flood_res->trg, tmpP, tmpS, tmpE);
            break;
        case 2: // mpls
            tmpE = ETHERTYPE_MPLS_UCAST;
            tmpS = bufS - bufP + preBuff + 6;
            int tmpL = label | (flood_res->lab << 12);
            put16msb(bufC, preBuff, tmpE);
            put32msb(bufC, preBuff + 2, tmpL);
            memcpy(&bufC[preBuff + 6], &bufD[bufP], tmpS);
            neigh_ntry.id = flood_res->trg;
            neigh_res = hasht_find(&neigh_table, &neigh_ntry);
            if (neigh_res == NULL) continue;
            send2neigh(&ctx2, neigh_res, tmpP, tmpS, tmpE);
            break;
        case 3: // bier mask
            tmpE = ETHERTYPE_MPLS_UCAST;
            tmpS = bufS - bufP + preBuff + 6;
            tmpL = label | (flood_res->lab << 12);
            put16msb(bufC, preBuff, tmpE);
            put32msb(bufC, preBuff + 2, tmpL);
            memcpy(&bufC[preBuff + 6], &bufD[bufP], tmpS);
            int o;
            int p;
            bierAnd(bufC, preBuff + 14, flood_res->bier, o, p);
            if (p == 0) continue;
            neigh_ntry.id = flood_res->trg;
            neigh_res = hasht_find(&neigh_table, &neigh_ntry);
            if (neigh_res == NULL) continue;
            send2neigh(&ctx2, neigh_res, tmpP, tmpS, tmpE);
            break;
        case 4: // bier set
            tmpE = ETHERTYPE_MPLS_UCAST;
            tmpS = bufS - bufP + preBuff + 46;
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
            memcpy(&bufC[preBuff + 46], &bufD[bufP], tmpS);
            neigh_ntry.id = flood_res->trg;
            neigh_res = hasht_find(&neigh_table, &neigh_ntry);
            if (neigh_res == NULL) continue;
            send2neigh(&ctx2, neigh_res, tmpP, tmpS, tmpE);
            break;
        case 5: // neighbor
            tmpE = ethtyp;
            tmpS = bufS - bufP + preBuff + 2;
            put16msb(bufC, preBuff, tmpE);
            memcpy(&bufC[preBuff + 2], &bufD[bufP], tmpS);
            neigh_ntry.id = flood_res->trg;
            neigh_res = hasht_find(&neigh_table, &neigh_ntry);
            if (neigh_res == NULL) continue;
            send2neigh(&ctx2, neigh_res, tmpP, tmpS, tmpE);
            break;
        case 6: // bier vpn
            tmpE = ETHERTYPE_MPLS_UCAST;
            tmpS = bufS - bufP + preBuff + 50;
            tmpL = label | (flood_res->lab << 12);
            put16msb(bufC, preBuff, tmpE);
            put32msb(bufC, preBuff + 2, tmpL);
            bufC[preBuff + 6] = 0x50; // ver
            bufC[preBuff + 7] = 0x30; // bsl
            bufC[preBuff + 8] = 0; // entropy
            bufC[preBuff + 9] = 0; // entropy
            bufC[preBuff + 10] = 0; // oam
            bufC[preBuff + 11] = 2; // proto
            put16msb(bufC, preBuff + 12, flood_res->src); // bfir
            put32msb(bufC, preBuff + 14, flood_res->bier[0]);
            put32msb(bufC, preBuff + 18, flood_res->bier[1]);
            put32msb(bufC, preBuff + 22, flood_res->bier[2]);
            put32msb(bufC, preBuff + 26, flood_res->bier[3]);
            put32msb(bufC, preBuff + 30, flood_res->bier[4]);
            put32msb(bufC, preBuff + 34, flood_res->bier[5]);
            put32msb(bufC, preBuff + 38, flood_res->bier[6]);
            put32msb(bufC, preBuff + 42, flood_res->bier[7]);
            tmpL = label | (flood_res->lab2 << 12);
            put32msb(bufC, preBuff + 46, tmpL);
            memcpy(&bufC[preBuff + 50], &bufD[bufP], tmpS);
            neigh_ntry.id = flood_res->trg;
            neigh_res = hasht_find(&neigh_table, &neigh_ntry);
            if (neigh_res == NULL) continue;
            send2neigh(&ctx2, neigh_res, tmpP, tmpS, tmpE);
            break;
        }
    }
}




int doTunnel(struct packetContext *ctx, struct tun4_entry *tun_res, int *bufP, int *bufS, int bufT) {
    unsigned char *bufD = ctx->bufD;
    tun_res->pack++;
    tun_res->byte += *bufS;
    switch (tun_res->command) {
    case 1: // gre
        *bufP = bufT + 2;
        return 0;
    case 2: // l2tp
        *bufP = bufT + 8;
        if ((get16msb(bufD, *bufP) & 0x8000) != 0) doCpuing;
        *bufP += 8;
        *bufP += 2;
        int ethtyp = get16msb(bufD, *bufP);
        ppptyp2ethtyp(*bufP);
        put16msb(bufD, *bufP, ethtyp);
        return 0;
    case 3: // vxlan
        *bufP = bufT + 8;
        *bufP += 8;
        *bufP -= 2;
        put16msb(bufD, *bufP, ETHERTYPE_ROUTEDMAC);
        return 0;
    case 4: // ipip
        *bufP = bufT - 2;
        ethtyp = tun_res->prot;
        iproto2ethtyp;
        put16msb(bufD, *bufP, ethtyp);
        return 0;
    case 5: // etherip
        *bufP = bufT + 2;
        *bufP -= 2;
        put16msb(bufD, *bufP, ETHERTYPE_ROUTEDMAC);
        return 0;
    case 6: // pckoudp
        *bufP = bufT + 8;
        *bufP -= 2;
        put16msb(bufD, *bufP, ETHERTYPE_ROUTEDMAC);
        return 0;
#ifndef HAVE_NOCRYPTO
    case 7: // esp
        *bufP = bufT;
        int tmp = *bufS - *bufP + preBuff - tun_res->hashBlkLen;
        if (tmp < 1) doDropper;
        int tmp2;
        if (tun_res->hashBlkLen > 0) {
            if (myHmacInit(ctx->dgst, tun_res->hashAlg, tun_res->hashKeyDat, tun_res->hashKeyLen) != 1) doDropper;
            if (EVP_DigestUpdate(ctx->dgst, &bufD[*bufP], tmp) != 1) doDropper;
            if (myHmacEnd(ctx->dgst, tun_res->hashAlg, tun_res->hashKeyDat, tun_res->hashKeyLen, &bufD[0]) != 1) doDropper;
            if (memcmp(&bufD[0], &bufD[*bufP + tmp], tun_res->hashBlkLen) !=0) doDropper;
            *bufS -= tun_res->hashBlkLen;
        }
        *bufP += 8;
        tmp -= 8;
        if (EVP_CIPHER_CTX_reset(ctx->encr) != 1) doDropper;
        if (tun_res->encrTagLen > 0) {
            memcpy(&bufD[0], tun_res->hashKeyDat, 4);
            memcpy(&bufD[4], &bufD[*bufP], 8);
            *bufP += 8;
            tmp -= 8;
            if (EVP_DecryptInit_ex(ctx->encr, tun_res->encrAlg, NULL, tun_res->encrKeyDat, bufD) != 1) doDropper;
            if (EVP_CIPHER_CTX_set_padding(ctx->encr, 0) != 1) doDropper;
            if (EVP_DecryptUpdate(ctx->encr, NULL, &tmp2, &bufD[*bufP - 16], 8) != 1) doDropper;
            if (EVP_DecryptUpdate(ctx->encr, &bufD[*bufP], &tmp2, &bufD[*bufP], tmp) != 1) doDropper;
            *bufS -= tun_res->encrTagLen;
            tmp -= tun_res->encrTagLen;
        } else {
            if (EVP_DecryptInit_ex(ctx->encr, tun_res->encrAlg, NULL, tun_res->encrKeyDat, &bufD[*bufP]) != 1) doDropper;
            *bufP += tun_res->encrBlkLen;
            tmp -= tun_res->encrBlkLen;
            if (EVP_CIPHER_CTX_set_padding(ctx->encr, 0) != 1) doDropper;
            if (EVP_DecryptUpdate(ctx->encr, &bufD[*bufP], &tmp2, &bufD[*bufP], tmp) != 1) doDropper;
        }
        ethtyp = bufD[*bufP + tmp - 1];
        *bufS -= bufD[*bufP + tmp - 2] + 2;
        iproto2ethtyp;
        *bufP -= 2;
        put16msb(bufD, *bufP, ethtyp);
        return 0;
    case 8: // openvpn
        *bufP = bufT + 8;
        *bufP += tun_res->hashBlkLen;
        tmp = *bufS - *bufP + preBuff;
        if (tmp < 1) doDropper;
        if (tun_res->hashBlkLen > 0) {
            if (myHmacInit(ctx->dgst, tun_res->hashAlg, tun_res->hashKeyDat, tun_res->hashKeyLen) != 1) doDropper;
            if (EVP_DigestUpdate(ctx->dgst, &bufD[*bufP], tmp) != 1) doDropper;
            if (myHmacEnd(ctx->dgst, tun_res->hashAlg, tun_res->hashKeyDat, tun_res->hashKeyLen, &bufD[0]) != 1) doDropper;
            if (memcmp(&bufD[0], &bufD[*bufP - tun_res->hashBlkLen], tun_res->hashBlkLen) !=0) doDropper;
        }
        if (EVP_CIPHER_CTX_reset(ctx->encr) != 1) doDropper;
        if (EVP_DecryptInit_ex(ctx->encr, tun_res->encrAlg, NULL, tun_res->encrKeyDat, &bufD[*bufP]) != 1) doDropper;
        *bufP += tun_res->encrBlkLen;
        tmp -= tun_res->encrBlkLen;
        if (EVP_CIPHER_CTX_set_padding(ctx->encr, 0) != 1) doDropper;
        if (EVP_DecryptUpdate(ctx->encr, &bufD[*bufP], &tmp2, &bufD[*bufP], tmp) != 1) doDropper;
        *bufP += 8;
        guessEthtyp;
        *bufP -= 2;
        put16msb(bufD, *bufP, ethtyp);
        return 0;
    case 9: // wireguard
        *bufP = bufT + 8;
        if (get32lsb(bufD, *bufP) != 4) doCpuing;
        tmp = *bufS - *bufP + preBuff;
        if (tmp < 32) doDropper;
        put32msb(bufD, *bufP + 4, 0);
        *bufP += 16;
        *bufS -= 16;
        tmp -= 32;
        if (EVP_CIPHER_CTX_reset(ctx->encr) != 1) doDropper;
        if (EVP_DecryptInit_ex(ctx->encr, EVP_chacha20_poly1305(), NULL, tun_res->encrKeyDat, &bufD[*bufP - 12]) != 1) doDropper;
        if (EVP_CIPHER_CTX_set_padding(ctx->encr, 0) != 1) doDropper;
        if (EVP_CIPHER_CTX_ctrl(ctx->encr, EVP_CTRL_AEAD_SET_TAG, 16, &bufD[*bufP + tmp]) != 1) doDropper;
        if (EVP_DecryptUpdate(ctx->encr, &bufD[*bufP], &tmp2, &bufD[*bufP], tmp) != 1) doDropper;
        if (EVP_DecryptFinal_ex(ctx->encr, &bufD[*bufP + tmp], &tmp2) != 1) doDropper;
        guessEthtyp;
        *bufP -= 2;
        put16msb(bufD, *bufP, ethtyp);
        return 0;
#endif
    case 10: // amt
        *bufP = bufT + 8;
        if (bufD[*bufP] != 6) doCpuing;
        *bufP += 2;
        guessEthtyp;
        *bufP -= 2;
        put16msb(bufD, *bufP, ethtyp);
        return 0;
    case 11: // gtp
        *bufP = bufT + 8;
        *bufP += 8;
        guessEthtyp;
        *bufP -= 2;
        put16msb(bufD, *bufP, ethtyp);
        return 0;
    case 12: // l3tp
        *bufP = bufT + 4;
        ethtyp = get16msb(bufD, *bufP);
        ppptyp2ethtyp(*bufP);
        put16msb(bufD, *bufP, ethtyp);
        return 0;
    case 13: // tmux
        *bufP = bufT + 2;
        ethtyp = get16msb(bufD, bufT);
        if (ethtyp < 4) doDropper;
        if (ethtyp > (*bufS - bufT + preBuff)) doDropper;
        *bufS = ethtyp + bufT - preBuff;
        ethtyp = bufD[*bufP];
        iproto2ethtyp;
        put16msb(bufD, *bufP, ethtyp);
        return 0;
    case 14: // eoip
        *bufP = bufT + 8;
        if (get16msb(bufD, bufT + 2) != 0x6400) doDropper;
        ethtyp = get16msb(bufD, bufT + 4);
        if (ethtyp > (*bufS - *bufP + preBuff)) doDropper;
        *bufS = ethtyp + *bufP - preBuff;
        *bufP -= 2;
        put16msb(bufD, *bufP, ETHERTYPE_ROUTEDMAC);
        return 0;
    }
drop:
    return 2;
cpu:
    return 1;
}


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
        putIpv6header(proto, route_res->srv1, route_res->srv2, route_res->srv3, route_res->srv4, route_res->srv1, route_res->srv2, route_res->srv3, route_res->srv4);   \
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
        memcpy(&bufH[0], &bufD[bufP], 12);                          \
        bufP += 12;                                                 \
        bufP += 2;                                                  \
        goto bridgevpls_rx;                                         \
    case 9:                                                         \
        bufP -= 20;                                                 \
        bufD[bufP + 0] = 0;                                         \
        bufD[bufP + 1] = ttl;                                       \
        put16msb(bufD, bufP + 2, ethtyp);                           \
        memcpy(&bufD[bufP + 4], route_res->polka, 16);              \
        neigh_ntry.id = route_res->nexthop;                         \
        ethtyp = ETHERTYPE_POLKA;                                   \
        goto ethtyp_tx;                                             \
    case 10:                                                        \
        bufP -= 20;                                                 \
        bufD[bufP + 0] = 1;                                         \
        bufD[bufP + 1] = ttl;                                       \
        put16msb(bufD, bufP + 2, ethtyp);                           \
        memcpy(&bufD[bufP + 4], route_res->polka, 16);              \
        neigh_ntry.id = route_res->nexthop;                         \
        ethtyp = ETHERTYPE_POLKA;                                   \
        goto ethtyp_tx;                                             \
    case 11:                                                        \
        doDropper;



#define doSampler                                                   \
    if (vrf2rib_res->samp > 0) {                                    \
        if ((vrf2rib_res->pack%vrf2rib_res->samp) == 0) {           \
            bufP -= 2;                                              \
            bufE = bufP;                                            \
            put16msb(bufD, bufP, ethtyp);                           \
            bufP -= 12;                                             \
            memcpy(&bufD[bufP], &bufD[preBuff], 12);                \
            bufP -= 4;                                              \
            tmp = 0x80000000 | prt;                                 \
            put32msb(bufD, bufP, tmp);                              \
            send2port(cpuPort);                                     \
            bufP += 18;                                             \
        }                                                           \
    }




void processDataPacket(struct packetContext *ctx, int bufS, int prt) {
    ctx->stat->packRx++;
    ctx->stat->byteRx += bufS;
    unsigned char *bufD = ctx->bufD;
    unsigned char *bufH = ctx->bufH;
    if (prt == cpuPort) {
        prt = get32msb(bufD, preBuff + 0);
        ctx->hash = get32msb(bufD, preBuff + 4) ^ get32msb(bufD, preBuff + 8) ^ get32msb(bufD, preBuff + 12);
        int ethtyp = get16msb(bufD, preBuff + 16);
        int bufP = preBuff + 16;
        ctx->sgt = -1;
        memcpy(&bufH[0], &bufD[preBuff + 4], 12);
        send2subif(ctx, prt, bufP, bufS, ethtyp);
        return;
    }
    struct nsh_entry nsh_ntry;
    struct polkaPoly_entry polkaPoly_ntry;
    struct polkaIdx_entry polkaIdx_ntry;
    struct mpls_entry mpls_ntry;
    struct port2vrf_entry port2vrf_ntry;
    struct vrf2rib_entry vrf2rib_ntry;
    struct route4_entry route4_ntry;
    struct route6_entry route6_ntry;
    struct neigh_entry neigh_ntry;
    struct vlanin_entry vlanin_ntry;
    struct bridge_entry bridge_ntry;
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
    struct polkaPoly_entry *polkaPoly_res;
    struct polkaIdx_entry *polkaIdx_res;
    struct nsh_entry *nsh_res;
    struct mpls_entry *mpls_res;
    struct port2vrf_entry *port2vrf_res;
    struct vrf2rib_entry *vrf2rib_res;
    struct route4_entry *route4_res;
    struct route6_entry *route6_res;
    struct neigh_entry *neigh_res;
    struct vlanin_entry *vlanin_res;
    struct bridge_entry *bridge_res;
    struct aclH_entry *aceh_res;
    struct insp4_entry *insp4_res;
    struct insp6_entry *insp6_res;
    struct nat4_entry *nat4_res;
    struct nat6_entry *nat6_res;
    struct pppoe_entry *pppoe_res;
    struct tun4_entry *tun4_res;
    struct tun6_entry *tun6_res;
    struct policer_entry *policer_res;
    struct mroute4_entry *mroute4_res;
    struct mroute6_entry *mroute6_res;
    ctx->sgt = 0;
    ctx->hash = 0;
    int label;
    int sum;
    int bufP;
    int bufE;
    int bufT;
    int frag;
    int ethtyp;
    int tmp;
    int ttl;
    bufP = preBuff;
    bufP += 6 * 2; // dmac, smac
ethtyp_rx:
    bufE = bufP;
    ethtyp = get16msb(bufD, bufP);
    bufP += 2;
    port2vrf_ntry.port = prt;
    port2vrf_res = hasht_find(&port2vrf_table, &port2vrf_ntry);
    if (port2vrf_res == NULL) goto etyped_rx;
    if (port2vrf_res->rateIn != 0) {
        policer_ntry.vrf = 0;
        policer_ntry.meter = port2vrf_res->rateIn;
        policer_ntry.dir = 5;
        policer_res = hasht_find(&policer_table, &policer_ntry);
        if (policer_res == NULL) doDropper;
        if (policer_res->avail < 1) doDropper;
        policer_res->avail -= bufS - bufP + preBuff;
    }
#ifndef HAVE_NOCRYPTO
    if (port2vrf_res->mcscEthtyp != 0) {
        port2vrf_res->mcscPackRx++;
        port2vrf_res->mcscByteRx += bufS;
        if (ethtyp != port2vrf_res->mcscEthtyp) doDropper;
        if (bufD[bufP] != 0x0c) doCpuing;
        int tmp2 = bufD[bufP + 1];
        int seq = port2vrf_res->mcscSeqRx = get32msb(bufD, bufP + 2);
        bufP += 6;
        tmp = bufS - bufP + preBuff - port2vrf_res->mcscHashBlkLen;
        if (tmp2 > 0) {
            tmp2 += port2vrf_res->mcscEncrTagLen;
            if (tmp2 > tmp) doDropper;
            tmp = tmp2;
        }
        if (tmp < 1) doDropper;
        if (port2vrf_res->mcscNeedAead == 0) if ((tmp % port2vrf_res->mcscEncrBlkLen) != 0) doDropper;
        if (port2vrf_res->mcscHashBlkLen > 0) {
            if (myHmacInit(ctx->dgst, port2vrf_res->mcscHashAlg, port2vrf_res->mcscDgRxKeyDat, port2vrf_res->mcscDgRxKeyLen) != 1) doDropper;
            if (port2vrf_res->mcscNeedMacs != 0) {
                if (EVP_DigestUpdate(ctx->dgst, &bufD[preBuff + 0], 12) != 1) doDropper;
            }
            if (EVP_DigestUpdate(ctx->dgst, &bufD[bufP - 8], 8) != 1) doDropper;
            if (EVP_DigestUpdate(ctx->dgst, &bufD[bufP], tmp) != 1) doDropper;
            if (myHmacEnd(ctx->dgst, port2vrf_res->mcscHashAlg, port2vrf_res->mcscDgRxKeyDat, port2vrf_res->mcscDgRxKeyLen, &bufD[0]) != 1) doDropper;
            if (memcmp(&bufD[0], &bufD[bufP + tmp], port2vrf_res->mcscHashBlkLen) !=0) doDropper;
        }
        if (EVP_CIPHER_CTX_reset(ctx->encr) != 1) doDropper;
        memcpy(&bufD[0], port2vrf_res->mcscIvRxKeyDat, port2vrf_res->mcscIvRxKeyLen);
        put32msb(bufD, port2vrf_res->mcscIvRxKeyLen, seq);
        if (EVP_DecryptInit_ex(ctx->encr, port2vrf_res->mcscEncrAlg, NULL, port2vrf_res->mcscCrRxKeyDat, bufD) != 1) doDropper;
        if (EVP_CIPHER_CTX_set_padding(ctx->encr, 0) != 1) doDropper;
        if (port2vrf_res->mcscNeedAead != 0) {
            if (port2vrf_res->mcscNeedMacs != 0) {
                if (EVP_DecryptUpdate(ctx->encr, NULL, &tmp2, &bufD[preBuff + 0], 12) != 1) doDropper;
            }
            if (EVP_DecryptUpdate(ctx->encr, NULL, &tmp2, &bufD[bufP - 8], 8) != 1) doDropper;
            tmp -= port2vrf_res->mcscEncrTagLen;
            if (EVP_DecryptUpdate(ctx->encr, &bufD[bufP], &tmp2, &bufD[bufP], tmp) != 1) doDropper;
            if (EVP_CIPHER_CTX_ctrl(ctx->encr, EVP_CTRL_GCM_SET_TAG, port2vrf_res->mcscEncrBlkLen, &bufD[bufP + tmp]) != 1) doDropper;
            if (EVP_DecryptFinal_ex(ctx->encr, &bufD[bufP + tmp], &tmp2) != 1) doDropper;
        } else {
            if (EVP_DecryptUpdate(ctx->encr, &bufD[bufP], &tmp2, &bufD[bufP], tmp) != 1) doDropper;
        }
        bufS = tmp + bufP - preBuff;
        bufE = bufP;
        ethtyp = get16msb(bufD, bufP);
        bufP += 2;
        port2vrf_res->mcscPackOk++;
        port2vrf_res->mcscByteOk += bufS;
    }
#endif
    if (port2vrf_res->sgtTag != 0) {
        if (ethtyp != ETHERTYPE_SGT) doDropper;
        if (get32msb(bufD, bufP + 0) != 0x01010001) doDropper;
        ctx->sgt = get16msb(bufD, bufP + 4);
        ethtyp = get16msb(bufD, bufP + 6);
        bufP += 8;
    }
    if (port2vrf_res->sgtSet >= 0) {
        ctx->sgt = port2vrf_res->sgtSet;
    }
    if (port2vrf_res->monTarget >= 0) {
        if ((port2vrf_res->monPackets++%port2vrf_res->monSample) == 0) {
            struct packetContext ctx2;
            unsigned char *bufC = ctx->bufC;
            tmp = bufS - bufP + preBuff + 2;
            if (tmp > port2vrf_res->monTruncate) tmp = port2vrf_res->monTruncate;
            memcpy(&bufC[preBuff], &bufD[bufP - 2], tmp);
            memcpy(&bufH[0], &bufD[preBuff], 12);
            if (shiftContext(&ctx2, ctx, bufC) == 0) send2subif(&ctx2, port2vrf_res->monTarget, preBuff, tmp, ethtyp);
        }
    }
    switch (port2vrf_res->command) {
    case 2: // bridge
        bridge_ntry.id = port2vrf_res->bridge;
        memcpy(&bufH[0], &bufD[preBuff], 12);
        switch (ethtyp) {
        case ETHERTYPE_IPV4: // ipv4
            bufT = bufD[bufP + 0] & 0xf; // ihl
            if (bufT < 5) doDropper;
            ttl = get16msb(bufD, bufP + 2) + bufP - preBuff; // len
            if (ttl > bufS) doDropper;
            bufS = ttl;
            if (port2vrf_res->pmtud4 > 0) {
                if ((bufS - bufP + preBuff) > port2vrf_res->pmtud4) doPunting;
            }
            bufT = bufP + (bufT << 2);
            frag = get16msb(bufD, bufP + 6) & 0x3fff; // frags
            acl4_ntry.sgtV = ctx->sgt;
            acl4_ntry.protV = bufD[bufP + 9];
            acl4_ntry.tosV = bufD[bufP + 1];
            acl4_ntry.flowV = get16msb(bufD, bufP + 4);
            acl4_ntry.srcAddr = get32msb(bufD, bufP + 12);
            acl4_ntry.trgAddr = get32msb(bufD, bufP + 16);
            ctx->hash ^= acl4_ntry.srcAddr ^ acl4_ntry.trgAddr;
            extract_layer4(acl4_ntry);
            update_tcpMss(acl4_ntry, port2vrf_res->tcpmss4);
            if (table_nonexist(&port2vrf_res->inacl4)) break;
            if (frag != 0) doPunting;
            if (apply_acl(&port2vrf_res->inacl4, &acl4_ntry, &acl4_matcher, bufS - bufP + preBuff) != 0) doPunting;
            break;
        case ETHERTYPE_IPV6: // ipv6
            ttl = get16msb(bufD, bufP + 4) + 40 + bufP - preBuff; // len
            if (ttl > bufS) doDropper;
            bufS = ttl;
            if (port2vrf_res->pmtud6 > 0) {
                if ((bufS - bufP + preBuff) > port2vrf_res->pmtud6) doPunting;
            }
            bufT = bufP + 40;
            acl6_ntry.sgtV = ctx->sgt;
            acl6_ntry.protV = bufD[bufP + 6];
            acl6_ntry.tosV = (get16msb(bufD, bufP + 0) >> 4) & 0xff;
            acl6_ntry.flowV = get32msb(bufD, bufP + 0) & 0xfffff;
            if (acl6_ntry.protV == 44) { // frags
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
            acl6_ntry.trgAddr1 = get32msb(bufD, bufP + 24);
            acl6_ntry.trgAddr2 = get32msb(bufD, bufP + 28);
            acl6_ntry.trgAddr3 = get32msb(bufD, bufP + 32);
            acl6_ntry.trgAddr4 = get32msb(bufD, bufP + 36);
            ctx->hash ^= acl6_ntry.srcAddr1 ^ acl6_ntry.trgAddr1 ^ acl6_ntry.srcAddr2 ^ acl6_ntry.trgAddr2 ^ acl6_ntry.srcAddr3 ^ acl6_ntry.trgAddr3 ^ acl6_ntry.srcAddr4 ^ acl6_ntry.trgAddr4;
            extract_layer4(acl6_ntry);
            update_tcpMss(acl6_ntry, port2vrf_res->tcpmss6);
            if (table_nonexist(&port2vrf_res->inacl6)) break;
            if (frag != 0) doPunting;
            if (apply_acl(&port2vrf_res->inacl6, &acl6_ntry, &acl6_matcher, bufS - bufP + preBuff) != 0) doPunting;
            break;
        case ETHERTYPE_ROUTEDMAC:
            goto etyped_rx;
        }
        goto bridgevpls_rx;
    case 3: // xconn
        memcpy(&bufH[0], &bufD[preBuff], 12);
        bufP -= 2;
        bufP -= 12;
        memcpy(&bufD[bufP], &bufH[0], 12);
        ethtyp = ETHERTYPE_MPLS_UCAST;
        bufP -= 4;
        label = 0x1ff | (port2vrf_res->label2 << 12);
        put32msb(bufD, bufP, label);
        bufP -= 4;
        label = 0xff | (port2vrf_res->label1 << 12);
        put32msb(bufD, bufP, label);
        neigh_ntry.id = port2vrf_res->nexthop;
        goto ethtyp_tx;
    case 4: // loconnifc
        bufP -= 2;
        memcpy(&bufH[0], &bufD[preBuff], 12);
        send2subif(ctx, port2vrf_res->label1, bufP, bufS, ethtyp);
        return;
    case 5: // loconnnei
        bufP -= 2;
        neigh_ntry.id = port2vrf_res->label1;
        goto nethtyp_tx;
    case 6: // nshconn
        memcpy(&bufH[0], &bufD[preBuff], 12);
        bufP -= 2;
        bufP -= 12;
        memcpy(&bufD[bufP], &bufH[0], 12);
        bufP -= 8;
        put16msb(bufD, bufP + 0, 0xfc2);
        put16msb(bufD, bufP + 2, 0x203);
        tmp = (port2vrf_res->label1 << 8) | port2vrf_res->label2;
        put32msb(bufD, bufP + 4, tmp);
        goto nsh_rx;
    }
etyped_rx:
    if (bufP < minBuff) doDropper;
    if (bufP > maxBuff) doDropper;
    switch (ethtyp) {
    case ETHERTYPE_MPLS_UCAST: // mpls
        if (port2vrf_res == NULL) doDropper;
        if (port2vrf_res->mpls == 0) doDropper;
        ctx->stat->packMpls++;
        ctx->stat->byteMpls += bufS;
mpls_rx:
        label = get32msb(bufD, bufP);
        ttl = (label & 0xff) - 1;
        if (ttl <= 1) doPunting;
        bufP += 4;
        mpls_ntry.label = (label >> 12) & 0xfffff;
        ctx->hash ^= mpls_ntry.label;
        mpls_res = hasht_find(&mpls_table, &mpls_ntry);
        if (mpls_res == NULL) doDropper;
        mpls_res->pack++;
        mpls_res->byte += bufS;
        switch (mpls_res->command) {
        case 1: // route
mpls_rou:
            if ((label & 0x100) == 0) {
                bufD[bufP + 3] = ttl + 1;
                goto mpls_rx;
            }
mpls_rout:
            vrf2rib_ntry.vrf = mpls_res->vrf;
            switch (mpls_res->ver) {
            case 4:
                ethtyp = ETHERTYPE_IPV4;
                goto ipv4_rx;
            case 6:
                ethtyp = ETHERTYPE_IPV6;
                goto ipv6_rx;
            }
            doDropper;
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
            neigh_res = hasht_find(&neigh_table, &neigh_ntry);
            if (neigh_res == NULL) doDropper;
neigh_tx:
            send2neigh(ctx, neigh_res, bufP, bufS, ethtyp);
            return;
        case 4: // xconn
            memcpy(&bufH[0], &bufD[bufP], 12);
            bufP += 12;
            send2subif(ctx, mpls_res->port, bufP, bufS, ethtyp);
            return;
        case 5: // vpls
            memcpy(&bufH[0], &bufD[bufP], 12);
            bufP += 12;
            bufP += 2;
            bridge_ntry.id = mpls_res->port;
            goto bridgevpls_rx;
        case 6: // punt
            doCpuing;
        case 7: // dup
            doFlood(ctx, &mpls_res->flood, bufP, bufS, ethtyp, (label & 0xf00) | ttl);
            if (mpls_res->swap != 0) goto mpls_rou;
            return;
        case 8: // bier
            if ((label & 0x100) == 0) doDropper;
            if (bufD[bufP] != 0x50) doDropper;
            doFlood(ctx, &mpls_res->flood, bufP, bufS, ethtyp, (label & 0xf00) | ttl);
            bierAnd(bufD, bufP + 8, mpls_res->bier, tmp, ttl);
            if (ttl == 0) return;
            ttl = bufD[bufP + 5] & 0x3f;
            bufP += 8;
            bufP += 32;
            switch (ttl) {
            case 1: // downstream
            case 2: // upstream
                goto mpls_rx;
            case 4: // ipv4
            case 6: // ipv6
                goto mpls_rout;
            }
            doDropper;
        case 9: // push
            bufP -= 4;
            label = (label & 0xf00) | ttl | (mpls_res->push << 12);
            put32msb(bufD, bufP, label);
            bufP -= 4;
            label = (label & 0xe00) | ttl | (mpls_res->swap << 12);
            put32msb(bufD, bufP, label);
            neigh_ntry.id = mpls_res->nexthop;
            goto ethtyp_tx;
        case 10: // pwhe
            memcpy(&bufH[0], &bufD[bufP], 12);
            memcpy(&bufD[preBuff], &bufH[0], 12);
            bufP += 12;
            prt = mpls_res->port;
            goto ethtyp_rx;
        }
        doDropper;
    case ETHERTYPE_VLAN: // dot1q
        ctx->stat->packVlan++;
        ctx->stat->byteVlan += bufS;
        ttl = get16msb(bufD, bufP) & 0xfff;
        ctx->hash ^= ttl;
        vlanin_ntry.port = prt;
        vlanin_ntry.vlan = ttl;
        bufP += 2;
        vlanin_res = hasht_find(&vlanin_table, &vlanin_ntry);
        if (vlanin_res == NULL) doDropper;
        prt = vlanin_res->id;
        vlanin_res->pack++;
        vlanin_res->byte += bufS;
        goto ethtyp_rx;
    case ETHERTYPE_IPV4: // ipv4
        if (port2vrf_res == NULL) doDropper;
        if (port2vrf_res->command != 1) doDropper;
        ctx->stat->packIpv4++;
        ctx->stat->byteIpv4 += bufS;
        vrf2rib_ntry.vrf = port2vrf_res->vrf;
ipv4_rx:
        vrf2rib_res = hasht_find(&vrf2rib4_table, &vrf2rib_ntry);
        if (vrf2rib_res == NULL) doDropper;
        if ((bufD[bufP + 0] & 0xf0) != 0x40) doDropper;
        bufT = bufD[bufP + 0] & 0xf; // ihl
        if (bufT < 5) doDropper;
        ttl = get16msb(bufD, bufP + 2) + bufP - preBuff; // len
        if (ttl > bufS) doDropper;
        bufS = ttl;
        if (port2vrf_res->pmtud4 > 0) {
            if ((bufS - bufP + preBuff) > port2vrf_res->pmtud4) doPunting;
        }
        bufT = bufP + (bufT << 2);
        frag = get16msb(bufD, bufP + 6) & 0x3fff; // frags
        acl4_ntry.sgtV = ctx->sgt;
        acl4_ntry.protV = bufD[bufP + 9];
        acl4_ntry.tosV = bufD[bufP + 1];
        acl4_ntry.flowV = get16msb(bufD, bufP + 4);
        acl4_ntry.srcAddr = get32msb(bufD, bufP + 12);
        acl4_ntry.trgAddr = get32msb(bufD, bufP + 16);
        ctx->hash ^= acl4_ntry.srcAddr ^ acl4_ntry.trgAddr;
        extract_layer4(acl4_ntry);
        update_tcpMss(acl4_ntry, port2vrf_res->tcpmss4);
        if (port2vrf_res->verify4 > 0) {
            route4_ntry.addr[0] = acl4_ntry.srcAddr;
            route4_ntry.mask = 32;
            route4_res = tree_lpm(&vrf2rib_res->rou, &route4_ntry);
            if (route4_res == NULL) doPunting;
            route4_res->packRx++;
            route4_res->byteRx += bufS;
            if (port2vrf_res->verify4 > 1) {
                neigh_ntry.id = route4_res->nexthop;
                neigh_res = hasht_find(&neigh_table, &neigh_ntry);
                if (neigh_res == NULL) doPunting;
                if (neigh_res->aclport != prt) doPunting;
            }
        }
        if (!table_nonexist(&port2vrf_res->inacl4)) {
            if (frag != 0) doPunting;
            insp4_ntry.prot = acl4_ntry.protV;
            insp4_ntry.srcAddr = acl4_ntry.srcAddr;
            insp4_ntry.trgAddr = acl4_ntry.trgAddr;
            insp4_ntry.srcPort = acl4_ntry.srcPortV;
            insp4_ntry.trgPort = acl4_ntry.trgPortV;
            insp4_res = hasht_find(&port2vrf_res->insp4, &insp4_ntry);
            if (insp4_res == NULL) {
                tmp = apply_acl(&port2vrf_res->inacl4, &acl4_ntry, &acl4_matcher, bufS - bufP + preBuff);
                if (tmp == 2) doCpuing;
                if (tmp != 0) doPunting;
            } else {
                insp4_res->packRx++;
                insp4_res->byteRx += bufS;
            }
        }
        if (table_nonexist(&port2vrf_res->inqos4)) goto ipv4_qosed;
        if (frag != 0) doPunting;
        aceh_res = search_ace(&port2vrf_res->inqos4, &acl4_ntry, &acl4_matcher, bufS - bufP + preBuff);
        if (aceh_res == NULL) goto ipv4_qosed;
        if (aceh_res->act != 0) goto ipv4_qosed;
        policer_ntry.vrf = 0;
        policer_ntry.meter = aceh_res->nexthop;
        policer_ntry.dir = 1;
        policer_res = hasht_find(&policer_table, &policer_ntry);
        if (policer_res == NULL) doDropper;
        if (policer_res->avail < 1) doDropper;
        policer_res->avail -= bufS - bufP + preBuff;
ipv4_qosed:
        if (port2vrf_res->nflw4 > 0) goto ipv4_flwed;
        if (table_nonexist(&vrf2rib_res->flws)) goto ipv4_flwed;
        aceh_res = search_ace(&vrf2rib_res->flws, &acl4_ntry, &acl4_matcher, bufS - bufP + preBuff);
        if (aceh_res == NULL) goto ipv4_flwed;
        if (aceh_res->act != 0) goto ipv4_flwed;
        policer_ntry.vrf = vrf2rib_ntry.vrf;
        policer_ntry.meter = aceh_res->pri;
        policer_ntry.dir = 3;
        policer_res = hasht_find(&policer_table, &policer_ntry);
        if (policer_res == NULL) doDropper;
        if (policer_res->avail < 1) doDropper;
        policer_res->avail -= bufS - bufP + preBuff;
ipv4_flwed:
        if (table_nonexist(&vrf2rib_res->natC)) goto ipv4_nated;
        if (frag != 0) goto ipv4_nated;
        nat4_ntry.prot = acl4_ntry.protV;
        nat4_ntry.oSrcAddr = acl4_ntry.srcAddr;
        nat4_ntry.oTrgAddr = acl4_ntry.trgAddr;
        nat4_ntry.oSrcPort = acl4_ntry.srcPortV;
        nat4_ntry.oTrgPort = acl4_ntry.trgPortV;
        nat4_res = hasht_find(&vrf2rib_res->natT, &nat4_ntry);
        if (nat4_res == NULL) {
            if (apply_acl(&vrf2rib_res->natC, &acl4_ntry, &acl4_matcher, bufS - bufP + preBuff) == 0) doCpuing;
            goto ipv4_nated;
        }
        nat4_res->pack++;
        nat4_res->byte += bufS;
        acl4_ntry.srcAddr = nat4_res->nSrcAddr;
        acl4_ntry.trgAddr = nat4_res->nTrgAddr;
        acl4_ntry.srcPortV = nat4_res->nSrcPort;
        acl4_ntry.trgPortV = nat4_res->nTrgPort;
        put32msb(bufD, bufP + 12, acl4_ntry.srcAddr);
        put32msb(bufD, bufP + 16, acl4_ntry.trgAddr);
        update_chksum(bufP + 10, nat4_res->sum3);
        update_layer4(nat4_res);
ipv4_nated:
        ttl = bufD[bufP + 8] - 1;
        if (ttl <= 1) doPunting;
        bufD[bufP + 8] = ttl;
        update_chksum(bufP + 10, -1);
        doSampler;
        ttl |= port2vrf_res->pttl4;
        if (table_nonexist(&vrf2rib_res->pbr)) goto ipv4_pbred;
        if (frag != 0) doPunting;
        aceh_res = search_ace(&vrf2rib_res->pbr, &acl4_ntry, &acl4_matcher, bufS - bufP + preBuff);
        if (aceh_res == NULL) goto ipv4_pbred;
        if (aceh_res->act != 0) goto ipv4_pbred;
        switch (aceh_res->cmd) {
        case 1: // normal
            break;
        case 2: // setvrf
            vrf2rib_ntry.vrf = aceh_res->vrf;
            vrf2rib_res = hasht_find(&vrf2rib4_table, &vrf2rib_ntry);
            if (vrf2rib_res == NULL) doDropper;
            break;
        case 3: // sethop
            vrf2rib_ntry.vrf = aceh_res->vrf;
            neigh_ntry.id = aceh_res->nexthop;
            neigh_res = hasht_find(&neigh_table, &neigh_ntry);
            if (neigh_res == NULL) doDropper;
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
ipv4_pbred:
        if (acl4_ntry.protV == 46) doCpuing;
        vrf2rib_res->pack++;
        vrf2rib_res->byte += bufS;
        route4_ntry.addr[0] = acl4_ntry.trgAddr;
        route4_ntry.mask = 32;
        route4_res = tree_lpm(&vrf2rib_res->rou, &route4_ntry);
        if (route4_res == NULL) doPunting;
        route4_res->packTx++;
        route4_res->byteTx += bufS;
        switch (route4_res->command) {
        case 1: // route
            neigh_ntry.id = route4_res->nexthop;
            neigh_res = hasht_find(&neigh_table, &neigh_ntry);
            if (neigh_res == NULL) doDropper;
ipv4_tx:
            port2vrf_ntry.port = neigh_res->aclport;
            port2vrf_res = hasht_find(&port2vrf_table, &port2vrf_ntry);
            if (port2vrf_res == NULL) doDropper;
            if (!table_nonexist(&port2vrf_res->outacl4)) {
                if (frag != 0) doPunting;
                insp4_ntry.prot = acl4_ntry.protV;
                insp4_ntry.trgAddr = acl4_ntry.srcAddr;
                insp4_ntry.srcAddr = acl4_ntry.trgAddr;
                insp4_ntry.trgPort = acl4_ntry.srcPortV;
                insp4_ntry.srcPort = acl4_ntry.trgPortV;
                insp4_res = hasht_find(&port2vrf_res->insp4, &insp4_ntry);
                if (insp4_res == NULL) {
                    tmp = apply_acl(&port2vrf_res->outacl4, &acl4_ntry, &acl4_matcher, bufS - bufP + preBuff);
                    if (tmp == 2) doCpuing;
                    if (tmp != 0) doPunting;
                } else {
                    insp4_res->packTx++;
                    insp4_res->byteTx += bufS;
                }
            }
            bufP -= 2;
            put16msb(bufD, bufP, ethtyp);
            if (table_nonexist(&port2vrf_res->outqos4)) goto neigh_tx;
            if (frag != 0) doPunting;
            aceh_res = search_ace(&port2vrf_res->outqos4, &acl4_ntry, &acl4_matcher, bufS - bufP + preBuff);
            if (aceh_res == NULL) goto neigh_tx;
            if (aceh_res->act != 0) goto neigh_tx;
            policer_ntry.vrf = 0;
            policer_ntry.meter = aceh_res->nexthop;
            policer_ntry.dir = 2;
            policer_res = hasht_find(&policer_table, &policer_ntry);
            if (policer_res == NULL) doDropper;
            if (policer_res->avail < 1) doDropper;
            policer_res->avail -= bufS - bufP + preBuff;
            goto neigh_tx;
        case 2: // punt
            tun4_ntry.srcAddr = acl4_ntry.srcAddr;
            tun4_ntry.trgAddr = acl4_ntry.trgAddr;
            tun4_ntry.prot = acl4_ntry.protV;
            tun4_ntry.srcPort = acl4_ntry.srcPortV;
            tun4_ntry.trgPort = acl4_ntry.trgPortV;
            tun4_res = hasht_find(&vrf2rib_res->tun, &tun4_ntry);
            if (tun4_res != NULL) {
                if (frag != 0) doPunting;
                switch (doTunnel(ctx, tun4_res, &bufP, &bufS, bufT)) {
                case 0:
                    prt = tun4_res->aclport;
                    goto ethtyp_rx;
                case 1:
                    doCpuing;
                }
                doDropper;
            }
            mroute4_ntry.src = acl4_ntry.srcAddr;
            mroute4_ntry.grp = acl4_ntry.trgAddr;
            mroute4_res = hasht_find(&vrf2rib_res->mcst, &mroute4_ntry);
            if (mroute4_res != NULL) {
                if (mroute4_res->ingr != prt) doDropper;
                mroute4_res->pack++;
                mroute4_res->byte += bufS;
                doFlood(ctx, &mroute4_res->flood, bufP, bufS, ethtyp, 0x100 | ttl);
                if (mroute4_res->local != 0) doCpuing;
                return;
            }
            if (table_nonexist(&vrf2rib_res->copp)) doCpuing;
            if (frag != 0) doPunting;
            if (apply_acl(&vrf2rib_res->copp, &acl4_ntry, &acl4_matcher, bufS - bufP + preBuff) != 0) doDropper;
            doCpuing;
            doRouted(route4_res, IP_PROTOCOL_IPV4);
        }
        doDropper;
    case ETHERTYPE_IPV6: // ipv6
        if (port2vrf_res == NULL) doDropper;
        if (port2vrf_res->command != 1) doDropper;
        ctx->stat->packIpv6++;
        ctx->stat->byteIpv6 += bufS;
        vrf2rib_ntry.vrf = port2vrf_res->vrf;
ipv6_rx:
        vrf2rib_res = hasht_find(&vrf2rib6_table, &vrf2rib_ntry);
        if (vrf2rib_res == NULL) doDropper;
        if ((bufD[bufP + 0] & 0xf0) != 0x60) doDropper;
        ttl = get16msb(bufD, bufP + 4) + 40 + bufP - preBuff; // len
        if (ttl > bufS) doDropper;
        bufS = ttl;
        if (port2vrf_res->pmtud6 > 0) {
            if ((bufS - bufP + preBuff) > port2vrf_res->pmtud6) doPunting;
        }
        bufT = bufP + 40;
        acl6_ntry.sgtV = ctx->sgt;
        acl6_ntry.protV = bufD[bufP + 6];
        acl6_ntry.tosV = (get16msb(bufD, bufP + 0) >> 4) & 0xff;
        acl6_ntry.flowV = get32msb(bufD, bufP + 0) & 0xfffff;
        if (acl6_ntry.protV == 44) { // frags
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
        acl6_ntry.trgAddr1 = get32msb(bufD, bufP + 24);
        acl6_ntry.trgAddr2 = get32msb(bufD, bufP + 28);
        acl6_ntry.trgAddr3 = get32msb(bufD, bufP + 32);
        acl6_ntry.trgAddr4 = get32msb(bufD, bufP + 36);
        ctx->hash ^= acl6_ntry.srcAddr1 ^ acl6_ntry.trgAddr1 ^ acl6_ntry.srcAddr2 ^ acl6_ntry.trgAddr2 ^ acl6_ntry.srcAddr3 ^ acl6_ntry.trgAddr3 ^ acl6_ntry.srcAddr4 ^ acl6_ntry.trgAddr4;
        extract_layer4(acl6_ntry);
        update_tcpMss(acl6_ntry, port2vrf_res->tcpmss6);
        if (port2vrf_res->verify6 > 0) {
            route6_ntry.addr[0] = acl6_ntry.srcAddr1;
            route6_ntry.addr[1] = acl6_ntry.srcAddr2;
            route6_ntry.addr[2] = acl6_ntry.srcAddr3;
            route6_ntry.addr[3] = acl6_ntry.srcAddr4;
            route6_ntry.mask = 128;
            route6_res = tree_lpm(&vrf2rib_res->rou, &route6_ntry);
            if (route6_res == NULL) doPunting;
            route6_res->packRx++;
            route6_res->byteRx += bufS;
            if (port2vrf_res->verify6 > 1) {
                neigh_ntry.id = route6_res->nexthop;
                neigh_res = hasht_find(&neigh_table, &neigh_ntry);
                if (neigh_res == NULL) doPunting;
                if (neigh_res->aclport != prt) doPunting;
            }
        }
        if (!table_nonexist(&port2vrf_res->inacl6)) {
            if (frag != 0) doPunting;
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
            insp6_res = hasht_find(&port2vrf_res->insp6, &insp6_ntry);
            if (insp6_res == NULL) {
                tmp = apply_acl(&port2vrf_res->inacl6, &acl6_ntry, &acl6_matcher, bufS - bufP + preBuff);
                if (tmp == 2) doCpuing;
                if (tmp != 0) doPunting;
            } else {
                insp6_res->packRx++;
                insp6_res->byteRx += bufS;
            }
        }
        if (table_nonexist(&port2vrf_res->inqos6)) goto ipv6_qosed;
        if (frag != 0) doPunting;
        aceh_res = search_ace(&port2vrf_res->inqos6, &acl6_ntry, &acl6_matcher, bufS - bufP + preBuff);
        if (aceh_res == NULL) goto ipv6_qosed;
        if (aceh_res->act != 0) goto ipv6_qosed;
        policer_ntry.vrf = 0;
        policer_ntry.meter = aceh_res->nexthop;
        policer_ntry.dir = 1;
        policer_res = hasht_find(&policer_table, &policer_ntry);
        if (policer_res == NULL) doDropper;
        if (policer_res->avail < 1) doDropper;
        policer_res->avail -= bufS - bufP + preBuff;
ipv6_qosed:
        if (port2vrf_res->nflw6 > 0) goto ipv6_flwed;
        if (table_nonexist(&vrf2rib_res->flws)) goto ipv6_flwed;
        aceh_res = search_ace(&vrf2rib_res->flws, &acl6_ntry, &acl6_matcher, bufS - bufP + preBuff);
        if (aceh_res == NULL) goto ipv6_flwed;
        if (aceh_res->act != 0) goto ipv6_flwed;
        policer_ntry.vrf = vrf2rib_ntry.vrf;
        policer_ntry.meter = aceh_res->pri;
        policer_ntry.dir = 4;
        policer_res = hasht_find(&policer_table, &policer_ntry);
        if (policer_res == NULL) doDropper;
        if (policer_res->avail < 1) doDropper;
        policer_res->avail -= bufS - bufP + preBuff;
ipv6_flwed:
        if (table_nonexist(&vrf2rib_res->natC)) goto ipv6_nated;
        if (frag != 0) goto ipv6_nated;
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
        nat6_res = hasht_find(&vrf2rib_res->natT, &nat6_ntry);
        if (nat6_res == NULL) {
            if (apply_acl(&vrf2rib_res->natC, &acl6_ntry, &acl6_matcher, bufS - bufP + preBuff) == 0) doCpuing;
            goto ipv6_nated;
        }
        nat6_res->pack++;
        nat6_res->byte += bufS;
        acl6_ntry.srcAddr1 = nat6_res->nSrcAddr1;
        acl6_ntry.srcAddr2 = nat6_res->nSrcAddr2;
        acl6_ntry.srcAddr3 = nat6_res->nSrcAddr3;
        acl6_ntry.srcAddr4 = nat6_res->nSrcAddr4;
        acl6_ntry.trgAddr1 = nat6_res->nTrgAddr1;
        acl6_ntry.trgAddr2 = nat6_res->nTrgAddr2;
        acl6_ntry.trgAddr3 = nat6_res->nTrgAddr3;
        acl6_ntry.trgAddr4 = nat6_res->nTrgAddr4;
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
ipv6_nated:
        ttl = bufD[bufP + 7] - 1;
        if (ttl <= 1) doPunting;
        bufD[bufP + 7] = ttl;
        doSampler;
        ttl |= port2vrf_res->pttl6;
        if (table_nonexist(&vrf2rib_res->pbr)) goto ipv6_pbred;
        if (frag != 0) doPunting;
        aceh_res = search_ace(&vrf2rib_res->pbr, &acl6_ntry, &acl6_matcher, bufS - bufP + preBuff);
        if (aceh_res == NULL) goto ipv6_pbred;
        if (aceh_res->act != 0) goto ipv6_pbred;
        switch (aceh_res->cmd) {
        case 1: // normal
            break;
        case 2: // setvrf
            vrf2rib_ntry.vrf = aceh_res->vrf;
            vrf2rib_res = hasht_find(&vrf2rib6_table, &vrf2rib_ntry);
            if (vrf2rib_res == NULL) doDropper;
            break;
        case 3: // sethop
            vrf2rib_ntry.vrf = aceh_res->vrf;
            neigh_ntry.id = aceh_res->nexthop;
            neigh_res = hasht_find(&neigh_table, &neigh_ntry);
            if (neigh_res == NULL) doDropper;
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
ipv6_pbred:
        if (acl6_ntry.protV == 0) doCpuing;
        vrf2rib_res->pack++;
        vrf2rib_res->byte += bufS;
        route6_ntry.addr[0] = acl6_ntry.trgAddr1;
        route6_ntry.addr[1] = acl6_ntry.trgAddr2;
        route6_ntry.addr[2] = acl6_ntry.trgAddr3;
        route6_ntry.addr[3] = acl6_ntry.trgAddr4;
        route6_ntry.mask = 128;
        route6_res = tree_lpm(&vrf2rib_res->rou, &route6_ntry);
        if (route6_res == NULL) doPunting;
        route6_res->packTx++;
        route6_res->byteTx += bufS;
        switch (route6_res->command) {
        case 1: // route
            neigh_ntry.id = route6_res->nexthop;
            neigh_res = hasht_find(&neigh_table, &neigh_ntry);
            if (neigh_res == NULL) doDropper;
ipv6_tx:
            port2vrf_ntry.port = neigh_res->aclport;
            port2vrf_res = hasht_find(&port2vrf_table, &port2vrf_ntry);
            if (port2vrf_res == NULL) doDropper;
            if (!table_nonexist(&port2vrf_res->outacl6)) {
                if (frag != 0) doPunting;
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
                insp6_res = hasht_find(&port2vrf_res->insp6, &insp6_ntry);
                if (insp6_res == NULL) {
                    tmp = apply_acl(&port2vrf_res->outacl6, &acl6_ntry, &acl6_matcher, bufS - bufP + preBuff);
                    if (tmp == 2) doCpuing;
                    if (tmp != 0) doPunting;
                } else {
                    insp6_res->packTx++;
                    insp6_res->byteTx += bufS;
                }
            }
            bufP -= 2;
            put16msb(bufD, bufP, ethtyp);
            if (table_nonexist(&port2vrf_res->outqos6)) goto neigh_tx;
            if (frag != 0) doPunting;
            aceh_res = search_ace(&port2vrf_res->outqos6, &acl6_ntry, &acl6_matcher, bufS - bufP + preBuff);
            if (aceh_res == NULL) goto neigh_tx;
            if (aceh_res->act != 0) goto neigh_tx;
            policer_ntry.vrf = 0;
            policer_ntry.meter = aceh_res->nexthop;
            policer_ntry.dir = 2;
            policer_res = hasht_find(&policer_table, &policer_ntry);
            if (policer_res == NULL) doDropper;
            if (policer_res->avail < 1) doDropper;
            policer_res->avail -= bufS - bufP + preBuff;
            goto neigh_tx;
        case 2: // punt
            tun6_ntry.srcAddr1 = acl6_ntry.srcAddr1;
            tun6_ntry.srcAddr2 = acl6_ntry.srcAddr2;
            tun6_ntry.srcAddr3 = acl6_ntry.srcAddr3;
            tun6_ntry.srcAddr4 = acl6_ntry.srcAddr4;
            tun6_ntry.trgAddr1 = acl6_ntry.trgAddr1;
            tun6_ntry.trgAddr2 = acl6_ntry.trgAddr2;
            tun6_ntry.trgAddr3 = acl6_ntry.trgAddr3;
            tun6_ntry.trgAddr4 = acl6_ntry.trgAddr4;
            tun6_ntry.prot = acl6_ntry.protV;
            tun6_ntry.srcPort = acl6_ntry.srcPortV;
            tun6_ntry.trgPort = acl6_ntry.trgPortV;
            tun6_res = hasht_find(&vrf2rib_res->tun, &tun6_ntry);
            if (tun6_res != NULL) {
                if (frag != 0) doPunting;
                switch (doTunnel(ctx, (struct tun4_entry *)((char*)tun6_res+sizeof(tun6_ntry)-sizeof(tun4_ntry)), &bufP, &bufS, bufT)) {
                case 0:
                    prt = tun6_res->aclport;
                    goto ethtyp_rx;
                case 1:
                    doCpuing;
                }
                doDropper;
            }
            mroute6_ntry.src1 = acl6_ntry.srcAddr1;
            mroute6_ntry.src2 = acl6_ntry.srcAddr2;
            mroute6_ntry.src3 = acl6_ntry.srcAddr3;
            mroute6_ntry.src4 = acl6_ntry.srcAddr4;
            mroute6_ntry.grp1 = acl6_ntry.trgAddr1;
            mroute6_ntry.grp2 = acl6_ntry.trgAddr2;
            mroute6_ntry.grp3 = acl6_ntry.trgAddr3;
            mroute6_ntry.grp4 = acl6_ntry.trgAddr4;
            mroute6_res = hasht_find(&vrf2rib_res->mcst, &mroute6_ntry);
            if (mroute6_res != NULL) {
                if (mroute6_res->ingr != prt) doDropper;
                mroute6_res->pack++;
                mroute6_res->byte += bufS;
                doFlood(ctx, &mroute6_res->flood, bufP, bufS, ethtyp, 0x100 | ttl);
                if (mroute6_res->local != 0) doCpuing;
                return;
            }
            if (table_nonexist(&vrf2rib_res->copp)) doCpuing;
            if (frag != 0) doPunting;
            if (apply_acl(&vrf2rib_res->copp, &acl6_ntry, &acl6_matcher, bufS - bufP + preBuff) != 0) doDropper;
            doCpuing;
            doRouted(route6_res, IP_PROTOCOL_IPV6);
        }
        doDropper;
    case ETHERTYPE_PPPOE_DATA: // pppoe
        ctx->stat->packPppoe++;
        ctx->stat->bytePppoe += bufS;
        pppoe_ntry.port = prt;
        pppoe_ntry.session = get16msb(bufD, bufP + 2);
        ctx->hash ^= pppoe_ntry.session;
        pppoe_res = hasht_find(&pppoe_table, &pppoe_ntry);
        if (pppoe_res == NULL) doDropper;
        pppoe_res->pack++;
        pppoe_res->byte += bufS;
        bufP += 6;
        ethtyp = get16msb(bufD, bufP);
        ppptyp2ethtyp(bufP);
        put16msb(bufD, bufP, ethtyp);
        prt = pppoe_res->aclport;
        goto ethtyp_rx;
    case ETHERTYPE_ROUTEDMAC: // routed bridge
        if (port2vrf_res == NULL) doDropper;
        ctx->stat->packBridge++;
        ctx->stat->byteBridge += bufS;
        if (port2vrf_res->command != 2) doDropper;
        bridge_ntry.id = port2vrf_res->bridge;
        memcpy(&bufH[0], &bufD[bufP], 12);
        bufP += 12;
        bufP += 2;
bridgevpls_rx:
        bridge_ntry.mac1 = get16msb(bufH, 6);
        bridge_ntry.mac2 = get32msb(bufH, 8);
        ctx->hash ^= bridge_ntry.mac1 ^ bridge_ntry.mac2;
        bridge_res = hasht_find(&bridge_table, &bridge_ntry);
        if (bridge_res == NULL) doCpuing;
        bridge_res->packRx++;
        bridge_res->byteRx += bufS;
        bridge_ntry.mac1 = get16msb(bufH, 0);
        bridge_ntry.mac2 = get32msb(bufH, 2);
        ctx->hash ^= bridge_ntry.mac1 ^ bridge_ntry.mac2;
        bridge_res = hasht_find(&bridge_table, &bridge_ntry);
        if (bridge_res == NULL) doCpuing;
        bridge_res->packTx++;
        bridge_res->byteTx += bufS;
        bufP -= 2;
        switch (bridge_res->command) {
        case 1: // port
            send2subif(ctx, bridge_res->port, bufP, bufS, ethtyp);
            return;
        case 2: // vpls
            bufP -= 12;
            memcpy(&bufD[bufP], &bufH[0], 12);
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
            memcpy(&bufD[bufP], &bufH[0], 12);
            ethtyp = ETHERTYPE_ROUTEDMAC;
            neigh_ntry.id = bridge_res->nexthop;
            goto ethtyp_tx;
        case 4: // vxlan4
            putVxlanHeader;
            putUdpHeader(bridge_res->srcPort, bridge_res->trgPort);
            putIpv4header(IP_PROTOCOL_UDP, bridge_res->srcAddr1, bridge_res->trgAddr1);
            neigh_ntry.id = bridge_res->nexthop;
            goto nethtyp_tx;
        case 5: // vxlan6
            putVxlanHeader;
            putUdpHeader(bridge_res->srcPort, bridge_res->trgPort);
            putIpv6header(IP_PROTOCOL_UDP, bridge_res->srcAddr1, bridge_res->srcAddr2, bridge_res->srcAddr3, bridge_res->srcAddr4, bridge_res->trgAddr1, bridge_res->trgAddr2, bridge_res->trgAddr3, bridge_res->trgAddr4);
            neigh_ntry.id = bridge_res->nexthop;
            goto nethtyp_tx;
        case 6: // pckoudp4
            putPckoudpHeader;
            putUdpHeader(bridge_res->srcPort, bridge_res->trgPort);
            putIpv4header(IP_PROTOCOL_UDP, bridge_res->srcAddr1, bridge_res->trgAddr1);
            neigh_ntry.id = bridge_res->nexthop;
            goto nethtyp_tx;
        case 7: // pckoudp6
            putPckoudpHeader;
            putUdpHeader(bridge_res->srcPort, bridge_res->trgPort);
            putIpv6header(IP_PROTOCOL_UDP, bridge_res->srcAddr1, bridge_res->srcAddr2, bridge_res->srcAddr3, bridge_res->srcAddr4, bridge_res->trgAddr1, bridge_res->trgAddr2, bridge_res->trgAddr3, bridge_res->trgAddr4);
            neigh_ntry.id = bridge_res->nexthop;
            goto nethtyp_tx;
        case 8: // srv4
            putPckoudpHeader;
            putIpv4header(IP_PROTOCOL_SRL2, bridge_res->srcAddr1, bridge_res->trgAddr1);
            neigh_ntry.id = bridge_res->nexthop;
            goto nethtyp_tx;
        case 9: // srv6
            putPckoudpHeader;
            putIpv6header(IP_PROTOCOL_SRL2, bridge_res->srcAddr1, bridge_res->srcAddr2, bridge_res->srcAddr3, bridge_res->srcAddr4, bridge_res->trgAddr1, bridge_res->trgAddr2, bridge_res->trgAddr3, bridge_res->trgAddr4);
            neigh_ntry.id = bridge_res->nexthop;
            goto nethtyp_tx;
        case 10: // etherip4
            putEtheripHeader;
            putIpv4header(IP_PROTOCOL_ETHERIP, bridge_res->srcAddr1, bridge_res->trgAddr1);
            neigh_ntry.id = bridge_res->nexthop;
            goto nethtyp_tx;
        case 11: // etherip6
            putEtheripHeader;
            putIpv6header(IP_PROTOCOL_ETHERIP, bridge_res->srcAddr1, bridge_res->srcAddr2, bridge_res->srcAddr3, bridge_res->srcAddr4, bridge_res->trgAddr1, bridge_res->trgAddr2, bridge_res->trgAddr3, bridge_res->trgAddr4);
            neigh_ntry.id = bridge_res->nexthop;
            goto nethtyp_tx;
        case 12: // eoip4
            putEoipHeader;
            putIpv4header(IP_PROTOCOL_GRE, bridge_res->srcAddr1, bridge_res->trgAddr1);
            neigh_ntry.id = bridge_res->nexthop;
            goto nethtyp_tx;
        case 13: // eoip6
            putEoipHeader;
            putIpv6header(IP_PROTOCOL_GRE, bridge_res->srcAddr1, bridge_res->srcAddr2, bridge_res->srcAddr3, bridge_res->srcAddr4, bridge_res->trgAddr1, bridge_res->trgAddr2, bridge_res->trgAddr3, bridge_res->trgAddr4);
            neigh_ntry.id = bridge_res->nexthop;
            goto nethtyp_tx;
        }
        doDropper;
    case ETHERTYPE_POLKA: // polka
        if (port2vrf_res == NULL) doDropper;
        ctx->stat->packPolka++;
        ctx->stat->bytePolka += bufS;
        ttl = bufD[bufP + 1];
        if ((ttl & 0xff) <= 1) doPunting;
        ttl--;
        bufD[bufP + 1] = ttl;
        vrf2rib_ntry.vrf = port2vrf_res->vrf;
        if (bufD[bufP + 0] == 0) {
            vrf2rib_res = hasht_find(&vrf2rib4_table, &vrf2rib_ntry);
            if (vrf2rib_res == NULL) doDropper;
            polkaPoly_ntry.port = prt;
            polkaPoly_res = hasht_find(&polkaPoly_table, &polkaPoly_ntry);
            if (polkaPoly_res == NULL) doDropper;
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
            int index = table_find(&vrf2rib_res->plk, &polkaIdx_ntry);
            if (index < 0) doDropper;
            polkaIdx_res = table_get(&vrf2rib_res->plk, index);
            polkaIdx_res->pack++;
            polkaIdx_res->byte += bufS;
            neigh_ntry.id = polkaIdx_res->nexthop;
            goto ethtyp_tx;
        }
        vrf2rib_res = hasht_find(&vrf2rib6_table, &vrf2rib_ntry);
        if (vrf2rib_res == NULL) doDropper;
        polkaPoly_ntry.port = prt;
        polkaPoly_res = hasht_find(&mpolkaPoly_table, &polkaPoly_ntry);
        if (polkaPoly_res == NULL) doDropper;
        polkaPoly_res->pack++;
        polkaPoly_res->byte += bufS;
        crc16calc(tmp, polkaPoly_res->tab, bufD, bufP + 4, 14);
        tmp ^= get16msb(bufD, bufP + 18);
        struct packetContext ctx2;
        unsigned char *bufC = ctx->bufC;
        for (int i = 0; i < vrf2rib_res->plk.size; i++) {
            if ((tmp & bitVals[30 - i]) == 0) continue;
            if (shiftContext(&ctx2, ctx, bufC) != 0) break;
            polkaIdx_res = table_get(&vrf2rib_res->plk, i);
            polkaIdx_res->pack++;
            polkaIdx_res->byte += bufS;
            neigh_ntry.id = polkaIdx_res->nexthop;
            neigh_res = hasht_find(&neigh_table, &neigh_ntry);
            if (neigh_res == NULL) continue;
            int tmpP = preBuff;
            int tmpS = bufS - bufP + preBuff + 2;
            int tmpE = ethtyp;
            put16msb(bufC, preBuff, tmpE);
            memcpy(&bufC[preBuff + 2], &bufD[bufP], tmpS);
            send2neigh(&ctx2, neigh_res, tmpP, tmpS, tmpE);
        }
        if ((tmp & 1) == 0) return;
        ethtyp = get16msb(bufD, bufP + 2);
        bufP += 20;
        goto etyped_rx;
    case ETHERTYPE_NSH: // nsh
        if (port2vrf_res == NULL) doDropper;
        if (port2vrf_res->nsh == 0) doDropper;
        ctx->stat->packNsh++;
        ctx->stat->byteNsh += bufS;
nsh_rx:
        ttl = get16msb(bufD, bufP + 0);
        if ((ttl & 0xe000) != 0) doDropper;
        if (((ttl >> 6) & 0x3f) <= 1) doPunting;
        tmp = get32msb(bufD, bufP + 4);
        ethtyp = bufD[bufP + 3];
        nsh_ntry.sp = tmp >> 8;
        nsh_ntry.si = tmp & 0xff;
        ctx->hash ^= tmp;
        nsh_res = hasht_find(&nsh_table, &nsh_ntry);
        if (nsh_res == NULL) doDropper;
        nsh_res->pack++;
        nsh_res->byte += bufS;
        switch (nsh_res->command) {
        case 1: // ifc
            ttl = ttl - 0x40;
            put16msb(bufD, bufP + 0, ttl);
            put32msb(bufD, bufP + 4, nsh_res->trg);
            ethtyp = ETHERTYPE_NSH;
            bufP -= 2;
            put16msb(bufD, bufP, ethtyp);
            memcpy(&bufH[0], &nsh_res->macs, 12);
            send2subif(ctx, nsh_res->port, bufP, bufS, ethtyp);
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
            case 3:
                bufP += 12;
                goto ethtyp_rx;
            }
            return;
        case 3: // nei
            ttl = ttl - 0x40;
            put16msb(bufD, bufP + 0, ttl);
            put32msb(bufD, bufP + 4, nsh_res->trg);
            ethtyp = ETHERTYPE_NSH;
            neigh_ntry.id = nsh_res->port;
            goto ethtyp_tx;
        }
        doDropper;
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
            ctx->stat->packDr++;
            ctx->stat->byteDr += bufS - bufP + preBuff;
            return;
        }
        punts--;
cpu:
        bufP = bufE - 12;
        memcpy(&bufH[0], &bufD[preBuff], 12);
        memcpy(&bufD[bufP], &bufH[0], 12);
        bufP -= 4;
        put32msb(bufD, bufP, prt);
        send2port(cpuPort);
        return;
    }
}
