void processDataPacket(unsigned char *bufA, unsigned char *bufB, unsigned char *bufC, unsigned char *bufD, int bufS, int port, int prt, EVP_CIPHER_CTX *encrCtx, EVP_MD_CTX *hashCtx);



void send2port(unsigned char *bufD, int bufS, int port) {
    if (port < 0) return;
    if (port >= ports) return;
    sendPack(bufD, bufS, port);
    packTx[port]++;
    byteTx[port] += bufS;
}



void send2cpu(unsigned char *bufD, int bufS, int port) {
    put16msb(bufD, preBuff - 2, port);
    send2port(&bufD[preBuff - 2], bufS + 2, cpuport);
}




void processCpuPack(unsigned char* bufD, int bufS) {
    struct vlan_entry vlan_ntry;
    struct vlan_entry *vlan_res;
    int index;
    int prt;
    packRx[cpuport]++;
    byteRx[cpuport] += bufS;
    prt = get16msb(bufD, preBuff);
    send2port(&bufD[preBuff + 2], bufS - 2, prt);
    if (get16msb(bufD, preBuff + 14) != ETHERTYPE_VLAN) return;
    vlan_ntry.port = prt;
    vlan_ntry.vlan = get16msb(bufD, preBuff + 16) & 0xfff;
    index = table_find(&vlanin_table, &vlan_ntry);
    if (index < 0) return;
    vlan_res = table_get(&vlanin_table, index);
    index = table_find(&vlanout_table, vlan_res);
    if (index < 0) return;
    vlan_res = table_get(&vlanout_table, index);
    vlan_res->pack++;
    vlan_res->byte += bufS;
}


#ifdef basicLoop


void processDataPacket(unsigned char *bufA, unsigned char *bufB, unsigned char *bufC, unsigned char *bufD, int bufS, int port, int prt, EVP_CIPHER_CTX *encrCtx, EVP_MD_CTX *hashCtx) {
    struct vlan_entry vlan_ntry;
    struct vlan_entry *vlan_res;
    int index;
    packRx[port]++;
    byteRx[port] += bufS;
    send2cpu(bufD, bufS, port);
    if (get16msb(bufD, preBuff + 12) != ETHERTYPE_VLAN) return;
    vlan_ntry.port = port;
    vlan_ntry.vlan = get16msb(bufD, preBuff + 14) & 0xfff;
    index = table_find(&vlanin_table, &vlan_ntry);
    if (index < 0) return;
    vlan_res = table_get(&vlanin_table, index);
    vlan_res->pack++;
    vlan_res->byte += bufS;
}



#else




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


#define putPseudoSum(buf, pos, prt, len, ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8)    \
    put32msb(buf, pos + 0, ip1);                                            \
    put32msb(buf, pos + 4, ip2);                                            \
    put32msb(buf, pos + 8, ip3);                                            \
    put32msb(buf, pos + 12, ip4);                                           \
    put32msb(buf, pos + 16, ip5);                                           \
    put32msb(buf, pos + 20, ip6);                                           \
    put32msb(buf, pos + 24, ip7);                                           \
    put32msb(buf, pos + 28, ip8);                                           \
    put16msb(buf, pos + 32, prt);                                           \
    put16msb(buf, pos + 34, len);





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
    hash ^= ntry.srcPortV ^ ntry.trgPortV;




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
    case ETHERTYPE_ROUTEDMAC:                                   \
        bufP -= 2;                                              \
        put16msb(bufD, bufP, 1);                                \
        ethtyp = PPPTYPE_ROUTEDMAC;                             \
        break;                                                  \
    default:                                                    \
        goto drop;                                              \
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
    case PPPTYPE_ROUTEDMAC:                                     \
        ethtyp = ETHERTYPE_ROUTEDMAC;                           \
        bufP += 2;                                              \
        break;                                                  \
    default:                                                    \
        goto drop;                                              \
    }




int masks[] = {
    0x00000000,
    0x80000000, 0xc0000000, 0xe0000000, 0xf0000000,
    0xf8000000, 0xfc000000, 0xfe000000, 0xff000000,
    0xff800000, 0xffc00000, 0xffe00000, 0xfff00000,
    0xfff80000, 0xfffc0000, 0xfffe0000, 0xffff0000,
    0xffff8000, 0xffffc000, 0xffffe000, 0xfffff000,
    0xfffff800, 0xfffffc00, 0xfffffe00, 0xffffff00,
    0xffffff80, 0xffffffc0, 0xffffffe0, 0xfffffff0,
    0xfffffff8, 0xfffffffc, 0xfffffffe, 0xffffffff
};





#define putIpv4header(bufP, bufS, ethtyp, proto, sip, dip)      \
    bufP -= 20;                                                 \
    put16msb(bufD, bufP + 0, 0x4500);                           \
    put16msb(bufD, bufP + 2, (bufS - bufP + preBuff));          \
    ipids++;                                                    \
    put16msb(bufD, bufP + 4, ipids);                            \
    put16msb(bufD, bufP + 6, 0);                                \
    bufD[bufP + 8] = 0xff;                                      \
    bufD[bufP + 9] = proto;                                     \
    put16msb(bufD, bufP + 10, 0);                               \
    put32msb(bufD, bufP + 12, sip);                             \
    put32msb(bufD, bufP + 16, dip);                             \
    ethtyp = 0xffff - calcIPsum(bufD, bufP, 20, 0);             \
    put16lsb(bufD, bufP + 10, ethtyp);                          \
    ethtyp = ETHERTYPE_IPV4;                                    \
    bufP -= 2;                                                  \
    put16msb(bufD, bufP, ethtyp);



#define putIpv6header(bufP, bufS, ethtyp, proto, sip1, sip2, sip3, sip4, dip1, dip2, dip3, dip4)    \
    bufP -= 40;                                                 \
    put16msb(bufD, bufP + 0, 0x6000);                           \
    put16msb(bufD, bufP + 2, 0);                                \
    put16msb(bufD, bufP + 4, (bufS - bufP + preBuff - 40));     \
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



#define putIpipHeader(bufP, ethtyp)                             \
    switch (ethtyp) {                                           \
    case ETHERTYPE_IPV4:                                        \
        ethtyp = 4;                                             \
        break;                                                  \
    case ETHERTYPE_IPV6:                                        \
        ethtyp = 41;                                            \
        break;                                                  \
    default:                                                    \
        goto drop;                                              \
    }                                                           \
    bufP += 2;


#define ipip2ethtyp                                             \
    switch (ethtyp) {                                           \
        case 4:                                                 \
            ethtyp = ETHERTYPE_IPV4;                            \
            break;                                              \
        case 41:                                                \
            ethtyp = ETHERTYPE_IPV6;                            \
            break;                                              \
    default:                                                    \
        goto drop;                                              \
    }


#define putEspHeader(bufP, bufS, ethtyp)                        \
    tmp = bufS - bufP + preBuff;                                \
    tmp2 = neigh_res->encrBlkLen - ((tmp + 2) % neigh_res->encrBlkLen); \
    for (int i=0; i<tmp2; i++) {                                \
        bufD[bufP + tmp + i] = i+1;                             \
    }                                                           \
    tmp += tmp2;                                                \
    bufS += tmp2;                                               \
    bufD[bufP + tmp + 0] = tmp2;                                \
    bufD[bufP + tmp + 1] = ethtyp;                              \
    tmp += 2;                                                   \
    bufS += 2;                                                  \
    bufP -= neigh_res->encrBlkLen;                              \
    RAND_bytes(&bufD[bufP], neigh_res->encrBlkLen);             \
    tmp += neigh_res->encrBlkLen;                               \
    if (EVP_CIPHER_CTX_reset(encrCtx) != 1) goto drop;          \
    if (EVP_EncryptInit_ex(encrCtx, neigh_res->encrAlg, NULL, neigh_res->encrKeyDat, neigh_res->hashKeyDat) != 1) goto drop;    \
    if (EVP_CIPHER_CTX_set_padding(encrCtx, 0) != 1) goto drop; \
    if (EVP_EncryptUpdate(encrCtx, &bufD[bufP], &tmp2, &bufD[bufP], tmp) != 1) goto drop;   \
    bufP -= 8;                                                  \
    put32msb(bufD, bufP + 0, neigh_res->spi);                   \
    put32msb(bufD, bufP + 4, neigh_res->seq);                   \
    tmp += 8;                                                   \
    if (EVP_MD_CTX_reset(hashCtx) != 1) goto drop;              \
    if (EVP_DigestSignInit(hashCtx, NULL, neigh_res->hashAlg, NULL, neigh_res->hashPkey) != 1) goto drop;   \
    if (EVP_DigestSignUpdate(hashCtx, &bufD[bufP], tmp) != 1) goto drop;    \
    if (EVP_DigestSignFinal(hashCtx, &bufD[bufP + tmp], &sizt) != 1) goto drop; \
    bufS += neigh_res->hashBlkLen;                              \
    neigh_res->seq++;


#define decapEsp(tun_res)                                       \
    bufP = bufT;                                                \
    if (get32msb(bufD, bufP + 0) != tun_res->spi) goto drop;    \
    tun_res->seq = get32msb(bufD, bufP + 4);                    \
    tmp = bufS - bufP + preBuff - tun_res->hashBlkLen;          \
    if (tmp < 1) goto drop;                                     \
    if (EVP_MD_CTX_reset(hashCtx) != 1) goto drop;              \
    if (EVP_DigestSignInit(hashCtx, NULL, tun_res->hashAlg, NULL, tun_res->hashPkey) != 1) goto drop; \
    if (EVP_DigestSignUpdate(hashCtx, &bufD[bufP], tmp) != 1) goto drop;    \
    if (EVP_DigestSignFinal(hashCtx, &bufH[0], &sizt) != 1) goto drop;      \
    if (memcmp(&bufH[0], &bufD[bufP + tmp], tun_res->hashBlkLen) !=0) goto drop;   \
    bufS -= tun_res->hashBlkLen;                                \
    bufP += 8;                                                  \
    tmp -= 8;                                                   \
    if (EVP_CIPHER_CTX_reset(encrCtx) != 1) goto drop;          \
    if (EVP_DecryptInit_ex(encrCtx, tun_res->encrAlg, NULL, tun_res->encrKeyDat, tun_res->hashKeyDat) != 1) goto drop;   \
    if (EVP_CIPHER_CTX_set_padding(encrCtx, 0) != 1) goto drop; \
    if (EVP_DecryptUpdate(encrCtx, &bufD[bufP], &tmp2, &bufD[bufP], tmp) != 1) goto drop;   \
    bufP += tun_res->encrBlkLen;                                \
    tmp -= tun_res->encrBlkLen;                                 \
    ethtyp = bufD[bufP + tmp - 1];                              \
    bufS -= bufD[bufP + tmp - 2] + 2;                           \
    ipip2ethtyp;                                                \
    bufP -= 2;                                                  \
    put16msb(bufD, bufP, ethtyp);



#define putUdpHeader(bufP, bufS, sprt, dprt, sip1, sip2, sip3, sip4, dip1, dip2, dip3, dip4)    \
    bufP -= 8;                                                  \
    put16msb(bufD, bufP + 0, sprt);                             \
    put16msb(bufD, bufP + 2, dprt);                             \
    put16msb(bufD, bufP + 4, (bufS - bufP + preBuff));          \
    put16msb(bufD, bufP + 6, 0);                                \
    putPseudoSum(bufH, 16, 17, (bufS - bufP + preBuff), sip1, sip2, sip3, sip4, dip1, dip2, dip3, dip4);    \
    tmp = calcIPsum(bufH, 16, 36, 0);                           \
    tmp = calcIPsum(bufD, bufP, bufS - bufP + preBuff, tmp);    \
    put16lsb(bufD, bufP + 6, (0xffff - tmp));



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
    put32msb(bufD, bufP + 4, bridge_res->instance << 8);


#define putPckoudpHeader                                        \
    bufP -= 12;                                                 \
    memmove(&bufD[bufP], &bufH[0], 12);


#define putOpenvpnHeader(bufP, bufS)                            \
    bufP += 2;                                                  \
    bufP -= 8;                                                  \
    put32msb(bufD, bufP + 0, neigh_res->seq);                   \
    put32msb(bufD, bufP + 4, neigh_res->tid);                   \
    tmp = bufS - bufP + preBuff;                                \
    tmp2 = neigh_res->encrBlkLen - (tmp % neigh_res->encrBlkLen);   \
    for (int i=0; i<tmp2; i++) {                                \
        bufD[bufP + tmp + i] = tmp2;                            \
    }                                                           \
    tmp += tmp2;                                                \
    bufS += tmp2;                                               \
    bufP -= neigh_res->encrBlkLen;                              \
    RAND_bytes(&bufD[bufP], neigh_res->encrBlkLen);             \
    tmp += neigh_res->encrBlkLen;                               \
    if (EVP_CIPHER_CTX_reset(encrCtx) != 1) goto drop;          \
    if (EVP_EncryptInit_ex(encrCtx, neigh_res->encrAlg, NULL, neigh_res->encrKeyDat, neigh_res->hashKeyDat) != 1) goto drop;    \
    if (EVP_CIPHER_CTX_set_padding(encrCtx, 0) != 1) goto drop; \
    if (EVP_EncryptUpdate(encrCtx, &bufD[bufP], &tmp2, &bufD[bufP], tmp) != 1) goto drop;   \
    if (EVP_MD_CTX_reset(hashCtx) != 1) goto drop;              \
    if (EVP_DigestSignInit(hashCtx, NULL, neigh_res->hashAlg, NULL, neigh_res->hashPkey) != 1) goto drop;   \
    if (EVP_DigestSignUpdate(hashCtx, &bufD[bufP], tmp) != 1) goto drop;    \
    bufP -= neigh_res->hashBlkLen;                              \
    if (EVP_DigestSignFinal(hashCtx, &bufD[bufP], &sizt) != 1) goto drop; \
    neigh_res->seq++;


#define guessEthtyp                                             \
    switch (bufD[bufP] & 0xf0) {                                \
        case 0x40:                                              \
            ethtyp = ETHERTYPE_IPV4;                            \
            break;                                              \
        case 0x60:                                              \
            ethtyp = ETHERTYPE_IPV6;                            \
            break;                                              \
    default:                                                    \
        goto drop;                                              \
    }


#define decapOpenvpn(tun_res)                                   \
    bufP = bufT + 8;                                            \
    bufP += tun_res->hashBlkLen;                                \
    tmp = bufS - bufP + preBuff;                                \
    if (tmp < 1) goto drop;                                     \
    if (EVP_MD_CTX_reset(hashCtx) != 1) goto drop;              \
    if (EVP_DigestSignInit(hashCtx, NULL, tun_res->hashAlg, NULL, tun_res->hashPkey) != 1) goto drop; \
    if (EVP_DigestSignUpdate(hashCtx, &bufD[bufP], tmp) != 1) goto drop;    \
    if (EVP_DigestSignFinal(hashCtx, &bufH[0], &sizt) != 1) goto drop;      \
    if (memcmp(&bufH[0], &bufD[bufP - tun_res->hashBlkLen], tun_res->hashBlkLen) !=0) goto drop;    \
    if (EVP_CIPHER_CTX_reset(encrCtx) != 1) goto drop;          \
    if (EVP_DecryptInit_ex(encrCtx, tun_res->encrAlg, NULL, tun_res->encrKeyDat, tun_res->hashKeyDat) != 1) goto drop;   \
    if (EVP_CIPHER_CTX_set_padding(encrCtx, 0) != 1) goto drop; \
    if (EVP_DecryptUpdate(encrCtx, &bufD[bufP], &tmp2, &bufD[bufP], tmp) != 1) goto drop;   \
    bufP += tun_res->encrBlkLen;                                \
    tmp -= tun_res->encrBlkLen;                                 \
    bufP += 8;                                                  \
    guessEthtyp;                                                \
    bufP -= 2;                                                  \
    put16msb(bufD, bufP, ethtyp);


#define putWireguardHeader(bufP, bufS)                          \
    bufP += 2;                                                  \
    tmp = bufS - bufP + preBuff;                                \
    tmp2 = 16 - (tmp % 16);                                     \
    for (int i=0; i<tmp2; i++) {                                \
        bufD[bufP + tmp + i] = 0;                               \
    }                                                           \
    tmp += tmp2;                                                \
    bufS += tmp2;                                               \
    put32lsb(bufH, 16, 0);                                      \
    put32lsb(bufH, 20, neigh_res->seq);                         \
    put32lsb(bufH, 24, 0);                                      \
    if (EVP_CIPHER_CTX_reset(encrCtx) != 1) goto drop;          \
    if (EVP_EncryptInit_ex(encrCtx, EVP_chacha20_poly1305(), NULL, neigh_res->encrKeyDat, &bufH[16]) != 1) goto drop;   \
    if (EVP_CIPHER_CTX_set_padding(encrCtx, 0) != 1) goto drop; \
    if (EVP_EncryptUpdate(encrCtx, &bufD[bufP], &tmp2, &bufD[bufP], tmp) != 1) goto drop;   \
    if (EVP_EncryptFinal_ex(encrCtx, &bufD[bufP + tmp], &tmp2) != 1) goto drop; \
    if (EVP_CIPHER_CTX_ctrl(encrCtx, EVP_CTRL_AEAD_GET_TAG, 16, &bufD[bufP + tmp]) != 1) goto drop; \
    tmp += 16;                                                  \
    bufS += 16;                                                 \
    bufP -= 16;                                                 \
    put32lsb(bufD, bufP + 0, 4);                                \
    put32msb(bufD, bufP + 4, neigh_res->tid);                   \
    put32lsb(bufD, bufP + 8, neigh_res->seq);                   \
    put32lsb(bufD, bufP + 12, 0);                               \
    neigh_res->seq++;


#define decapWireguard(tun_res)                                 \
    bufP = bufT + 8;                                            \
    if (get32lsb(bufD, bufP) != 4) goto cpu;                    \
    tmp = bufS - bufP + preBuff;                                \
    if (tmp < 32) goto drop;                                    \
    put32msb(bufD, bufP + 4, 0);                                \
    bufP += 16;                                                 \
    bufS -= 16;                                                 \
    tmp -= 32;                                                  \
    if (EVP_CIPHER_CTX_reset(encrCtx) != 1) goto drop;          \
    if (EVP_DecryptInit_ex(encrCtx, EVP_chacha20_poly1305(), NULL, tun_res->encrKeyDat, &bufD[bufP - 12]) != 1) goto drop;  \
    if (EVP_CIPHER_CTX_set_padding(encrCtx, 0) != 1) goto drop; \
    if (EVP_CIPHER_CTX_ctrl(encrCtx, EVP_CTRL_AEAD_SET_TAG, 16, &bufD[bufP + tmp]) != 1) goto drop; \
    if (EVP_DecryptUpdate(encrCtx, &bufD[bufP], &tmp2, &bufD[bufP], tmp) != 1) goto drop;   \
    if (EVP_DecryptFinal_ex(encrCtx, &bufD[bufP + tmp], &tmp2) != 1) goto drop; \
    guessEthtyp;                                                \
    bufP -= 2;                                                  \
    put16msb(bufD, bufP, ethtyp);



#define checkLayer2                                             \
    portvrf_ntry.port = prt;                                    \
    index = table_find(&portvrf_table, &portvrf_ntry);          \
    if (index >= 0) {                                           \
        portvrf_res = table_get(&portvrf_table, index);         \
        switch (portvrf_res->command) {                         \
        case 2:                                                 \
            goto bridge_rx;                                     \
        case 3:                                                 \
            goto xconn_rx;                                      \
        }                                                       \
    }



int macsec_apply(int prt, EVP_CIPHER_CTX *encrCtx, EVP_MD_CTX *hashCtx, unsigned char *bufD, int *bufP, int *bufS, unsigned char *bufH, int *ethtyp) {
    struct macsec_entry macsec_ntry;
    struct macsec_entry *macsec_res;
    size_t sizt;
    macsec_ntry.port = prt;
    int index = table_find(&macsec_table, &macsec_ntry);
    if (index < 0) return 0;
    macsec_res = table_get(&macsec_table, index);
    macsec_res->packTx++;
    macsec_res->byteTx += *bufS;
    int tmp = *bufS - *bufP + preBuff;
    int tmp2 = tmp % macsec_res->encrBlkLen;
    if (tmp2 != 0) {
        tmp2 = macsec_res->encrBlkLen - tmp2;
        RAND_bytes(&bufD[*bufP + tmp], tmp2);
        *bufS += tmp2;
    }
    *bufP -= macsec_res->encrBlkLen;
    RAND_bytes(&bufD[*bufP], macsec_res->encrBlkLen);
    tmp = *bufS - *bufP + preBuff;
    if (EVP_CIPHER_CTX_reset(encrCtx) != 1) return 1;
    if (EVP_EncryptInit_ex(encrCtx, macsec_res->encrAlg, NULL, macsec_res->encrKeyDat, macsec_res->hashKeyDat) != 1) return 1;
    if (EVP_CIPHER_CTX_set_padding(encrCtx, 0) != 1) return 1;
    if (EVP_EncryptUpdate(encrCtx, &bufD[*bufP], &tmp2, &bufD[*bufP], tmp) != 1) return 1;
    if (EVP_MD_CTX_reset(hashCtx) != 1) return 1;
    if (EVP_DigestSignInit(hashCtx, NULL, macsec_res->hashAlg, NULL, macsec_res->hashPkey) != 1) return 1;
    if (macsec_res->needMacs != 0) {
        if (EVP_DigestSignUpdate(hashCtx, &bufH[6], 6) != 1) return 1;
        if (EVP_DigestSignUpdate(hashCtx, &bufH[0], 6) != 1) return 1;
    }
    if (EVP_DigestSignUpdate(hashCtx, &bufD[*bufP], tmp) != 1) return 1;
    if (EVP_DigestSignFinal(hashCtx, &bufD[*bufP + tmp], &sizt) != 1) return 1;
    *bufS += macsec_res->hashBlkLen;
    *bufP -= 8;
    *ethtyp = macsec_res->ethtyp;
    put16msb(bufD, *bufP + 0, macsec_res->ethtyp);
    bufD[*bufP + 2] = 0x08; // tci=v,e
    bufD[*bufP + 3] = 0; // sl
    put32msb(bufD, *bufP + 4, macsec_res->seqTx);
    macsec_res->seqTx++;
    return 0;
}



int send2subif(int prt, EVP_CIPHER_CTX *encrCtx, EVP_MD_CTX *hashCtx, int hash, unsigned char *bufD, int *bufP, int *bufS, unsigned char *bufH, int *ethtyp) {
    if (macsec_apply(prt, encrCtx, hashCtx, bufD, &*bufP, &*bufS, bufH, &*ethtyp) != 0) return -1;
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
        if (macsec_apply(prt, encrCtx, hashCtx, bufD, &*bufP, &*bufS, bufH, &*ethtyp) != 0) return -1;
    }
    *bufP -= 12;
    memmove(&bufD[*bufP], &bufH[0], 12);
    bundle_ntry.id = prt;
    index = table_find(&bundle_table, &bundle_ntry);
    if (index >= 0) {
        bundle_res = table_get(&bundle_table, index);
        hash = ((hash >> 16) ^ hash) & 0xffff;
        hash = ((hash >> 8) ^ hash) & 0xff;
        hash = ((hash >> 4) ^ hash) & 0xf;
        prt = bundle_res->out[hash];
        bundle_res->pack++;
        bundle_res->byte += *bufS;
        if (bundle_res->command == 2) {
            *bufS = *bufS - *bufP + preBuff;
            memmove(&bufD[preBuff], &bufD[*bufP], *bufS);
            return prt;
        }
    }
    send2port(&bufD[*bufP], *bufS - *bufP + preBuff, prt);
    return -1;
}



int send2neigh(struct neigh_entry *neigh_res, EVP_CIPHER_CTX *encrCtx, EVP_MD_CTX *hashCtx, int hash, unsigned char *bufD, int *bufP, int *bufS, unsigned char *bufH, int *ethtyp) {
    int tmp;
    int tmp2;
    size_t sizt;
    neigh_res->pack++;
    neigh_res->byte += *bufS;
    int prt = neigh_res->port;
    memmove(&bufH[0], &neigh_res->dmac, 6);
    memmove(&bufH[6], &neigh_res->smac, 6);
    if (neigh_res->aclport != prt) {
        if (macsec_apply(neigh_res->aclport, encrCtx, hashCtx, bufD, &*bufP, &*bufS, bufH, &*ethtyp) != 0) goto drop;
    }
    switch (neigh_res->command) {
    case 1: // raw ip
        break;
    case 2: // pppoe
        ethtyp2ppptyp(*bufP, *ethtyp);
        put16msb(bufD, *bufP, *ethtyp);
        tmp = *bufS - *bufP + preBuff;
        *bufP -= 6;
        put16msb(bufD, *bufP + 0, 0x1100);
        put16msb(bufD, *bufP + 2, neigh_res->session);
        put16msb(bufD, *bufP + 4, tmp);
        *ethtyp = ETHERTYPE_PPPOE_DATA;
        *bufP -= 2;
        put16msb(bufD, *bufP, *ethtyp);
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
        putIpipHeader(*bufP, *ethtyp);
        putIpv4header(*bufP, *bufS, *ethtyp, *ethtyp, neigh_res->sip1, neigh_res->dip1);
        break;
    case 8: // ipip6
        putIpipHeader(*bufP, *ethtyp);
        putIpv6header(*bufP, *bufS, *ethtyp, *ethtyp, neigh_res->sip1, neigh_res->sip2, neigh_res->sip3, neigh_res->sip4, neigh_res->dip1, neigh_res->dip2, neigh_res->dip3, neigh_res->dip4);
        break;
    case 9: // esp4
        putIpipHeader(*bufP, *ethtyp);
        putEspHeader(*bufP, *bufS, *ethtyp);
        putIpv4header(*bufP, *bufS, *ethtyp, 50, neigh_res->sip1, neigh_res->dip1);
        break;
    case 10: // esp6
        putIpipHeader(*bufP, *ethtyp);
        putEspHeader(*bufP, *bufS, *ethtyp);
        putIpv6header(*bufP, *bufS, *ethtyp, 50, neigh_res->sip1, neigh_res->sip2, neigh_res->sip3, neigh_res->sip4, neigh_res->dip1, neigh_res->dip2, neigh_res->dip3, neigh_res->dip4);
        break;
    case 11: // openvpn4
        putOpenvpnHeader(*bufP, *bufS);
        putUdpHeader(*bufP, *bufS, neigh_res->sprt, neigh_res->dprt, neigh_res->sip1, 0, 0, 0, neigh_res->dip1, 0, 0, 0);
        putIpv4header(*bufP, *bufS, *ethtyp, 17, neigh_res->sip1, neigh_res->dip1);
        break;
    case 12: // openvpn6
        putOpenvpnHeader(*bufP, *bufS);
        putUdpHeader(*bufP, *bufS, neigh_res->sprt, neigh_res->dprt, neigh_res->sip1, neigh_res->sip2, neigh_res->sip3, neigh_res->sip4, neigh_res->dip1, neigh_res->dip2, neigh_res->dip3, neigh_res->dip4);
        putIpv6header(*bufP, *bufS, *ethtyp, 17, neigh_res->sip1, neigh_res->sip2, neigh_res->sip3, neigh_res->sip4, neigh_res->dip1, neigh_res->dip2, neigh_res->dip3, neigh_res->dip4);
        break;
    case 13: // wireguard4
        putWireguardHeader(*bufP, *bufS);
        putUdpHeader(*bufP, *bufS, neigh_res->sprt, neigh_res->dprt, neigh_res->sip1, 0, 0, 0, neigh_res->dip1, 0, 0, 0);
        putIpv4header(*bufP, *bufS, *ethtyp, 17, neigh_res->sip1, neigh_res->dip1);
        break;
    case 14: // wireguard6
        putWireguardHeader(*bufP, *bufS);
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
    default:
        goto drop;
    }
    return send2subif(prt, encrCtx, hashCtx, hash, &*bufD, &*bufP, &*bufS, bufH, &*ethtyp);
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



void doFlood(struct table_head flood, EVP_CIPHER_CTX *encrCtx, EVP_MD_CTX *hashCtx, int hash, unsigned char *bufA, unsigned char *bufB, unsigned char *bufC, unsigned char *bufD, int bufP, int bufS, unsigned char *bufH, int ethtyp, int label, int port) {
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
            tmp = send2subif(flood_res->trg, encrCtx, hashCtx, hash, bufC, &tmpP, &tmp2, bufH, &tmpE);
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
            tmp = send2neigh(neigh_res, encrCtx, hashCtx, hash, bufC, &tmpP, &tmp2, bufH, &tmpE);
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
            tmp = send2neigh(neigh_res, encrCtx, hashCtx, hash, bufC, &tmpP, &tmp2, bufH, &tmpE);
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
            tmp = send2neigh(neigh_res, encrCtx, hashCtx, hash, bufC, &tmpP, &tmp2, bufH, &tmpE);
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
    route4_ntry.vrf = mpls_res->vrf;                                \
    route6_ntry.vrf = mpls_res->vrf;                                \
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
    goto drop;


#define doTunneled(tun_res)                                         \
    switch (tun_res->command) {                                     \
    case 1:                                                         \
        bufP = bufT + 2;                                            \
        break;                                                      \
    case 2:                                                         \
        bufP = bufT + 8;                                            \
        if ((get16msb(bufD, bufP) & 0x8000) != 0) goto cpu;         \
        bufP += 8;                                                  \
        bufP += 2;                                                  \
        ethtyp = get16msb(bufD, bufP);                              \
        if ((ethtyp & 0x8000) != 0) goto cpu;                       \
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
        decapEsp(tun_res);                                          \
        break;                                                      \
    case 7:                                                         \
        bufP = bufT + 8;                                            \
        bufP -= 2;                                                  \
        put16msb(bufD, bufP, ETHERTYPE_ROUTEDMAC);                  \
        break;                                                      \
    case 8:                                                         \
        decapOpenvpn(tun_res);                                      \
        break;                                                      \
    case 9:                                                         \
        decapWireguard(tun_res);                                    \
        break;                                                      \
    case 10:                                                        \
        bufP = bufT + 8;                                            \
        if (bufD[bufP] != 6) goto cpu;                              \
        bufP += 2;                                                  \
        guessEthtyp;                                                \
        bufP -= 2;                                                  \
        put16msb(bufD, bufP, ethtyp);                               \
        break;                                                      \
    default:                                                        \
        goto drop;                                                  \
    }                                                               \
    bufP -= 12;                                                     \
    memset(&bufD[bufP], 0, 12);                                     \
    tun_res->pack++;                                                \
    tun_res->byte += bufS;                                          \
    bufS = bufS - bufP + preBuff;                                   \
    memmove(&bufD[preBuff], &bufD[bufP], bufS);                     \
    prt2 = prt = tun_res->aclport;                                  \
    goto ether_rx;                                                  \


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
        route4_ntry.vrf = route_res->srv1;                          \
        bufP = bufT;                                                \
        ethtyp = ETHERTYPE_IPV4;                                    \
        goto ipv4_rx;                                               \
    case 7:                                                         \
        route6_ntry.vrf = route_res->srv1;                          \
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




void processDataPacket(unsigned char *bufA, unsigned char *bufB, unsigned char *bufC, unsigned char *bufD, int bufS, int port, int prt, EVP_CIPHER_CTX *encrCtx, EVP_MD_CTX *hashCtx) {
    packRx[port]++;
    byteRx[port] += bufS;
    unsigned char bufH[preBuff];
    struct nsh_entry nsh_ntry;
    struct polkaPoly_entry polkaPoly_ntry;
    struct polkaIdx_entry polkaIdx_ntry;
    struct mpls_entry mpls_ntry;
    struct portvrf_entry portvrf_ntry;
    struct route4_entry route4_ntry;
    struct route6_entry route6_ntry;
    struct neigh_entry neigh_ntry;
    struct vlan_entry vlan_ntry;
    struct bridge_entry bridge_ntry;
    struct acls_entry acls_ntry;
    struct acl4_entry acl4_ntry;
    struct acl6_entry acl6_ntry;
    struct nat4_entry nat4_ntry;
    struct nat6_entry nat6_ntry;
    struct pppoe_entry pppoe_ntry;
    struct tun4_entry tun4_ntry;
    struct tun6_entry tun6_ntry;
    struct macsec_entry macsec_ntry;
    struct policer_entry policer_ntry;
    struct monitor_entry monitor_ntry;
    struct mroute4_entry mroute4_ntry;
    struct mroute6_entry mroute6_ntry;
    struct polkaPoly_entry *polkaPoly_res = NULL;
    struct polkaIdx_entry *polkaIdx_res = NULL;
    struct nsh_entry *nsh_res = NULL;
    struct mpls_entry *mpls_res = NULL;
    struct portvrf_entry *portvrf_res = NULL;
    struct route4_entry *route4_res = NULL;
    struct route6_entry *route6_res = NULL;
    struct neigh_entry *neigh_res = NULL;
    struct vlan_entry *vlan_res = NULL;
    struct bridge_entry *bridge_res = NULL;
    struct acls_entry *acls_res = NULL;
    struct aclH_entry *aceh_res = NULL;
    struct nat4_entry *nat4_res = NULL;
    struct nat6_entry *nat6_res = NULL;
    struct pppoe_entry *pppoe_res = NULL;
    struct tun4_entry *tun4_res = NULL;
    struct tun6_entry *tun6_res = NULL;
    struct macsec_entry *macsec_res = NULL;
    struct policer_entry *policer_res = NULL;
    struct monitor_entry *monitor_res = NULL;
    struct mroute4_entry *mroute4_res = NULL;
    struct mroute6_entry *mroute6_res = NULL;
    int index = 0;
    int label = 0;
    int sum = 0;
    int ttl = 0;
    int hash = 0;
    int bufP = 0;
    int bufT = 0;
    int frag = 0;
    int ethtyp = 0;
    int prt2 = prt;
    int tmp = 0;
    int tmp2 = 0;
    int i = 0;
    size_t sizt = 0;
ether_rx:
    bufP = preBuff;
    bufP += 6 * 2; // dmac, smac
ethtyp_rx:
    ethtyp = get16msb(bufD, bufP);
    bufP += 2;
    macsec_ntry.port = prt;
    index = table_find(&macsec_table, &macsec_ntry);
    if (index >= 0) {
        macsec_res = table_get(&macsec_table, index);
        macsec_res->packRx++;
        macsec_res->byteRx += bufS;
        if (ethtyp != macsec_res->ethtyp) goto drop;
        if (bufD[bufP] != 0x08) goto cpu;
        macsec_res->seqRx = get32msb(bufD, bufP + 2);
        bufP += 6;
        tmp = bufS - bufP + preBuff - macsec_res->hashBlkLen;
        if (tmp < 1) goto drop;
        if ((tmp % macsec_res->encrBlkLen) != 0) goto drop;
        if (EVP_MD_CTX_reset(hashCtx) != 1) goto drop;
        if (EVP_DigestSignInit(hashCtx, NULL, macsec_res->hashAlg, NULL, macsec_res->hashPkey) != 1) goto drop;
        if (macsec_res->needMacs != 0) {
            if (EVP_DigestSignUpdate(hashCtx, &bufD[preBuff + 6], 6) != 1) goto drop;
            if (EVP_DigestSignUpdate(hashCtx, &bufD[preBuff + 0], 6) != 1) goto drop;
        }
        if (EVP_DigestSignUpdate(hashCtx, &bufD[bufP], tmp) != 1) goto drop;
        if (EVP_DigestSignFinal(hashCtx, &bufH[0], &sizt) != 1) goto drop;
        if (memcmp(&bufH[0], &bufD[bufP + tmp], macsec_res->hashBlkLen) !=0) goto drop;
        bufS -= macsec_res->hashBlkLen;
        if (EVP_CIPHER_CTX_reset(encrCtx) != 1) goto drop;
        if (EVP_DecryptInit_ex(encrCtx, macsec_res->encrAlg, NULL, macsec_res->encrKeyDat, macsec_res->hashKeyDat) != 1) goto drop;
        if (EVP_CIPHER_CTX_set_padding(encrCtx, 0) != 1) goto drop;
        if (EVP_DecryptUpdate(encrCtx, &bufD[bufP], &tmp2, &bufD[bufP], tmp) != 1) goto drop;
        bufP += macsec_res->encrBlkLen;
        tmp -= macsec_res->encrBlkLen;
        memmove(&bufD[preBuff + 12], &bufD[bufP], tmp);
        bufP = preBuff + 12;
        bufS = tmp + 12;
        prt2 = prt;
        ethtyp = get16msb(bufD, bufP);
        bufP += 2;
    }
    monitor_ntry.port = prt;
    index = table_find(&monitor_table, &monitor_ntry);
    if (index >= 0) {
        monitor_res = table_get(&monitor_table, index);
        if ((monitor_res->packets++%monitor_res->sample) == 0) {
            int tmpS = bufS - bufP + preBuff + 2;
            if (tmpS > monitor_res->truncate) tmpS = monitor_res->truncate;
            memmove(&bufC[preBuff], &bufD[bufP - 2], tmpS);
            memmove(&bufH[0], &bufD[preBuff], 12);
            int tmpP = preBuff;
            int tmpE = ethtyp;
            send2subif(monitor_res->target, encrCtx, hashCtx, hash, bufC, &tmpP, &tmpS, bufH, &tmpE);
        }
    }
etyped_rx:
    switch (ethtyp) {
    case ETHERTYPE_MPLS_UCAST: // mpls
        checkLayer2;
mpls_rx:
        label = get32msb(bufD, bufP);
        ttl = (label & 0xff) - 1;
        if (ttl <= 1) goto punt;
        bufP += 4;
        mpls_ntry.label = (label >> 12) & 0xfffff;
        hash ^= mpls_ntry.label;
        index = table_find(&mpls_table, &mpls_ntry);
        if (index < 0) goto drop;
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
            goto drop;
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
            if (index < 0) goto drop;
            neigh_res = table_get(&neigh_table, index);
neigh_tx:
            prt2 = prt = send2neigh(neigh_res, encrCtx, hashCtx, hash, bufD, &bufP, &bufS, bufH, &ethtyp);
            if (prt2 >= 0) goto ether_rx;
            return;
        case 4: // xconn
            memmove(&bufH[0], &bufD[bufP], 12);
            bufP += 12;
            prt = mpls_res->port;
            prt2 = prt = send2subif(prt, encrCtx, hashCtx, hash, bufD, &bufP, &bufS, bufH, &ethtyp);
            if (prt2 >= 0) goto ether_rx;
            return;
        case 5: // vpls
            memmove(&bufH[0], &bufD[bufP], 12);
            bufP += 12;
            bufP += 2;
            bridge_ntry.id = mpls_res->bridge;
            goto bridgevpls_rx;
        case 6: // punt
            goto cpu;
        case 7: // dup
            doFlood(mpls_res->flood, encrCtx, hashCtx, hash, bufA, bufB, bufC, bufD, bufP, bufS, bufH, ethtyp, (label & 0xf00) | ttl, port);
            if (mpls_res->swap != 0) goto mpls_rou;
            return;
        case 8: // bier
            if ((label & 0x100) == 0) goto drop;
            if (bufD[bufP] != 0x50) goto drop;
            doFlood(mpls_res->flood, encrCtx, hashCtx, hash, bufA, bufB, bufC, bufD, bufP, bufS, bufH, ethtyp, (label & 0xf00) | ttl, port);
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
        checkLayer2;
        vlan_ntry.port = prt;
        vlan_ntry.vlan = get16msb(bufD, bufP) & 0xfff;
        bufP += 2;
        index = table_find(&vlanin_table, &vlan_ntry);
        if (index < 0) goto drop;
        vlan_res = table_get(&vlanin_table, index);
        prt = vlan_res->id;
        vlan_res->pack++;
        vlan_res->byte += bufS;
        goto ethtyp_rx;
    case ETHERTYPE_IPV4: // ipv4
        checkLayer2;
        if (index < 0) goto drop;
        route4_ntry.vrf = portvrf_res->vrf;
ipv4_rx:
        if ((bufD[bufP + 0] & 0xf0) != 0x40) goto drop;
        bufT = bufD[bufP + 0] & 0xf;
        if (bufT < 5) goto drop;
        ttl = get16msb(bufD, bufP + 2) + bufP - preBuff;
        if (ttl > bufS) goto drop;
        bufS = ttl;
        bufT = bufP + (bufT << 2);
        frag = get16msb(bufD, bufP + 6) & 0x3fff;
        acl4_ntry.protV = bufD[bufP + 9];
        acl4_ntry.tosV = bufD[bufP + 1];
        acl4_ntry.flowV = get16msb(bufD, bufP + 4);
        acl4_ntry.srcAddr = mroute4_ntry.src = get32msb(bufD, bufP + 12);
        acl4_ntry.trgAddr = mroute4_ntry.grp =route4_ntry.addr = get32msb(bufD, bufP + 16);
        hash ^= acl4_ntry.srcAddr ^ acl4_ntry.trgAddr;
        ttl = bufD[bufP + 8] - 1;
        if (ttl <= 1) goto punt;
        bufD[bufP + 8] = ttl;
        update_chksum(bufP + 10, -1);
        extract_layer4(acl4_ntry, portvrf_res->tcpmss4);
        acls_ntry.ver = 4;
        acls_ntry.dir = 1;
        acls_ntry.port = prt;
        index = table_find(&acls_table, &acls_ntry);
        if (index >= 0) {
            if (frag != 0) goto punt;
            acls_res = table_get(&acls_table, index);
            if (apply_acl(&acls_res->aces, &acl4_ntry, &acl4_matcher, bufS - bufP + preBuff) != 0) goto punt;
        }
        acls_ntry.dir = 6;
        index = table_find(&acls_table, &acls_ntry);
        if (index >= 0) {
            if (frag != 0) goto punt;
            acls_res = table_get(&acls_table, index);
            if (apply_acl(&acls_res->aces, &acl4_ntry, &acl4_matcher, bufS - bufP + preBuff) != 0) goto ipv4_qosed;
            policer_ntry.vrf = 0;
            policer_ntry.meter = acls_res->hop;
            policer_ntry.dir = 1;
            index = table_find(&policer_table, &policer_ntry);
            if (index < 0) goto drop;
            policer_res = table_get(&policer_table, index);
            if (policer_res->avail < 1) goto drop;
            policer_res->avail -= bufS - bufP + preBuff;
        }
ipv4_qosed:
        acls_ntry.dir = 8;
        acls_ntry.port = route4_ntry.vrf;
        index = table_find(&acls_table, &acls_ntry);
        if (index >= 0) {
            acls_res = table_get(&acls_table, index);
            aceh_res = search_ace(&acls_res->aces, &acl4_ntry, &acl4_matcher, bufS - bufP + preBuff);
            if (aceh_res == NULL) goto ipv4_flwed;
            if (aceh_res->act != 0) goto ipv4_flwed;
            policer_ntry.vrf = route4_ntry.vrf;
            policer_ntry.meter = aceh_res->pri;
            policer_ntry.dir = 3;
            index = table_find(&policer_table, &policer_ntry);
            if (index < 0) goto drop;
            policer_res = table_get(&policer_table, index);
            if (policer_res->avail < 1) goto drop;
            policer_res->avail -= bufS - bufP + preBuff;
        }
ipv4_flwed:
        nat4_ntry.vrf = route4_ntry.vrf;
        nat4_ntry.prot = acl4_ntry.protV;
        nat4_ntry.oSrcAddr = acl4_ntry.srcAddr;
        nat4_ntry.oTrgAddr = acl4_ntry.trgAddr;
        nat4_ntry.oSrcPort = acl4_ntry.srcPortV;
        nat4_ntry.oTrgPort = acl4_ntry.trgPortV;
        index = table_find(&nat4_table, &nat4_ntry);
        if (index >= 0) {
            if (frag != 0) goto ipv4_natted;
            nat4_res = table_get(&nat4_table, index);
            nat4_res->pack++;
            nat4_res->byte += bufS;
            acl4_ntry.srcAddr = mroute4_ntry.src = nat4_res->nSrcAddr;
            acl4_ntry.trgAddr = mroute4_ntry.grp = route4_ntry.addr = nat4_res->nTrgAddr;
            acl4_ntry.srcPortV = nat4_res->nSrcPort;
            acl4_ntry.trgPortV = nat4_res->nTrgPort;
            put32msb(bufD, bufP + 12, acl4_ntry.srcAddr);
            put32msb(bufD, bufP + 16, acl4_ntry.trgAddr);
            update_chksum(bufP + 10, nat4_res->sum3);
            update_layer4(nat4_res);
        } else {
            acls_ntry.dir = 3;
            acls_ntry.port = route4_ntry.vrf;
            index = table_find(&acls_table, &acls_ntry);
            if (index < 0) goto ipv4_natted;
            if (frag != 0) goto ipv4_natted;
            acls_res = table_get(&acls_table, index);
            if (apply_acl(&acls_res->aces, &acl4_ntry, &acl4_matcher, bufS - bufP + preBuff) == 0) goto cpu;
        }
ipv4_natted:
        acls_ntry.dir = 5;
        acls_ntry.port = route4_ntry.vrf;
        index = table_find(&acls_table, &acls_ntry);
        if (index >= 0) {
            if (frag != 0) goto punt;
            acls_res = table_get(&acls_table, index);
            if (apply_acl(&acls_res->aces, &acl4_ntry, &acl4_matcher, bufS - bufP + preBuff) != 0) goto ipv4_pbred;
            switch (acls_res->cmd) {
            case 1: // normal
                break;
            case 2: // setvrf
                route4_ntry.vrf = acls_res->vrf;
                break;
            case 3: // sethop
                route4_ntry.vrf = acls_res->vrf;
                neigh_ntry.id = acls_res->hop;
                index = table_find(&neigh_table, &neigh_ntry);
                if (index < 0) goto drop;
                neigh_res = table_get(&neigh_table, index);
                goto ipv4_tx;
            case 4: // setlab
                ethtyp = ETHERTYPE_MPLS_UCAST;
                bufP -= 4;
                label = 0x100 | ttl | (acls_res->label << 12);
                put32msb(bufD, bufP, label);
                route4_ntry.vrf = acls_res->vrf;
                neigh_ntry.id = acls_res->hop;
                goto ethtyp_tx;
            default:
                goto drop;
            }
        }
ipv4_pbred:
        mroute4_ntry.vrf = route4_ntry.vrf;
        index = table_find(&mroute4_table, &mroute4_ntry);
        if (index >= 0) {
            mroute4_res = table_get(&mroute4_table, index);
            if (mroute4_res->ingr != prt) goto drop;
            mroute4_res->pack++;
            mroute4_res->byte += bufS;
            doFlood(mroute4_res->flood, encrCtx, hashCtx, hash, bufA, bufB, bufC, bufD, bufP, bufS, bufH, ethtyp, 0x100 | ttl, port);
            if (mroute4_res->local != 0) goto cpu;
            return;
        }
        if (acl4_ntry.protV == 46) goto cpu;
        for (i = 32; i >= 0; i--) {
            route4_ntry.mask = i;
            route4_ntry.addr &= masks[i];
            index = table_find(&route4_table, &route4_ntry);
            if (index < 0) continue;
            route4_res = table_get(&route4_table, index);
            route4_res->pack++;
            route4_res->byte += bufS;
            switch (route4_res->command) {
            case 1: // route
                neigh_ntry.id = route4_res->nexthop;
                index = table_find(&neigh_table, &neigh_ntry);
                if (index < 0) goto drop;
                neigh_res = table_get(&neigh_table, index);
ipv4_tx:
                acls_ntry.dir = 2;
                acls_ntry.port = neigh_res->aclport;
                index = table_find(&acls_table, &acls_ntry);
                if (index >= 0) {
                    if (frag != 0) goto punt;
                    acls_res = table_get(&acls_table, index);
                    if (apply_acl(&acls_res->aces, &acl4_ntry, &acl4_matcher, bufS - bufP + preBuff) != 0) goto punt;
                }
                bufP -= 2;
                put16msb(bufD, bufP, ethtyp);
                acls_ntry.dir = 7;
                index = table_find(&acls_table, &acls_ntry);
                if (index < 0) goto neigh_tx;
                if (frag != 0) goto punt;
                acls_res = table_get(&acls_table, index);
                if (apply_acl(&acls_res->aces, &acl4_ntry, &acl4_matcher, bufS - bufP + preBuff) != 0) goto neigh_tx;
                policer_ntry.vrf = 0;
                policer_ntry.meter = acls_res->hop;
                policer_ntry.dir = 2;
                index = table_find(&policer_table, &policer_ntry);
                if (index < 0) goto drop;
                policer_res = table_get(&policer_table, index);
                if (policer_res->avail < 1) goto drop;
                policer_res->avail -= bufS - bufP + preBuff;
                goto neigh_tx;
            case 2: // punt
                tun4_ntry.vrf = route4_ntry.vrf;
                tun4_ntry.prot = acl4_ntry.protV;
                tun4_ntry.srcAddr = acl4_ntry.srcAddr;
                tun4_ntry.trgAddr = acl4_ntry.trgAddr;
                tun4_ntry.srcPort = acl4_ntry.srcPortV;
                tun4_ntry.trgPort = acl4_ntry.trgPortV;
                index = table_find(&tun4_table, &tun4_ntry);
                if (index >= 0) {
                    if (frag != 0) goto punt;
                    tun4_res = table_get(&tun4_table, index);
                    doTunneled(tun4_res);
                }
                acls_ntry.dir = 4;
                acls_ntry.port = 0;
                index = table_find(&acls_table, &acls_ntry);
                if (index >= 0) {
                    if (frag != 0) goto punt;
                    acls_res = table_get(&acls_table, index);
                    if (apply_acl(&acls_res->aces, &acl4_ntry, &acl4_matcher, bufS - bufP + preBuff) != 0) goto drop;
                }
                goto cpu;
            doRouted(route4_res, 4);
            }
        }
        goto punt;
    case ETHERTYPE_IPV6: // ipv6
        checkLayer2;
        if (index < 0) goto drop;
        route6_ntry.vrf = portvrf_res->vrf;
ipv6_rx:
        if ((bufD[bufP + 0] & 0xf0) != 0x60) goto drop;
        ttl = get16msb(bufD, bufP + 4) + 40 + bufP - preBuff;
        if (ttl > bufS) goto drop;
        bufS = ttl;
        bufT = bufP + 40;
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
        acl6_ntry.srcAddr1 = mroute6_ntry.src1 = get32msb(bufD, bufP + 8);
        acl6_ntry.srcAddr2 = mroute6_ntry.src2 = get32msb(bufD, bufP + 12);
        acl6_ntry.srcAddr3 = mroute6_ntry.src3 = get32msb(bufD, bufP + 16);
        acl6_ntry.srcAddr4 = mroute6_ntry.src4 = get32msb(bufD, bufP + 20);
        acl6_ntry.trgAddr1 = mroute6_ntry.grp1 = route6_ntry.addr1 = get32msb(bufD, bufP + 24);
        acl6_ntry.trgAddr2 = mroute6_ntry.grp2 = route6_ntry.addr2 = get32msb(bufD, bufP + 28);
        acl6_ntry.trgAddr3 = mroute6_ntry.grp3 = route6_ntry.addr3 = get32msb(bufD, bufP + 32);
        acl6_ntry.trgAddr4 = mroute6_ntry.grp4 = route6_ntry.addr4 = get32msb(bufD, bufP + 36);
        hash ^= acl6_ntry.srcAddr4 ^ acl6_ntry.trgAddr4;
        ttl = bufD[bufP + 7] - 1;
        if (ttl <= 1) goto punt;
        bufD[bufP + 7] = ttl;
        extract_layer4(acl6_ntry, portvrf_res->tcpmss6);
        acls_ntry.ver = 6;
        acls_ntry.dir = 1;
        acls_ntry.port = prt;
        index = table_find(&acls_table, &acls_ntry);
        if (index >= 0) {
            if (frag != 0) goto punt;
            acls_res = table_get(&acls_table, index);
            if (apply_acl(&acls_res->aces, &acl6_ntry, &acl6_matcher, bufS - bufP + preBuff) != 0) goto punt;
        }
        acls_ntry.dir = 6;
        index = table_find(&acls_table, &acls_ntry);
        if (index >= 0) {
            if (frag != 0) goto punt;
            acls_res = table_get(&acls_table, index);
            if (apply_acl(&acls_res->aces, &acl6_ntry, &acl6_matcher, bufS - bufP + preBuff) != 0) goto ipv6_qosed;
            policer_ntry.vrf = 0;
            policer_ntry.meter = acls_res->hop;
            policer_ntry.dir = 1;
            index = table_find(&policer_table, &policer_ntry);
            if (index < 0) goto drop;
            policer_res = table_get(&policer_table, index);
            if (policer_res->avail < 1) goto drop;
            policer_res->avail -= bufS - bufP + preBuff;
        }
ipv6_qosed:
        acls_ntry.dir = 8;
        acls_ntry.port = route6_ntry.vrf;
        index = table_find(&acls_table, &acls_ntry);
        if (index >= 0) {
            acls_res = table_get(&acls_table, index);
            aceh_res = search_ace(&acls_res->aces, &acl6_ntry, &acl6_matcher, bufS - bufP + preBuff);
            if (aceh_res == NULL) goto ipv6_flwed;
            if (aceh_res->act != 0) goto ipv6_flwed;
            policer_ntry.vrf = route6_ntry.vrf;
            policer_ntry.meter = aceh_res->pri;
            policer_ntry.dir = 4;
            index = table_find(&policer_table, &policer_ntry);
            if (index < 0) goto drop;
            policer_res = table_get(&policer_table, index);
            if (policer_res->avail < 1) goto drop;
            policer_res->avail -= bufS - bufP + preBuff;
        }
ipv6_flwed:
        nat6_ntry.vrf = route6_ntry.vrf;
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
        index = table_find(&nat6_table, &nat6_ntry);
        if (index >= 0) {
            if (frag != 0) goto ipv6_natted;
            nat6_res = table_get(&nat6_table, index);
            nat6_res->pack++;
            nat6_res->byte += bufS;
            acl6_ntry.srcAddr1 = mroute6_ntry.src1 = nat6_res->nSrcAddr1;
            acl6_ntry.srcAddr2 = mroute6_ntry.src2 = nat6_res->nSrcAddr2;
            acl6_ntry.srcAddr3 = mroute6_ntry.src3 = nat6_res->nSrcAddr3;
            acl6_ntry.srcAddr4 = mroute6_ntry.src4 = nat6_res->nSrcAddr4;
            acl6_ntry.trgAddr1 = mroute6_ntry.grp1 = route6_ntry.addr1 = nat6_res->nTrgAddr1;
            acl6_ntry.trgAddr2 = mroute6_ntry.grp2 = route6_ntry.addr2 = nat6_res->nTrgAddr2;
            acl6_ntry.trgAddr3 = mroute6_ntry.grp3 = route6_ntry.addr3 = nat6_res->nTrgAddr3;
            acl6_ntry.trgAddr4 = mroute6_ntry.grp4 = route6_ntry.addr4 = nat6_res->nTrgAddr4;
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
            acls_ntry.port = route6_ntry.vrf;
            index = table_find(&acls_table, &acls_ntry);
            if (index < 0) goto ipv6_natted;
            if (frag != 0) goto ipv6_natted;
            acls_res = table_get(&acls_table, index);
            if (apply_acl(&acls_res->aces, &acl6_ntry, &acl6_matcher, bufS - bufP + preBuff) == 0) goto cpu;
        }
ipv6_natted:
        acls_ntry.dir = 5;
        acls_ntry.port = route6_ntry.vrf;
        index = table_find(&acls_table, &acls_ntry);
        if (index >= 0) {
            if (frag != 0) goto punt;
            acls_res = table_get(&acls_table, index);
            if (apply_acl(&acls_res->aces, &acl6_ntry, &acl6_matcher, bufS - bufP + preBuff) != 0) goto ipv6_pbred;
            switch (acls_res->cmd) {
            case 1: // normal
                break;
            case 2: // setvrf
                route6_ntry.vrf = acls_res->vrf;
                break;
            case 3: // sethop
                route6_ntry.vrf = acls_res->vrf;
                neigh_ntry.id = acls_res->hop;
                index = table_find(&neigh_table, &neigh_ntry);
                if (index < 0) goto drop;
                neigh_res = table_get(&neigh_table, index);
                goto ipv6_tx;
            case 4: // setlab
                ethtyp = ETHERTYPE_MPLS_UCAST;
                bufP -= 4;
                label = 0x100 | ttl | (acls_res->label << 12);
                put32msb(bufD, bufP, label);
                route6_ntry.vrf = acls_res->vrf;
                neigh_ntry.id = acls_res->hop;
                goto ethtyp_tx;
            default:
                goto drop;
            }
        }
ipv6_pbred:
        mroute6_ntry.vrf = route6_ntry.vrf;
        index = table_find(&mroute6_table, &mroute6_ntry);
        if (index >= 0) {
            mroute6_res = table_get(&mroute6_table, index);
            if (mroute6_res->ingr != prt) goto drop;
            mroute6_res->pack++;
            mroute6_res->byte += bufS;
            doFlood(mroute6_res->flood, encrCtx, hashCtx, hash, bufA, bufB, bufC, bufD, bufP, bufS, bufH, ethtyp, 0x100 | ttl, port);
            if (mroute6_res->local != 0) goto cpu;
            return;
        }
        if (acl6_ntry.protV == 0) goto cpu;
        for (i = 32; i >= 0; i--) {
            route6_ntry.mask = 96 + i;
            route6_ntry.addr4 &= masks[i];
            index = table_find(&route6_table, &route6_ntry);
            if (index < 0) continue;
            goto ipv6_hit;
        }
        for (i = 32; i >= 0; i--) {
            route6_ntry.mask = 64 + i;
            route6_ntry.addr3 &= masks[i];
            index = table_find(&route6_table, &route6_ntry);
            if (index < 0) continue;
            goto ipv6_hit;
        }
        for (i = 32; i >= 0; i--) {
            route6_ntry.mask = 32 + i;
            route6_ntry.addr2 &= masks[i];
            index = table_find(&route6_table, &route6_ntry);
            if (index < 0) continue;
            goto ipv6_hit;
        }
        for (i = 32; i >= 0; i--) {
            route6_ntry.mask = i;
            route6_ntry.addr1 &= masks[i];
            index = table_find(&route6_table, &route6_ntry);
            if (index < 0) continue;
ipv6_hit:
            route6_res = table_get(&route6_table, index);
            route6_res->pack++;
            route6_res->byte += bufS;
            switch (route6_res->command) {
            case 1: // route
                neigh_ntry.id = route6_res->nexthop;
                index = table_find(&neigh_table, &neigh_ntry);
                if (index < 0) goto drop;
                neigh_res = table_get(&neigh_table, index);
ipv6_tx:
                acls_ntry.dir = 2;
                acls_ntry.port = neigh_res->aclport;
                index = table_find(&acls_table, &acls_ntry);
                if (index >= 0) {
                    if (frag != 0) goto punt;
                    acls_res = table_get(&acls_table, index);
                    if (apply_acl(&acls_res->aces, &acl6_ntry, &acl6_matcher, bufS - bufP + preBuff) != 0) goto punt;
                }
                bufP -= 2;
                put16msb(bufD, bufP, ethtyp);
                acls_ntry.dir = 7;
                index = table_find(&acls_table, &acls_ntry);
                if (index < 0) goto neigh_tx;
                if (frag != 0) goto punt;
                acls_res = table_get(&acls_table, index);
                if (apply_acl(&acls_res->aces, &acl6_ntry, &acl6_matcher, bufS - bufP + preBuff) != 0) goto neigh_tx;
                policer_ntry.vrf = 0;
                policer_ntry.meter = acls_res->hop;
                policer_ntry.dir = 2;
                index = table_find(&policer_table, &policer_ntry);
                if (index < 0) goto drop;
                policer_res = table_get(&policer_table, index);
                if (policer_res->avail < 1) goto drop;
                policer_res->avail -= bufS - bufP + preBuff;
                goto neigh_tx;
            case 2: // punt
                tun6_ntry.vrf = route6_ntry.vrf;
                tun6_ntry.prot = acl6_ntry.protV;
                tun6_ntry.srcAddr1 = acl6_ntry.srcAddr1;
                tun6_ntry.srcAddr2 = acl6_ntry.srcAddr2;
                tun6_ntry.srcAddr3 = acl6_ntry.srcAddr3;
                tun6_ntry.srcAddr4 = acl6_ntry.srcAddr4;
                tun6_ntry.trgAddr1 = acl6_ntry.trgAddr1;
                tun6_ntry.trgAddr2 = acl6_ntry.trgAddr2;
                tun6_ntry.trgAddr3 = acl6_ntry.trgAddr3;
                tun6_ntry.trgAddr4 = acl6_ntry.trgAddr4;
                tun6_ntry.srcPort = acl6_ntry.srcPortV;
                tun6_ntry.trgPort = acl6_ntry.trgPortV;
                index = table_find(&tun6_table, &tun6_ntry);
                if (index >= 0) {
                    if (frag != 0) goto punt;
                    tun6_res = table_get(&tun6_table, index);
                    doTunneled(tun6_res);
                }
                acls_ntry.dir = 4;
                acls_ntry.port = 0;
                index = table_find(&acls_table, &acls_ntry);
                if (index >= 0) {
                    if (frag != 0) goto punt;
                    acls_res = table_get(&acls_table, index);
                    if (apply_acl(&acls_res->aces, &acl6_ntry, &acl6_matcher, bufS - bufP + preBuff) != 0) goto drop;
                }
                goto cpu;
            doRouted(route6_res, 41);
            }
        }
        goto punt;
    case ETHERTYPE_PPPOE_DATA: // pppoe
        checkLayer2;
        pppoe_ntry.port = prt;
        pppoe_ntry.session = get16msb(bufD, bufP + 2);
        index = table_find(&pppoe_table, &pppoe_ntry);
        if (index < 0) goto drop;
        pppoe_res = table_get(&pppoe_table, index);
        pppoe_res->pack++;
        pppoe_res->byte += bufS;
        prt = pppoe_res->aclport;
        bufP += 6;
        ethtyp = get16msb(bufD, bufP);
        if ((ethtyp & 0x8000) != 0) goto cpu;
        ppptyp2ethtyp;
        put16msb(bufD, bufP, ethtyp);
        bufP -= 12;
        memset(&bufD[bufP], 0, 12);
        bufS = bufS - bufP + preBuff;
        memmove(&bufD[preBuff], &bufD[bufP], bufS);
        prt2 = prt;
        goto ether_rx;
    case ETHERTYPE_ROUTEDMAC: // routed bridge
        portvrf_ntry.port = prt;
        index = table_find(&portvrf_table, &portvrf_ntry);
        if (index < 0) goto drop;
        portvrf_res = table_get(&portvrf_table, index);
        if (portvrf_res->command != 2) goto drop;
        bridge_ntry.id = portvrf_res->bridge;
        memmove(&bufH[0], &bufD[bufP], 12);
        bufP += 12;
        bufP += 2;
        goto bridgevpls_rx;
    case ETHERTYPE_POLKA: // polka
        checkLayer2;
        ttl = get16msb(bufD, bufP + 0);
        if ((ttl & 0xff00) != 0) goto drop;
        if ((ttl & 0xff) <= 1) goto punt;
        ttl--;
        bufD[bufP + 1] = ttl;
        polkaPoly_ntry.port = prt;
        index = table_find(&polkaPoly_table, &polkaPoly_ntry);
        if (index < 0) goto drop;
        polkaPoly_res = table_get(&polkaPoly_table, index);
        polkaPoly_res->pack++;
        polkaPoly_res->byte += bufS;
        tmp = 0;
        for (i = 0; i < 14; i++) {
            tmp = ((tmp << 8) & 0xffff) ^ polkaPoly_res->tab[(tmp >> 8) ^ (bufD[bufP + 4 + i] & 0xff)];
        }
        tmp ^= get16msb(bufD, bufP + 18);
        if (tmp == 0) {
            ethtyp = get16msb(bufD, bufP + 2);
            bufP += 20;
            goto etyped_rx;
        }
        polkaIdx_ntry.vrf = portvrf_res->vrf;
        polkaIdx_ntry.index = tmp;
        index = table_find(&polkaIdx_table, &polkaIdx_ntry);
        if (index < 0) goto drop;
        polkaIdx_res = table_get(&polkaIdx_table, index);
        polkaIdx_res->pack++;
        polkaIdx_res->byte += bufS;
        neigh_ntry.id = polkaIdx_res->nexthop;
        goto ethtyp_tx;
    case ETHERTYPE_NSH: // nsh
        checkLayer2;
        ttl = get16msb(bufD, bufP + 0);
        if ((ttl & 0xe000) != 0) goto drop;
        if (((ttl >> 6) & 0x3f) <= 1) goto punt;
        tmp = get32msb(bufD, bufP + 4);
        ethtyp = bufD[bufP + 3];
        nsh_ntry.sp = tmp >> 8;
        nsh_ntry.si = tmp & 0xff;
        index = table_find(&nsh_table, &nsh_ntry);
        if (index < 0) goto drop;
        nsh_res = table_get(&nsh_table, index);
        nsh_res->pack++;
        nsh_res->byte += bufS;
        switch (nsh_res->command) {
        case 1: // fwd
            put16msb(bufD, bufP + 0, (ttl - 0x40));
            put32msb(bufD, bufP + 4, nsh_res->trg);
            ethtyp = ETHERTYPE_NSH;
            bufP -= 2;
            put16msb(bufD, bufP, ethtyp);
            memmove(&bufH[0], &nsh_res->dmac, 6);
            memmove(&bufH[6], &nsh_res->smac, 6);
            prt2 = prt = send2subif(nsh_res->port, encrCtx, hashCtx, hash, bufD, &bufP, &bufS, bufH, &ethtyp);
            if (prt2 >= 0) goto ether_rx;
            return;
        case 2: // vrf
            bufP += ((ttl & 0x3f) * 4);
            route4_ntry.vrf = nsh_res->vrf;
            route6_ntry.vrf = nsh_res->vrf;
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
        label = 0x1ff | (portvrf_res->label2 << 12);
        put32msb(bufD, bufP, label);
        bufP -= 4;
        label = 0xff | (portvrf_res->label1 << 12);
        put32msb(bufD, bufP, label);
        neigh_ntry.id = portvrf_res->nexthop;
        goto ethtyp_tx;
bridge_rx:
        bridge_ntry.id = portvrf_res->bridge;
        memmove(&bufH[0], &bufD[preBuff], 12);
        goto bridgevpls_rx;
bridgevpls_rx:
        bridge_ntry.mac1 = get16msb(bufH, 6);
        bridge_ntry.mac2 = get32msb(bufH, 8);
        hash ^= bridge_ntry.mac2;
        index = table_find(&bridge_table, &bridge_ntry);
        if (index < 0) goto cpu;
        bridge_res = table_get(&bridge_table, index);
        bridge_res->packRx++;
        bridge_res->byteRx += bufS;
        bridge_ntry.mac1 = get16msb(bufH, 0);
        bridge_ntry.mac2 = get32msb(bufH, 2);
        hash ^= bridge_ntry.mac2;
        index = table_find(&bridge_table, &bridge_ntry);
        if (index < 0) goto cpu;
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
        prt2 = prt = send2subif(prt, encrCtx, hashCtx, hash, bufD, &bufP, &bufS, bufH, &ethtyp);
        if (prt2 >= 0) goto ether_rx;
        return;
    case ETHERTYPE_ARP: // arp
        checkLayer2;
        goto cpu;
    case ETHERTYPE_PPPOE_CTRL: // pppoe ctrl
        checkLayer2;
        goto cpu;
    case ETHERTYPE_MACSEC: // macsec
        checkLayer2;
        goto cpu;
    case ETHERTYPE_LACP: // lacp
        checkLayer2;
        goto cpu;
    case ETHERTYPE_LLDP: // lldp
        checkLayer2;
        goto cpu;
    default:
        checkLayer2;
punt:
        if (punts < 0) {
drop:
            packDr[port]++;
            byteDr[port] += bufS - bufP + preBuff;
            return;
        }
        punts--;
cpu:
        send2cpu(bufD, bufS, prt2);
        return;
    }
}


#endif

