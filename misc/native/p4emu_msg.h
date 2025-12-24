unsigned char portStatsBuf[16384];
int portStatsLen = 0;
int printCmds = 0;
int commandSock;
FILE *commandRx;
FILE *commandTx;



void str2mac(unsigned char *dst, char *src) {
    sscanf(src, "%hhx:%hhx:%hhx:%hhx:%hhx:%hhx", &dst[0], &dst[1], &dst[2], &dst[3], &dst[4], &dst[5]);
}

void mac2str(unsigned char *src, unsigned char *dst) {
    snprintf((char*)dst, 128, "%02x:%02x:%02x:%02x:%02x:%02x", src[0], src[1], src[2], src[3], src[4], src[5]);
}



int str2key(char *str, unsigned char *key) {
    unsigned char buf[4];
    int s = 0;
    for (int i=0;;) {
        memcpy(&buf, &str[i], 2);
        buf[2] = 0;
        if (str[i] == 0) break;
        sscanf((char*)buf, "%hhx", &key[s]);
        s++;
        i += 2;
    }
    return s;
}


#ifndef HAVE_NOCRYPTO
const EVP_CIPHER* getEncrAlg(char *buf) {
    if (strcmp(buf, "none") == 0) return EVP_enc_null();
    if (strcmp(buf, "des") == 0) return EVP_des_cbc();
    if (strcmp(buf, "rc2") == 0) return EVP_rc2_cbc();
    if (strcmp(buf, "3des") == 0) return EVP_des_ede3_cbc();
    if (strcmp(buf, "blowfish") == 0) return EVP_bf_cbc();
    if (strcmp(buf, "aes128cbc") == 0) return EVP_aes_128_cbc();
    if (strcmp(buf, "aes192cbc") == 0) return EVP_aes_192_cbc();
    if (strcmp(buf, "aes256cbc") == 0) return EVP_aes_256_cbc();
    if (strcmp(buf, "aes128cfb") == 0) return EVP_aes_128_cfb();
    if (strcmp(buf, "aes192cfb") == 0) return EVP_aes_192_cfb();
    if (strcmp(buf, "aes256cfb") == 0) return EVP_aes_256_cfb();
    if (strcmp(buf, "aes128ecb") == 0) return EVP_aes_128_ecb();
    if (strcmp(buf, "aes192ecb") == 0) return EVP_aes_192_ecb();
    if (strcmp(buf, "aes256ecb") == 0) return EVP_aes_256_ecb();
    if (strcmp(buf, "aes128gcm") == 0) return EVP_aes_128_gcm();
    if (strcmp(buf, "aes192gcm") == 0) return EVP_aes_192_gcm();
    if (strcmp(buf, "aes256gcm") == 0) return EVP_aes_256_gcm();
    if (strcmp(buf, "aes128ctr") == 0) return EVP_aes_128_ctr();
    if (strcmp(buf, "aes192ctr") == 0) return EVP_aes_192_ctr();
    if (strcmp(buf, "aes256ctr") == 0) return EVP_aes_256_ctr();
    if (strcmp(buf, "aes128ofb") == 0) return EVP_aes_128_ofb();
    if (strcmp(buf, "aes192ofb") == 0) return EVP_aes_192_ofb();
    if (strcmp(buf, "aes256ofb") == 0) return EVP_aes_256_ofb();
    return NULL;
}


const EVP_MD* getHashAlg(char *buf) {
    if (strcmp(buf, "none") == 0) return EVP_md_null();
    if (strcmp(buf, "md5") == 0) return EVP_md5();
    if (strcmp(buf, "sha1") == 0) return EVP_sha1();
    if (strcmp(buf, "sha224") == 0) return EVP_sha224();
    if (strcmp(buf, "sha256") == 0) return EVP_sha256();
    if (strcmp(buf, "sha384") == 0) return EVP_sha384();
    if (strcmp(buf, "sha512") == 0) return EVP_sha512();
    if (strcmp(buf, "sha3224") == 0) return EVP_sha3_224();
    if (strcmp(buf, "sha3256") == 0) return EVP_sha3_256();
    if (strcmp(buf, "sha3384") == 0) return EVP_sha3_384();
    if (strcmp(buf, "sha3512") == 0) return EVP_sha3_512();
    return NULL;
}
#endif


long readRate(char**arg) {
    float res = atof(arg[3]) * 100.0 / atof(arg[4]);
    return res;
}


int readAclMod(char*arg) {
    if (strcmp(arg, "permit") == 0) return 0;
    if (strcmp(arg, "punt") == 0) return 2;
    return 1;
}

void readAcl4(struct acl4_entry *acl4_ntry, char**arg) {
    unsigned char buf2[1024];
    acl4_ntry->pri = atoi(arg[2]);
    acl4_ntry->act = readAclMod(arg[3]);
    acl4_ntry->protV = atoi(arg[4]);
    acl4_ntry->protM = atoi(arg[5]);
    inet_pton(AF_INET, arg[6], buf2);
    acl4_ntry->srcAddr = get32msb(buf2, 0);
    inet_pton(AF_INET, arg[7], buf2);
    acl4_ntry->srcMask = get32msb(buf2, 0);
    inet_pton(AF_INET, arg[8], buf2);
    acl4_ntry->trgAddr = get32msb(buf2, 0);
    inet_pton(AF_INET, arg[9], buf2);
    acl4_ntry->trgMask = get32msb(buf2, 0);
    acl4_ntry->srcPortV = atoi(arg[10]);
    acl4_ntry->srcPortM = atoi(arg[11]);
    acl4_ntry->trgPortV = atoi(arg[12]);
    acl4_ntry->trgPortM = atoi(arg[13]);
    acl4_ntry->tosV = atoi(arg[14]);
    acl4_ntry->tosM = atoi(arg[15]);
    acl4_ntry->flowV = atoi(arg[16]);
    acl4_ntry->flowM = atoi(arg[17]);
    acl4_ntry->sgtV = atoi(arg[18]);
    acl4_ntry->sgtM = atoi(arg[19]);
}



void readAcl6(struct acl6_entry *acl6_ntry, char**arg) {
    unsigned char buf2[1024];
    acl6_ntry->pri = atoi(arg[2]);
    acl6_ntry->act = readAclMod(arg[3]);
    acl6_ntry->protV = atoi(arg[4]);
    acl6_ntry->protM = atoi(arg[5]);
    inet_pton(AF_INET6, arg[6], buf2);
    acl6_ntry->srcAddr1 = get32msb(buf2, 0);
    acl6_ntry->srcAddr2 = get32msb(buf2, 4);
    acl6_ntry->srcAddr3 = get32msb(buf2, 8);
    acl6_ntry->srcAddr4 = get32msb(buf2, 12);
    inet_pton(AF_INET6, arg[7], buf2);
    acl6_ntry->srcMask1 = get32msb(buf2, 0);
    acl6_ntry->srcMask2 = get32msb(buf2, 4);
    acl6_ntry->srcMask3 = get32msb(buf2, 8);
    acl6_ntry->srcMask4 = get32msb(buf2, 12);
    inet_pton(AF_INET6, arg[8], buf2);
    acl6_ntry->trgAddr1 = get32msb(buf2, 0);
    acl6_ntry->trgAddr2 = get32msb(buf2, 4);
    acl6_ntry->trgAddr3 = get32msb(buf2, 8);
    acl6_ntry->trgAddr4 = get32msb(buf2, 12);
    inet_pton(AF_INET6, arg[9], buf2);
    acl6_ntry->trgMask1 = get32msb(buf2, 0);
    acl6_ntry->trgMask2 = get32msb(buf2, 4);
    acl6_ntry->trgMask3 = get32msb(buf2, 8);
    acl6_ntry->trgMask4 = get32msb(buf2, 12);
    acl6_ntry->srcPortV = atoi(arg[10]);
    acl6_ntry->srcPortM = atoi(arg[11]);
    acl6_ntry->trgPortV = atoi(arg[12]);
    acl6_ntry->trgPortM = atoi(arg[13]);
    acl6_ntry->tosV = atoi(arg[14]);
    acl6_ntry->tosM = atoi(arg[15]);
    acl6_ntry->flowV = atoi(arg[16]);
    acl6_ntry->flowM = atoi(arg[17]);
    acl6_ntry->sgtV = atoi(arg[18]);
    acl6_ntry->sgtM = atoi(arg[19]);
}



#define accumulate_sum(sum, val, mul)                           \
    put32msb(buf2, 0, val);                                     \
    sum += mul*get16lsb(buf2, 0);                               \
    sum += mul*get16lsb(buf2, 2);









int doOneCommand(struct packetContext *ctx, unsigned char* buf) {
    unsigned char buf2[1024];
    char* arg[128];
    int cnt;
    cnt = 0;
    arg[0] = (char*)&buf[0];
    int i = 0;
    int o = 0;
    for (;;) {
        if (cnt >= 128) break;
        switch (buf[i]) {
        case 0:
        case 10:
        case 13:
            o = 1;
            break;
        case ' ':
        case '/':
        case '_':
            buf[i] = 0;
            cnt++;
            arg[cnt] = (char*)&buf[i + 1];
            break;
        }
        if (o > 0) break;
        i++;
    }
    o = i;
    cnt++;
    buf[o] = 0;
    for (int i=cnt; i < 128; i++) arg[i]=(char*)&buf[o];
    if (printCmds != 0) {
        printf("rx: ");
        for (int i=0; i < cnt; i++) printf("'%s' ",arg[i]);
        printf("\n");
    }
    int del = strcmp(arg[1], "del");
    if (del != 0) del = 1;
    struct polkaIdx_entry polkaIdx_ntry;
    memset(&polkaIdx_ntry, 0, sizeof(polkaIdx_ntry));
    struct polkaPoly_entry polkaPoly_ntry;
    memset(&polkaPoly_ntry, 0, sizeof(polkaPoly_ntry));
    struct nsh_entry nsh_ntry;
    memset(&nsh_ntry, 0, sizeof(nsh_ntry));
    struct mpls_entry *mpls_res;
    struct mpls_entry mpls_ntry;
    memset(&mpls_ntry, 0, sizeof(mpls_ntry));
    struct port2vrf_entry *port2vrf_res;
    struct port2vrf_entry port2vrf_ntry;
    port2vrf_ntry.sgtSet = -1;
    port2vrf_ntry.monTarget = -1;
    memset(&port2vrf_ntry, 0, sizeof(port2vrf_ntry));
    struct vrf2rib_entry *vrf2rib_res;
    struct vrf2rib_entry vrf2rib_ntry;
    memset(&vrf2rib_ntry, 0, sizeof(vrf2rib_ntry));
    struct route4_entry route4_ntry;
    memset(&route4_ntry, 0, sizeof(route4_ntry));
    struct route6_entry route6_ntry;
    memset(&route6_ntry, 0, sizeof(route6_ntry));
    struct neigh_entry neigh_ntry;
    memset(&neigh_ntry, 0, sizeof(neigh_ntry));
    struct neigh_entry *neigh_res;
    struct vlanin_entry vlanin_ntry;
    memset(&vlanin_ntry, 0, sizeof(vlanin_ntry));
    struct vlanout_entry vlanout_ntry;
    memset(&vlanout_ntry, 0, sizeof(vlanout_ntry));
    struct bridge_entry bridge_ntry;
    memset(&bridge_ntry, 0, sizeof(bridge_ntry));
    struct acl4_entry acl4_ntry;
    memset(&acl4_ntry, 0, sizeof(acl4_ntry));
    struct acl6_entry acl6_ntry;
    memset(&acl6_ntry, 0, sizeof(acl6_ntry));
    struct nat4_entry nat4_ntry;
    memset(&nat4_ntry, 0, sizeof(nat4_ntry));
    struct nat6_entry nat6_ntry;
    memset(&nat6_ntry, 0, sizeof(nat6_ntry));
    struct insp4_entry insp4_ntry;
    memset(&insp4_ntry, 0, sizeof(insp4_ntry));
    struct insp6_entry insp6_ntry;
    memset(&insp6_ntry, 0, sizeof(insp6_ntry));
    struct bundle_entry bundle_ntry;
    memset(&bundle_ntry, 0, sizeof(bundle_ntry));
    struct bundle_entry *bundle_res;
    struct pppoe_entry pppoe_ntry;
    memset(&pppoe_ntry, 0, sizeof(pppoe_ntry));
    struct tun4_entry tun4_ntry;
    memset(&tun4_ntry, 0, sizeof(tun4_ntry));
    struct tun6_entry tun6_ntry;
    memset(&tun6_ntry, 0, sizeof(tun6_ntry));
    struct policer_entry policer_ntry;
    memset(&policer_ntry, 0, sizeof(policer_ntry));
    struct flood_entry flood_ntry;
    memset(&flood_ntry, 0, sizeof(flood_ntry));
    struct mroute4_entry *mroute4_res;
    struct mroute4_entry mroute4_ntry;
    memset(&mroute4_ntry, 0, sizeof(mroute4_ntry));
    struct mroute6_entry *mroute6_res;
    struct mroute6_entry mroute6_ntry;
    memset(&mroute6_ntry, 0, sizeof(mroute6_ntry));
    if (strcmp(arg[0], "quit") == 0) {
        return 1;
    }
    if (strcmp(arg[0], "stats") == 0) {
        i = atoi(arg[1]);
        if (i < 0) return 0;
        if (i >= dataPorts) return 0;
        portStatsLen = snprintf((char*)&portStatsBuf, 128, "stats_beg %i\r\n", i);
        snprintf((char*)&buf2, 128, "stats_txt %i ", i);
        getStats(i, &portStatsBuf[0], &buf2[0], &portStatsLen);
        portStatsLen += snprintf((char*)&portStatsBuf[portStatsLen], 128, "stats_end %i\r\n", i);
        return 0;
    }
    if (strcmp(arg[0], "state") == 0) {
        i = atoi(arg[1]);
        o = atoi(arg[2]);
        if (i < 0) return 0;
        if (i >= dataPorts) return 0;
        setState(i, o);
        return 0;
    }
    if (strcmp(arg[0], "mtu") == 0) {
        i = atoi(arg[1]);
        o = atoi(arg[2]);
        if (i < 0) return 0;
        if (i >= dataPorts) return 0;
        setMtu(i, o);
        return 0;
    }
    if (strcmp(arg[0], "mylabel4") == 0) {
        mpls_ntry.label = atoi(arg[2]);
        mpls_ntry.vrf = atoi(arg[3]);
        mpls_ntry.ver = 4;
        mpls_ntry.command = 1;
        if (del == 0) hasht_del(&mpls_table, &mpls_ntry);
        else hasht_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "mylabel6") == 0) {
        mpls_ntry.label = atoi(arg[2]);
        mpls_ntry.vrf = atoi(arg[3]);
        mpls_ntry.ver = 6;
        mpls_ntry.command = 1;
        if (del == 0) hasht_del(&mpls_table, &mpls_ntry);
        else hasht_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "unlabel4") == 0) {
        mpls_ntry.label = atoi(arg[2]);
        mpls_ntry.nexthop = atoi(arg[3]);
        mpls_ntry.ver = 4;
        mpls_ntry.command = 2;
        if (del == 0) hasht_del(&mpls_table, &mpls_ntry);
        else hasht_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "unlabel6") == 0) {
        mpls_ntry.label = atoi(arg[2]);
        mpls_ntry.nexthop = atoi(arg[3]);
        mpls_ntry.ver = 6;
        mpls_ntry.command = 2;
        if (del == 0) hasht_del(&mpls_table, &mpls_ntry);
        else hasht_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "label4") == 0) {
        mpls_ntry.label = atoi(arg[2]);
        mpls_ntry.nexthop = atoi(arg[3]);
        mpls_ntry.swap = atoi(arg[5]);
        mpls_ntry.ver = 4;
        mpls_ntry.command = 3;
        if (del == 0) hasht_del(&mpls_table, &mpls_ntry);
        else hasht_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "label6") == 0) {
        mpls_ntry.label = atoi(arg[2]);
        mpls_ntry.nexthop = atoi(arg[3]);
        mpls_ntry.swap = atoi(arg[5]);
        mpls_ntry.ver = 6;
        mpls_ntry.command = 3;
        if (del == 0) hasht_del(&mpls_table, &mpls_ntry);
        else hasht_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "vpnlabel4") == 0) {
        mpls_ntry.label = atoi(arg[2]);
        mpls_ntry.nexthop = atoi(arg[3]);
        mpls_ntry.swap = atoi(arg[5]);
        mpls_ntry.push = atoi(arg[6]);
        mpls_ntry.ver = 4;
        mpls_ntry.command = 9;
        if (del == 0) hasht_del(&mpls_table, &mpls_ntry);
        else hasht_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "vpnlabel6") == 0) {
        mpls_ntry.label = atoi(arg[2]);
        mpls_ntry.nexthop = atoi(arg[3]);
        mpls_ntry.swap = atoi(arg[5]);
        mpls_ntry.push = atoi(arg[6]);
        mpls_ntry.ver = 6;
        mpls_ntry.command = 9;
        if (del == 0) hasht_del(&mpls_table, &mpls_ntry);
        else hasht_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "pwhelab") == 0) {
        mpls_ntry.label = atoi(arg[2]);
        mpls_ntry.port = atoi(arg[3]);
        mpls_ntry.command = 10;
        if (del == 0) hasht_del(&mpls_table, &mpls_ntry);
        else hasht_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "cpulabel") == 0) {
        mpls_ntry.label = atoi(arg[2]);
        mpls_ntry.command = 6;
        if (del == 0) hasht_del(&mpls_table, &mpls_ntry);
        else hasht_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "sampler4") == 0) {
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init4;
        vrf2rib_res->samp = atoi(arg[3]);
        if (del == 0) vrf2rib_res->samp = 0;
        return 0;
    }
    if (strcmp(arg[0], "sampler6") == 0) {
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init6;
        vrf2rib_res->samp = atoi(arg[3]);
        if (del == 0) vrf2rib_res->samp = 0;
        return 0;
    }
    if (strcmp(arg[0], "polkaidx") == 0) {
        vrf2rib_ntry.vrf = atoi(arg[3]);
        vrf2rib_res = vrf2rib_init4;
        polkaIdx_ntry.index = atoi(arg[2]);
        polkaIdx_ntry.nexthop = atoi(arg[4]);
        if (del == 0) table_del(&vrf2rib_res->plk, &polkaIdx_ntry);
        else table_add(&vrf2rib_res->plk, &polkaIdx_ntry);
        return 0;
    }
    if (strcmp(arg[0], "polkapoly") == 0) {
        polkaPoly_ntry.port = atoi(arg[2]);
        int p = atoi(arg[3]);
        crc16mktab(polkaPoly_ntry.tab, p);
        if (del == 0) hasht_del(&polkaPoly_table, &polkaPoly_ntry);
        else hasht_add(&polkaPoly_table, &polkaPoly_ntry);
        return 0;
    }
    if (strcmp(arg[0], "mpolkaidx") == 0) {
        vrf2rib_ntry.vrf = atoi(arg[3]);
        vrf2rib_res = vrf2rib_init6;
        polkaIdx_ntry.index = atoi(arg[2]);
        polkaIdx_ntry.nexthop = atoi(arg[4]);
        if (del == 0) table_del(&vrf2rib_res->plk, &polkaIdx_ntry);
        else table_add(&vrf2rib_res->plk, &polkaIdx_ntry);
        return 0;
    }
    if (strcmp(arg[0], "mpolkapoly") == 0) {
        polkaPoly_ntry.port = atoi(arg[2]);
        int p = atoi(arg[3]);
        crc16mktab(polkaPoly_ntry.tab, p);
        if (del == 0) hasht_del(&mpolkaPoly_table, &polkaPoly_ntry);
        else hasht_add(&mpolkaPoly_table, &polkaPoly_ntry);
        return 0;
    }
    if (strcmp(arg[0], "nshifc") == 0) {
        nsh_ntry.sp = atoi(arg[2]);
        nsh_ntry.si = atoi(arg[3]);
        nsh_ntry.command = 1;
        nsh_ntry.port = atoi(arg[4]);
        str2mac(&nsh_ntry.macs[6], arg[5]);
        str2mac(&nsh_ntry.macs[0], arg[6]);
        nsh_ntry.trg = (atoi(arg[7]) << 8) | atoi(arg[8]);
        if (del == 0) hasht_del(&nsh_table, &nsh_ntry);
        else hasht_add(&nsh_table, &nsh_ntry);
        return 0;
    }
    if (strcmp(arg[0], "nshnei") == 0) {
        nsh_ntry.sp = atoi(arg[2]);
        nsh_ntry.si = atoi(arg[3]);
        nsh_ntry.command = 3;
        nsh_ntry.port = atoi(arg[4]);
        nsh_ntry.trg = (atoi(arg[5]) << 8) | atoi(arg[6]);
        if (del == 0) hasht_del(&nsh_table, &nsh_ntry);
        else hasht_add(&nsh_table, &nsh_ntry);
        return 0;
    }
    if (strcmp(arg[0], "nshloc") == 0) {
        nsh_ntry.sp = atoi(arg[2]);
        nsh_ntry.si = atoi(arg[3]);
        nsh_ntry.command = 2;
        nsh_ntry.vrf = atoi(arg[4]);
        if (del == 0) hasht_del(&nsh_table, &nsh_ntry);
        else hasht_add(&nsh_table, &nsh_ntry);
        return 0;
    }
    if (strcmp(arg[0], "portvrf") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        if (del == 0) {
            port2vrf_deinit(&port2vrf_ntry);
            return 0;
        }
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        port2vrf_res->bridge = 0;
        port2vrf_res->vrf = atoi(arg[3]);
        port2vrf_res->command = 1;
        return 0;
    }
    if (strcmp(arg[0], "tcpmss4in") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        port2vrf_res->tcpmss4 = atoi(arg[3]);
        return 0;
    }
    if (strcmp(arg[0], "tcpmss6in") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        port2vrf_res->tcpmss6 = atoi(arg[3]);
        return 0;
    }
    if (strcmp(arg[0], "verify4") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        port2vrf_res->verify4 = atoi(arg[3]);
        return 0;
    }
    if (strcmp(arg[0], "verify6") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        port2vrf_res->verify6 = atoi(arg[3]);
        return 0;
    }
    if (strcmp(arg[0], "pmtud4in") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        port2vrf_res->pmtud4 = atoi(arg[3]);
        return 0;
    }
    if (strcmp(arg[0], "pmtud6in") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        port2vrf_res->pmtud6 = atoi(arg[3]);
        return 0;
    }
    if (strcmp(arg[0], "mplsttl4") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        port2vrf_res->pttl4 = atoi(arg[3]);
        if (port2vrf_res->pttl4 == 0) port2vrf_res->pttl4 = 0xff;
        else port2vrf_res->pttl4 = 0;
        return 0;
    }
    if (strcmp(arg[0], "mplsttl6") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        port2vrf_res->pttl6 = atoi(arg[3]);
        if (port2vrf_res->pttl6 == 0) port2vrf_res->pttl6 = 0xff;
        else port2vrf_res->pttl6 = 0;
        return 0;
    }
    if (strcmp(arg[0], "flowdis4") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        port2vrf_res->nflw4 = atoi(arg[3]);
        return 0;
    }
    if (strcmp(arg[0], "flowdis6") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        port2vrf_res->nflw6 = atoi(arg[3]);
        return 0;
    }
    if (strcmp(arg[0], "mplspack") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        port2vrf_res->mpls = atoi(arg[3]);
        return 0;
    }
    if (strcmp(arg[0], "nshpack") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        port2vrf_res->nsh = atoi(arg[3]);
        return 0;
    }
    if (strcmp(arg[0], "loconnifc") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        if (del == 0) {
            port2vrf_deinit(&port2vrf_ntry);
            return 0;
        }
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        port2vrf_res->bridge = 0;
        port2vrf_res->vrf = 0;
        port2vrf_res->label1 = atoi(arg[3]);
        port2vrf_res->command = 4;
        return 0;
    }
    if (strcmp(arg[0], "loconnnei") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        if (del == 0) {
            port2vrf_deinit(&port2vrf_ntry);
            return 0;
        }
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        port2vrf_res->bridge = 0;
        port2vrf_res->vrf = 0;
        port2vrf_res->label1 = atoi(arg[3]);
        port2vrf_res->command = 5;
        return 0;
    }
    if (strcmp(arg[0], "nshconn") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        if (del == 0) {
            port2vrf_deinit(&port2vrf_ntry);
            return 0;
        }
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        port2vrf_res->bridge = 0;
        port2vrf_res->vrf = 0;
        port2vrf_res->label1 = atoi(arg[3]);
        port2vrf_res->label2 = atoi(arg[4]);
        port2vrf_res->command = 6;
        return 0;
    }
    if (strcmp(arg[0], "xconnect") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        port2vrf_res->bridge = 0;
        port2vrf_res->vrf = 0;
        port2vrf_res->nexthop = atoi(arg[4]);
        port2vrf_res->label1 = atoi(arg[5]);
        port2vrf_res->label2 = atoi(arg[7]);
        port2vrf_res->command = 3;
        mpls_ntry.label = atoi(arg[6]);
        mpls_ntry.port = port2vrf_res->port;
        mpls_ntry.command = 4;
        if (del == 0) port2vrf_deinit(&port2vrf_ntry);
        if (del == 0) hasht_del(&mpls_table, &mpls_ntry);
        else hasht_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "bridgevpls") == 0) {
        bridge_ntry.id = atoi(arg[2]);
        str2mac(buf2, arg[3]);
        bridge_ntry.mac1 = get16msb(buf2, 0);
        bridge_ntry.mac2 = get32msb(buf2, 2);
        bridge_ntry.nexthop = atoi(arg[5]);
        bridge_ntry.label1 = atoi(arg[6]);
        bridge_ntry.label2 = atoi(arg[7]);
        bridge_ntry.command = 2;
        if (del == 0) hasht_del(&bridge_table, &bridge_ntry);
        else hasht_add(&bridge_table, &bridge_ntry);
        return 0;
    }
    if (strcmp(arg[0], "bridgelabel") == 0) {
        mpls_ntry.label = atoi(arg[3]);
        mpls_ntry.port = atoi(arg[2]);
        mpls_ntry.command = 5;
        if (del == 0) hasht_del(&mpls_table, &mpls_ntry);
        else hasht_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "portbridge") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        if (del == 0) {
            port2vrf_deinit(&port2vrf_ntry);
            return 0;
        }
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        port2vrf_res->vrf = 0;
        port2vrf_res->bridge = atoi(arg[3]);
        port2vrf_res->command = 2;
        return 0;
    }
    if (strcmp(arg[0], "bridgemac") == 0) {
        bridge_ntry.id = atoi(arg[2]);
        str2mac(buf2, arg[3]);
        bridge_ntry.mac1 = get16msb(buf2, 0);
        bridge_ntry.mac2 = get32msb(buf2, 2);
        bridge_ntry.port = atoi(arg[4]);
        bridge_ntry.command = 1;
        if (del == 0) hasht_del(&bridge_table, &bridge_ntry);
        else hasht_add(&bridge_table, &bridge_ntry);
        return 0;
    }
    if (strcmp(arg[0], "routedmac") == 0) {
        bridge_ntry.id = atoi(arg[2]);
        str2mac(buf2, arg[3]);
        bridge_ntry.mac1 = get16msb(buf2, 0);
        bridge_ntry.mac2 = get32msb(buf2, 2);
        bridge_ntry.nexthop = atoi(arg[4]);
        bridge_ntry.command = 3;
        if (del == 0) hasht_del(&bridge_table, &bridge_ntry);
        else hasht_add(&bridge_table, &bridge_ntry);
        return 0;
    }
    if (strcmp(arg[0], "bridgevxlan4") == 0) {
        bridge_ntry.id = atoi(arg[2]);
        str2mac(buf2, arg[3]);
        bridge_ntry.mac1 = get16msb(buf2, 0);
        bridge_ntry.mac2 = get32msb(buf2, 2);
        inet_pton(AF_INET, arg[4], buf2);
        tun4_ntry.trgAddr = bridge_ntry.srcAddr1 = get32msb(buf2, 0);
        inet_pton(AF_INET, arg[5], buf2);
        tun4_ntry.srcAddr = bridge_ntry.trgAddr1 = get32msb(buf2, 0);
        bridge_ntry.nexthop = atoi(arg[6]);
        bridge_ntry.instance = atoi(arg[7]);
        bridge_ntry.command = 4;
        vrf2rib_ntry.vrf = atoi(arg[8]);
        vrf2rib_res = vrf2rib_init4;
        tun4_ntry.aclport = atoi(arg[9]);
        tun4_ntry.trgPort = bridge_ntry.srcPort = atoi(arg[10]);
        tun4_ntry.srcPort = bridge_ntry.trgPort = atoi(arg[11]);
        tun4_ntry.prot = IP_PROTOCOL_UDP;
        tun4_ntry.command = 3;
        if (del == 0) hasht_del(&bridge_table, &bridge_ntry);
        else hasht_add(&bridge_table, &bridge_ntry);
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun4_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "bridgevxlan6") == 0) {
        bridge_ntry.id = atoi(arg[2]);
        str2mac(buf2, arg[3]);
        bridge_ntry.mac1 = get16msb(buf2, 0);
        bridge_ntry.mac2 = get32msb(buf2, 2);
        inet_pton(AF_INET6, arg[4], buf2);
        tun6_ntry.trgAddr1 = bridge_ntry.srcAddr1 = get32msb(buf2, 0);
        tun6_ntry.trgAddr2 = bridge_ntry.srcAddr2 = get32msb(buf2, 4);
        tun6_ntry.trgAddr3 = bridge_ntry.srcAddr3 = get32msb(buf2, 8);
        tun6_ntry.trgAddr4 = bridge_ntry.srcAddr4 = get32msb(buf2, 12);
        inet_pton(AF_INET6, arg[5], buf2);
        tun6_ntry.srcAddr1 = bridge_ntry.trgAddr1 = get32msb(buf2, 0);
        tun6_ntry.srcAddr2 = bridge_ntry.trgAddr2 = get32msb(buf2, 4);
        tun6_ntry.srcAddr3 = bridge_ntry.trgAddr3 = get32msb(buf2, 8);
        tun6_ntry.srcAddr4 = bridge_ntry.trgAddr4 = get32msb(buf2, 12);
        bridge_ntry.nexthop = atoi(arg[6]);
        bridge_ntry.instance = atoi(arg[7]);
        bridge_ntry.command = 5;
        vrf2rib_ntry.vrf = atoi(arg[8]);
        vrf2rib_res = vrf2rib_init6;
        tun6_ntry.aclport = atoi(arg[9]);
        tun6_ntry.trgPort = bridge_ntry.srcPort = atoi(arg[10]);
        tun6_ntry.srcPort = bridge_ntry.trgPort = atoi(arg[11]);
        tun6_ntry.prot = IP_PROTOCOL_UDP;
        tun6_ntry.command = 3;
        if (del == 0) hasht_del(&bridge_table, &bridge_ntry);
        else hasht_add(&bridge_table, &bridge_ntry);
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun6_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "bridgeetherip4") == 0) {
        bridge_ntry.id = atoi(arg[2]);
        str2mac(buf2, arg[3]);
        bridge_ntry.mac1 = get16msb(buf2, 0);
        bridge_ntry.mac2 = get32msb(buf2, 2);
        inet_pton(AF_INET, arg[4], buf2);
        tun4_ntry.trgAddr = bridge_ntry.srcAddr1 = get32msb(buf2, 0);
        inet_pton(AF_INET, arg[5], buf2);
        tun4_ntry.srcAddr = bridge_ntry.trgAddr1 = get32msb(buf2, 0);
        bridge_ntry.nexthop = atoi(arg[6]);
        bridge_ntry.command = 10;
        vrf2rib_ntry.vrf = atoi(arg[7]);
        vrf2rib_res = vrf2rib_init4;
        tun4_ntry.aclport = atoi(arg[8]);
        tun4_ntry.prot = IP_PROTOCOL_ETHERIP;
        tun4_ntry.command = 5;
        if (del == 0) hasht_del(&bridge_table, &bridge_ntry);
        else hasht_add(&bridge_table, &bridge_ntry);
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun4_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "bridgeetherip6") == 0) {
        bridge_ntry.id = atoi(arg[2]);
        str2mac(buf2, arg[3]);
        bridge_ntry.mac1 = get16msb(buf2, 0);
        bridge_ntry.mac2 = get32msb(buf2, 2);
        inet_pton(AF_INET6, arg[4], buf2);
        tun6_ntry.trgAddr1 = bridge_ntry.srcAddr1 = get32msb(buf2, 0);
        tun6_ntry.trgAddr2 = bridge_ntry.srcAddr2 = get32msb(buf2, 4);
        tun6_ntry.trgAddr3 = bridge_ntry.srcAddr3 = get32msb(buf2, 8);
        tun6_ntry.trgAddr4 = bridge_ntry.srcAddr4 = get32msb(buf2, 12);
        inet_pton(AF_INET6, arg[5], buf2);
        tun6_ntry.srcAddr1 = bridge_ntry.trgAddr1 = get32msb(buf2, 0);
        tun6_ntry.srcAddr2 = bridge_ntry.trgAddr2 = get32msb(buf2, 4);
        tun6_ntry.srcAddr3 = bridge_ntry.trgAddr3 = get32msb(buf2, 8);
        tun6_ntry.srcAddr4 = bridge_ntry.trgAddr4 = get32msb(buf2, 12);
        bridge_ntry.nexthop = atoi(arg[6]);
        bridge_ntry.command = 11;
        vrf2rib_ntry.vrf = atoi(arg[7]);
        vrf2rib_res = vrf2rib_init6;
        tun6_ntry.aclport = atoi(arg[8]);
        tun6_ntry.prot = IP_PROTOCOL_ETHERIP;
        tun6_ntry.command = 5;
        if (del == 0) hasht_del(&bridge_table, &bridge_ntry);
        else hasht_add(&bridge_table, &bridge_ntry);
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun6_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "bridgeeoip4") == 0) {
        bridge_ntry.id = atoi(arg[2]);
        str2mac(buf2, arg[3]);
        bridge_ntry.mac1 = get16msb(buf2, 0);
        bridge_ntry.mac2 = get32msb(buf2, 2);
        inet_pton(AF_INET, arg[4], buf2);
        tun4_ntry.trgAddr = bridge_ntry.srcAddr1 = get32msb(buf2, 0);
        inet_pton(AF_INET, arg[5], buf2);
        tun4_ntry.srcAddr = bridge_ntry.trgAddr1 = get32msb(buf2, 0);
        bridge_ntry.nexthop = atoi(arg[6]);
        bridge_ntry.instance = atoi(arg[7]);
        bridge_ntry.command = 12;
        vrf2rib_ntry.vrf = atoi(arg[8]);
        vrf2rib_res = vrf2rib_init4;
        tun4_ntry.aclport = atoi(arg[9]);
        tun4_ntry.prot = IP_PROTOCOL_GRE;
        tun4_ntry.command = 14;
        if (del == 0) hasht_del(&bridge_table, &bridge_ntry);
        else hasht_add(&bridge_table, &bridge_ntry);
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun4_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "bridgeeoip6") == 0) {
        bridge_ntry.id = atoi(arg[2]);
        str2mac(buf2, arg[3]);
        bridge_ntry.mac1 = get16msb(buf2, 0);
        bridge_ntry.mac2 = get32msb(buf2, 2);
        inet_pton(AF_INET6, arg[4], buf2);
        tun6_ntry.trgAddr1 = bridge_ntry.srcAddr1 = get32msb(buf2, 0);
        tun6_ntry.trgAddr2 = bridge_ntry.srcAddr2 = get32msb(buf2, 4);
        tun6_ntry.trgAddr3 = bridge_ntry.srcAddr3 = get32msb(buf2, 8);
        tun6_ntry.trgAddr4 = bridge_ntry.srcAddr4 = get32msb(buf2, 12);
        inet_pton(AF_INET6, arg[5], buf2);
        tun6_ntry.srcAddr1 = bridge_ntry.trgAddr1 = get32msb(buf2, 0);
        tun6_ntry.srcAddr2 = bridge_ntry.trgAddr2 = get32msb(buf2, 4);
        tun6_ntry.srcAddr3 = bridge_ntry.trgAddr3 = get32msb(buf2, 8);
        tun6_ntry.srcAddr4 = bridge_ntry.trgAddr4 = get32msb(buf2, 12);
        bridge_ntry.nexthop = atoi(arg[6]);
        bridge_ntry.instance = atoi(arg[7]);
        bridge_ntry.command = 13;
        vrf2rib_ntry.vrf = atoi(arg[8]);
        vrf2rib_res = vrf2rib_init6;
        tun6_ntry.aclport = atoi(arg[9]);
        tun6_ntry.prot = IP_PROTOCOL_GRE;
        tun6_ntry.command = 14;
        if (del == 0) hasht_del(&bridge_table, &bridge_ntry);
        else hasht_add(&bridge_table, &bridge_ntry);
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun6_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "bridgepckoudp4") == 0) {
        bridge_ntry.id = atoi(arg[2]);
        str2mac(buf2, arg[3]);
        bridge_ntry.mac1 = get16msb(buf2, 0);
        bridge_ntry.mac2 = get32msb(buf2, 2);
        inet_pton(AF_INET, arg[4], buf2);
        tun4_ntry.trgAddr = bridge_ntry.srcAddr1 = get32msb(buf2, 0);
        inet_pton(AF_INET, arg[5], buf2);
        tun4_ntry.srcAddr = bridge_ntry.trgAddr1 = get32msb(buf2, 0);
        tun4_ntry.trgPort = bridge_ntry.srcPort = atoi(arg[6]);
        tun4_ntry.srcPort = bridge_ntry.trgPort = atoi(arg[7]);
        bridge_ntry.nexthop = atoi(arg[8]);
        bridge_ntry.command = 6;
        vrf2rib_ntry.vrf = atoi(arg[9]);
        vrf2rib_res = vrf2rib_init4;
        tun4_ntry.aclport = atoi(arg[10]);
        tun4_ntry.prot = IP_PROTOCOL_UDP;
        tun4_ntry.command = 6;
        if (del == 0) hasht_del(&bridge_table, &bridge_ntry);
        else hasht_add(&bridge_table, &bridge_ntry);
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun4_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "bridgepckoudp6") == 0) {
        bridge_ntry.id = atoi(arg[2]);
        str2mac(buf2, arg[3]);
        bridge_ntry.mac1 = get16msb(buf2, 0);
        bridge_ntry.mac2 = get32msb(buf2, 2);
        inet_pton(AF_INET6, arg[4], buf2);
        tun6_ntry.trgAddr1 = bridge_ntry.srcAddr1 = get32msb(buf2, 0);
        tun6_ntry.trgAddr2 = bridge_ntry.srcAddr2 = get32msb(buf2, 4);
        tun6_ntry.trgAddr3 = bridge_ntry.srcAddr3 = get32msb(buf2, 8);
        tun6_ntry.trgAddr4 = bridge_ntry.srcAddr4 = get32msb(buf2, 12);
        inet_pton(AF_INET6, arg[5], buf2);
        tun6_ntry.srcAddr1 = bridge_ntry.trgAddr1 = get32msb(buf2, 0);
        tun6_ntry.srcAddr2 = bridge_ntry.trgAddr2 = get32msb(buf2, 4);
        tun6_ntry.srcAddr3 = bridge_ntry.trgAddr3 = get32msb(buf2, 8);
        tun6_ntry.srcAddr4 = bridge_ntry.trgAddr4 = get32msb(buf2, 12);
        tun6_ntry.trgPort = bridge_ntry.srcPort = atoi(arg[6]);
        tun6_ntry.srcPort = bridge_ntry.trgPort = atoi(arg[7]);
        bridge_ntry.nexthop = atoi(arg[8]);
        bridge_ntry.command = 7;
        vrf2rib_ntry.vrf = atoi(arg[9]);
        vrf2rib_res = vrf2rib_init6;
        tun6_ntry.aclport = atoi(arg[10]);
        tun6_ntry.prot = IP_PROTOCOL_UDP;
        tun6_ntry.command = 6;
        if (del == 0) hasht_del(&bridge_table, &bridge_ntry);
        else hasht_add(&bridge_table, &bridge_ntry);
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun6_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "bridgesrv4") == 0) {
        bridge_ntry.id = atoi(arg[2]);
        str2mac(buf2, arg[3]);
        bridge_ntry.mac1 = get16msb(buf2, 0);
        bridge_ntry.mac2 = get32msb(buf2, 2);
        inet_pton(AF_INET, arg[6], buf2);
        bridge_ntry.srcAddr1 = bridge_ntry.trgAddr1 = get32msb(buf2, 0);
        bridge_ntry.nexthop = atoi(arg[5]);
        bridge_ntry.command = 8;
        if (del == 0) hasht_del(&bridge_table, &bridge_ntry);
        else hasht_add(&bridge_table, &bridge_ntry);
        return 0;
    }
    if (strcmp(arg[0], "bridgesrv6") == 0) {
        bridge_ntry.id = atoi(arg[2]);
        str2mac(buf2, arg[3]);
        bridge_ntry.mac1 = get16msb(buf2, 0);
        bridge_ntry.mac2 = get32msb(buf2, 2);
        inet_pton(AF_INET6, arg[6], buf2);
        bridge_ntry.srcAddr1 = bridge_ntry.trgAddr1 = get32msb(buf2, 0);
        bridge_ntry.srcAddr2 = bridge_ntry.trgAddr2 = get32msb(buf2, 4);
        bridge_ntry.srcAddr3 = bridge_ntry.trgAddr3 = get32msb(buf2, 8);
        bridge_ntry.srcAddr4 = bridge_ntry.trgAddr4 = get32msb(buf2, 12);
        bridge_ntry.nexthop = atoi(arg[5]);
        bridge_ntry.command = 9;
        if (del == 0) hasht_del(&bridge_table, &bridge_ntry);
        else hasht_add(&bridge_table, &bridge_ntry);
        return 0;
    }
    if (strcmp(arg[0], "portvlan") == 0) {
        vlanout_ntry.id = vlanin_ntry.id = atoi(arg[2]);
        vlanout_ntry.port = vlanin_ntry.port = atoi(arg[3]);
        vlanout_ntry.vlan = vlanin_ntry.vlan = atoi(arg[4]);
        if (del == 0) hasht_del(&vlanin_table, &vlanin_ntry);
        else hasht_add(&vlanin_table, &vlanin_ntry);
        if (del == 0) hasht_del(&vlanout_table, &vlanout_ntry);
        else hasht_add(&vlanout_table, &vlanout_ntry);
        return 0;
    }
    if (strcmp(arg[0], "portqinq") == 0) {
        vlanout_ntry.id = vlanin_ntry.id = atoi(arg[2]);
        vlanout_ntry.port = atoi(arg[3]);
        vlanout_ntry.port2 = vlanin_ntry.port = atoi(arg[4]);
        vlanout_ntry.vlan2 = atoi(arg[5]);
        vlanout_ntry.vlan = vlanin_ntry.vlan = atoi(arg[6]);
        if (del == 0) hasht_del(&vlanin_table, &vlanin_ntry);
        else hasht_add(&vlanin_table, &vlanin_ntry);
        if (del == 0) hasht_del(&vlanout_table, &vlanout_ntry);
        else hasht_add(&vlanout_table, &vlanout_ntry);
        return 0;
    }
    if (strcmp(arg[0], "myaddr4") == 0) {
        inet_pton(AF_INET, arg[2], buf2);
        vrf2rib_ntry.vrf = atoi(arg[5]);
        vrf2rib_res = vrf2rib_init4;
        route4_ntry.addr[0] = get32msb(buf2, 0);
        route4_ntry.mask = atoi(arg[3]);
        route4_ntry.command = 2;
        if (del == 0) tree_del(&vrf2rib_res->rou, &route4_ntry);
        else tree_add(&vrf2rib_res->rou, &route4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "route4") == 0) {
        inet_pton(AF_INET, arg[2], buf2);
        vrf2rib_ntry.vrf = atoi(arg[6]);
        vrf2rib_res = vrf2rib_init4;
        route4_ntry.addr[0] = get32msb(buf2, 0);
        route4_ntry.mask = atoi(arg[3]);
        route4_ntry.nexthop = atoi(arg[4]);
        route4_ntry.command = 1;
        if (del == 0) tree_del(&vrf2rib_res->rou, &route4_ntry);
        else tree_add(&vrf2rib_res->rou, &route4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "labroute4") == 0) {
        inet_pton(AF_INET, arg[2], buf2);
        vrf2rib_ntry.vrf = atoi(arg[6]);
        vrf2rib_res = vrf2rib_init4;
        route4_ntry.addr[0] = get32msb(buf2, 0);
        route4_ntry.mask = atoi(arg[3]);
        route4_ntry.nexthop = atoi(arg[4]);
        route4_ntry.label1 = atoi(arg[7]);
        route4_ntry.command = 3;
        if (del == 0) tree_del(&vrf2rib_res->rou, &route4_ntry);
        else tree_add(&vrf2rib_res->rou, &route4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "vpnroute4") == 0) {
        inet_pton(AF_INET, arg[2], buf2);
        vrf2rib_ntry.vrf = atoi(arg[6]);
        vrf2rib_res = vrf2rib_init4;
        route4_ntry.addr[0] = get32msb(buf2, 0);
        route4_ntry.mask = atoi(arg[3]);
        route4_ntry.nexthop = atoi(arg[4]);
        route4_ntry.label1 = atoi(arg[7]);
        route4_ntry.label2 = atoi(arg[8]);
        route4_ntry.command = 4;
        if (del == 0) tree_del(&vrf2rib_res->rou, &route4_ntry);
        else tree_add(&vrf2rib_res->rou, &route4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "srvroute4") == 0) {
        inet_pton(AF_INET, arg[2], buf2);
        vrf2rib_ntry.vrf = atoi(arg[6]);
        vrf2rib_res = vrf2rib_init4;
        route4_ntry.addr[0] = get32msb(buf2, 0);
        route4_ntry.mask = atoi(arg[3]);
        route4_ntry.nexthop = atoi(arg[4]);
        inet_pton(AF_INET6, arg[7], buf2);
        route4_ntry.srv1 = get32msb(buf2, 0);
        route4_ntry.srv2 = get32msb(buf2, 4);
        route4_ntry.srv3 = get32msb(buf2, 8);
        route4_ntry.srv4 = get32msb(buf2, 12);
        route4_ntry.command = 5;
        if (del == 0) tree_del(&vrf2rib_res->rou, &route4_ntry);
        else tree_add(&vrf2rib_res->rou, &route4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "polroute4") == 0) {
        inet_pton(AF_INET, arg[2], buf2);
        vrf2rib_ntry.vrf = atoi(arg[6]);
        vrf2rib_res = vrf2rib_init4;
        route4_ntry.addr[0] = get32msb(buf2, 0);
        route4_ntry.mask = atoi(arg[3]);
        route4_ntry.nexthop = atoi(arg[4]);
        str2key(arg[7], route4_ntry.polka);
        route4_ntry.command = 9;
        if (del == 0) tree_del(&vrf2rib_res->rou, &route4_ntry);
        else tree_add(&vrf2rib_res->rou, &route4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "mpolroute4") == 0) {
        inet_pton(AF_INET, arg[2], buf2);
        vrf2rib_ntry.vrf = atoi(arg[6]);
        vrf2rib_res = vrf2rib_init4;
        route4_ntry.addr[0] = get32msb(buf2, 0);
        route4_ntry.mask = atoi(arg[3]);
        route4_ntry.nexthop = atoi(arg[4]);
        str2key(arg[7], route4_ntry.polka);
        route4_ntry.command = 10;
        if (del == 0) tree_del(&vrf2rib_res->rou, &route4_ntry);
        else tree_add(&vrf2rib_res->rou, &route4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "droproute4") == 0) {
        inet_pton(AF_INET, arg[2], buf2);
        vrf2rib_ntry.vrf = atoi(arg[4]);
        vrf2rib_res = vrf2rib_init4;
        route4_ntry.addr[0] = get32msb(buf2, 0);
        route4_ntry.mask = atoi(arg[3]);
        route4_ntry.command = 11;
        if (del == 0) tree_del(&vrf2rib_res->rou, &route4_ntry);
        else tree_add(&vrf2rib_res->rou, &route4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "neigh4") == 0) {
        route4_ntry.nexthop = atoi(arg[2]);
        inet_pton(AF_INET, arg[3], buf2);
        vrf2rib_ntry.vrf = atoi(arg[5]);
        vrf2rib_res = vrf2rib_init4;
        route4_ntry.addr[0] = get32msb(buf2, 0);
        route4_ntry.mask = 32;
        route4_ntry.command = 1;
        neigh_ntry.id = route4_ntry.nexthop;
        neigh_ntry.vrf = vrf2rib_ntry.vrf;
        neigh_ntry.aclport = neigh_ntry.port = atoi(arg[7]);
        neigh_ntry.command = 1;
        str2mac(&neigh_ntry.macs[0], arg[4]);
        str2mac(&neigh_ntry.macs[6], arg[6]);
        if (del == 0) tree_del(&vrf2rib_res->rou, &route4_ntry);
        else tree_add(&vrf2rib_res->rou, &route4_ntry);
        if (del == 0) hasht_del(&neigh_table, &neigh_ntry);
        else hasht_add(&neigh_table, &neigh_ntry);
        return 0;
    }
    if (strcmp(arg[0], "pwhenei4") == 0) {
        route4_ntry.nexthop = atoi(arg[2]);
        inet_pton(AF_INET, arg[3], buf2);
        vrf2rib_ntry.vrf = atoi(arg[5]);
        vrf2rib_res = vrf2rib_init4;
        route4_ntry.addr[0] = get32msb(buf2, 0);
        route4_ntry.mask = 32;
        route4_ntry.command = 1;
        neigh_ntry.id = route4_ntry.nexthop;
        neigh_ntry.vrf = vrf2rib_ntry.vrf;
        neigh_ntry.port = atoi(arg[7]);
        neigh_ntry.aclport = atoi(arg[8]);
        neigh_ntry.command = 23;
        str2mac(&neigh_ntry.macs[0], arg[4]);
        str2mac(&neigh_ntry.macs[6], arg[6]);
        str2mac(&neigh_ntry.mac2[0], arg[9]);
        str2mac(&neigh_ntry.mac2[6], arg[10]);
        neigh_ntry.sprt = atoi(arg[11]);
        neigh_ntry.dprt = atoi(arg[12]);
        if (del == 0) tree_del(&vrf2rib_res->rou, &route4_ntry);
        else tree_add(&vrf2rib_res->rou, &route4_ntry);
        if (del == 0) hasht_del(&neigh_table, &neigh_ntry);
        else hasht_add(&neigh_table, &neigh_ntry);
        return 0;
    }
    if (strcmp(arg[0], "myaddr6") == 0) {
        inet_pton(AF_INET6, arg[2], buf2);
        vrf2rib_ntry.vrf = atoi(arg[5]);
        vrf2rib_res = vrf2rib_init6;
        route6_ntry.addr[0] = get32msb(buf2, 0);
        route6_ntry.addr[1] = get32msb(buf2, 4);
        route6_ntry.addr[2] = get32msb(buf2, 8);
        route6_ntry.addr[3] = get32msb(buf2, 12);
        route6_ntry.mask = atoi(arg[3]);
        route6_ntry.command = 2;
        if (del == 0) tree_del(&vrf2rib_res->rou, &route6_ntry);
        else tree_add(&vrf2rib_res->rou, &route6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "route6") == 0) {
        inet_pton(AF_INET6, arg[2], buf2);
        vrf2rib_ntry.vrf = atoi(arg[6]);
        vrf2rib_res = vrf2rib_init6;
        route6_ntry.addr[0] = get32msb(buf2, 0);
        route6_ntry.addr[1] = get32msb(buf2, 4);
        route6_ntry.addr[2] = get32msb(buf2, 8);
        route6_ntry.addr[3] = get32msb(buf2, 12);
        route6_ntry.mask = atoi(arg[3]);
        route6_ntry.nexthop = atoi(arg[4]);
        route6_ntry.command = 1;
        if (del == 0) tree_del(&vrf2rib_res->rou, &route6_ntry);
        else tree_add(&vrf2rib_res->rou, &route6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "labroute6") == 0) {
        inet_pton(AF_INET6, arg[2], buf2);
        vrf2rib_ntry.vrf = atoi(arg[6]);
        vrf2rib_res = vrf2rib_init6;
        route6_ntry.addr[0] = get32msb(buf2, 0);
        route6_ntry.addr[1] = get32msb(buf2, 4);
        route6_ntry.addr[2] = get32msb(buf2, 8);
        route6_ntry.addr[3] = get32msb(buf2, 12);
        route6_ntry.mask = atoi(arg[3]);
        route6_ntry.nexthop = atoi(arg[4]);
        route6_ntry.label1 = atoi(arg[7]);
        route6_ntry.command = 3;
        if (del == 0) tree_del(&vrf2rib_res->rou, &route6_ntry);
        else tree_add(&vrf2rib_res->rou, &route6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "vpnroute6") == 0) {
        inet_pton(AF_INET6, arg[2], buf2);
        vrf2rib_ntry.vrf = atoi(arg[6]);
        vrf2rib_res = vrf2rib_init6;
        route6_ntry.addr[0] = get32msb(buf2, 0);
        route6_ntry.addr[1] = get32msb(buf2, 4);
        route6_ntry.addr[2] = get32msb(buf2, 8);
        route6_ntry.addr[3] = get32msb(buf2, 12);
        route6_ntry.mask = atoi(arg[3]);
        route6_ntry.nexthop = atoi(arg[4]);
        route6_ntry.label1 = atoi(arg[7]);
        route6_ntry.label2 = atoi(arg[8]);
        route6_ntry.command = 4;
        if (del == 0) tree_del(&vrf2rib_res->rou, &route6_ntry);
        else tree_add(&vrf2rib_res->rou, &route6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "srvroute6") == 0) {
        inet_pton(AF_INET6, arg[2], buf2);
        vrf2rib_ntry.vrf = atoi(arg[6]);
        vrf2rib_res = vrf2rib_init6;
        route6_ntry.addr[0] = get32msb(buf2, 0);
        route6_ntry.addr[1] = get32msb(buf2, 4);
        route6_ntry.addr[2] = get32msb(buf2, 8);
        route6_ntry.addr[3] = get32msb(buf2, 12);
        route6_ntry.mask = atoi(arg[3]);
        route6_ntry.nexthop = atoi(arg[4]);
        inet_pton(AF_INET6, arg[7], buf2);
        route6_ntry.srv1 = get32msb(buf2, 0);
        route6_ntry.srv2 = get32msb(buf2, 4);
        route6_ntry.srv3 = get32msb(buf2, 8);
        route6_ntry.srv4 = get32msb(buf2, 12);
        route6_ntry.command = 5;
        if (del == 0) tree_del(&vrf2rib_res->rou, &route6_ntry);
        else tree_add(&vrf2rib_res->rou, &route6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "polroute6") == 0) {
        inet_pton(AF_INET6, arg[2], buf2);
        vrf2rib_ntry.vrf = atoi(arg[6]);
        vrf2rib_res = vrf2rib_init6;
        route6_ntry.addr[0] = get32msb(buf2, 0);
        route6_ntry.addr[1] = get32msb(buf2, 4);
        route6_ntry.addr[2] = get32msb(buf2, 8);
        route6_ntry.addr[3] = get32msb(buf2, 12);
        route6_ntry.mask = atoi(arg[3]);
        route6_ntry.nexthop = atoi(arg[4]);
        str2key(arg[7], route6_ntry.polka);
        route6_ntry.command = 9;
        if (del == 0) tree_del(&vrf2rib_res->rou, &route6_ntry);
        else tree_add(&vrf2rib_res->rou, &route6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "mpolroute6") == 0) {
        inet_pton(AF_INET6, arg[2], buf2);
        vrf2rib_ntry.vrf = atoi(arg[6]);
        vrf2rib_res = vrf2rib_init6;
        route6_ntry.addr[0] = get32msb(buf2, 0);
        route6_ntry.addr[1] = get32msb(buf2, 4);
        route6_ntry.addr[2] = get32msb(buf2, 8);
        route6_ntry.addr[3] = get32msb(buf2, 12);
        route6_ntry.mask = atoi(arg[3]);
        route6_ntry.nexthop = atoi(arg[4]);
        str2key(arg[7], route6_ntry.polka);
        route6_ntry.command = 10;
        if (del == 0) tree_del(&vrf2rib_res->rou, &route6_ntry);
        else tree_add(&vrf2rib_res->rou, &route6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "droproute6") == 0) {
        inet_pton(AF_INET6, arg[2], buf2);
        vrf2rib_ntry.vrf = atoi(arg[4]);
        vrf2rib_res = vrf2rib_init6;
        route6_ntry.addr[0] = get32msb(buf2, 0);
        route6_ntry.addr[1] = get32msb(buf2, 4);
        route6_ntry.addr[2] = get32msb(buf2, 8);
        route6_ntry.addr[3] = get32msb(buf2, 12);
        route6_ntry.mask = atoi(arg[3]);
        route6_ntry.command = 11;
        if (del == 0) tree_del(&vrf2rib_res->rou, &route6_ntry);
        else tree_add(&vrf2rib_res->rou, &route6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "neigh6") == 0) {
        route6_ntry.nexthop = atoi(arg[2]);
        inet_pton(AF_INET6, arg[3], buf2);
        vrf2rib_ntry.vrf = atoi(arg[5]);
        vrf2rib_res = vrf2rib_init6;
        route6_ntry.addr[0] = get32msb(buf2, 0);
        route6_ntry.addr[1] = get32msb(buf2, 4);
        route6_ntry.addr[2] = get32msb(buf2, 8);
        route6_ntry.addr[3] = get32msb(buf2, 12);
        route6_ntry.mask = 128;
        route6_ntry.command = 1;
        neigh_ntry.id = route6_ntry.nexthop;
        neigh_ntry.vrf = vrf2rib_ntry.vrf;
        neigh_ntry.aclport = neigh_ntry.port = atoi(arg[7]);
        neigh_ntry.command = 1;
        str2mac(&neigh_ntry.macs[0], arg[4]);
        str2mac(&neigh_ntry.macs[6], arg[6]);
        if (del == 0) tree_del(&vrf2rib_res->rou, &route6_ntry);
        else tree_add(&vrf2rib_res->rou, &route6_ntry);
        if (del == 0) hasht_del(&neigh_table, &neigh_ntry);
        else hasht_add(&neigh_table, &neigh_ntry);
        return 0;
    }
    if (strcmp(arg[0], "pwhenei6") == 0) {
        route6_ntry.nexthop = atoi(arg[2]);
        inet_pton(AF_INET6, arg[3], buf2);
        vrf2rib_ntry.vrf = atoi(arg[5]);
        vrf2rib_res = vrf2rib_init6;
        route6_ntry.addr[0] = get32msb(buf2, 0);
        route6_ntry.addr[1] = get32msb(buf2, 4);
        route6_ntry.addr[2] = get32msb(buf2, 8);
        route6_ntry.addr[3] = get32msb(buf2, 12);
        route6_ntry.mask = 128;
        route6_ntry.command = 1;
        neigh_ntry.id = route6_ntry.nexthop;
        neigh_ntry.vrf = vrf2rib_ntry.vrf;
        neigh_ntry.port = atoi(arg[7]);
        neigh_ntry.aclport = atoi(arg[8]);
        neigh_ntry.command = 23;
        str2mac(&neigh_ntry.macs[0], arg[4]);
        str2mac(&neigh_ntry.macs[6], arg[6]);
        str2mac(&neigh_ntry.mac2[0], arg[9]);
        str2mac(&neigh_ntry.mac2[6], arg[10]);
        neigh_ntry.sprt = atoi(arg[11]);
        neigh_ntry.dprt = atoi(arg[12]);
        if (del == 0) tree_del(&vrf2rib_res->rou, &route6_ntry);
        else tree_add(&vrf2rib_res->rou, &route6_ntry);
        if (del == 0) hasht_del(&neigh_table, &neigh_ntry);
        else hasht_add(&neigh_table, &neigh_ntry);
        return 0;
    }
    if (strcmp(arg[0], "labsnei") == 0) {
        neigh_ntry.id = atoi(arg[2]);
        neigh_ntry.vrf = atoi(arg[4]);
        neigh_ntry.port = atoi(arg[6]);
        neigh_ntry.aclport = atoi(arg[7]);
        neigh_ntry.command = 24;
        str2mac(&neigh_ntry.macs[0], arg[3]);
        str2mac(&neigh_ntry.macs[6], arg[5]);
        neigh_ntry.tid = atoi(arg[8]);
        neigh_ntry.dprt = atoi(arg[9]);
        neigh_ntry.sprt = atoi(arg[10]);
        neigh_ntry.sip1 = atoi(arg[11]);
        neigh_ntry.sip2 = atoi(arg[12]);
        neigh_ntry.sip3 = atoi(arg[13]);
        neigh_ntry.sip4 = atoi(arg[14]);
        neigh_ntry.dip1 = atoi(arg[15]);
        neigh_ntry.dip2 = atoi(arg[16]);
        neigh_ntry.dip3 = atoi(arg[17]);
        neigh_ntry.dip4 = atoi(arg[18]);
        if (del == 0) hasht_del(&neigh_table, &neigh_ntry);
        else hasht_add(&neigh_table, &neigh_ntry);
        return 0;
    }
    if (strcmp(arg[0], "mysrv4") == 0) {
        inet_pton(AF_INET6, arg[3], buf2);
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init6;
        route6_ntry.addr[0] = get32msb(buf2, 0);
        route6_ntry.addr[1] = get32msb(buf2, 4);
        route6_ntry.addr[2] = get32msb(buf2, 8);
        route6_ntry.addr[3] = get32msb(buf2, 12);
        route6_ntry.mask = 128;
        route6_ntry.srv1 = atoi(arg[4]);
        route6_ntry.command = 6;
        if (del == 0) tree_del(&vrf2rib_res->rou, &route6_ntry);
        else tree_add(&vrf2rib_res->rou, &route6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "mysrv6") == 0) {
        inet_pton(AF_INET6, arg[3], buf2);
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init6;
        route6_ntry.addr[0] = get32msb(buf2, 0);
        route6_ntry.addr[1] = get32msb(buf2, 4);
        route6_ntry.addr[2] = get32msb(buf2, 8);
        route6_ntry.addr[3] = get32msb(buf2, 12);
        route6_ntry.mask = 128;
        route6_ntry.srv1 = atoi(arg[4]);
        route6_ntry.command = 7;
        if (del == 0) tree_del(&vrf2rib_res->rou, &route6_ntry);
        else tree_add(&vrf2rib_res->rou, &route6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "bridgesrv") == 0) {
        inet_pton(AF_INET6, arg[4], buf2);
        vrf2rib_ntry.vrf = atoi(arg[3]);
        vrf2rib_res = vrf2rib_init6;
        route6_ntry.addr[0] = get32msb(buf2, 0);
        route6_ntry.addr[1] = get32msb(buf2, 4);
        route6_ntry.addr[2] = get32msb(buf2, 8);
        route6_ntry.addr[3] = get32msb(buf2, 12);
        route6_ntry.mask = 128;
        route6_ntry.srv1 = atoi(arg[2]);
        route6_ntry.command = 8;
        if (del == 0) tree_del(&vrf2rib_res->rou, &route6_ntry);
        else tree_add(&vrf2rib_res->rou, &route6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "inacl4") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        acl4init(&port2vrf_res->inacl4);
        readAcl4(&acl4_ntry, &arg[1]);
        if (del == 0) table_del(&port2vrf_res->inacl4, &acl4_ntry);
        else table_add(&port2vrf_res->inacl4, &acl4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "inacl6") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        acl6init(&port2vrf_res->inacl6);
        readAcl6(&acl6_ntry, &arg[1]);
        if (del == 0) table_del(&port2vrf_res->inacl6, &acl6_ntry);
        else table_add(&port2vrf_res->inacl6, &acl6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "outacl4") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        acl4init(&port2vrf_res->outacl4);
        readAcl4(&acl4_ntry, &arg[1]);
        if (del == 0) table_del(&port2vrf_res->outacl4, &acl4_ntry);
        else table_add(&port2vrf_res->outacl4, &acl4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "outacl6") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        acl6init(&port2vrf_res->outacl6);
        readAcl6(&acl6_ntry, &arg[1]);
        if (del == 0) table_del(&port2vrf_res->outacl6, &acl6_ntry);
        else table_add(&port2vrf_res->outacl6, &acl6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "ratein") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        policer_ntry.meter = port2vrf_ntry.port;
        policer_ntry.dir = 5;
        policer_ntry.allow = readRate(&arg[0]);
        if (del == 0) hasht_del(&policer_table, &policer_ntry);
        else hasht_add(&policer_table, &policer_ntry);
        if (del == 0) port2vrf_res->rateIn = 0;
        else port2vrf_res->rateIn = port2vrf_ntry.port;
        return 0;
    }
    if (strcmp(arg[0], "rateout") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        policer_ntry.meter = port2vrf_ntry.port;
        policer_ntry.dir = 6;
        policer_ntry.allow = readRate(&arg[0]);
        if (del == 0) hasht_del(&policer_table, &policer_ntry);
        else hasht_add(&policer_table, &policer_ntry);
        if (del == 0) port2vrf_res->rateOut = 0;
        else port2vrf_res->rateOut = port2vrf_ntry.port;
        return 0;
    }
    if (strcmp(arg[0], "inqos4") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        acl4init(&port2vrf_res->inqos4);
        acl4_ntry.nexthop = atoi(arg[3]);
        readAcl4(&acl4_ntry, &arg[2]);
        if (del == 0) table_del(&port2vrf_res->inqos4, &acl4_ntry);
        else table_add(&port2vrf_res->inqos4, &acl4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "inqos6") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        acl6init(&port2vrf_res->inqos6);
        acl6_ntry.nexthop = atoi(arg[3]);
        readAcl6(&acl6_ntry, &arg[2]);
        if (del == 0) table_del(&port2vrf_res->inqos6, &acl6_ntry);
        else table_add(&port2vrf_res->inqos6, &acl6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "outqos4") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        acl4init(&port2vrf_res->outqos4);
        acl4_ntry.nexthop = atoi(arg[3]);
        readAcl4(&acl4_ntry, &arg[2]);
        if (del == 0) table_del(&port2vrf_res->outqos4, &acl4_ntry);
        else table_add(&port2vrf_res->outqos4, &acl4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "outqos6") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        acl6init(&port2vrf_res->outqos6);
        acl6_ntry.nexthop = atoi(arg[3]);
        readAcl6(&acl6_ntry, &arg[2]);
        if (del == 0) table_del(&port2vrf_res->outqos6, &acl6_ntry);
        else table_add(&port2vrf_res->outqos6, &acl6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "natcfg4") == 0) {
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init4;
        acl4init(&vrf2rib_res->natC);
        readAcl4(&acl4_ntry, &arg[1]);
        if (del == 0) table_del(&vrf2rib_res->natC, &acl4_ntry);
        else table_add(&vrf2rib_res->natC, &acl4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "natcfg6") == 0) {
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init6;
        acl6init(&vrf2rib_res->natC);
        readAcl6(&acl6_ntry, &arg[1]);
        if (del == 0) table_del(&vrf2rib_res->natC, &acl6_ntry);
        else table_add(&vrf2rib_res->natC, &acl6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "copp4") == 0) {
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init4;
        acl4init(&vrf2rib_res->copp);
        readAcl4(&acl4_ntry, &arg[1]);
        if (del == 0) table_del(&vrf2rib_res->copp, &acl4_ntry);
        else table_add(&vrf2rib_res->copp, &acl4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "copp6") == 0) {
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init6;
        acl6init(&vrf2rib_res->copp);
        readAcl6(&acl6_ntry, &arg[1]);
        if (del == 0) table_del(&vrf2rib_res->copp, &acl6_ntry);
        else table_add(&vrf2rib_res->copp, &acl6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "nattrns4") == 0) {
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init4;
        nat4_ntry.prot = atoi(arg[3]);
        inet_pton(AF_INET, arg[4], buf2);
        nat4_ntry.oSrcAddr = get32msb(buf2, 0);
        nat4_ntry.oSrcPort = atoi(arg[5]);
        inet_pton(AF_INET, arg[6], buf2);
        nat4_ntry.oTrgAddr = get32msb(buf2, 0);
        nat4_ntry.oTrgPort = atoi(arg[7]);
        inet_pton(AF_INET, arg[8], buf2);
        nat4_ntry.nSrcAddr = get32msb(buf2, 0);
        nat4_ntry.nSrcPort = atoi(arg[9]);
        inet_pton(AF_INET, arg[10], buf2);
        nat4_ntry.nTrgAddr = get32msb(buf2, 0);
        nat4_ntry.nTrgPort = atoi(arg[11]);
        nat4_ntry.sum3 = 0;
        accumulate_sum(nat4_ntry.sum3, nat4_ntry.oSrcAddr, -1);
        accumulate_sum(nat4_ntry.sum3, nat4_ntry.oTrgAddr, -1);
        accumulate_sum(nat4_ntry.sum3, nat4_ntry.nSrcAddr, +1);
        accumulate_sum(nat4_ntry.sum3, nat4_ntry.nTrgAddr, +1);
        nat4_ntry.sum4 = nat4_ntry.sum3;
        accumulate_sum(nat4_ntry.sum4, nat4_ntry.oSrcPort, -1);
        accumulate_sum(nat4_ntry.sum4, nat4_ntry.oTrgPort, -1);
        accumulate_sum(nat4_ntry.sum4, nat4_ntry.nSrcPort, +1);
        accumulate_sum(nat4_ntry.sum4, nat4_ntry.nTrgPort, +1);
        if (del == 0) hasht_del(&vrf2rib_res->natT, &nat4_ntry);
        else hasht_add(&vrf2rib_res->natT, &nat4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "nattrns6") == 0) {
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init6;
        nat6_ntry.prot = atoi(arg[3]);
        inet_pton(AF_INET6, arg[4], buf2);
        nat6_ntry.oSrcAddr1 = get32msb(buf2, 0);
        nat6_ntry.oSrcAddr2 = get32msb(buf2, 4);
        nat6_ntry.oSrcAddr3 = get32msb(buf2, 8);
        nat6_ntry.oSrcAddr4 = get32msb(buf2, 12);
        nat6_ntry.oSrcPort = atoi(arg[5]);
        inet_pton(AF_INET6, arg[6], buf2);
        nat6_ntry.oTrgAddr1 = get32msb(buf2, 0);
        nat6_ntry.oTrgAddr2 = get32msb(buf2, 4);
        nat6_ntry.oTrgAddr3 = get32msb(buf2, 8);
        nat6_ntry.oTrgAddr4 = get32msb(buf2, 12);
        nat6_ntry.oTrgPort = atoi(arg[7]);
        inet_pton(AF_INET6, arg[8], buf2);
        nat6_ntry.nSrcAddr1 = get32msb(buf2, 0);
        nat6_ntry.nSrcAddr2 = get32msb(buf2, 4);
        nat6_ntry.nSrcAddr3 = get32msb(buf2, 8);
        nat6_ntry.nSrcAddr4 = get32msb(buf2, 12);
        nat6_ntry.nSrcPort = atoi(arg[9]);
        inet_pton(AF_INET6, arg[10], buf2);
        nat6_ntry.nTrgAddr1 = get32msb(buf2, 0);
        nat6_ntry.nTrgAddr2 = get32msb(buf2, 4);
        nat6_ntry.nTrgAddr3 = get32msb(buf2, 8);
        nat6_ntry.nTrgAddr4 = get32msb(buf2, 12);
        nat6_ntry.nTrgPort = atoi(arg[11]);
        nat6_ntry.sum3 = 0;
        accumulate_sum(nat6_ntry.sum3, nat6_ntry.oSrcAddr1, -1);
        accumulate_sum(nat6_ntry.sum3, nat6_ntry.oSrcAddr2, -1);
        accumulate_sum(nat6_ntry.sum3, nat6_ntry.oSrcAddr3, -1);
        accumulate_sum(nat6_ntry.sum3, nat6_ntry.oSrcAddr4, -1);
        accumulate_sum(nat6_ntry.sum3, nat6_ntry.oTrgAddr1, -1);
        accumulate_sum(nat6_ntry.sum3, nat6_ntry.oTrgAddr2, -1);
        accumulate_sum(nat6_ntry.sum3, nat6_ntry.oTrgAddr3, -1);
        accumulate_sum(nat6_ntry.sum3, nat6_ntry.oTrgAddr4, -1);
        accumulate_sum(nat6_ntry.sum3, nat6_ntry.nSrcAddr1, +1);
        accumulate_sum(nat6_ntry.sum3, nat6_ntry.nSrcAddr2, +1);
        accumulate_sum(nat6_ntry.sum3, nat6_ntry.nSrcAddr3, +1);
        accumulate_sum(nat6_ntry.sum3, nat6_ntry.nSrcAddr4, +1);
        accumulate_sum(nat6_ntry.sum3, nat6_ntry.nTrgAddr1, +1);
        accumulate_sum(nat6_ntry.sum3, nat6_ntry.nTrgAddr2, +1);
        accumulate_sum(nat6_ntry.sum3, nat6_ntry.nTrgAddr3, +1);
        accumulate_sum(nat6_ntry.sum3, nat6_ntry.nTrgAddr4, +1);
        nat6_ntry.sum4 = nat6_ntry.sum3;
        accumulate_sum(nat6_ntry.sum4, nat6_ntry.oSrcPort, -1);
        accumulate_sum(nat6_ntry.sum4, nat6_ntry.oTrgPort, -1);
        accumulate_sum(nat6_ntry.sum4, nat6_ntry.nSrcPort, +1);
        accumulate_sum(nat6_ntry.sum4, nat6_ntry.nTrgPort, +1);
        if (del == 0) hasht_del(&vrf2rib_res->natT, &nat6_ntry);
        else hasht_add(&vrf2rib_res->natT, &nat6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "inspect4") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        if (hasht_nonexist(&port2vrf_res->insp4)) hasht_init(&port2vrf_res->insp4, sizeof(struct insp4_entry), 5);
        insp4_ntry.prot = atoi(arg[3]);
        inet_pton(AF_INET, arg[4], buf2);
        insp4_ntry.srcAddr = get32msb(buf2, 0);
        insp4_ntry.srcPort = atoi(arg[5]);
        inet_pton(AF_INET, arg[6], buf2);
        insp4_ntry.trgAddr = get32msb(buf2, 0);
        insp4_ntry.trgPort = atoi(arg[7]);
        if (del == 0) hasht_del(&port2vrf_res->insp4, &insp4_ntry);
        else hasht_add(&port2vrf_res->insp4, &insp4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "inspect6") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        if (hasht_nonexist(&port2vrf_res->insp6)) hasht_init(&port2vrf_res->insp6, sizeof(struct insp6_entry), 11);
        insp6_ntry.prot = atoi(arg[3]);
        inet_pton(AF_INET6, arg[4], buf2);
        insp6_ntry.srcAddr1 = get32msb(buf2, 0);
        insp6_ntry.srcAddr2 = get32msb(buf2, 4);
        insp6_ntry.srcAddr3 = get32msb(buf2, 8);
        insp6_ntry.srcAddr4 = get32msb(buf2, 12);
        insp6_ntry.srcPort = atoi(arg[5]);
        inet_pton(AF_INET6, arg[6], buf2);
        insp6_ntry.trgAddr1 = get32msb(buf2, 0);
        insp6_ntry.trgAddr2 = get32msb(buf2, 4);
        insp6_ntry.trgAddr3 = get32msb(buf2, 8);
        insp6_ntry.trgAddr4 = get32msb(buf2, 12);
        insp6_ntry.trgPort = atoi(arg[7]);
        if (del == 0) hasht_del(&port2vrf_res->insp6, &insp6_ntry);
        else hasht_add(&port2vrf_res->insp6, &insp6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "inqos") == 0) {
        policer_ntry.meter = atoi(arg[2]);
        policer_ntry.dir = 1;
        policer_ntry.allow = readRate(&arg[0]);
        if (del == 0) hasht_del(&policer_table, &policer_ntry);
        else hasht_add(&policer_table, &policer_ntry);
        return 0;
    }
    if (strcmp(arg[0], "outqos") == 0) {
        policer_ntry.meter = atoi(arg[2]);
        policer_ntry.dir = 2;
        policer_ntry.allow = readRate(&arg[0]);
        if (del == 0) hasht_del(&policer_table, &policer_ntry);
        else hasht_add(&policer_table, &policer_ntry);
        return 0;
    }
    if (strcmp(arg[0], "flowspec4") == 0) {
        policer_ntry.vrf = vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init4;
        acl4init(&vrf2rib_res->flws);
        policer_ntry.dir = 3;
        policer_ntry.allow = readRate(&arg[1]);
        readAcl4(&acl4_ntry, &arg[4]);
        policer_ntry.meter = acl4_ntry.pri;
        if (del == 0) table_del(&vrf2rib_res->flws, &acl4_ntry);
        else table_add(&vrf2rib_res->flws, &acl4_ntry);
        if (del == 0) hasht_del(&policer_table, &policer_ntry);
        else hasht_add(&policer_table, &policer_ntry);
        return 0;
    }
    if (strcmp(arg[0], "flowspec6") == 0) {
        policer_ntry.vrf = vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init6;
        acl6init(&vrf2rib_res->flws);
        policer_ntry.dir = 4;
        policer_ntry.allow = readRate(&arg[1]);
        readAcl6(&acl6_ntry, &arg[4]);
        policer_ntry.meter = acl6_ntry.pri;
        if (del == 0) table_del(&vrf2rib_res->flws, &acl6_ntry);
        else table_add(&vrf2rib_res->flws, &acl6_ntry);
        if (del == 0) hasht_del(&policer_table, &policer_ntry);
        else hasht_add(&policer_table, &policer_ntry);
        return 0;
    }
    if (strcmp(arg[0], "pbr4norm") == 0) {
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init4;
        acl4init(&vrf2rib_res->pbr);
        acl4_ntry.cmd = 1;
        readAcl4(&acl4_ntry, &arg[3]);
        if (del == 0) table_del(&vrf2rib_res->pbr, &acl4_ntry);
        else table_add(&vrf2rib_res->pbr, &acl4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "pbr6norm") == 0) {
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init6;
        acl6init(&vrf2rib_res->pbr);
        acl6_ntry.cmd = 1;
        readAcl6(&acl6_ntry, &arg[3]);
        if (del == 0) table_del(&vrf2rib_res->pbr, &acl6_ntry);
        else table_add(&vrf2rib_res->pbr, &acl6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "pbr4vrf") == 0) {
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init4;
        acl4init(&vrf2rib_res->pbr);
        acl4_ntry.cmd = 2;
        acl4_ntry.vrf = atoi(arg[3]);
        readAcl4(&acl4_ntry, &arg[3]);
        if (del == 0) table_del(&vrf2rib_res->pbr, &acl4_ntry);
        else table_add(&vrf2rib_res->pbr, &acl4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "pbr6vrf") == 0) {
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init6;
        acl6init(&vrf2rib_res->pbr);
        acl6_ntry.cmd = 2;
        acl6_ntry.vrf = atoi(arg[3]);
        readAcl6(&acl6_ntry, &arg[3]);
        if (del == 0) table_del(&vrf2rib_res->pbr, &acl6_ntry);
        else table_add(&vrf2rib_res->pbr, &acl6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "pbr4hop") == 0) {
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init4;
        acl4init(&vrf2rib_res->pbr);
        acl4_ntry.cmd = 3;
        acl4_ntry.vrf = atoi(arg[3]);
        acl4_ntry.nexthop = atoi(arg[4]);
        readAcl4(&acl4_ntry, &arg[3]);
        if (del == 0) table_del(&vrf2rib_res->pbr, &acl4_ntry);
        else table_add(&vrf2rib_res->pbr, &acl4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "pbr6hop") == 0) {
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init6;
        acl6init(&vrf2rib_res->pbr);
        acl6_ntry.cmd = 3;
        acl6_ntry.vrf = atoi(arg[3]);
        acl6_ntry.nexthop = atoi(arg[4]);
        readAcl6(&acl6_ntry, &arg[3]);
        if (del == 0) table_del(&vrf2rib_res->pbr, &acl6_ntry);
        else table_add(&vrf2rib_res->pbr, &acl6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "pbr4lab") == 0) {
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init4;
        acl4init(&vrf2rib_res->pbr);
        acl4_ntry.cmd = 4;
        acl4_ntry.vrf = atoi(arg[3]);
        acl4_ntry.nexthop = atoi(arg[4]);
        acl4_ntry.label = atoi(arg[5]);
        readAcl4(&acl4_ntry, &arg[4]);
        if (del == 0) table_del(&vrf2rib_res->pbr, &acl4_ntry);
        else table_add(&vrf2rib_res->pbr, &acl4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "pbr6lab") == 0) {
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init6;
        acl6init(&vrf2rib_res->pbr);
        acl6_ntry.cmd = 4;
        acl6_ntry.vrf = atoi(arg[3]);
        acl6_ntry.nexthop = atoi(arg[4]);
        acl6_ntry.label = atoi(arg[5]);
        readAcl6(&acl6_ntry, &arg[4]);
        if (del == 0) table_del(&vrf2rib_res->pbr, &acl6_ntry);
        else table_add(&vrf2rib_res->pbr, &acl6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "hairpin") == 0) {
        bundle_ntry.id = atoi(arg[2]);
        bundle_ntry.command = 2;
        o = atoi(arg[3]);
        for (int i = 0; i < 16; i++) bundle_ntry.out[i] = o;
        if (del == 0) hasht_del(&bundle_table, &bundle_ntry);
        else hasht_add(&bundle_table, &bundle_ntry);
        return 0;
    }
    if (strcmp(arg[0], "portbundle") == 0) {
        bundle_ntry.id = atoi(arg[2]);
        if (del == 0) {
            hasht_del(&bundle_table, &bundle_ntry);
            return 0;
        }
        bundle_res = hasht_find(&bundle_table, &bundle_ntry);
        if (bundle_res == NULL) {
            bundle_res = hasht_add(&bundle_table, &bundle_ntry);
        }
        i = atoi(arg[3]);
        bundle_res->command = 1;
        bundle_res->out[i & 15] = atoi(arg[4]);
        return 0;
    }
    if (strcmp(arg[0], "bundlevlan") == 0) {
        vlanin_ntry.id = atoi(arg[4]);
        vlanin_ntry.port = atoi(arg[2]);
        vlanin_ntry.vlan = atoi(arg[3]);
        if (del == 0) hasht_del(&vlanin_table, &vlanin_ntry);
        else hasht_add(&vlanin_table, &vlanin_ntry);
        return 0;
    }
    if (strcmp(arg[0], "bundleqinq") == 0) {
        vlanin_ntry.id = atoi(arg[4]);
        vlanin_ntry.port = atoi(arg[2]);
        vlanin_ntry.vlan = atoi(arg[3]);
        if (del == 0) hasht_del(&vlanin_table, &vlanin_ntry);
        else hasht_add(&vlanin_table, &vlanin_ntry);
        return 0;
    }
    if (strcmp(arg[0], "pppoe") == 0) {
        neigh_ntry.aclport = pppoe_ntry.aclport = atoi(arg[2]);
        neigh_ntry.port = pppoe_ntry.port = atoi(arg[3]);
        neigh_ntry.tid = pppoe_ntry.session = atoi(arg[6]);
        neigh_ntry.id = pppoe_ntry.neigh = atoi(arg[4]);
        neigh_ntry.vrf = atoi(arg[5]);
        neigh_ntry.command = 2;
        str2mac(&neigh_ntry.macs[0], arg[7]);
        str2mac(&neigh_ntry.macs[6], arg[8]);
        if (del == 0) hasht_del(&pppoe_table, &pppoe_ntry);
        else hasht_add(&pppoe_table, &pppoe_ntry);
        if (del == 0) hasht_del(&neigh_table, &neigh_ntry);
        else hasht_add(&neigh_table, &neigh_ntry);
        return 0;
    }
    if (strcmp(arg[0], "gre4") == 0) {
        tun4_ntry.neigh = neigh_ntry.id = atoi(arg[2]);
        tun4_ntry.aclport = neigh_ntry.aclport = atoi(arg[3]);
        neigh_ntry.port = atoi(arg[4]);
        inet_pton(AF_INET, arg[5], buf2);
        tun4_ntry.trgAddr = neigh_ntry.sip1 = get32msb(buf2, 0);
        inet_pton(AF_INET, arg[6], buf2);
        tun4_ntry.srcAddr = neigh_ntry.dip1 = get32msb(buf2, 0);
        vrf2rib_ntry.vrf = neigh_ntry.vrf = atoi(arg[8]);
        vrf2rib_res = vrf2rib_init4;
        str2mac(&neigh_ntry.macs[0], arg[7]);
        str2mac(&neigh_ntry.macs[6], arg[9]);
        neigh_ntry.command = 3;
        neigh_ntry.layer3 = 1;
        tun4_ntry.srcPort = 0;
        tun4_ntry.trgPort = 0;
        tun4_ntry.prot = IP_PROTOCOL_GRE;
        tun4_ntry.command = 1;
        if (del == 0) hasht_del(&neigh_table, &neigh_ntry);
        else hasht_add(&neigh_table, &neigh_ntry);
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun4_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "gre6") == 0) {
        tun6_ntry.neigh = neigh_ntry.id = atoi(arg[2]);
        tun6_ntry.aclport = neigh_ntry.aclport = atoi(arg[3]);
        neigh_ntry.port = atoi(arg[4]);
        inet_pton(AF_INET6, arg[5], buf2);
        tun6_ntry.trgAddr1 = neigh_ntry.sip1 = get32msb(buf2, 0);
        tun6_ntry.trgAddr2 = neigh_ntry.sip2 = get32msb(buf2, 4);
        tun6_ntry.trgAddr3 = neigh_ntry.sip3 = get32msb(buf2, 8);
        tun6_ntry.trgAddr4 = neigh_ntry.sip4 = get32msb(buf2, 12);
        inet_pton(AF_INET6, arg[6], buf2);
        tun6_ntry.srcAddr1 = neigh_ntry.dip1 = get32msb(buf2, 0);
        tun6_ntry.srcAddr2 = neigh_ntry.dip2 = get32msb(buf2, 4);
        tun6_ntry.srcAddr3 = neigh_ntry.dip3 = get32msb(buf2, 8);
        tun6_ntry.srcAddr4 = neigh_ntry.dip4 = get32msb(buf2, 12);
        vrf2rib_ntry.vrf = neigh_ntry.vrf = atoi(arg[8]);
        vrf2rib_res = vrf2rib_init6;
        str2mac(&neigh_ntry.macs[0], arg[7]);
        str2mac(&neigh_ntry.macs[6], arg[9]);
        neigh_ntry.command = 3;
        neigh_ntry.layer3 = 2;
        tun6_ntry.srcPort = 0;
        tun6_ntry.trgPort = 0;
        tun6_ntry.prot = IP_PROTOCOL_GRE;
        tun6_ntry.command = 1;
        if (del == 0) hasht_del(&neigh_table, &neigh_ntry);
        else hasht_add(&neigh_table, &neigh_ntry);
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun6_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "mgre4") == 0) {
        tun4_ntry.neigh = neigh_ntry.id = atoi(arg[2]);
        tun4_ntry.aclport = neigh_ntry.aclport = atoi(arg[3]);
        neigh_ntry.port = atoi(arg[4]);
        inet_pton(AF_INET, arg[5], buf2);
        neigh_ntry.sip1 = get32msb(buf2, 0);
        inet_pton(AF_INET, arg[6], buf2);
        tun4_ntry.trgAddr = neigh_ntry.dip1 = get32msb(buf2, 0);
        inet_pton(AF_INET, arg[7], buf2);
        tun4_ntry.srcAddr = get32msb(buf2, 0);
        vrf2rib_ntry.vrf = neigh_ntry.vrf = atoi(arg[9]);
        vrf2rib_res = vrf2rib_init4;
        str2mac(&neigh_ntry.macs[0], arg[8]);
        str2mac(&neigh_ntry.macs[6], arg[10]);
        neigh_ntry.command = 3;
        neigh_ntry.layer3 = 1;
        tun4_ntry.srcPort = 0;
        tun4_ntry.trgPort = 0;
        tun4_ntry.prot = IP_PROTOCOL_GRE;
        tun4_ntry.command = 1;
        if (del == 0) hasht_del(&neigh_table, &neigh_ntry);
        else hasht_add(&neigh_table, &neigh_ntry);
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun4_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "mgre6") == 0) {
        tun6_ntry.neigh = neigh_ntry.id = atoi(arg[2]);
        tun6_ntry.aclport = neigh_ntry.aclport = atoi(arg[3]);
        neigh_ntry.port = atoi(arg[4]);
        inet_pton(AF_INET6, arg[5], buf2);
        neigh_ntry.sip1 = get32msb(buf2, 0);
        neigh_ntry.sip2 = get32msb(buf2, 4);
        neigh_ntry.sip3 = get32msb(buf2, 8);
        neigh_ntry.sip4 = get32msb(buf2, 12);
        inet_pton(AF_INET6, arg[6], buf2);
        tun6_ntry.trgAddr1 = neigh_ntry.dip1 = get32msb(buf2, 0);
        tun6_ntry.trgAddr2 = neigh_ntry.dip2 = get32msb(buf2, 4);
        tun6_ntry.trgAddr3 = neigh_ntry.dip3 = get32msb(buf2, 8);
        tun6_ntry.trgAddr4 = neigh_ntry.dip4 = get32msb(buf2, 12);
        inet_pton(AF_INET6, arg[7], buf2);
        tun6_ntry.srcAddr1 = get32msb(buf2, 0);
        tun6_ntry.srcAddr2 = get32msb(buf2, 4);
        tun6_ntry.srcAddr3 = get32msb(buf2, 8);
        tun6_ntry.srcAddr4 = get32msb(buf2, 12);
        vrf2rib_ntry.vrf = neigh_ntry.vrf = atoi(arg[9]);
        vrf2rib_res = vrf2rib_init6;
        str2mac(&neigh_ntry.macs[0], arg[8]);
        str2mac(&neigh_ntry.macs[6], arg[10]);
        neigh_ntry.command = 3;
        neigh_ntry.layer3 = 2;
        tun6_ntry.srcPort = 0;
        tun6_ntry.trgPort = 0;
        tun6_ntry.prot = IP_PROTOCOL_GRE;
        tun6_ntry.command = 1;
        if (del == 0) hasht_del(&neigh_table, &neigh_ntry);
        else hasht_add(&neigh_table, &neigh_ntry);
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun6_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "ipip4") == 0) {
        tun4_ntry.neigh = neigh_ntry.id = atoi(arg[2]);
        tun4_ntry.aclport = neigh_ntry.aclport = atoi(arg[3]);
        neigh_ntry.port = atoi(arg[4]);
        inet_pton(AF_INET, arg[5], buf2);
        tun4_ntry.trgAddr = neigh_ntry.sip1 = get32msb(buf2, 0);
        inet_pton(AF_INET, arg[6], buf2);
        tun4_ntry.srcAddr = neigh_ntry.dip1 = get32msb(buf2, 0);
        vrf2rib_ntry.vrf = neigh_ntry.vrf = atoi(arg[8]);
        vrf2rib_res = vrf2rib_init4;
        str2mac(&neigh_ntry.macs[0], arg[7]);
        str2mac(&neigh_ntry.macs[6], arg[9]);
        neigh_ntry.command = 5;
        neigh_ntry.layer3 = 1;
        if (del == 0) hasht_del(&neigh_table, &neigh_ntry);
        else hasht_add(&neigh_table, &neigh_ntry);
        tun4_ntry.srcPort = 0;
        tun4_ntry.trgPort = 0;
        tun4_ntry.command = 4;
        tun4_ntry.prot = IP_PROTOCOL_IPV4;
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun4_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun4_ntry);
        tun4_ntry.prot = IP_PROTOCOL_IPV6;
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun4_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun4_ntry);
        tun4_ntry.prot = IP_PROTOCOL_SRL2;
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun4_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun4_ntry);
        tun4_ntry.prot = IP_PROTOCOL_MPLS;
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun4_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun4_ntry);
        tun4_ntry.prot = IP_PROTOCOL_NSH;
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun4_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun4_ntry);
        tun4_ntry.prot = IP_PROTOCOL_SKIP;
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun4_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun4_ntry);
        tun4_ntry.prot = IP_PROTOCOL_SWIPE;
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun4_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "ipip6") == 0) {
        tun6_ntry.neigh = neigh_ntry.id = atoi(arg[2]);
        tun6_ntry.aclport = neigh_ntry.aclport = atoi(arg[3]);
        neigh_ntry.port = atoi(arg[4]);
        inet_pton(AF_INET6, arg[5], buf2);
        tun6_ntry.trgAddr1 = neigh_ntry.sip1 = get32msb(buf2, 0);
        tun6_ntry.trgAddr2 = neigh_ntry.sip2 = get32msb(buf2, 4);
        tun6_ntry.trgAddr3 = neigh_ntry.sip3 = get32msb(buf2, 8);
        tun6_ntry.trgAddr4 = neigh_ntry.sip4 = get32msb(buf2, 12);
        inet_pton(AF_INET6, arg[6], buf2);
        tun6_ntry.srcAddr1 = neigh_ntry.dip1 = get32msb(buf2, 0);
        tun6_ntry.srcAddr2 = neigh_ntry.dip2 = get32msb(buf2, 4);
        tun6_ntry.srcAddr3 = neigh_ntry.dip3 = get32msb(buf2, 8);
        tun6_ntry.srcAddr4 = neigh_ntry.dip4 = get32msb(buf2, 12);
        vrf2rib_ntry.vrf = neigh_ntry.vrf = atoi(arg[8]);
        vrf2rib_res = vrf2rib_init6;
        str2mac(&neigh_ntry.macs[0], arg[7]);
        str2mac(&neigh_ntry.macs[6], arg[9]);
        neigh_ntry.command = 5;
        neigh_ntry.layer3 = 2;
        if (del == 0) hasht_del(&neigh_table, &neigh_ntry);
        else hasht_add(&neigh_table, &neigh_ntry);
        tun6_ntry.srcPort = 0;
        tun6_ntry.trgPort = 0;
        tun6_ntry.command = 4;
        tun6_ntry.prot = IP_PROTOCOL_IPV4;
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun6_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun6_ntry);
        tun6_ntry.prot = IP_PROTOCOL_IPV6;
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun6_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun6_ntry);
        tun6_ntry.prot = IP_PROTOCOL_SRL2;
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun6_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun6_ntry);
        tun6_ntry.prot = IP_PROTOCOL_MPLS;
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun6_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun6_ntry);
        tun6_ntry.prot = IP_PROTOCOL_NSH;
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun6_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun6_ntry);
        tun6_ntry.prot = IP_PROTOCOL_SKIP;
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun6_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun6_ntry);
        tun6_ntry.prot = IP_PROTOCOL_SWIPE;
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun6_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "tmux4") == 0) {
        tun4_ntry.neigh = neigh_ntry.id = atoi(arg[2]);
        tun4_ntry.aclport = neigh_ntry.aclport = atoi(arg[3]);
        neigh_ntry.port = atoi(arg[4]);
        inet_pton(AF_INET, arg[5], buf2);
        tun4_ntry.trgAddr = neigh_ntry.sip1 = get32msb(buf2, 0);
        inet_pton(AF_INET, arg[6], buf2);
        tun4_ntry.srcAddr = neigh_ntry.dip1 = get32msb(buf2, 0);
        vrf2rib_ntry.vrf = neigh_ntry.vrf = atoi(arg[8]);
        vrf2rib_res = vrf2rib_init4;
        str2mac(&neigh_ntry.macs[0], arg[7]);
        str2mac(&neigh_ntry.macs[6], arg[9]);
        neigh_ntry.command = 21;
        neigh_ntry.tid = atoi(arg[10]);
        tun4_ntry.prot = IP_PROTOCOL_TMUX;
        tun4_ntry.command = 13;
        if (del == 0) hasht_del(&neigh_table, &neigh_ntry);
        else hasht_add(&neigh_table, &neigh_ntry);
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun4_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "tmux6") == 0) {
        tun6_ntry.neigh = neigh_ntry.id = atoi(arg[2]);
        tun6_ntry.aclport = neigh_ntry.aclport = atoi(arg[3]);
        neigh_ntry.port = atoi(arg[4]);
        inet_pton(AF_INET6, arg[5], buf2);
        tun6_ntry.trgAddr1 = neigh_ntry.sip1 = get32msb(buf2, 0);
        tun6_ntry.trgAddr2 = neigh_ntry.sip2 = get32msb(buf2, 4);
        tun6_ntry.trgAddr3 = neigh_ntry.sip3 = get32msb(buf2, 8);
        tun6_ntry.trgAddr4 = neigh_ntry.sip4 = get32msb(buf2, 12);
        inet_pton(AF_INET6, arg[6], buf2);
        tun6_ntry.srcAddr1 = neigh_ntry.dip1 = get32msb(buf2, 0);
        tun6_ntry.srcAddr2 = neigh_ntry.dip2 = get32msb(buf2, 4);
        tun6_ntry.srcAddr3 = neigh_ntry.dip3 = get32msb(buf2, 8);
        tun6_ntry.srcAddr4 = neigh_ntry.dip4 = get32msb(buf2, 12);
        vrf2rib_ntry.vrf = neigh_ntry.vrf = atoi(arg[8]);
        vrf2rib_res = vrf2rib_init6;
        str2mac(&neigh_ntry.macs[0], arg[7]);
        str2mac(&neigh_ntry.macs[6], arg[9]);
        neigh_ntry.command = 22;
        neigh_ntry.tid = atoi(arg[10]);
        tun6_ntry.prot = IP_PROTOCOL_TMUX;
        tun6_ntry.command = 13;
        if (del == 0) hasht_del(&neigh_table, &neigh_ntry);
        else hasht_add(&neigh_table, &neigh_ntry);
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun6_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "l3tp4") == 0) {
        tun4_ntry.neigh = neigh_ntry.id = atoi(arg[2]);
        tun4_ntry.aclport = neigh_ntry.aclport = atoi(arg[3]);
        neigh_ntry.port = atoi(arg[4]);
        inet_pton(AF_INET, arg[5], buf2);
        tun4_ntry.trgAddr = neigh_ntry.sip1 = get32msb(buf2, 0);
        inet_pton(AF_INET, arg[6], buf2);
        tun4_ntry.srcAddr = neigh_ntry.dip1 = get32msb(buf2, 0);
        vrf2rib_ntry.vrf = neigh_ntry.vrf = atoi(arg[8]);
        vrf2rib_res = vrf2rib_init4;
        str2mac(&neigh_ntry.macs[0], arg[7]);
        str2mac(&neigh_ntry.macs[6], arg[9]);
        neigh_ntry.command = 19;
        neigh_ntry.tid = atoi(arg[10]);
        tun4_ntry.prot = IP_PROTOCOL_L2TP;
        tun4_ntry.command = 12;
        if (del == 0) hasht_del(&neigh_table, &neigh_ntry);
        else hasht_add(&neigh_table, &neigh_ntry);
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun4_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "l3tp6") == 0) {
        tun6_ntry.neigh = neigh_ntry.id = atoi(arg[2]);
        tun6_ntry.aclport = neigh_ntry.aclport = atoi(arg[3]);
        neigh_ntry.port = atoi(arg[4]);
        inet_pton(AF_INET6, arg[5], buf2);
        tun6_ntry.trgAddr1 = neigh_ntry.sip1 = get32msb(buf2, 0);
        tun6_ntry.trgAddr2 = neigh_ntry.sip2 = get32msb(buf2, 4);
        tun6_ntry.trgAddr3 = neigh_ntry.sip3 = get32msb(buf2, 8);
        tun6_ntry.trgAddr4 = neigh_ntry.sip4 = get32msb(buf2, 12);
        inet_pton(AF_INET6, arg[6], buf2);
        tun6_ntry.srcAddr1 = neigh_ntry.dip1 = get32msb(buf2, 0);
        tun6_ntry.srcAddr2 = neigh_ntry.dip2 = get32msb(buf2, 4);
        tun6_ntry.srcAddr3 = neigh_ntry.dip3 = get32msb(buf2, 8);
        tun6_ntry.srcAddr4 = neigh_ntry.dip4 = get32msb(buf2, 12);
        vrf2rib_ntry.vrf = neigh_ntry.vrf = atoi(arg[8]);
        vrf2rib_res = vrf2rib_init6;
        str2mac(&neigh_ntry.macs[0], arg[7]);
        str2mac(&neigh_ntry.macs[6], arg[9]);
        neigh_ntry.command = 20;
        neigh_ntry.tid = atoi(arg[10]);
        tun6_ntry.prot = IP_PROTOCOL_L2TP;
        tun6_ntry.command = 12;
        if (del == 0) hasht_del(&neigh_table, &neigh_ntry);
        else hasht_add(&neigh_table, &neigh_ntry);
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun6_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "l2tp4") == 0) {
        tun4_ntry.neigh = neigh_ntry.id = atoi(arg[2]);
        tun4_ntry.aclport = neigh_ntry.aclport = atoi(arg[3]);
        neigh_ntry.port = atoi(arg[4]);
        inet_pton(AF_INET, arg[5], buf2);
        tun4_ntry.trgAddr = neigh_ntry.sip1 = get32msb(buf2, 0);
        inet_pton(AF_INET, arg[6], buf2);
        tun4_ntry.srcAddr = neigh_ntry.dip1 = get32msb(buf2, 0);
        vrf2rib_ntry.vrf = neigh_ntry.vrf = atoi(arg[8]);
        vrf2rib_res = vrf2rib_init4;
        str2mac(&neigh_ntry.macs[0], arg[7]);
        str2mac(&neigh_ntry.macs[6], arg[9]);
        neigh_ntry.command = 4;
        neigh_ntry.layer3 = 3;
        neigh_ntry.sprt = atoi(arg[10]);
        neigh_ntry.dprt = atoi(arg[11]);
        neigh_ntry.tid = atoi(arg[12]);
        tun4_ntry.srcPort = neigh_ntry.dprt;
        tun4_ntry.trgPort = neigh_ntry.sprt;
        tun4_ntry.prot = IP_PROTOCOL_UDP;
        tun4_ntry.command = 2;
        if (del == 0) hasht_del(&neigh_table, &neigh_ntry);
        else hasht_add(&neigh_table, &neigh_ntry);
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun4_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "l2tp6") == 0) {
        tun6_ntry.neigh = neigh_ntry.id = atoi(arg[2]);
        tun6_ntry.aclport = neigh_ntry.aclport = atoi(arg[3]);
        neigh_ntry.port = atoi(arg[4]);
        inet_pton(AF_INET6, arg[5], buf2);
        tun6_ntry.trgAddr1 = neigh_ntry.sip1 = get32msb(buf2, 0);
        tun6_ntry.trgAddr2 = neigh_ntry.sip2 = get32msb(buf2, 4);
        tun6_ntry.trgAddr3 = neigh_ntry.sip3 = get32msb(buf2, 8);
        tun6_ntry.trgAddr4 = neigh_ntry.sip4 = get32msb(buf2, 12);
        inet_pton(AF_INET6, arg[6], buf2);
        tun6_ntry.srcAddr1 = neigh_ntry.dip1 = get32msb(buf2, 0);
        tun6_ntry.srcAddr2 = neigh_ntry.dip2 = get32msb(buf2, 4);
        tun6_ntry.srcAddr3 = neigh_ntry.dip3 = get32msb(buf2, 8);
        tun6_ntry.srcAddr4 = neigh_ntry.dip4 = get32msb(buf2, 12);
        vrf2rib_ntry.vrf = neigh_ntry.vrf = atoi(arg[8]);
        vrf2rib_res = vrf2rib_init6;
        str2mac(&neigh_ntry.macs[0], arg[7]);
        str2mac(&neigh_ntry.macs[6], arg[9]);
        neigh_ntry.command = 4;
        neigh_ntry.layer3 = 4;
        neigh_ntry.sprt = atoi(arg[10]);
        neigh_ntry.dprt = atoi(arg[11]);
        neigh_ntry.tid = atoi(arg[12]);
        tun6_ntry.srcPort = neigh_ntry.dprt;
        tun6_ntry.trgPort = neigh_ntry.sprt;
        tun6_ntry.prot = IP_PROTOCOL_UDP;
        tun6_ntry.command = 2;
        if (del == 0) hasht_del(&neigh_table, &neigh_ntry);
        else hasht_add(&neigh_table, &neigh_ntry);
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun6_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "amt4") == 0) {
        tun4_ntry.neigh = neigh_ntry.id = atoi(arg[2]);
        tun4_ntry.aclport = neigh_ntry.aclport = atoi(arg[3]);
        neigh_ntry.port = atoi(arg[4]);
        inet_pton(AF_INET, arg[5], buf2);
        tun4_ntry.trgAddr = neigh_ntry.sip1 = get32msb(buf2, 0);
        inet_pton(AF_INET, arg[6], buf2);
        tun4_ntry.srcAddr = neigh_ntry.dip1 = get32msb(buf2, 0);
        vrf2rib_ntry.vrf = neigh_ntry.vrf = atoi(arg[8]);
        vrf2rib_res = vrf2rib_init4;
        str2mac(&neigh_ntry.macs[0], arg[7]);
        str2mac(&neigh_ntry.macs[6], arg[9]);
        neigh_ntry.command = 9;
        neigh_ntry.layer3 = 3;
        neigh_ntry.sprt = atoi(arg[10]);
        neigh_ntry.dprt = atoi(arg[11]);
        tun4_ntry.srcPort = neigh_ntry.dprt;
        tun4_ntry.trgPort = neigh_ntry.sprt;
        tun4_ntry.prot = IP_PROTOCOL_UDP;
        tun4_ntry.command = 10;
        if (del == 0) hasht_del(&neigh_table, &neigh_ntry);
        else hasht_add(&neigh_table, &neigh_ntry);
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun4_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "amt6") == 0) {
        tun6_ntry.neigh = neigh_ntry.id = atoi(arg[2]);
        tun6_ntry.aclport = neigh_ntry.aclport = atoi(arg[3]);
        neigh_ntry.port = atoi(arg[4]);
        inet_pton(AF_INET6, arg[5], buf2);
        tun6_ntry.trgAddr1 = neigh_ntry.sip1 = get32msb(buf2, 0);
        tun6_ntry.trgAddr2 = neigh_ntry.sip2 = get32msb(buf2, 4);
        tun6_ntry.trgAddr3 = neigh_ntry.sip3 = get32msb(buf2, 8);
        tun6_ntry.trgAddr4 = neigh_ntry.sip4 = get32msb(buf2, 12);
        inet_pton(AF_INET6, arg[6], buf2);
        tun6_ntry.srcAddr1 = neigh_ntry.dip1 = get32msb(buf2, 0);
        tun6_ntry.srcAddr2 = neigh_ntry.dip2 = get32msb(buf2, 4);
        tun6_ntry.srcAddr3 = neigh_ntry.dip3 = get32msb(buf2, 8);
        tun6_ntry.srcAddr4 = neigh_ntry.dip4 = get32msb(buf2, 12);
        vrf2rib_ntry.vrf = neigh_ntry.vrf = atoi(arg[8]);
        vrf2rib_res = vrf2rib_init6;
        str2mac(&neigh_ntry.macs[0], arg[7]);
        str2mac(&neigh_ntry.macs[6], arg[9]);
        neigh_ntry.command = 9;
        neigh_ntry.layer3 = 4;
        neigh_ntry.sprt = atoi(arg[10]);
        neigh_ntry.dprt = atoi(arg[11]);
        tun6_ntry.srcPort = neigh_ntry.dprt;
        tun6_ntry.trgPort = neigh_ntry.sprt;
        tun6_ntry.prot = IP_PROTOCOL_UDP;
        tun6_ntry.command = 10;
        if (del == 0) hasht_del(&neigh_table, &neigh_ntry);
        else hasht_add(&neigh_table, &neigh_ntry);
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun6_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "gtp4") == 0) {
        tun4_ntry.neigh = neigh_ntry.id = atoi(arg[2]);
        tun4_ntry.aclport = neigh_ntry.aclport = atoi(arg[3]);
        neigh_ntry.port = atoi(arg[4]);
        inet_pton(AF_INET, arg[5], buf2);
        tun4_ntry.trgAddr = neigh_ntry.sip1 = get32msb(buf2, 0);
        inet_pton(AF_INET, arg[6], buf2);
        tun4_ntry.srcAddr = neigh_ntry.dip1 = get32msb(buf2, 0);
        vrf2rib_ntry.vrf = neigh_ntry.vrf = atoi(arg[8]);
        vrf2rib_res = vrf2rib_init4;
        str2mac(&neigh_ntry.macs[0], arg[7]);
        str2mac(&neigh_ntry.macs[6], arg[9]);
        neigh_ntry.command = 17;
        neigh_ntry.sprt = atoi(arg[10]);
        neigh_ntry.dprt = atoi(arg[11]);
        neigh_ntry.tid = atoi(arg[12]);
        tun4_ntry.srcPort = neigh_ntry.dprt;
        tun4_ntry.trgPort = neigh_ntry.sprt;
        tun4_ntry.prot = IP_PROTOCOL_UDP;
        tun4_ntry.command = 11;
        if (del == 0) hasht_del(&neigh_table, &neigh_ntry);
        else hasht_add(&neigh_table, &neigh_ntry);
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun4_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "gtp6") == 0) {
        tun6_ntry.neigh = neigh_ntry.id = atoi(arg[2]);
        tun6_ntry.aclport = neigh_ntry.aclport = atoi(arg[3]);
        neigh_ntry.port = atoi(arg[4]);
        inet_pton(AF_INET6, arg[5], buf2);
        tun6_ntry.trgAddr1 = neigh_ntry.sip1 = get32msb(buf2, 0);
        tun6_ntry.trgAddr2 = neigh_ntry.sip2 = get32msb(buf2, 4);
        tun6_ntry.trgAddr3 = neigh_ntry.sip3 = get32msb(buf2, 8);
        tun6_ntry.trgAddr4 = neigh_ntry.sip4 = get32msb(buf2, 12);
        inet_pton(AF_INET6, arg[6], buf2);
        tun6_ntry.srcAddr1 = neigh_ntry.dip1 = get32msb(buf2, 0);
        tun6_ntry.srcAddr2 = neigh_ntry.dip2 = get32msb(buf2, 4);
        tun6_ntry.srcAddr3 = neigh_ntry.dip3 = get32msb(buf2, 8);
        tun6_ntry.srcAddr4 = neigh_ntry.dip4 = get32msb(buf2, 12);
        vrf2rib_ntry.vrf = neigh_ntry.vrf = atoi(arg[8]);
        vrf2rib_res = vrf2rib_init6;
        str2mac(&neigh_ntry.macs[0], arg[7]);
        str2mac(&neigh_ntry.macs[6], arg[9]);
        neigh_ntry.command = 18;
        neigh_ntry.sprt = atoi(arg[10]);
        neigh_ntry.dprt = atoi(arg[11]);
        neigh_ntry.tid = atoi(arg[12]);
        tun6_ntry.srcPort = neigh_ntry.dprt;
        tun6_ntry.trgPort = neigh_ntry.sprt;
        tun6_ntry.prot = IP_PROTOCOL_UDP;
        tun6_ntry.command = 11;
        if (del == 0) hasht_del(&neigh_table, &neigh_ntry);
        else hasht_add(&neigh_table, &neigh_ntry);
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun6_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "sgttag") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        if (del == 0) port2vrf_res->sgtTag = 0;
        else port2vrf_res->sgtTag = 1;
        return 0;
    }
    if (strcmp(arg[0], "sgtset") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        port2vrf_res->sgtSet = atoi(arg[3]);
        if (del == 0) port2vrf_res->sgtSet = -1;
        return 0;
    }
#ifndef HAVE_NOCRYPTO
    if (strcmp(arg[0], "macsec") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        port2vrf_res->mcscEthtyp = atoi(arg[3]);
        port2vrf_res->mcscSeqTx = 0;
        port2vrf_res->mcscSeqRx = 0;
        port2vrf_res->mcscPackRx = 0;
        port2vrf_res->mcscByteRx = 0;
        port2vrf_res->mcscPackOk = 0;
        port2vrf_res->mcscByteOk = 0;
        port2vrf_res->mcscPackTx = 0;
        port2vrf_res->mcscByteTx = 0;
        port2vrf_res->mcscEncrBlkLen = atoi(arg[4]);
        port2vrf_res->mcscHashBlkLen = atoi(arg[5]);
        port2vrf_res->mcscEncrTagLen = atoi(arg[6]);
        i = atoi(arg[7]);
        port2vrf_res->mcscNeedMacs = i & 1;
        port2vrf_res->mcscNeedAead = (i >> 1) & 1;
        port2vrf_res->mcscEncrAlg = getEncrAlg(arg[8]);
        if (port2vrf_res->mcscEncrAlg == NULL) {
            port2vrf_res->mcscEthtyp = 0;
            return 0;
        }
        port2vrf_res->mcscHashAlg = getHashAlg(arg[9]);
        if (port2vrf_res->mcscHashAlg == NULL) {
            port2vrf_res->mcscEthtyp = 0;
            return 0;
        }
        port2vrf_res->mcscCrRxKeyLen = str2key(arg[10], port2vrf_res->mcscCrRxKeyDat);
        port2vrf_res->mcscCrTxKeyLen = str2key(arg[11], port2vrf_res->mcscCrTxKeyDat);
        port2vrf_res->mcscIvRxKeyLen = str2key(arg[12], port2vrf_res->mcscIvRxKeyDat);
        port2vrf_res->mcscIvTxKeyLen = str2key(arg[13], port2vrf_res->mcscIvTxKeyDat);
        port2vrf_res->mcscDgRxKeyLen = str2key(arg[14], port2vrf_res->mcscDgRxKeyDat);
        port2vrf_res->mcscDgTxKeyLen = str2key(arg[15], port2vrf_res->mcscDgTxKeyDat);
        myHmacSetup(port2vrf_res->mcscHashAlg, port2vrf_res->mcscDgRxKeyDat, &port2vrf_res->mcscDgRxKeyLen);
        myHmacSetup(port2vrf_res->mcscHashAlg, port2vrf_res->mcscDgTxKeyDat, &port2vrf_res->mcscDgTxKeyLen);
        if (del == 0) port2vrf_res->mcscEthtyp = 0;
        return 0;
    }
#endif
#ifndef HAVE_NOCRYPTO
    if (strcmp(arg[0], "ipsec4") == 0) {
        tun4_ntry.neigh = neigh_ntry.id = atoi(arg[2]);
        tun4_ntry.aclport = neigh_ntry.aclport = atoi(arg[3]);
        neigh_ntry.port = atoi(arg[4]);
        inet_pton(AF_INET, arg[5], buf2);
        tun4_ntry.trgAddr = neigh_ntry.sip1 = get32msb(buf2, 0);
        inet_pton(AF_INET, arg[6], buf2);
        tun4_ntry.srcAddr = neigh_ntry.dip1 = get32msb(buf2, 0);
        vrf2rib_ntry.vrf = neigh_ntry.vrf = atoi(arg[8]);
        vrf2rib_res = vrf2rib_init4;
        str2mac(&neigh_ntry.macs[0], arg[7]);
        str2mac(&neigh_ntry.macs[6], arg[9]);
        neigh_ntry.command = 6;
        neigh_ntry.layer3 = 1;
        tun4_ntry.encrBlkLen = neigh_ntry.encrBlkLen = atoi(arg[10]);
        tun4_ntry.encrTagLen = neigh_ntry.encrTagLen = atoi(arg[11]);
        tun4_ntry.hashBlkLen = neigh_ntry.hashBlkLen = atoi(arg[12]);
        tun4_ntry.encrAlg = neigh_ntry.encrAlg = getEncrAlg(arg[13]);
        if (neigh_ntry.encrAlg == NULL) return 0;
        tun4_ntry.hashAlg = neigh_ntry.hashAlg = getHashAlg(arg[14]);
        if (neigh_ntry.hashAlg == NULL) return 0;
        tun4_ntry.encrKeyLen = str2key(arg[16], tun4_ntry.encrKeyDat);
        tun4_ntry.hashKeyLen = str2key(arg[17], tun4_ntry.hashKeyDat);
        myHmacSetup(tun4_ntry.hashAlg, tun4_ntry.hashKeyDat, &tun4_ntry.hashKeyLen);
        neigh_ntry.tid = atoi(arg[18]);
        neigh_ntry.encrKeyLen = str2key(arg[19], neigh_ntry.encrKeyDat);
        neigh_ntry.hashKeyLen = str2key(arg[20], neigh_ntry.hashKeyDat);
        myHmacSetup(neigh_ntry.hashAlg, neigh_ntry.hashKeyDat, &neigh_ntry.hashKeyLen);
        tun4_ntry.prot = IP_PROTOCOL_ESP;
        tun4_ntry.command = 7;
        if (del == 0) hasht_del(&neigh_table, &neigh_ntry);
        else hasht_add(&neigh_table, &neigh_ntry);
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun4_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun4_ntry);
        return 0;
    }
#endif
#ifndef HAVE_NOCRYPTO
    if (strcmp(arg[0], "ipsec6") == 0) {
        tun6_ntry.neigh = neigh_ntry.id = atoi(arg[2]);
        tun6_ntry.aclport = neigh_ntry.aclport = atoi(arg[3]);
        neigh_ntry.port = atoi(arg[4]);
        inet_pton(AF_INET6, arg[5], buf2);
        tun6_ntry.trgAddr1 = neigh_ntry.sip1 = get32msb(buf2, 0);
        tun6_ntry.trgAddr2 = neigh_ntry.sip2 = get32msb(buf2, 4);
        tun6_ntry.trgAddr3 = neigh_ntry.sip3 = get32msb(buf2, 8);
        tun6_ntry.trgAddr4 = neigh_ntry.sip4 = get32msb(buf2, 12);
        inet_pton(AF_INET6, arg[6], buf2);
        tun6_ntry.srcAddr1 = neigh_ntry.dip1 = get32msb(buf2, 0);
        tun6_ntry.srcAddr2 = neigh_ntry.dip2 = get32msb(buf2, 4);
        tun6_ntry.srcAddr3 = neigh_ntry.dip3 = get32msb(buf2, 8);
        tun6_ntry.srcAddr4 = neigh_ntry.dip4 = get32msb(buf2, 12);
        vrf2rib_ntry.vrf = neigh_ntry.vrf = atoi(arg[8]);
        vrf2rib_res = vrf2rib_init6;
        str2mac(&neigh_ntry.macs[0], arg[7]);
        str2mac(&neigh_ntry.macs[6], arg[9]);
        neigh_ntry.command = 6;
        neigh_ntry.layer3 = 2;
        tun6_ntry.encrBlkLen = neigh_ntry.encrBlkLen = atoi(arg[10]);
        tun6_ntry.encrTagLen = neigh_ntry.encrTagLen = atoi(arg[11]);
        tun6_ntry.hashBlkLen = neigh_ntry.hashBlkLen = atoi(arg[12]);
        tun6_ntry.encrAlg = neigh_ntry.encrAlg = getEncrAlg(arg[13]);
        if (neigh_ntry.encrAlg == NULL) return 0;
        tun6_ntry.hashAlg = neigh_ntry.hashAlg = getHashAlg(arg[14]);
        if (neigh_ntry.hashAlg == NULL) return 0;
        tun6_ntry.encrKeyLen = str2key(arg[16], tun6_ntry.encrKeyDat);
        tun6_ntry.hashKeyLen = str2key(arg[17], tun6_ntry.hashKeyDat);
        myHmacSetup(tun6_ntry.hashAlg, tun6_ntry.hashKeyDat, &tun6_ntry.hashKeyLen);
        neigh_ntry.tid = atoi(arg[18]);
        neigh_ntry.encrKeyLen = str2key(arg[19], neigh_ntry.encrKeyDat);
        neigh_ntry.hashKeyLen = str2key(arg[20], neigh_ntry.hashKeyDat);
        myHmacSetup(neigh_ntry.hashAlg, neigh_ntry.hashKeyDat, &neigh_ntry.hashKeyLen);
        tun6_ntry.prot = IP_PROTOCOL_ESP;
        tun6_ntry.command = 7;
        if (del == 0) hasht_del(&neigh_table, &neigh_ntry);
        else hasht_add(&neigh_table, &neigh_ntry);
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun6_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun6_ntry);
        return 0;
    }
#endif
#ifndef HAVE_NOCRYPTO
    if (strcmp(arg[0], "openvpn4") == 0) {
        tun4_ntry.neigh = neigh_ntry.id = atoi(arg[2]);
        tun4_ntry.aclport = neigh_ntry.aclport = atoi(arg[3]);
        neigh_ntry.port = atoi(arg[4]);
        inet_pton(AF_INET, arg[5], buf2);
        tun4_ntry.trgAddr = neigh_ntry.sip1 = get32msb(buf2, 0);
        inet_pton(AF_INET, arg[6], buf2);
        tun4_ntry.srcAddr = neigh_ntry.dip1 = get32msb(buf2, 0);
        vrf2rib_ntry.vrf = neigh_ntry.vrf = atoi(arg[8]);
        vrf2rib_res = vrf2rib_init4;
        str2mac(&neigh_ntry.macs[0], arg[7]);
        str2mac(&neigh_ntry.macs[6], arg[9]);
        neigh_ntry.command = 7;
        neigh_ntry.layer3 = 3;
        tun4_ntry.trgPort = neigh_ntry.sprt = atoi(arg[10]);
        tun4_ntry.srcPort = neigh_ntry.dprt = atoi(arg[11]);
        neigh_ntry.tid = atoi(arg[12]);
        tun4_ntry.encrBlkLen = neigh_ntry.encrBlkLen = atoi(arg[13]);
        tun4_ntry.hashBlkLen = neigh_ntry.hashBlkLen = atoi(arg[14]);
        tun4_ntry.encrAlg = neigh_ntry.encrAlg = getEncrAlg(arg[15]);
        if (neigh_ntry.encrAlg == NULL) return 0;
        tun4_ntry.hashAlg = neigh_ntry.hashAlg = getHashAlg(arg[16]);
        if (neigh_ntry.hashAlg == NULL) return 0;
        tun4_ntry.encrKeyLen = str2key(arg[17], tun4_ntry.encrKeyDat);
        tun4_ntry.hashKeyLen = str2key(arg[18], tun4_ntry.hashKeyDat);
        neigh_ntry.encrKeyLen = str2key(arg[17], neigh_ntry.encrKeyDat);
        neigh_ntry.hashKeyLen = str2key(arg[18], neigh_ntry.hashKeyDat);
        myHmacSetup(tun4_ntry.hashAlg, tun4_ntry.hashKeyDat, &tun4_ntry.hashKeyLen);
        myHmacSetup(neigh_ntry.hashAlg, neigh_ntry.hashKeyDat, &neigh_ntry.hashKeyLen);
        tun4_ntry.prot = IP_PROTOCOL_UDP;
        tun4_ntry.command = 8;
        if (del == 0) hasht_del(&neigh_table, &neigh_ntry);
        else hasht_add(&neigh_table, &neigh_ntry);
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun4_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun4_ntry);
        return 0;
    }
#endif
#ifndef HAVE_NOCRYPTO
    if (strcmp(arg[0], "openvpn6") == 0) {
        tun6_ntry.neigh = neigh_ntry.id = atoi(arg[2]);
        tun6_ntry.aclport = neigh_ntry.aclport = atoi(arg[3]);
        neigh_ntry.port = atoi(arg[4]);
        inet_pton(AF_INET6, arg[5], buf2);
        tun6_ntry.trgAddr1 = neigh_ntry.sip1 = get32msb(buf2, 0);
        tun6_ntry.trgAddr2 = neigh_ntry.sip2 = get32msb(buf2, 4);
        tun6_ntry.trgAddr3 = neigh_ntry.sip3 = get32msb(buf2, 8);
        tun6_ntry.trgAddr4 = neigh_ntry.sip4 = get32msb(buf2, 12);
        inet_pton(AF_INET6, arg[6], buf2);
        tun6_ntry.srcAddr1 = neigh_ntry.dip1 = get32msb(buf2, 0);
        tun6_ntry.srcAddr2 = neigh_ntry.dip2 = get32msb(buf2, 4);
        tun6_ntry.srcAddr3 = neigh_ntry.dip3 = get32msb(buf2, 8);
        tun6_ntry.srcAddr4 = neigh_ntry.dip4 = get32msb(buf2, 12);
        vrf2rib_ntry.vrf = neigh_ntry.vrf = atoi(arg[8]);
        vrf2rib_res = vrf2rib_init6;
        str2mac(&neigh_ntry.macs[0], arg[7]);
        str2mac(&neigh_ntry.macs[6], arg[9]);
        neigh_ntry.command = 7;
        neigh_ntry.layer3 = 4;
        tun6_ntry.trgPort = neigh_ntry.sprt = atoi(arg[10]);
        tun6_ntry.srcPort = neigh_ntry.dprt = atoi(arg[11]);
        neigh_ntry.tid = atoi(arg[12]);
        tun6_ntry.encrBlkLen = neigh_ntry.encrBlkLen = atoi(arg[13]);
        tun6_ntry.hashBlkLen = neigh_ntry.hashBlkLen = atoi(arg[14]);
        tun6_ntry.encrAlg = neigh_ntry.encrAlg = getEncrAlg(arg[15]);
        if (neigh_ntry.encrAlg == NULL) return 0;
        tun6_ntry.hashAlg = neigh_ntry.hashAlg = getHashAlg(arg[16]);
        if (neigh_ntry.hashAlg == NULL) return 0;
        tun6_ntry.encrKeyLen = str2key(arg[17], tun6_ntry.encrKeyDat);
        tun6_ntry.hashKeyLen = str2key(arg[18], tun6_ntry.hashKeyDat);
        neigh_ntry.encrKeyLen = str2key(arg[17], neigh_ntry.encrKeyDat);
        neigh_ntry.hashKeyLen = str2key(arg[18], neigh_ntry.hashKeyDat);
        myHmacSetup(tun6_ntry.hashAlg, tun6_ntry.hashKeyDat, &tun6_ntry.hashKeyLen);
        myHmacSetup(neigh_ntry.hashAlg, neigh_ntry.hashKeyDat, &neigh_ntry.hashKeyLen);
        tun6_ntry.prot = IP_PROTOCOL_UDP;
        tun6_ntry.command = 8;
        if (del == 0) hasht_del(&neigh_table, &neigh_ntry);
        else hasht_add(&neigh_table, &neigh_ntry);
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun6_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun6_ntry);
        return 0;
    }
#endif
#ifndef HAVE_NOCRYPTO
    if (strcmp(arg[0], "wireguard4") == 0) {
        tun4_ntry.neigh = neigh_ntry.id = atoi(arg[2]);
        tun4_ntry.aclport = neigh_ntry.aclport = atoi(arg[3]);
        neigh_ntry.port = atoi(arg[4]);
        inet_pton(AF_INET, arg[5], buf2);
        tun4_ntry.trgAddr = neigh_ntry.sip1 = get32msb(buf2, 0);
        inet_pton(AF_INET, arg[6], buf2);
        tun4_ntry.srcAddr = neigh_ntry.dip1 = get32msb(buf2, 0);
        vrf2rib_ntry.vrf = neigh_ntry.vrf = atoi(arg[8]);
        vrf2rib_res = vrf2rib_init4;
        str2mac(&neigh_ntry.macs[0], arg[7]);
        str2mac(&neigh_ntry.macs[6], arg[9]);
        neigh_ntry.command = 8;
        neigh_ntry.layer3 = 3;
        tun4_ntry.trgPort = neigh_ntry.sprt = atoi(arg[10]);
        tun4_ntry.srcPort = neigh_ntry.dprt = atoi(arg[11]);
        neigh_ntry.tid = atoi(arg[12]);
        neigh_ntry.encrKeyLen = str2key(arg[13], neigh_ntry.encrKeyDat);
        tun4_ntry.encrKeyLen = str2key(arg[14], tun4_ntry.encrKeyDat);
        tun4_ntry.prot = IP_PROTOCOL_UDP;
        tun4_ntry.command = 9;
        if (del == 0) hasht_del(&neigh_table, &neigh_ntry);
        else hasht_add(&neigh_table, &neigh_ntry);
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun4_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun4_ntry);
        return 0;
    }
#endif
#ifndef HAVE_NOCRYPTO
    if (strcmp(arg[0], "wireguard6") == 0) {
        tun6_ntry.neigh = neigh_ntry.id = atoi(arg[2]);
        tun6_ntry.aclport = neigh_ntry.aclport = atoi(arg[3]);
        neigh_ntry.port = atoi(arg[4]);
        inet_pton(AF_INET6, arg[5], buf2);
        tun6_ntry.trgAddr1 = neigh_ntry.sip1 = get32msb(buf2, 0);
        tun6_ntry.trgAddr2 = neigh_ntry.sip2 = get32msb(buf2, 4);
        tun6_ntry.trgAddr3 = neigh_ntry.sip3 = get32msb(buf2, 8);
        tun6_ntry.trgAddr4 = neigh_ntry.sip4 = get32msb(buf2, 12);
        inet_pton(AF_INET6, arg[6], buf2);
        tun6_ntry.srcAddr1 = neigh_ntry.dip1 = get32msb(buf2, 0);
        tun6_ntry.srcAddr2 = neigh_ntry.dip2 = get32msb(buf2, 4);
        tun6_ntry.srcAddr3 = neigh_ntry.dip3 = get32msb(buf2, 8);
        tun6_ntry.srcAddr4 = neigh_ntry.dip4 = get32msb(buf2, 12);
        vrf2rib_ntry.vrf = neigh_ntry.vrf = atoi(arg[8]);
        vrf2rib_res = vrf2rib_init6;
        str2mac(&neigh_ntry.macs[0], arg[7]);
        str2mac(&neigh_ntry.macs[6], arg[9]);
        neigh_ntry.command = 8;
        neigh_ntry.layer3 = 4;
        tun6_ntry.trgPort = neigh_ntry.sprt = atoi(arg[10]);
        tun6_ntry.srcPort = neigh_ntry.dprt = atoi(arg[11]);
        neigh_ntry.tid = atoi(arg[12]);
        neigh_ntry.encrKeyLen = str2key(arg[13], neigh_ntry.encrKeyDat);
        tun6_ntry.encrKeyLen = str2key(arg[14], tun6_ntry.encrKeyDat);
        tun6_ntry.prot = IP_PROTOCOL_UDP;
        tun6_ntry.command = 9;
        if (del == 0) hasht_del(&neigh_table, &neigh_ntry);
        else hasht_add(&neigh_table, &neigh_ntry);
        if (del == 0) hasht_del(&vrf2rib_res->tun, &tun6_ntry);
        else hasht_add(&vrf2rib_res->tun, &tun6_ntry);
        return 0;
    }
#endif
    if (strcmp(arg[0], "monitor") == 0) {
        port2vrf_ntry.port = atoi(arg[2]);
        port2vrf_res = port2vrf_init(&port2vrf_ntry);
        port2vrf_res->monTarget = atoi(arg[3]);
        port2vrf_res->monSample = atoi(arg[6]);
        port2vrf_res->monTruncate = atoi(arg[7]);
        if (del == 0) port2vrf_res->monTarget = -1;
        return 0;
    }
    if (strcmp(arg[0], "mlocal4") == 0) {
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init4;
        inet_pton(AF_INET, arg[4], buf2);
        mroute4_ntry.grp = get32msb(buf2, 0);
        inet_pton(AF_INET, arg[5], buf2);
        mroute4_ntry.src = get32msb(buf2, 0);
        mroute4_res = mcast_init4;
        mroute4_res->ingr = atoi(arg[6]);
        mroute4_res->local = del;
        return 0;
    }
    if (strcmp(arg[0], "mlocal6") == 0) {
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init6;
        inet_pton(AF_INET6, arg[4], buf2);
        mroute6_ntry.grp1 = get32msb(buf2, 0);
        mroute6_ntry.grp2 = get32msb(buf2, 4);
        mroute6_ntry.grp3 = get32msb(buf2, 8);
        mroute6_ntry.grp4 = get32msb(buf2, 12);
        inet_pton(AF_INET6, arg[5], buf2);
        mroute6_ntry.src1 = get32msb(buf2, 0);
        mroute6_ntry.src2 = get32msb(buf2, 4);
        mroute6_ntry.src3 = get32msb(buf2, 8);
        mroute6_ntry.src4 = get32msb(buf2, 12);
        mroute6_res = mcast_init6;
        mroute6_res->ingr = atoi(arg[6]);
        mroute6_res->local = del;
        return 0;
    }
    if (strcmp(arg[0], "mroute4") == 0) {
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init4;
        inet_pton(AF_INET, arg[4], buf2);
        mroute4_ntry.grp = get32msb(buf2, 0);
        inet_pton(AF_INET, arg[5], buf2);
        mroute4_ntry.src = get32msb(buf2, 0);
        mroute4_res = mcast_init4;
        mroute4_res->ingr = atoi(arg[6]);
        flood_ntry.trg = atoi(arg[8]);
        flood_ntry.command = 1;
        str2mac(&flood_ntry.macs[6], arg[9]);
        str2mac(&flood_ntry.macs[0], arg[10]);
        if (del == 0) table_del(&mroute4_res->flood, &flood_ntry);
        else table_add(&mroute4_res->flood, &flood_ntry);
        return 0;
    }
    if (strcmp(arg[0], "mroute6") == 0) {
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init6;
        inet_pton(AF_INET6, arg[4], buf2);
        mroute6_ntry.grp1 = get32msb(buf2, 0);
        mroute6_ntry.grp2 = get32msb(buf2, 4);
        mroute6_ntry.grp3 = get32msb(buf2, 8);
        mroute6_ntry.grp4 = get32msb(buf2, 12);
        inet_pton(AF_INET6, arg[5], buf2);
        mroute6_ntry.src1 = get32msb(buf2, 0);
        mroute6_ntry.src2 = get32msb(buf2, 4);
        mroute6_ntry.src3 = get32msb(buf2, 8);
        mroute6_ntry.src4 = get32msb(buf2, 12);
        mroute6_res = mcast_init6;
        mroute6_res->ingr = atoi(arg[6]);
        flood_ntry.trg = atoi(arg[8]);
        flood_ntry.command = 1;
        str2mac(&flood_ntry.macs[6], arg[9]);
        str2mac(&flood_ntry.macs[0], arg[10]);
        if (del == 0) table_del(&mroute6_res->flood, &flood_ntry);
        else table_add(&mroute6_res->flood, &flood_ntry);
        return 0;
    }
    if (strcmp(arg[0], "duplabloc4") == 0) {
        mpls_ntry.vrf = atoi(arg[2]);
        mpls_ntry.label = atoi(arg[4]);
        mpls_ntry.ver = 4;
        mpls_ntry.command = 7;
        mpls_res = mpls_init;
        mpls_res->command = 7;
        mpls_res->swap = del;
        return 0;
    }
    if (strcmp(arg[0], "duplabloc6") == 0) {
        mpls_ntry.vrf = atoi(arg[2]);
        mpls_ntry.label = atoi(arg[4]);
        mpls_ntry.ver = 6;
        mpls_ntry.command = 7;
        mpls_res = mpls_init;
        mpls_res->command = 7;
        mpls_res->swap = del;
        return 0;
    }
    if (strcmp(arg[0], "duplabel4") == 0) {
        mpls_ntry.vrf = atoi(arg[2]);
        mpls_ntry.label = atoi(arg[4]);
        mpls_ntry.ver = 4;
        mpls_ntry.command = 7;
        mpls_res = mpls_init;
        flood_ntry.trg = atoi(arg[7]);
        flood_ntry.command = 2;
        flood_ntry.lab = atoi(arg[8]);
        if (del == 0) table_del(&mpls_res->flood, &flood_ntry);
        else table_add(&mpls_res->flood, &flood_ntry);
        return 0;
    }
    if (strcmp(arg[0], "duplabel6") == 0) {
        mpls_ntry.vrf = atoi(arg[2]);
        mpls_ntry.label = atoi(arg[4]);
        mpls_ntry.ver = 6;
        mpls_ntry.command = 7;
        mpls_res = mpls_init;
        flood_ntry.trg = atoi(arg[7]);
        flood_ntry.command = 2;
        flood_ntry.lab = atoi(arg[8]);
        if (del == 0) table_del(&mpls_res->flood, &flood_ntry);
        else table_add(&mpls_res->flood, &flood_ntry);
        return 0;
    }
    if (strcmp(arg[0], "mlabroute4") == 0) {
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init4;
        inet_pton(AF_INET, arg[4], buf2);
        mroute4_ntry.grp = get32msb(buf2, 0);
        inet_pton(AF_INET, arg[5], buf2);
        mroute4_ntry.src = get32msb(buf2, 0);
        mroute4_res = mcast_init4;
        mroute4_res->ingr = atoi(arg[6]);
        flood_ntry.trg = atoi(arg[8]);
        flood_ntry.lab = atoi(arg[9]);
        flood_ntry.command = 2;
        if (del == 0) table_del(&mroute4_res->flood, &flood_ntry);
        else table_add(&mroute4_res->flood, &flood_ntry);
        return 0;
    }
    if (strcmp(arg[0], "mlabroute6") == 0) {
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init6;
        inet_pton(AF_INET6, arg[4], buf2);
        mroute6_ntry.grp1 = get32msb(buf2, 0);
        mroute6_ntry.grp2 = get32msb(buf2, 4);
        mroute6_ntry.grp3 = get32msb(buf2, 8);
        mroute6_ntry.grp4 = get32msb(buf2, 12);
        inet_pton(AF_INET6, arg[5], buf2);
        mroute6_ntry.src1 = get32msb(buf2, 0);
        mroute6_ntry.src2 = get32msb(buf2, 4);
        mroute6_ntry.src3 = get32msb(buf2, 8);
        mroute6_ntry.src4 = get32msb(buf2, 12);
        mroute6_res = mcast_init6;
        mroute6_res->ingr = atoi(arg[6]);
        flood_ntry.trg = atoi(arg[8]);
        flood_ntry.lab = atoi(arg[9]);
        flood_ntry.command = 2;
        if (del == 0) table_del(&mroute6_res->flood, &flood_ntry);
        else table_add(&mroute6_res->flood, &flood_ntry);
        return 0;
    }
    if (strcmp(arg[0], "bierlabel4") == 0) {
        mpls_ntry.vrf = atoi(arg[2]);
        mpls_ntry.label = atoi(arg[4]);
        mpls_ntry.ver = 4;
        mpls_ntry.command = 8;
        mpls_res = mpls_init;
        flood_ntry.trg = atoi(arg[7]);
        flood_ntry.command = 3;
        flood_ntry.lab = atoi(arg[8]);
        for (int i=0; i<8; i++) flood_ntry.bier[i] = atoi(arg[9+i]);
        if (del == 0) table_del(&mpls_res->flood, &flood_ntry);
        else table_add(&mpls_res->flood, &flood_ntry);
        return 0;
    }
    if (strcmp(arg[0], "bierlabel6") == 0) {
        mpls_ntry.vrf = atoi(arg[2]);
        mpls_ntry.label = atoi(arg[4]);
        mpls_ntry.ver = 6;
        mpls_ntry.command = 8;
        mpls_res = mpls_init;
        flood_ntry.trg = atoi(arg[7]);
        flood_ntry.command = 3;
        flood_ntry.lab = atoi(arg[8]);
        for (int i=0; i<8; i++) flood_ntry.bier[i] = atoi(arg[9+i]);
        if (del == 0) table_del(&mpls_res->flood, &flood_ntry);
        else table_add(&mpls_res->flood, &flood_ntry);
        return 0;
    }
    if (strcmp(arg[0], "bierlabloc4") == 0) {
        mpls_ntry.vrf = atoi(arg[2]);
        mpls_ntry.label = atoi(arg[4]);
        mpls_ntry.ver = 4;
        mpls_ntry.command = 8;
        mpls_res = mpls_init;
        mpls_res->command = 8;
        for (int i=0; i<8; i++) mpls_res->bier[i] = atoi(arg[5+i]);
        if (del == 0) hasht_del(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "bierlabloc6") == 0) {
        mpls_ntry.vrf = atoi(arg[2]);
        mpls_ntry.label = atoi(arg[4]);
        mpls_ntry.ver = 6;
        mpls_ntry.command = 8;
        mpls_res = mpls_init;
        mpls_res->command = 8;
        for (int i=0; i<8; i++) mpls_res->bier[i] = atoi(arg[5+i]);
        if (del == 0) hasht_del(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "mbierroute4") == 0) {
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init4;
        inet_pton(AF_INET, arg[4], buf2);
        mroute4_ntry.grp = get32msb(buf2, 0);
        inet_pton(AF_INET, arg[5], buf2);
        mroute4_ntry.src = get32msb(buf2, 0);
        mroute4_res = mcast_init4;
        mroute4_res->ingr = atoi(arg[6]);
        flood_ntry.trg = atoi(arg[8]);
        flood_ntry.lab = atoi(arg[9]);
        flood_ntry.command = 4;
        flood_ntry.src = atoi(arg[11]);
        flood_ntry.id =  atoi(arg[12]);
        for (int i=0; i<8; i++) flood_ntry.bier[i] = atoi(arg[13+i]);
        if (del == 0) table_del(&mroute4_res->flood, &flood_ntry);
        else table_add(&mroute4_res->flood, &flood_ntry);
        return 0;
    }
    if (strcmp(arg[0], "mbierroute6") == 0) {
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init6;
        inet_pton(AF_INET6, arg[4], buf2);
        mroute6_ntry.grp1 = get32msb(buf2, 0);
        mroute6_ntry.grp2 = get32msb(buf2, 4);
        mroute6_ntry.grp3 = get32msb(buf2, 8);
        mroute6_ntry.grp4 = get32msb(buf2, 12);
        inet_pton(AF_INET6, arg[5], buf2);
        mroute6_ntry.src1 = get32msb(buf2, 0);
        mroute6_ntry.src2 = get32msb(buf2, 4);
        mroute6_ntry.src3 = get32msb(buf2, 8);
        mroute6_ntry.src4 = get32msb(buf2, 12);
        mroute6_res = mcast_init6;
        mroute6_res->ingr = atoi(arg[6]);
        flood_ntry.trg = atoi(arg[8]);
        flood_ntry.lab = atoi(arg[9]);
        flood_ntry.command = 4;
        flood_ntry.src = atoi(arg[11]);
        flood_ntry.id =  atoi(arg[12]);
        for (int i=0; i<8; i++) flood_ntry.bier[i] = atoi(arg[13+i]);
        if (del == 0) table_del(&mroute6_res->flood, &flood_ntry);
        else table_add(&mroute6_res->flood, &flood_ntry);
        return 0;
    }
    if (strcmp(arg[0], "mbiervpnrou4") == 0) {
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init4;
        inet_pton(AF_INET, arg[4], buf2);
        mroute4_ntry.grp = get32msb(buf2, 0);
        inet_pton(AF_INET, arg[5], buf2);
        mroute4_ntry.src = get32msb(buf2, 0);
        mroute4_res = mcast_init4;
        mroute4_res->ingr = atoi(arg[6]);
        flood_ntry.trg = atoi(arg[8]);
        flood_ntry.lab = atoi(arg[9]);
        flood_ntry.lab2 = atoi(arg[10]);
        flood_ntry.command = 6;
        flood_ntry.src = atoi(arg[12]);
        flood_ntry.id =  atoi(arg[13]);
        for (int i=0; i<8; i++) flood_ntry.bier[i] = atoi(arg[14+i]);
        if (del == 0) table_del(&mroute4_res->flood, &flood_ntry);
        else table_add(&mroute4_res->flood, &flood_ntry);
        return 0;
    }
    if (strcmp(arg[0], "mbiervpnrou6") == 0) {
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init6;
        inet_pton(AF_INET6, arg[4], buf2);
        mroute6_ntry.grp1 = get32msb(buf2, 0);
        mroute6_ntry.grp2 = get32msb(buf2, 4);
        mroute6_ntry.grp3 = get32msb(buf2, 8);
        mroute6_ntry.grp4 = get32msb(buf2, 12);
        inet_pton(AF_INET6, arg[5], buf2);
        mroute6_ntry.src1 = get32msb(buf2, 0);
        mroute6_ntry.src2 = get32msb(buf2, 4);
        mroute6_ntry.src3 = get32msb(buf2, 8);
        mroute6_ntry.src4 = get32msb(buf2, 12);
        mroute6_res = mcast_init6;
        mroute6_res->ingr = atoi(arg[6]);
        flood_ntry.trg = atoi(arg[8]);
        flood_ntry.lab = atoi(arg[9]);
        flood_ntry.lab2 = atoi(arg[10]);
        flood_ntry.command = 6;
        flood_ntry.src = atoi(arg[12]);
        flood_ntry.id =  atoi(arg[13]);
        for (int i=0; i<8; i++) flood_ntry.bier[i] = atoi(arg[14+i]);
        if (del == 0) table_del(&mroute6_res->flood, &flood_ntry);
        else table_add(&mroute6_res->flood, &flood_ntry);
        return 0;
    }
    if (strcmp(arg[0], "mneiroute4") == 0) {
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init4;
        inet_pton(AF_INET, arg[4], buf2);
        mroute4_ntry.grp = get32msb(buf2, 0);
        inet_pton(AF_INET, arg[5], buf2);
        mroute4_ntry.src = get32msb(buf2, 0);
        mroute4_res = mcast_init4;
        mroute4_res->ingr = atoi(arg[6]);
        flood_ntry.trg = atoi(arg[8]);
        flood_ntry.command = 5;
        if (del == 0) table_del(&mroute4_res->flood, &flood_ntry);
        else table_add(&mroute4_res->flood, &flood_ntry);
        return 0;
    }
    if (strcmp(arg[0], "mneiroute6") == 0) {
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init6;
        inet_pton(AF_INET6, arg[4], buf2);
        mroute6_ntry.grp1 = get32msb(buf2, 0);
        mroute6_ntry.grp2 = get32msb(buf2, 4);
        mroute6_ntry.grp3 = get32msb(buf2, 8);
        mroute6_ntry.grp4 = get32msb(buf2, 12);
        inet_pton(AF_INET6, arg[5], buf2);
        mroute6_ntry.src1 = get32msb(buf2, 0);
        mroute6_ntry.src2 = get32msb(buf2, 4);
        mroute6_ntry.src3 = get32msb(buf2, 8);
        mroute6_ntry.src4 = get32msb(buf2, 12);
        mroute6_res = mcast_init6;
        mroute6_res->ingr = atoi(arg[6]);
        flood_ntry.trg = atoi(arg[8]);
        flood_ntry.command = 5;
        if (del == 0) table_del(&mroute6_res->flood, &flood_ntry);
        else table_add(&mroute6_res->flood, &flood_ntry);
        return 0;
    }
    if (strcmp(arg[0], "packout") == 0) {
        int cntr = atoi(arg[2]);
        int bufS = atoi(arg[3]);
        int prt = atoi(arg[4]);
        ctx->stat = ifaceStat[cpuPort];
        ctx->sgt = atoi(arg[6]);
        ctx->hash = atoi(arg[7]);
        unsigned char orig[totBuff];
        unsigned char *bufD = ctx->bufD;
        unsigned char *bufH = ctx->bufH;
        memset(&orig, 0, totBuff);
        str2key(arg[8], orig);
        bufS -= 12;
        for (i=0; i<cntr; i++) {
            memcpy(&bufD[preBuff], &orig[12], bufS);
            memcpy(&bufH[0], &orig[0], 16);
            int ethtyp = get16msb(orig, 12);
            int bufP = preBuff;
            send2subif(ctx, prt, bufP, bufS, ethtyp);
        }
        return 0;
    }
    if (strcmp(arg[0], "neighout") == 0) {
        int cntr = atoi(arg[2]);
        int bufS = atoi(arg[3]);
        int nei = atoi(arg[4]);
        ctx->stat = ifaceStat[cpuPort];
        ctx->sgt = atoi(arg[6]);
        ctx->hash = atoi(arg[7]);
        unsigned char orig[totBuff];
        unsigned char *bufD = ctx->bufD;
        unsigned char *bufH = ctx->bufH;
        memset(&orig, 0, totBuff);
        str2key(arg[8], orig);
        bufS -= 12;
        neigh_ntry.id = nei;
        neigh_res = hasht_find(&neigh_table, &neigh_ntry);
        if (neigh_res == NULL) return 0;
        for (i=0; i<cntr; i++) {
            memcpy(&bufD[preBuff], &orig[12], bufS);
            memcpy(&bufH[0], &orig[0], 16);
            int ethtyp = get16msb(orig, 12);
            int bufP = preBuff;
            send2neigh(ctx, neigh_res, bufP, bufS, ethtyp);
        }
        return 0;
    }
    return 0;
}






void doStatRound_rou4(void* buffer, int fixed) {
    struct route4_entry *ntry = buffer;
    unsigned char buf[128];
    unsigned char buf2[32];
    put32msb(buf2, 0, ntry->addr[0]);
    inet_ntop(AF_INET, &buf2[0], (char*)&buf[0], sizeof(buf));
    fprintf(commandTx, "route4_cnt %i %s %i %li %li %li %li\r\n", fixed, (char*)&buf[0], ntry->mask, ntry->packTx, ntry->byteTx, ntry->packRx, ntry->byteRx);
}

void doStatRound_rou6(void* buffer, int fixed) {
    struct route6_entry *ntry = buffer;
    unsigned char buf[128];
    unsigned char buf2[32];
    put32msb(buf2, 0, ntry->addr[0]);
    put32msb(buf2, 4, ntry->addr[1]);
    put32msb(buf2, 8, ntry->addr[2]);
    put32msb(buf2, 12, ntry->addr[3]);
    inet_ntop(AF_INET6, &buf2[0], (char*)&buf[0], sizeof(buf));
    fprintf(commandTx, "route6_cnt %i %s %i %li %li %li %li\r\n", fixed, (char*)&buf[0], ntry->mask, ntry->packTx, ntry->byteTx, ntry->packRx, ntry->byteRx);
}

void doStatRound_nat4(void* buffer, int fixed) {
    struct nat4_entry *ntry = buffer;
    unsigned char buf[1024];
    unsigned char buf2[1024];
    unsigned char buf3[1024];
    put32msb(buf, 0, ntry->oSrcAddr);
    inet_ntop(AF_INET, &buf[0], (char*)&buf2[0], sizeof(buf2));
    put32msb(buf, 0, ntry->oTrgAddr);
    inet_ntop(AF_INET, &buf[0], (char*)&buf3[0], sizeof(buf3));
    fprintf(commandTx, "nattrns4_cnt %i %i %s %s %i %i %li %li\r\n", fixed, ntry->prot, (char*)&buf2[0], (char*)&buf3[0], ntry->oSrcPort, ntry->oTrgPort, ntry->pack, ntry->byte);
}

void doStatRound_nat6(void* buffer, int fixed) {
    struct nat6_entry *ntry = buffer;
    unsigned char buf[1024];
    unsigned char buf2[1024];
    unsigned char buf3[1024];
    put32msb(buf, 0, ntry->oSrcAddr1);
    put32msb(buf, 4, ntry->oSrcAddr2);
    put32msb(buf, 8, ntry->oSrcAddr3);
    put32msb(buf, 12, ntry->oSrcAddr4);
    inet_ntop(AF_INET6, &buf[0], (char*)&buf2[0], sizeof(buf2));
    put32msb(buf, 0, ntry->oTrgAddr1);
    put32msb(buf, 4, ntry->oTrgAddr2);
    put32msb(buf, 8, ntry->oTrgAddr3);
    put32msb(buf, 12, ntry->oTrgAddr4);
    inet_ntop(AF_INET6, &buf[0], (char*)&buf3[0], sizeof(buf3));
    fprintf(commandTx, "nattrns6_cnt %i %i %s %s %i %i %li %li\r\n", fixed, ntry->prot, (char*)&buf2[0], (char*)&buf3[0], ntry->oSrcPort, ntry->oTrgPort, ntry->pack, ntry->byte);
}

void doStatRound_tun4(void* buffer, int fixed) {
    struct tun4_entry *intry = buffer;
    unsigned char buf[1024];
    unsigned char buf2[1024];
    unsigned char buf3[1024];
    put32msb(buf, 0, intry->srcAddr);
    inet_ntop(AF_INET, &buf[0], (char*)&buf2[0], sizeof(buf2));
    put32msb(buf, 0, intry->trgAddr);
    inet_ntop(AF_INET, &buf[0], (char*)&buf3[0], sizeof(buf3));
    fprintf(commandTx, "tunnel4_cnt %i %i %s %s %i %i %li %li\r\n", fixed, intry->prot, (char*)&buf2[0], (char*)&buf3[0], intry->srcPort, intry->trgPort, intry->pack, intry->byte);
    struct neigh_entry oval;
    oval.id = intry->neigh;
    struct neigh_entry *ontry = hasht_find(&neigh_table, &oval);
    if (ontry == NULL) return;
    fprintf(commandTx, "counter %i %li %li %li %li 0 0\r\n", intry->aclport, intry->pack, intry->byte, ontry->pack, ontry->byte);
}

void doStatRound_tun6(void* buffer, int fixed) {
    struct tun6_entry *intry = buffer;
    unsigned char buf[1024];
    unsigned char buf2[1024];
    unsigned char buf3[1024];
    put32msb(buf, 0, intry->srcAddr1);
    put32msb(buf, 4, intry->srcAddr2);
    put32msb(buf, 8, intry->srcAddr3);
    put32msb(buf, 12, intry->srcAddr4);
    inet_ntop(AF_INET6, &buf[0], (char*)&buf2[0], sizeof(buf2));
    put32msb(buf, 0, intry->trgAddr1);
    put32msb(buf, 4, intry->trgAddr2);
    put32msb(buf, 8, intry->trgAddr3);
    put32msb(buf, 12, intry->trgAddr4);
    inet_ntop(AF_INET6, &buf[0], (char*)&buf3[0], sizeof(buf3));
    fprintf(commandTx, "tunnel6_cnt %i %i %s %s %i %i %li %li\r\n", fixed, intry->prot, (char*)&buf2[0], (char*)&buf3[0], intry->srcPort, intry->trgPort, intry->pack, intry->byte);
    struct neigh_entry oval;
    oval.id = intry->neigh;
    struct neigh_entry *ontry = hasht_find(&neigh_table, &oval);
    if (ontry == NULL) return;
    fprintf(commandTx, "counter %i %li %li %li %li 0 0\r\n", intry->aclport, intry->pack, intry->byte, ontry->pack, ontry->byte);
}

void doStatRound_mcst4(void* buffer, int fixed) {
    struct mroute4_entry *ntry = buffer;
    unsigned char buf[1024];
    unsigned char buf2[1024];
    unsigned char buf3[1024];
    put32msb(buf, 0, ntry->src);
    inet_ntop(AF_INET, &buf[0], (char*)&buf2[0], sizeof(buf2));
    put32msb(buf, 0, ntry->grp);
    inet_ntop(AF_INET, &buf[0], (char*)&buf3[0], sizeof(buf3));
    fprintf(commandTx, "mroute4_cnt %i %s %s %li %li\r\n", fixed, (char*)&buf2[0], (char*)&buf3[0], ntry->pack, ntry->byte);
}

void doStatRound_mcst6(void* buffer, int fixed) {
    struct mroute6_entry *ntry = buffer;
    unsigned char buf[1024];
    unsigned char buf2[1024];
    unsigned char buf3[1024];
    put32msb(buf, 0, ntry->src1);
    put32msb(buf, 4, ntry->src2);
    put32msb(buf, 8, ntry->src3);
    put32msb(buf, 12, ntry->src4);
    inet_ntop(AF_INET6, &buf[0], (char*)&buf2[0], sizeof(buf2));
    put32msb(buf, 0, ntry->grp1);
    put32msb(buf, 4, ntry->grp2);
    put32msb(buf, 8, ntry->grp3);
    put32msb(buf, 12, ntry->grp4);
    inet_ntop(AF_INET6, &buf[0], (char*)&buf3[0], sizeof(buf3));
    fprintf(commandTx, "mroute6_cnt %i %s %s %li %li\r\n", fixed, (char*)&buf2[0], (char*)&buf3[0], ntry->pack, ntry->byte);
}

void doStatRound_polka4(void* buffer, int fixed) {
    struct polkaIdx_entry *ntry = buffer;
    fprintf(commandTx, "polka_cnt %i %i %li %li\r\n", fixed, ntry->index, ntry->pack, ntry->byte);
}

void doStatRound_polka6(void* buffer, int fixed) {
    struct polkaIdx_entry *ntry = buffer;
    fprintf(commandTx, "mpolka_cnt %i %i %li %li\r\n", fixed, ntry->index, ntry->pack, ntry->byte);
}

void doStatRound_acl(char *buf2, int port, struct table_head *acl) {
    for (int i=0; i<acl->size; i++) {
        struct aclH_entry *ntry2 = table_get(acl, i);
        fprintf(commandTx, "%s %i %i %li %li\r\n", (char*)&buf2[0], port, ntry2->pri, ntry2->pack, ntry2->byte);
    }
}

void doStatRound_vrf(void* buffer, int fixed) {
    struct vrf2rib_entry *res = buffer;
    fprintf(commandTx, "vrf%i_cnt %i %li %li\r\n", fixed, res->vrf, res->pack, res->byte);
    if (fixed == 4) {
        tree_walk(&res->rou, &doStatRound_rou4, res->vrf);
        hasht_walk(&res->natT, &doStatRound_nat4, res->vrf);
        hasht_walk(&res->tun, &doStatRound_tun4, res->vrf);
        hasht_walk(&res->mcst, &doStatRound_mcst4, res->vrf);
        table_walk(&res->plk, &doStatRound_polka4, res->vrf);
        doStatRound_acl("natacl4_cnt", res->vrf, &res->natC);
        doStatRound_acl("pbracl4_cnt", res->vrf, &res->pbr);
        doStatRound_acl("coppacl4_cnt", res->vrf, &res->copp);
        doStatRound_acl("flowspec4_cnt", res->vrf, &res->flws);
    } else {
        tree_walk(&res->rou, &doStatRound_rou6, res->vrf);
        hasht_walk(&res->natT, &doStatRound_nat6, res->vrf);
        hasht_walk(&res->tun, &doStatRound_tun6, res->vrf);
        hasht_walk(&res->mcst, &doStatRound_mcst6, res->vrf);
        table_walk(&res->plk, &doStatRound_polka6, res->vrf);
        doStatRound_acl("natacl6_cnt", res->vrf, &res->natC);
        doStatRound_acl("pbracl6_cnt", res->vrf, &res->pbr);
        doStatRound_acl("coppacl6_cnt", res->vrf, &res->copp);
        doStatRound_acl("flowspec6_cnt", res->vrf, &res->flws);
    }
}

void doStatRound_insp4(void* buffer, int fixed) {
    unsigned char buf[1024];
    unsigned char buf2[1024];
    unsigned char buf3[1024];
    struct insp4_entry *ntry2 = buffer;
    put32msb(buf, 0, ntry2->srcAddr);
    inet_ntop(AF_INET, &buf[0], (char*)&buf2[0], sizeof(buf2));
    put32msb(buf, 0, ntry2->trgAddr);
    inet_ntop(AF_INET, &buf[0], (char*)&buf3[0], sizeof(buf3));
    fprintf(commandTx, "inspect4_cnt %i %i %s %s %i %i %li %li %li %li\r\n", fixed, ntry2->prot, (char*)&buf2[0], (char*)&buf3[0], ntry2->srcPort, ntry2->trgPort, ntry2->packRx, ntry2->byteRx, ntry2->packTx, ntry2->byteTx);
}

void doStatRound_insp6(void* buffer, int fixed) {
    unsigned char buf[1024];
    unsigned char buf2[1024];
    unsigned char buf3[1024];
    struct insp6_entry *ntry2 = buffer;
    put32msb(buf, 0, ntry2->srcAddr1);
    put32msb(buf, 4, ntry2->srcAddr2);
    put32msb(buf, 8, ntry2->srcAddr3);
    put32msb(buf, 12, ntry2->srcAddr4);
    inet_ntop(AF_INET6, &buf[0], (char*)&buf2[0], sizeof(buf2));
    put32msb(buf, 0, ntry2->trgAddr1);
    put32msb(buf, 4, ntry2->trgAddr2);
    put32msb(buf, 8, ntry2->trgAddr3);
    put32msb(buf, 12, ntry2->trgAddr4);
    inet_ntop(AF_INET6, &buf[0], (char*)&buf3[0], sizeof(buf3));
    fprintf(commandTx, "inspect6_cnt %i %i %s %s %i %i %li %li %li %li\r\n", fixed, ntry2->prot, (char*)&buf2[0], (char*)&buf3[0], ntry2->srcPort, ntry2->trgPort, ntry2->packRx, ntry2->byteRx, ntry2->packTx, ntry2->byteTx);
}

void doStatRound_neigh(void* buffer, int fixed) {
    struct neigh_entry *ntry = buffer;
    fprintf(commandTx, "neigh_cnt %i %li %li\r\n", ntry->id, ntry->pack, ntry->byte);
}

void doStatRound_vlan(void* buffer, int fixed) {
    struct vlanout_entry *ontry = buffer;
    struct vlanin_entry ival;
    if (ontry->port2 != 0) ival.port = ontry->port2;
    else ival.port = ontry->port;
    ival.vlan = ontry->vlan;
    struct vlanin_entry *intry = hasht_find(&vlanin_table, &ival);
    if (intry == NULL) return;
    fprintf(commandTx, "counter %i %li %li %li %li 0 0\r\n", intry->id, intry->pack, intry->byte, ontry->pack, ontry->byte);
}

void doStatRound_port(void* buffer, int fixed) {
    struct port2vrf_entry *ntry = buffer;
    hasht_walk(&ntry->insp4, &doStatRound_insp4, ntry->port);
    hasht_walk(&ntry->insp6, &doStatRound_insp6, ntry->port);
    doStatRound_acl("inacl4_cnt", ntry->port, &ntry->inacl4);
    doStatRound_acl("inacl6_cnt", ntry->port, &ntry->inacl6);
    doStatRound_acl("outacl4_cnt", ntry->port, &ntry->outacl4);
    doStatRound_acl("outacl6_cnt", ntry->port, &ntry->outacl6);
    doStatRound_acl("inqos4_cnt", ntry->port, &ntry->inqos4);
    doStatRound_acl("inqos6_cnt", ntry->port, &ntry->inqos6);
    doStatRound_acl("outqos4_cnt", ntry->port, &ntry->outqos4);
    doStatRound_acl("outqos6_cnt", ntry->port, &ntry->outqos6);
#ifndef HAVE_NOCRYPTO
    if (ntry->mcscEthtyp == 0) return;
    fprintf(commandTx, "macsec_cnt %i %li %li %li %li %li %li\r\n", ntry->port, ntry->mcscPackRx, ntry->mcscByteRx, ntry->mcscPackTx, ntry->mcscByteTx, (ntry->mcscPackRx - ntry->mcscPackOk), (ntry->mcscByteRx - ntry->mcscByteOk));
#endif
}

void doStatRound_bundle(void* buffer, int fixed) {
    struct bundle_entry *ntry = buffer;
    fprintf(commandTx, "counter %i 0 0 %li %li 0 0\r\n", ntry->id, ntry->pack, ntry->byte);
}

void doStatRound_nsh(void* buffer, int fixed) {
    struct nsh_entry *ntry = buffer;
    fprintf(commandTx, "nsh_cnt %i %i %li %li\r\n", ntry->sp, ntry->si, ntry->pack, ntry->byte);
}

void doStatRound_pppoe(void* buffer, int fixed) {
    struct pppoe_entry *intry = buffer;
    struct neigh_entry oval;
    oval.id = intry->neigh;
    struct neigh_entry *ontry = hasht_find(&neigh_table, &oval);
    if (ontry == NULL) return;
    fprintf(commandTx, "counter %i %li %li %li %li 0 0\r\n", intry->aclport, intry->pack, intry->byte, ontry->pack, ontry->byte);
}

void doStatRound_bridge(void* buffer, int fixed) {
    unsigned char buf[1024];
    unsigned char buf2[1024];
    struct bridge_entry *ntry = buffer;
    put16msb(buf2, 0, ntry->mac1);
    put32msb(buf2, 2, ntry->mac2);
    mac2str(buf2, buf);
    fprintf(commandTx, "bridge_cnt %i %s %li %li %li %li\r\n", ntry->id, (char*)&buf[0], ntry->packRx, ntry->byteRx, ntry->packTx, ntry->byteTx);
}

void doStatRound_mpls(void* buffer, int fixed) {
    struct mpls_entry *ntry = buffer;
    fprintf(commandTx, "mpls_cnt %i %li %li\r\n", ntry->label, ntry->pack, ntry->byte);
}

void doStatRound_policer(void* buffer, int fixed) {
    struct policer_entry *ntry = buffer;
    ntry->avail = ntry->allow;
}




void doConsoleCommand_ipv4(void* buffer, int fixed) {
    struct route4_entry *ntry = buffer;
    unsigned char buf[32];
    unsigned char buf2[128];
    put32msb(buf, 0, ntry->addr[0]);
    inet_ntop(AF_INET, &buf[0], (char*)&buf2[0], sizeof(buf2));
    printf("%16s %3i %10i %3i %10i %10i %10i\n", (char*)&buf2[0], ntry->mask, fixed, ntry->command, ntry->nexthop, ntry->label1, ntry->label2);
}

void doConsoleCommand_ipv6(void* buffer, int fixed) {
    struct route6_entry *ntry = buffer;
    unsigned char buf[32];
    unsigned char buf2[128];
    put32msb(buf, 0, ntry->addr[0]);
    put32msb(buf, 4, ntry->addr[1]);
    put32msb(buf, 8, ntry->addr[2]);
    put32msb(buf, 12, ntry->addr[3]);
    inet_ntop(AF_INET6, &buf[0], (char*)&buf2[0], sizeof(buf2));
    printf("%40s %3i %10i %3i %10i %10i %10i\n", (char*)&buf2[0], ntry->mask, fixed, ntry->command, ntry->nexthop, ntry->label1, ntry->label2);
}

void doConsoleCommand_neigh(void* buffer, int fixed) {
    struct neigh_entry *ntry = buffer;
    unsigned char buf2[1024];
    unsigned char buf3[1024];
    mac2str(&ntry->macs[6], buf2);
    mac2str(&ntry->macs[0], buf3);
    printf("%10i %10i %10i %10i %s %s\n", ntry->id, ntry->vrf, ntry->port, ntry->aclport, (char*)&buf2[0], (char*)&buf3[0]);
}

void doConsoleCommand_port(void* buffer, int fixed) {
    struct port2vrf_entry *ntry = buffer;
    printf("%10i %3i %10i %10i\n", ntry->port, ntry->command, ntry->vrf, ntry->bridge);
}

void doConsoleCommand_vlan(void* buffer, int fixed) {
    struct vlanin_entry *ntry = buffer;
    printf("%10i %10i %10i\n", ntry->id, ntry->vlan, ntry->port);
}

void doConsoleCommand_bridge(void* buffer, int fixed) {
    struct bridge_entry *ntry = buffer;
    unsigned char buf[1024];
    unsigned char buf2[1024];
    put16msb(buf2, 0, ntry->mac1);
    put32msb(buf2, 2, ntry->mac2);
    mac2str(buf2, buf);
    printf("%10i %s %10i %10i\n", ntry->id, (char*)&buf[0], ntry->port, ntry->nexthop);
}

void doConsoleCommand_mpls(void* buffer, int fixed) {
    struct mpls_entry *ntry = buffer;
    printf("%10i %2i %10i %3i %10i %10i\n", ntry->label, ntry->ver, ntry->vrf, ntry->command, ntry->swap, ntry->nexthop);
}

void doConsoleCommand_policer(void* buffer, int fixed) {
    struct policer_entry *ntry = buffer;
    printf("%10i %10i %3i %10li\n", ntry->vrf, ntry->meter, ntry->dir, ntry->allow);
}





void doNegotiate(char*name) {
    setgid(1);
    setuid(1);
    commandRx = fdopen(commandSock, "r");
    if (commandRx == NULL) err("failed to open file");
    commandTx = fdopen(commandSock, "w");
    if (commandTx == NULL) err("failed to open file");
    fprintf(commandTx, "platform p4emu/%s\r\n", name);
    fprintf(commandTx, "capabilities %s%s\r\n",
            "packout punting copp acl nat vlan bundle bridge pppoe hairpin gre l2tp l3tp tmux route mpls vpls evpn eompls gretap pppoetap l2tptap l3tptap tmuxtap ipiptap ipsectap vxlan etherip eoip ipip pckoudp srv6 pbr qos flwspc mroute duplab bier amt nsh racl inspect sgt vrfysrc gtp loconn tcpmss pmtud mpolka polka pwhe mgre",
#ifdef HAVE_NOCRYPTO
            ""
#else
            " macsec ipsec openvpn wireguard"
#endif
           );
    for (int i = 0; i < dataPorts; i++) fprintf(commandTx, "portname %i %s\r\n", i, ifaceName[i]);
    fprintf(commandTx, "cpuport %i\r\n", cpuPort);
    fprintf(commandTx, "dynrange %i 1073741823\r\n", maxPorts);
    fprintf(commandTx, "vrfrange 1 1073741823\r\n");
    fprintf(commandTx, "neirange 4096 1073741823\r\n");
    fprintf(commandTx, "nomore\r\n");
    fflush(commandTx);
}



void doSockLoop() {
    printCmds = getenv("p4emuNOCMDS") == NULL;
    struct packetContext ctx;
    if (initContext(&ctx) != 0) err("error initializing context");
    unsigned char buf[16384];
    for (;;) {
        memset(&buf, 0, sizeof(buf));
        if (fgets((char*)&buf[0], sizeof(buf), commandRx) == NULL) break;
        if (doOneCommand(&ctx, &buf[0]) != 0) break;
    }
    err("command thread exited");
}



void doStatLoop() {
    int round = 0;
    for (;;) {
        round++;
        usleep(100000);
        punts = 10;
        hasht_walk(&policer_table, &doStatRound_policer, 0);
        if (portStatsLen > 0) {
            fprintf(commandTx, "%s", (char*)&portStatsBuf[0]);
            portStatsLen = 0;
            fflush(commandTx);
        }
        if ((round % 10) != 0) continue;
        for (int i = 0; i < dataPorts; i++) {
            struct ifaceStat_entry *stat = ifaceStat[i];
            fprintf(commandTx, "counter %i %li %li %li %li %li %li\r\n", i, stat->packRx, stat->byteRx, stat->packTx, stat->byteTx, stat->packDr, stat->byteDr);
            int o = getState(i);
            fprintf(commandTx, "state %i %i\r\n", i, o);
        }
        hasht_walk(&bundle_table, &doStatRound_bundle, 0);
        hasht_walk(&vlanout_table, &doStatRound_vlan, 0);
        if ((round % 150) != 0) {
            fflush(commandTx);
            continue;
        }
        hasht_walk(&pppoe_table, &doStatRound_pppoe, 0);
        for (int i = 0; i < dataPorts; i++) {
            struct ifaceStat_entry *stat = ifaceStat[i];
            fprintf(commandTx, "ethertype %i %i %li %li\r\n", i, ETHERTYPE_MPLS_UCAST, stat->packMpls, stat->byteMpls);
            fprintf(commandTx, "ethertype %i %i %li %li\r\n", i, ETHERTYPE_VLAN, stat->packVlan, stat->byteVlan);
            fprintf(commandTx, "ethertype %i %i %li %li\r\n", i, ETHERTYPE_IPV4, stat->packIpv4, stat->byteIpv4);
            fprintf(commandTx, "ethertype %i %i %li %li\r\n", i, ETHERTYPE_IPV6, stat->packIpv6, stat->byteIpv6);
            fprintf(commandTx, "ethertype %i %i %li %li\r\n", i, ETHERTYPE_PPPOE_DATA, stat->packPppoe, stat->bytePppoe);
            fprintf(commandTx, "ethertype %i %i %li %li\r\n", i, ETHERTYPE_ROUTEDMAC, stat->packBridge, stat->byteBridge);
            fprintf(commandTx, "ethertype %i %i %li %li\r\n", i, ETHERTYPE_POLKA, stat->packPolka, stat->bytePolka);
            fprintf(commandTx, "ethertype %i %i %li %li\r\n", i, ETHERTYPE_NSH, stat->packNsh, stat->byteNsh);
        }
        hasht_walk(&nsh_table, &doStatRound_nsh, 0);
        hasht_walk(&mpls_table, &doStatRound_mpls, 0);
        hasht_walk(&neigh_table, &doStatRound_neigh, 0);
        hasht_walk(&bridge_table, &doStatRound_bridge, 0);
        hasht_walk(&vrf2rib4_table, &doStatRound_vrf, 4);
        hasht_walk(&vrf2rib6_table, &doStatRound_vrf, 6);
        hasht_walk(&port2vrf_table, &doStatRound_port, 0);
#ifdef HAVE_DEBUG
        for (int i=0; i < sizeof(dropStat)/sizeof(int); i++) {
            if (dropStat[i] == 0) continue;
            fprintf(commandTx, "dataplane-say debugging hit line %i with %i packets\r\n", i, dropStat[i]);
            dropStat[i] = 0;
        }
#endif
        fflush(commandTx);
    }
    err("stat thread exited");
}




void doMainLoop() {
    if (getenv("p4emuNOCONS") != NULL) for (;;) sleep(1);
    unsigned char buf[1024];
    for (;;) {
        printf("> ");
        buf[0] = 0;
        int i = scanf("%1023s", buf);
        if (i < 1) {
            sleep(1);
            continue;
        }
        switch (buf[0]) {
        case 'H':
        case 'h':
        case '?':
            printf("commands:\n");
            printf("h - this help\n");
            printf("x - exit process\n");
            printf("i - interface counters\n");
            printf("p - display portvrf table\n");
            printf("b - display bridge table\n");
            printf("m - display mpls table\n");
            printf("4 - display ipv4 table\n");
            printf("6 - display ipv6 table\n");
            printf("n - display nexthop table\n");
            printf("q - display qos table\n");
            printf("v - display vlan table\n");
            break;
        case 'x':
        case 'X':
            err("exit requested");
            break;
        case 'i':
        case 'I':
            printf("                           iface         rx         tx       drop         rx         tx       drop\n");
            for (int i=0; i<dataPorts; i++) {
                struct ifaceStat_entry *stat = ifaceStat[i];
                printf("%32s %10li %10li %10li %10li %10li %10li\n", ifaceName[i], stat->packRx, stat->packTx, stat->packDr, stat->byteRx, stat->byteTx, stat->byteDr);
            }
            break;
        case 'm':
        case 'M':
            printf("     label ip        vrf cmd       swap    nexthop\n");
            hasht_walk(&mpls_table, &doConsoleCommand_mpls, 0);
            break;
        case 'p':
        case 'P':
            printf("      port cmd        vrf     bridge\n");
            hasht_walk(&port2vrf_table, &doConsoleCommand_port, 0);
            break;
        case 'n':
        case 'N':
            printf("        id        vrf       port    aclport              smac              dmac\n");
            hasht_walk(&neigh_table, &doConsoleCommand_neigh, 0);
            break;
        case 'b':
        case 'B':
            printf("    bridge               mac       port    nexthop\n");
            hasht_walk(&bridge_table, &doConsoleCommand_bridge, 0);
            break;
        case 'q':
        case 'Q':
            printf("       vrf      meter dir       rate\n");
            hasht_walk(&policer_table, &doConsoleCommand_policer, 0);
            break;
        case 'v':
        case 'V':
            printf("        id       vlan       port\n");
            hasht_walk(&vlanin_table, &doConsoleCommand_vlan, 0);
            break;
        case '4':
            printf("            addr msk        vrf cmd    nexthop     label1     label2\n");
            hasht_walk(&vrf2rib4_table, &doConsoleCommand_ipv4, 0);
            break;
        case '6':
            printf("                                    addr msk        vrf cmd    nexthop     label1     label2\n");
            hasht_walk(&vrf2rib6_table, &doConsoleCommand_ipv6, 0);
            break;
        default:
            printf("unknown command '%s', try ?\n", buf);
            break;
        }
    }
    err("main thread exited");
}
