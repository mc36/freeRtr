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
        memmove(&buf, &str[i], 2);
        buf[2] = 0;
        if (str[i] == 0) break;
        sscanf((char*)buf, "%hhx", &key[s]);
        s++;
        i += 2;
    }
    return s;
}


const EVP_CIPHER* getEncrAlg(char *buf) {
    if (strcmp(buf, "none") == 0) return EVP_enc_null();
    if (strcmp(buf, "des") == 0) return EVP_des_cbc();
    if (strcmp(buf, "3des") == 0) return EVP_des_ede3_cbc();
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
    return NULL;
}


long readRate(char**arg) {
    float res = atof(arg[3]) * 100.0 / atof(arg[4]);
    return res;
}


void readAcl4(struct acl4_entry *acl4_ntry, char**arg) {
    unsigned char buf2[1024];
    acl4_ntry->pri = atoi(arg[2]);
    acl4_ntry->act = strcmp(arg[3], "permit");
    if (acl4_ntry->act != 0) acl4_ntry->act = 1;
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
}



void readAcl6(struct acl6_entry *acl6_ntry, char**arg) {
    unsigned char buf2[1024];
    acl6_ntry->pri = atoi(arg[2]);
    acl6_ntry->act = strcmp(arg[3], "permit");
    if (acl6_ntry->act != 0) acl6_ntry->act = 1;
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
}



#define accumulate_sum(sum, val, mul)                           \
    put32msb(buf2, 0, val);                                     \
    sum += mul*get16lsb(buf2, 0);                               \
    sum += mul*get16lsb(buf2, 2);





int doOneCommand(unsigned char* buf) {
#ifndef basicLoop
    unsigned char buf2[1024];
    char* arg[128];
    int cnt;
    cnt = 0;
    arg[0] = (char*)&buf[0];
    int i = 0;
    int o = 0;
    for (;;) {
        switch (buf[i]) {
        case 0:
        case 10:
        case 13:
            o = 1;
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
    printf("rx: ");
    for (int i=0; i < cnt; i++) printf("'%s' ",arg[i]);
    printf("\n");
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
    struct portvrf_entry *portvrf_res;
    struct portvrf_entry portvrf_ntry;
    memset(&portvrf_ntry, 0, sizeof(portvrf_ntry));
    struct vrf2rib_entry *vrf2rib_res;
    struct vrf2rib_entry vrf2rib_ntry;
    memset(&vrf2rib_ntry, 0, sizeof(vrf2rib_ntry));
    struct route4_entry route4_ntry;
    memset(&route4_ntry, 0, sizeof(route4_ntry));
    struct route6_entry route6_ntry;
    memset(&route6_ntry, 0, sizeof(route6_ntry));
    struct neigh_entry neigh_ntry;
    memset(&neigh_ntry, 0, sizeof(neigh_ntry));
    struct vlan_entry vlan_ntry;
    memset(&vlan_ntry, 0, sizeof(vlan_ntry));
    struct bridge_entry bridge_ntry;
    memset(&bridge_ntry, 0, sizeof(bridge_ntry));
    struct acls_entry acls_ntry;
    memset(&acls_ntry, 0, sizeof(acls_ntry));
    struct acls_entry *acls_res;
    struct acl4_entry acl4_ntry;
    memset(&acl4_ntry, 0, sizeof(acl4_ntry));
    struct acl6_entry acl6_ntry;
    memset(&acl6_ntry, 0, sizeof(acl6_ntry));
    struct nat4_entry nat4_ntry;
    memset(&nat4_ntry, 0, sizeof(nat4_ntry));
    struct nat6_entry nat6_ntry;
    memset(&nat6_ntry, 0, sizeof(nat6_ntry));
    struct bundle_entry bundle_ntry;
    memset(&bundle_ntry, 0, sizeof(bundle_ntry));
    struct bundle_entry *bundle_res;
    struct pppoe_entry pppoe_ntry;
    memset(&pppoe_ntry, 0, sizeof(pppoe_ntry));
    struct tun4_entry tun4_ntry;
    memset(&tun4_ntry, 0, sizeof(tun4_ntry));
    struct tun6_entry tun6_ntry;
    memset(&tun6_ntry, 0, sizeof(tun6_ntry));
    struct macsec_entry macsec_ntry;
    memset(&macsec_ntry, 0, sizeof(macsec_ntry));
    struct policer_entry policer_ntry;
    memset(&policer_ntry, 0, sizeof(policer_ntry));
    struct monitor_entry monitor_ntry;
    memset(&monitor_ntry, 0, sizeof(monitor_ntry));
    struct flood_entry flood_ntry;
    memset(&flood_ntry, 0, sizeof(flood_ntry));
    struct mroute4_entry *mroute4_res;
    struct mroute4_entry mroute4_ntry;
    memset(&mroute4_ntry, 0, sizeof(mroute4_ntry));
    struct mroute6_entry *mroute6_res;
    struct mroute6_entry mroute6_ntry;
    memset(&mroute6_ntry, 0, sizeof(mroute6_ntry));
    int index = 0;
    if (strcmp(arg[0], "quit") == 0) {
        return 1;
    }
    if (strcmp(arg[0], "state") == 0) {
        i = atoi(arg[1]);
        o = atoi(arg[2]);
        if (i < 0) return 0;
        if (i >= ports) return 0;
        setState(i, o);
        return 0;
    }
    if (strcmp(arg[0], "mtu") == 0) {
        i = atoi(arg[1]);
        o = atoi(arg[2]);
        if (i < 0) return 0;
        if (i >= ports) return 0;
        setMtu(i, o);
        return 0;
    }
    if (strcmp(arg[0], "mylabel4") == 0) {
        mpls_ntry.label = atoi(arg[2]);
        mpls_ntry.vrf = atoi(arg[3]);
        mpls_ntry.ver = 4;
        mpls_ntry.command = 1;
        if (del == 0) table_del(&mpls_table, &mpls_ntry);
        else table_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "mylabel6") == 0) {
        mpls_ntry.label = atoi(arg[2]);
        mpls_ntry.vrf = atoi(arg[3]);
        mpls_ntry.ver = 6;
        mpls_ntry.command = 1;
        if (del == 0) table_del(&mpls_table, &mpls_ntry);
        else table_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "unlabel4") == 0) {
        mpls_ntry.label = atoi(arg[2]);
        mpls_ntry.nexthop = atoi(arg[3]);
        mpls_ntry.ver = 4;
        mpls_ntry.command = 2;
        if (del == 0) table_del(&mpls_table, &mpls_ntry);
        else table_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "unlabel6") == 0) {
        mpls_ntry.label = atoi(arg[2]);
        mpls_ntry.nexthop = atoi(arg[3]);
        mpls_ntry.ver = 6;
        mpls_ntry.command = 2;
        if (del == 0) table_del(&mpls_table, &mpls_ntry);
        else table_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "label4") == 0) {
        mpls_ntry.label = atoi(arg[2]);
        mpls_ntry.nexthop = atoi(arg[3]);
        mpls_ntry.swap = atoi(arg[5]);
        mpls_ntry.ver = 4;
        mpls_ntry.command = 3;
        if (del == 0) table_del(&mpls_table, &mpls_ntry);
        else table_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "label6") == 0) {
        mpls_ntry.label = atoi(arg[2]);
        mpls_ntry.nexthop = atoi(arg[3]);
        mpls_ntry.swap = atoi(arg[5]);
        mpls_ntry.ver = 6;
        mpls_ntry.command = 3;
        if (del == 0) table_del(&mpls_table, &mpls_ntry);
        else table_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "cpulabel") == 0) {
        mpls_ntry.label = atoi(arg[2]);
        mpls_ntry.command = 6;
        if (del == 0) table_del(&mpls_table, &mpls_ntry);
        else table_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "polkaidx") == 0) {
        polkaIdx_ntry.index = atoi(arg[2]);
        polkaIdx_ntry.vrf = atoi(arg[3]);
        polkaIdx_ntry.nexthop = atoi(arg[4]);
        if (del == 0) table_del(&polkaIdx_table, &polkaIdx_ntry);
        else table_add(&polkaIdx_table, &polkaIdx_ntry);
        return 0;
    }
    if (strcmp(arg[0], "polkapoly") == 0) {
        polkaPoly_ntry.port = atoi(arg[2]);
        int p = atoi(arg[3]);
        for (int o=0; o < 256; o++) {
            int v = o << 8;
            for (int i = 0; i < 8; ++i) {
                v <<= 1;
                if ((v & 0x10000) != 0) {
                    v ^= p;
                }
            }
            polkaPoly_ntry.tab[o] = v & 0xffff;
        }
        if (del == 0) table_del(&polkaPoly_table, &polkaPoly_ntry);
        else table_add(&polkaPoly_table, &polkaPoly_ntry);
        return 0;
    }
    if (strcmp(arg[0], "nshfwd") == 0) {
        nsh_ntry.sp = atoi(arg[2]);
        nsh_ntry.si = atoi(arg[3]);
        nsh_ntry.command = 1;
        nsh_ntry.port = atoi(arg[4]);
        str2mac(nsh_ntry.smac, arg[5]);
        str2mac(nsh_ntry.dmac, arg[6]);
        nsh_ntry.trg = (atoi(arg[7]) << 8) | atoi(arg[8]);
        if (del == 0) table_del(&nsh_table, &nsh_ntry);
        else table_add(&nsh_table, &nsh_ntry);
        return 0;
    }
    if (strcmp(arg[0], "nshloc") == 0) {
        nsh_ntry.sp = atoi(arg[2]);
        nsh_ntry.si = atoi(arg[3]);
        nsh_ntry.command = 2;
        nsh_ntry.vrf = atoi(arg[4]);
        if (del == 0) table_del(&nsh_table, &nsh_ntry);
        else table_add(&nsh_table, &nsh_ntry);
        return 0;
    }
    if (strcmp(arg[0], "portvrf") == 0) {
        portvrf_ntry.command = 1;
        portvrf_ntry.port = atoi(arg[2]);
        portvrf_ntry.vrf = atoi(arg[3]);
        if (del == 0) table_del(&portvrf_table, &portvrf_ntry);
        else table_add(&portvrf_table, &portvrf_ntry);
        return 0;
    }
    if (strcmp(arg[0], "tcpmss4in") == 0) {
        portvrf_ntry.port = atoi(arg[2]);
        index = table_find(&portvrf_table, &portvrf_ntry);
        if (index < 0) return 0;
        portvrf_res = table_get(&portvrf_table, index);
        portvrf_res->tcpmss4 = atoi(arg[3]);
        return 0;
    }
    if (strcmp(arg[0], "tcpmss6in") == 0) {
        portvrf_ntry.port = atoi(arg[2]);
        index = table_find(&portvrf_table, &portvrf_ntry);
        if (index < 0) return 0;
        portvrf_res = table_get(&portvrf_table, index);
        portvrf_res->tcpmss6 = atoi(arg[3]);
        return 0;
    }
    if (strcmp(arg[0], "mplspack") == 0) {
        portvrf_ntry.port = atoi(arg[2]);
        index = table_find(&portvrf_table, &portvrf_ntry);
        if (index < 0) return 0;
        portvrf_res = table_get(&portvrf_table, index);
        portvrf_res->mpls = atoi(arg[3]);
        return 0;
    }
    if (strcmp(arg[0], "nshpack") == 0) {
        portvrf_ntry.port = atoi(arg[2]);
        index = table_find(&portvrf_table, &portvrf_ntry);
        if (index < 0) return 0;
        portvrf_res = table_get(&portvrf_table, index);
        portvrf_res->nsh = atoi(arg[3]);
        return 0;
    }
    if (strcmp(arg[0], "xconnect") == 0) {
        portvrf_ntry.command = 3;
        portvrf_ntry.port = atoi(arg[2]);
        portvrf_ntry.nexthop = atoi(arg[4]);
        portvrf_ntry.label1 = atoi(arg[5]);
        portvrf_ntry.label2 = atoi(arg[7]);
        mpls_ntry.label = atoi(arg[6]);
        mpls_ntry.port = portvrf_ntry.port;
        mpls_ntry.command = 4;
        if (del == 0) table_del(&portvrf_table, &portvrf_ntry);
        else table_add(&portvrf_table, &portvrf_ntry);
        if (del == 0) table_del(&mpls_table, &mpls_ntry);
        else table_add(&mpls_table, &mpls_ntry);
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
        if (del == 0) table_del(&bridge_table, &bridge_ntry);
        else table_add(&bridge_table, &bridge_ntry);
        return 0;
    }
    if (strcmp(arg[0], "bridgelabel") == 0) {
        mpls_ntry.label = atoi(arg[3]);
        mpls_ntry.bridge = atoi(arg[2]);
        mpls_ntry.command = 5;
        if (del == 0) table_del(&mpls_table, &mpls_ntry);
        else table_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "portbridge") == 0) {
        portvrf_ntry.command = 2;
        portvrf_ntry.port = atoi(arg[2]);
        portvrf_ntry.bridge = atoi(arg[3]);
        if (del == 0) table_del(&portvrf_table, &portvrf_ntry);
        else table_add(&portvrf_table, &portvrf_ntry);
        return 0;
    }
    if (strcmp(arg[0], "bridgemac") == 0) {
        bridge_ntry.id = atoi(arg[2]);
        str2mac(buf2, arg[3]);
        bridge_ntry.mac1 = get16msb(buf2, 0);
        bridge_ntry.mac2 = get32msb(buf2, 2);
        bridge_ntry.port = atoi(arg[4]);
        bridge_ntry.command = 1;
        if (del == 0) table_del(&bridge_table, &bridge_ntry);
        else table_add(&bridge_table, &bridge_ntry);
        return 0;
    }
    if (strcmp(arg[0], "routedmac") == 0) {
        bridge_ntry.id = atoi(arg[2]);
        str2mac(buf2, arg[3]);
        bridge_ntry.mac1 = get16msb(buf2, 0);
        bridge_ntry.mac2 = get32msb(buf2, 2);
        bridge_ntry.nexthop = atoi(arg[4]);
        bridge_ntry.command = 3;
        if (del == 0) table_del(&bridge_table, &bridge_ntry);
        else table_add(&bridge_table, &bridge_ntry);
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
        tun4_ntry.srcPort = 4789;
        tun4_ntry.trgPort = 4789;
        tun4_ntry.prot = 17;
        tun4_ntry.command = 3;
        if (del == 0) table_del(&bridge_table, &bridge_ntry);
        else table_add(&bridge_table, &bridge_ntry);
        if (del == 0) table_del(&vrf2rib_res->tun, &tun4_ntry);
        else table_add(&vrf2rib_res->tun, &tun4_ntry);
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
        tun6_ntry.srcPort = 4789;
        tun6_ntry.trgPort = 4789;
        tun6_ntry.prot = 17;
        tun6_ntry.command = 3;
        if (del == 0) table_del(&bridge_table, &bridge_ntry);
        else table_add(&bridge_table, &bridge_ntry);
        if (del == 0) table_del(&vrf2rib_res->tun, &tun6_ntry);
        else table_add(&vrf2rib_res->tun, &tun6_ntry);
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
        tun4_ntry.prot = 17;
        tun4_ntry.command = 7;
        if (del == 0) table_del(&bridge_table, &bridge_ntry);
        else table_add(&bridge_table, &bridge_ntry);
        if (del == 0) table_del(&vrf2rib_res->tun, &tun4_ntry);
        else table_add(&vrf2rib_res->tun, &tun4_ntry);
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
        tun6_ntry.prot = 17;
        tun6_ntry.command = 7;
        if (del == 0) table_del(&bridge_table, &bridge_ntry);
        else table_add(&bridge_table, &bridge_ntry);
        if (del == 0) table_del(&vrf2rib_res->tun, &tun6_ntry);
        else table_add(&vrf2rib_res->tun, &tun6_ntry);
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
        if (del == 0) table_del(&bridge_table, &bridge_ntry);
        else table_add(&bridge_table, &bridge_ntry);
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
        if (del == 0) table_del(&bridge_table, &bridge_ntry);
        else table_add(&bridge_table, &bridge_ntry);
        return 0;
    }
    if (strcmp(arg[0], "portvlan") == 0) {
        vlan_ntry.id = atoi(arg[2]);
        vlan_ntry.port = atoi(arg[3]);
        vlan_ntry.vlan = atoi(arg[4]);
        if (del == 0) table_del(&vlanin_table, &vlan_ntry);
        else table_add(&vlanin_table, &vlan_ntry);
        if (del == 0) table_del(&vlanout_table, &vlan_ntry);
        else table_add(&vlanout_table, &vlan_ntry);
        return 0;
    }
    if (strcmp(arg[0], "myaddr4") == 0) {
        inet_pton(AF_INET, arg[2], buf2);
        vrf2rib_ntry.vrf = atoi(arg[5]);
        vrf2rib_res = vrf2rib_init4;
        route4_ntry.addr = get32msb(buf2, 0);
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
        route4_ntry.addr = get32msb(buf2, 0);
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
        route4_ntry.addr = get32msb(buf2, 0);
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
        route4_ntry.addr = get32msb(buf2, 0);
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
        route4_ntry.addr = get32msb(buf2, 0);
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
        route4_ntry.addr = get32msb(buf2, 0);
        route4_ntry.mask = atoi(arg[3]);
        route4_ntry.nexthop = atoi(arg[4]);
        str2key(arg[7], route4_ntry.polka);
        route4_ntry.command = 9;
        if (del == 0) tree_del(&vrf2rib_res->rou, &route4_ntry);
        else tree_add(&vrf2rib_res->rou, &route4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "neigh4") == 0) {
        route4_ntry.nexthop = atoi(arg[2]);
        inet_pton(AF_INET, arg[3], buf2);
        vrf2rib_ntry.vrf = atoi(arg[5]);
        vrf2rib_res = vrf2rib_init4;
        route4_ntry.addr = get32msb(buf2, 0);
        route4_ntry.mask = 32;
        route4_ntry.command = 1;
        neigh_ntry.id = route4_ntry.nexthop;
        neigh_ntry.vrf = vrf2rib_ntry.vrf;
        neigh_ntry.aclport = neigh_ntry.port = atoi(arg[7]);
        neigh_ntry.command = 1;
        str2mac(neigh_ntry.dmac, arg[4]);
        str2mac(neigh_ntry.smac, arg[6]);
        if (del == 0) tree_del(&vrf2rib_res->rou, &route4_ntry);
        else tree_add(&vrf2rib_res->rou, &route4_ntry);
        if (del == 0) table_del(&neigh_table, &neigh_ntry);
        else table_add(&neigh_table, &neigh_ntry);
        return 0;
    }
    if (strcmp(arg[0], "myaddr6") == 0) {
        inet_pton(AF_INET6, arg[2], buf);
        vrf2rib_ntry.vrf = atoi(arg[5]);
        vrf2rib_res = vrf2rib_init6;
        route6_ntry.addr1 = get32msb(buf, 0);
        route6_ntry.addr2 = get32msb(buf, 4);
        route6_ntry.addr3 = get32msb(buf, 8);
        route6_ntry.addr4 = get32msb(buf, 12);
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
        route6_ntry.addr1 = get32msb(buf2, 0);
        route6_ntry.addr2 = get32msb(buf2, 4);
        route6_ntry.addr3 = get32msb(buf2, 8);
        route6_ntry.addr4 = get32msb(buf2, 12);
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
        route6_ntry.addr1 = get32msb(buf2, 0);
        route6_ntry.addr2 = get32msb(buf2, 4);
        route6_ntry.addr3 = get32msb(buf2, 8);
        route6_ntry.addr4 = get32msb(buf2, 12);
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
        route6_ntry.addr1 = get32msb(buf2, 0);
        route6_ntry.addr2 = get32msb(buf2, 4);
        route6_ntry.addr3 = get32msb(buf2, 8);
        route6_ntry.addr4 = get32msb(buf2, 12);
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
        route6_ntry.addr1 = get32msb(buf2, 0);
        route6_ntry.addr2 = get32msb(buf2, 4);
        route6_ntry.addr3 = get32msb(buf2, 8);
        route6_ntry.addr4 = get32msb(buf2, 12);
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
        route6_ntry.addr1 = get32msb(buf2, 0);
        route6_ntry.addr2 = get32msb(buf2, 4);
        route6_ntry.addr3 = get32msb(buf2, 8);
        route6_ntry.addr4 = get32msb(buf2, 12);
        route6_ntry.mask = atoi(arg[3]);
        route6_ntry.nexthop = atoi(arg[4]);
        str2key(arg[7], route6_ntry.polka);
        route6_ntry.command = 9;
        if (del == 0) tree_del(&vrf2rib_res->rou, &route6_ntry);
        else tree_add(&vrf2rib_res->rou, &route6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "neigh6") == 0) {
        route6_ntry.nexthop = atoi(arg[2]);
        inet_pton(AF_INET6, arg[3], buf2);
        vrf2rib_ntry.vrf = atoi(arg[5]);
        vrf2rib_res = vrf2rib_init6;
        route6_ntry.addr1 = get32msb(buf2, 0);
        route6_ntry.addr2 = get32msb(buf2, 4);
        route6_ntry.addr3 = get32msb(buf2, 8);
        route6_ntry.addr4 = get32msb(buf2, 12);
        route6_ntry.mask = 128;
        route6_ntry.command = 1;
        neigh_ntry.id = route6_ntry.nexthop;
        neigh_ntry.vrf = vrf2rib_ntry.vrf;
        neigh_ntry.aclport = neigh_ntry.port = atoi(arg[7]);
        neigh_ntry.command = 1;
        str2mac(neigh_ntry.dmac, arg[4]);
        str2mac(neigh_ntry.smac, arg[6]);
        if (del == 0) tree_del(&vrf2rib_res->rou, &route6_ntry);
        else tree_add(&vrf2rib_res->rou, &route6_ntry);
        if (del == 0) table_del(&neigh_table, &neigh_ntry);
        else table_add(&neigh_table, &neigh_ntry);
        return 0;
    }
    if (strcmp(arg[0], "mysrv4") == 0) {
        inet_pton(AF_INET6, arg[3], buf2);
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init6;
        route6_ntry.addr1 = get32msb(buf2, 0);
        route6_ntry.addr2 = get32msb(buf2, 4);
        route6_ntry.addr3 = get32msb(buf2, 8);
        route6_ntry.addr4 = get32msb(buf2, 12);
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
        route6_ntry.addr1 = get32msb(buf2, 0);
        route6_ntry.addr2 = get32msb(buf2, 4);
        route6_ntry.addr3 = get32msb(buf2, 8);
        route6_ntry.addr4 = get32msb(buf2, 12);
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
        route6_ntry.addr1 = get32msb(buf2, 0);
        route6_ntry.addr2 = get32msb(buf2, 4);
        route6_ntry.addr3 = get32msb(buf2, 8);
        route6_ntry.addr4 = get32msb(buf2, 12);
        route6_ntry.mask = 128;
        route6_ntry.srv1 = atoi(arg[2]);
        route6_ntry.command = 8;
        if (del == 0) tree_del(&vrf2rib_res->rou, &route6_ntry);
        else tree_add(&vrf2rib_res->rou, &route6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "inacl4") == 0) {
        acls_ntry.dir = 1;
        acls_ntry.port = atoi(arg[2]);
        acls_res = table_addinited(&acls4_table, &acls_ntry, &acls_ntry.aces, sizeof(struct acl4_entry), &acl4_compare);
        readAcl4(&acl4_ntry, &arg[1]);
        if (del == 0) table_del(&acls_res->aces, &acl4_ntry);
        else table_add(&acls_res->aces, &acl4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "outacl4") == 0) {
        acls_ntry.dir = 2;
        acls_ntry.port = atoi(arg[2]);
        acls_res = table_addinited(&acls4_table, &acls_ntry, &acls_ntry.aces, sizeof(struct acl4_entry), &acl4_compare);
        readAcl4(&acl4_ntry, &arg[1]);
        if (del == 0) table_del(&acls_res->aces, &acl4_ntry);
        else table_add(&acls_res->aces, &acl4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "inqos4") == 0) {
        acls_ntry.dir = 6;
        acls_ntry.port = atoi(arg[2]);
        acls_ntry.hop = atoi(arg[3]);
        acls_res = table_addinited(&acls4_table, &acls_ntry, &acls_ntry.aces, sizeof(struct acl4_entry), &acl4_compare);
        readAcl4(&acl4_ntry, &arg[2]);
        if (del == 0) table_del(&acls_res->aces, &acl4_ntry);
        else table_add(&acls_res->aces, &acl4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "outqos4") == 0) {
        acls_ntry.dir = 7;
        acls_ntry.port = atoi(arg[2]);
        acls_ntry.hop = atoi(arg[3]);
        acls_res = table_addinited(&acls4_table, &acls_ntry, &acls_ntry.aces, sizeof(struct acl4_entry), &acl4_compare);
        readAcl4(&acl4_ntry, &arg[2]);
        if (del == 0) table_del(&acls_res->aces, &acl4_ntry);
        else table_add(&acls_res->aces, &acl4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "natcfg4") == 0) {
        acls_ntry.dir = 3;
        acls_ntry.port = atoi(arg[2]);
        acls_res = table_addinited(&acls4_table, &acls_ntry, &acls_ntry.aces, sizeof(struct acl4_entry), &acl4_compare);
        readAcl4(&acl4_ntry, &arg[1]);
        if (del == 0) table_del(&acls_res->aces, &acl4_ntry);
        else table_add(&acls_res->aces, &acl4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "copp4") == 0) {
        acls_ntry.dir = 4;
        acls_ntry.port = 0;
        acls_res = table_addinited(&acls4_table, &acls_ntry, &acls_ntry.aces, sizeof(struct acl4_entry), &acl4_compare);
        readAcl4(&acl4_ntry, &arg[0]);
        if (del == 0) table_del(&acls_res->aces, &acl4_ntry);
        else table_add(&acls_res->aces, &acl4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "inacl6") == 0) {
        acls_ntry.dir = 1;
        acls_ntry.port = atoi(arg[2]);
        acls_res = table_addinited(&acls6_table, &acls_ntry, &acls_ntry.aces, sizeof(struct acl6_entry), &acl6_compare);
        readAcl6(&acl6_ntry, &arg[1]);
        if (del == 0) table_del(&acls_res->aces, &acl6_ntry);
        else table_add(&acls_res->aces, &acl6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "outacl6") == 0) {
        acls_ntry.dir = 2;
        acls_ntry.port = atoi(arg[2]);
        acls_res = table_addinited(&acls6_table, &acls_ntry, &acls_ntry.aces, sizeof(struct acl6_entry), &acl6_compare);
        readAcl6(&acl6_ntry, &arg[1]);
        if (del == 0) table_del(&acls_res->aces, &acl6_ntry);
        else table_add(&acls_res->aces, &acl6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "inqos6") == 0) {
        acls_ntry.dir = 6;
        acls_ntry.port = atoi(arg[2]);
        acls_ntry.hop = atoi(arg[3]);
        acls_res = table_addinited(&acls6_table, &acls_ntry, &acls_ntry.aces, sizeof(struct acl6_entry), &acl6_compare);
        readAcl6(&acl6_ntry, &arg[2]);
        if (del == 0) table_del(&acls_res->aces, &acl6_ntry);
        else table_add(&acls_res->aces, &acl6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "outqos6") == 0) {
        acls_ntry.dir = 7;
        acls_ntry.port = atoi(arg[2]);
        acls_ntry.hop = atoi(arg[3]);
        acls_res = table_addinited(&acls6_table, &acls_ntry, &acls_ntry.aces, sizeof(struct acl6_entry), &acl6_compare);
        readAcl6(&acl6_ntry, &arg[2]);
        if (del == 0) table_del(&acls_res->aces, &acl6_ntry);
        else table_add(&acls_res->aces, &acl6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "natcfg6") == 0) {
        acls_ntry.dir = 3;
        acls_ntry.port = atoi(arg[2]);
        acls_res = table_addinited(&acls6_table, &acls_ntry, &acls_ntry.aces, sizeof(struct acl6_entry), &acl6_compare);
        readAcl6(&acl6_ntry, &arg[1]);
        if (del == 0) table_del(&acls_res->aces, &acl6_ntry);
        else table_add(&acls_res->aces, &acl6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "copp6") == 0) {
        acls_ntry.dir = 4;
        acls_ntry.port = 0;
        acls_res = table_addinited(&acls6_table, &acls_ntry, &acls_ntry.aces, sizeof(struct acl6_entry), &acl6_compare);
        readAcl6(&acl6_ntry, &arg[0]);
        if (del == 0) table_del(&acls_res->aces, &acl6_ntry);
        else table_add(&acls_res->aces, &acl6_ntry);
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
        if (del == 0) table_del(&vrf2rib_res->nat, &nat4_ntry);
        else table_add(&vrf2rib_res->nat, &nat4_ntry);
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
        if (del == 0) table_del(&vrf2rib_res->nat, &nat6_ntry);
        else table_add(&vrf2rib_res->nat, &nat6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "inqos") == 0) {
        policer_ntry.meter = atoi(arg[2]);
        policer_ntry.dir = 1;
        policer_ntry.allow = readRate(&arg[0]);
        if (del == 0) table_del(&policer_table, &policer_ntry);
        else table_add(&policer_table, &policer_ntry);
        return 0;
    }
    if (strcmp(arg[0], "outqos") == 0) {
        policer_ntry.meter = atoi(arg[2]);
        policer_ntry.dir = 2;
        policer_ntry.allow = readRate(&arg[0]);
        if (del == 0) table_del(&policer_table, &policer_ntry);
        else table_add(&policer_table, &policer_ntry);
        return 0;
    }
    if (strcmp(arg[0], "flowspec4") == 0) {
        acls_ntry.dir = 8;
        policer_ntry.dir = 3;
        policer_ntry.vrf = acls_ntry.port = atoi(arg[2]);
        policer_ntry.allow = readRate(&arg[1]);
        acls_res = table_addinited(&acls4_table, &acls_ntry, &acls_ntry.aces, sizeof(struct acl4_entry), &acl4_compare);
        readAcl4(&acl4_ntry, &arg[4]);
        policer_ntry.meter = acl4_ntry.pri;
        if (del == 0) table_del(&acls_res->aces, &acl4_ntry);
        else table_add(&acls_res->aces, &acl4_ntry);
        if (del == 0) table_del(&policer_table, &policer_ntry);
        else table_add(&policer_table, &policer_ntry);
        return 0;
    }
    if (strcmp(arg[0], "flowspec6") == 0) {
        acls_ntry.dir = 8;
        policer_ntry.dir = 4;
        policer_ntry.vrf = acls_ntry.port = atoi(arg[2]);
        policer_ntry.allow = readRate(&arg[1]);
        acls_res = table_addinited(&acls6_table, &acls_ntry, &acls_ntry.aces, sizeof(struct acl6_entry), &acl6_compare);
        readAcl6(&acl6_ntry, &arg[4]);
        policer_ntry.meter = acl6_ntry.pri;
        if (del == 0) table_del(&acls_res->aces, &acl6_ntry);
        else table_add(&acls_res->aces, &acl6_ntry);
        if (del == 0) table_del(&policer_table, &policer_ntry);
        else table_add(&policer_table, &policer_ntry);
        return 0;
    }
    if (strcmp(arg[0], "pbr4norm") == 0) {
        acls_ntry.dir = 5;
        acls_ntry.port = atoi(arg[2]);
        acls_res = table_addinited(&acls4_table, &acls_ntry, &acls_ntry.aces, sizeof(struct acl4_entry), &acl4_compare);
        acls_res->cmd = 1;
        readAcl4(&acl4_ntry, &arg[3]);
        if (del == 0) table_del(&acls_res->aces, &acl4_ntry);
        else table_add(&acls_res->aces, &acl4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "pbr6norm") == 0) {
        acls_ntry.dir = 5;
        acls_ntry.port = atoi(arg[2]);
        acls_res = table_addinited(&acls6_table, &acls_ntry, &acls_ntry.aces, sizeof(struct acl6_entry), &acl6_compare);
        acls_res->cmd = 1;
        readAcl6(&acl6_ntry, &arg[3]);
        if (del == 0) table_del(&acls_res->aces, &acl6_ntry);
        else table_add(&acls_res->aces, &acl6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "pbr4vrf") == 0) {
        acls_ntry.dir = 5;
        acls_ntry.port = atoi(arg[2]);
        acls_res = table_addinited(&acls4_table, &acls_ntry, &acls_ntry.aces, sizeof(struct acl4_entry), &acl4_compare);
        acls_res->cmd = 2;
        acls_res->vrf = atoi(arg[3]);
        readAcl4(&acl4_ntry, &arg[3]);
        if (del == 0) table_del(&acls_res->aces, &acl4_ntry);
        else table_add(&acls_res->aces, &acl4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "pbr6vrf") == 0) {
        acls_ntry.dir = 5;
        acls_ntry.port = atoi(arg[2]);
        acls_res = table_addinited(&acls6_table, &acls_ntry, &acls_ntry.aces, sizeof(struct acl6_entry), &acl6_compare);
        acls_res->cmd = 2;
        acls_res->vrf = atoi(arg[3]);
        readAcl6(&acl6_ntry, &arg[3]);
        if (del == 0) table_del(&acls_res->aces, &acl6_ntry);
        else table_add(&acls_res->aces, &acl6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "pbr4hop") == 0) {
        acls_ntry.dir = 5;
        acls_ntry.port = atoi(arg[2]);
        acls_res = table_addinited(&acls4_table, &acls_ntry, &acls_ntry.aces, sizeof(struct acl4_entry), &acl4_compare);
        acls_res->cmd = 3;
        acls_res->vrf = atoi(arg[3]);
        acls_res->hop = atoi(arg[4]);
        readAcl4(&acl4_ntry, &arg[3]);
        if (del == 0) table_del(&acls_res->aces, &acl4_ntry);
        else table_add(&acls_res->aces, &acl4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "pbr6hop") == 0) {
        acls_ntry.dir = 5;
        acls_ntry.port = atoi(arg[2]);
        acls_res = table_addinited(&acls6_table, &acls_ntry, &acls_ntry.aces, sizeof(struct acl6_entry), &acl6_compare);
        acls_res->cmd = 3;
        acls_res->vrf = atoi(arg[3]);
        acls_res->hop = atoi(arg[4]);
        readAcl6(&acl6_ntry, &arg[3]);
        if (del == 0) table_del(&acls_res->aces, &acl6_ntry);
        else table_add(&acls_res->aces, &acl6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "pbr4lab") == 0) {
        acls_ntry.dir = 5;
        acls_ntry.port = atoi(arg[2]);
        acls_res = table_addinited(&acls4_table, &acls_ntry, &acls_ntry.aces, sizeof(struct acl4_entry), &acl4_compare);
        acls_res->cmd = 4;
        acls_res->vrf = atoi(arg[3]);
        acls_res->hop = atoi(arg[4]);
        acls_res->label = atoi(arg[5]);
        readAcl4(&acl4_ntry, &arg[4]);
        if (del == 0) table_del(&acls_res->aces, &acl4_ntry);
        else table_add(&acls_res->aces, &acl4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "pbr6lab") == 0) {
        acls_ntry.dir = 5;
        acls_ntry.port = atoi(arg[2]);
        acls_res = table_addinited(&acls6_table, &acls_ntry, &acls_ntry.aces, sizeof(struct acl6_entry), &acl6_compare);
        acls_res->cmd = 4;
        acls_res->vrf = atoi(arg[3]);
        acls_res->hop = atoi(arg[4]);
        acls_res->label = atoi(arg[5]);
        readAcl6(&acl6_ntry, &arg[4]);
        if (del == 0) table_del(&acls_res->aces, &acl6_ntry);
        else table_add(&acls_res->aces, &acl6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "hairpin") == 0) {
        bundle_ntry.id = atoi(arg[2]);
        bundle_ntry.command = 2;
        o = atoi(arg[3]);
        for (int i = 0; i < 16; i++) bundle_ntry.out[i] = o;
        if (del == 0) table_del(&bundle_table, &bundle_ntry);
        else table_add(&bundle_table, &bundle_ntry);
        return 0;
    }
    if (strcmp(arg[0], "portbundle") == 0) {
        bundle_ntry.id = atoi(arg[2]);
        if (del == 0) {
            table_del(&bundle_table, &bundle_ntry);
            return 0;
        }
        index = table_find(&bundle_table, &bundle_ntry);
        if (index < 0) {
            table_add(&bundle_table, &bundle_ntry);
            bundle_res = table_get(&bundle_table, table_find(&bundle_table, &bundle_ntry));
        } else {
            bundle_res = table_get(&bundle_table, index);
        }
        i = atoi(arg[3]);
        bundle_res->command = 1;
        bundle_res->out[i] = atoi(arg[4]);
        return 0;
    }
    if (strcmp(arg[0], "bundlevlan") == 0) {
        vlan_ntry.id = atoi(arg[4]);
        vlan_ntry.port = atoi(arg[2]);
        vlan_ntry.vlan = atoi(arg[3]);
        if (del == 0) table_del(&vlanin_table, &vlan_ntry);
        else table_add(&vlanin_table, &vlan_ntry);
        return 0;
    }
    if (strcmp(arg[0], "pppoe") == 0) {
        pppoe_ntry.aclport = atoi(arg[2]);
        pppoe_ntry.port = atoi(arg[3]);
        pppoe_ntry.session = atoi(arg[6]);
        neigh_ntry.id = atoi(arg[4]);
        neigh_ntry.vrf = atoi(arg[5]);
        neigh_ntry.port = pppoe_ntry.port;
        neigh_ntry.aclport = pppoe_ntry.aclport;
        neigh_ntry.session = pppoe_ntry.session;
        neigh_ntry.command = 2;
        str2mac(neigh_ntry.dmac, arg[7]);
        str2mac(neigh_ntry.smac, arg[8]);
        if (del == 0) table_del(&pppoe_table, &pppoe_ntry);
        else table_add(&pppoe_table, &pppoe_ntry);
        if (del == 0) table_del(&neigh_table, &neigh_ntry);
        else table_add(&neigh_table, &neigh_ntry);
        return 0;
    }
    if (strcmp(arg[0], "gre4") == 0) {
        neigh_ntry.id = atoi(arg[2]);
        tun4_ntry.aclport = neigh_ntry.aclport = atoi(arg[3]);
        neigh_ntry.port = atoi(arg[4]);
        inet_pton(AF_INET, arg[5], buf2);
        tun4_ntry.trgAddr = neigh_ntry.sip1 = get32msb(buf2, 0);
        inet_pton(AF_INET, arg[6], buf2);
        tun4_ntry.srcAddr = neigh_ntry.dip1 = get32msb(buf2, 0);
        vrf2rib_ntry.vrf = neigh_ntry.vrf = atoi(arg[8]);
        vrf2rib_res = vrf2rib_init4;
        str2mac(neigh_ntry.dmac, arg[7]);
        str2mac(neigh_ntry.smac, arg[9]);
        neigh_ntry.command = 3;
        tun4_ntry.srcPort = 0;
        tun4_ntry.trgPort = 0;
        tun4_ntry.prot = 47;
        tun4_ntry.command = 1;
        if (del == 0) table_del(&neigh_table, &neigh_ntry);
        else table_add(&neigh_table, &neigh_ntry);
        if (del == 0) table_del(&vrf2rib_res->tun, &tun4_ntry);
        else table_add(&vrf2rib_res->tun, &tun4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "gre6") == 0) {
        neigh_ntry.id = atoi(arg[2]);
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
        str2mac(neigh_ntry.dmac, arg[7]);
        str2mac(neigh_ntry.smac, arg[9]);
        neigh_ntry.command = 4;
        tun6_ntry.srcPort = 0;
        tun6_ntry.trgPort = 0;
        tun6_ntry.prot = 47;
        tun6_ntry.command = 1;
        if (del == 0) table_del(&neigh_table, &neigh_ntry);
        else table_add(&neigh_table, &neigh_ntry);
        if (del == 0) table_del(&vrf2rib_res->tun, &tun6_ntry);
        else table_add(&vrf2rib_res->tun, &tun6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "ipip4") == 0) {
        neigh_ntry.id = atoi(arg[2]);
        tun4_ntry.aclport = neigh_ntry.aclport = atoi(arg[3]);
        neigh_ntry.port = atoi(arg[4]);
        inet_pton(AF_INET, arg[5], buf2);
        tun4_ntry.trgAddr = neigh_ntry.sip1 = get32msb(buf2, 0);
        inet_pton(AF_INET, arg[6], buf2);
        tun4_ntry.srcAddr = neigh_ntry.dip1 = get32msb(buf2, 0);
        vrf2rib_ntry.vrf = neigh_ntry.vrf = atoi(arg[8]);
        vrf2rib_res = vrf2rib_init4;
        str2mac(neigh_ntry.dmac, arg[7]);
        str2mac(neigh_ntry.smac, arg[9]);
        neigh_ntry.command = 7;
        if (del == 0) table_del(&neigh_table, &neigh_ntry);
        else table_add(&neigh_table, &neigh_ntry);
        tun4_ntry.srcPort = 0;
        tun4_ntry.trgPort = 0;
        tun4_ntry.prot = 4;
        tun4_ntry.command = 4;
        if (del == 0) table_del(&vrf2rib_res->tun, &tun4_ntry);
        else table_add(&vrf2rib_res->tun, &tun4_ntry);
        tun4_ntry.prot = 41;
        tun4_ntry.command = 5;
        if (del == 0) table_del(&vrf2rib_res->tun, &tun4_ntry);
        else table_add(&vrf2rib_res->tun, &tun4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "ipip6") == 0) {
        neigh_ntry.id = atoi(arg[2]);
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
        str2mac(neigh_ntry.dmac, arg[7]);
        str2mac(neigh_ntry.smac, arg[9]);
        neigh_ntry.command = 8;
        if (del == 0) table_del(&neigh_table, &neigh_ntry);
        else table_add(&neigh_table, &neigh_ntry);
        tun6_ntry.srcPort = 0;
        tun6_ntry.trgPort = 0;
        tun6_ntry.prot = 4;
        tun6_ntry.command = 4;
        if (del == 0) table_del(&vrf2rib_res->tun, &tun6_ntry);
        else table_add(&vrf2rib_res->tun, &tun6_ntry);
        tun6_ntry.prot = 41;
        tun6_ntry.command = 5;
        if (del == 0) table_del(&vrf2rib_res->tun, &tun6_ntry);
        else table_add(&vrf2rib_res->tun, &tun6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "l2tp4") == 0) {
        neigh_ntry.id = atoi(arg[2]);
        tun4_ntry.aclport = neigh_ntry.aclport = atoi(arg[3]);
        neigh_ntry.port = atoi(arg[4]);
        inet_pton(AF_INET, arg[5], buf2);
        tun4_ntry.trgAddr = neigh_ntry.sip1 = get32msb(buf2, 0);
        inet_pton(AF_INET, arg[6], buf2);
        tun4_ntry.srcAddr = neigh_ntry.dip1 = get32msb(buf2, 0);
        vrf2rib_ntry.vrf = neigh_ntry.vrf = atoi(arg[8]);
        vrf2rib_res = vrf2rib_init4;
        str2mac(neigh_ntry.dmac, arg[7]);
        str2mac(neigh_ntry.smac, arg[9]);
        neigh_ntry.command = 5;
        neigh_ntry.sprt = atoi(arg[10]);
        neigh_ntry.dprt = atoi(arg[11]);
        neigh_ntry.tid = atoi(arg[12]);
        tun4_ntry.srcPort = neigh_ntry.dprt;
        tun4_ntry.trgPort = neigh_ntry.sprt;
        tun4_ntry.prot = 17;
        tun4_ntry.command = 2;
        if (del == 0) table_del(&neigh_table, &neigh_ntry);
        else table_add(&neigh_table, &neigh_ntry);
        if (del == 0) table_del(&vrf2rib_res->tun, &tun4_ntry);
        else table_add(&vrf2rib_res->tun, &tun4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "l2tp6") == 0) {
        neigh_ntry.id = atoi(arg[2]);
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
        str2mac(neigh_ntry.dmac, arg[7]);
        str2mac(neigh_ntry.smac, arg[9]);
        neigh_ntry.command = 6;
        neigh_ntry.sprt = atoi(arg[10]);
        neigh_ntry.dprt = atoi(arg[11]);
        neigh_ntry.tid = atoi(arg[12]);
        tun6_ntry.srcPort = neigh_ntry.dprt;
        tun6_ntry.trgPort = neigh_ntry.sprt;
        tun6_ntry.prot = 17;
        tun6_ntry.command = 2;
        if (del == 0) table_del(&neigh_table, &neigh_ntry);
        else table_add(&neigh_table, &neigh_ntry);
        if (del == 0) table_del(&vrf2rib_res->tun, &tun6_ntry);
        else table_add(&vrf2rib_res->tun, &tun6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "amt4") == 0) {
        neigh_ntry.id = atoi(arg[2]);
        tun4_ntry.aclport = neigh_ntry.aclport = atoi(arg[3]);
        neigh_ntry.port = atoi(arg[4]);
        inet_pton(AF_INET, arg[5], buf2);
        tun4_ntry.trgAddr = neigh_ntry.sip1 = get32msb(buf2, 0);
        inet_pton(AF_INET, arg[6], buf2);
        tun4_ntry.srcAddr = neigh_ntry.dip1 = get32msb(buf2, 0);
        vrf2rib_ntry.vrf = neigh_ntry.vrf = atoi(arg[8]);
        vrf2rib_res = vrf2rib_init4;
        str2mac(neigh_ntry.dmac, arg[7]);
        str2mac(neigh_ntry.smac, arg[9]);
        neigh_ntry.command = 15;
        neigh_ntry.sprt = atoi(arg[10]);
        neigh_ntry.dprt = atoi(arg[11]);
        tun4_ntry.srcPort = neigh_ntry.dprt;
        tun4_ntry.trgPort = neigh_ntry.sprt;
        tun4_ntry.prot = 17;
        tun4_ntry.command = 10;
        if (del == 0) table_del(&neigh_table, &neigh_ntry);
        else table_add(&neigh_table, &neigh_ntry);
        if (del == 0) table_del(&vrf2rib_res->tun, &tun4_ntry);
        else table_add(&vrf2rib_res->tun, &tun4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "amt6") == 0) {
        neigh_ntry.id = atoi(arg[2]);
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
        str2mac(neigh_ntry.dmac, arg[7]);
        str2mac(neigh_ntry.smac, arg[9]);
        neigh_ntry.command = 16;
        neigh_ntry.sprt = atoi(arg[10]);
        neigh_ntry.dprt = atoi(arg[11]);
        tun6_ntry.srcPort = neigh_ntry.dprt;
        tun6_ntry.trgPort = neigh_ntry.sprt;
        tun6_ntry.prot = 17;
        tun6_ntry.command = 10;
        if (del == 0) table_del(&neigh_table, &neigh_ntry);
        else table_add(&neigh_table, &neigh_ntry);
        if (del == 0) table_del(&vrf2rib_res->tun, &tun6_ntry);
        else table_add(&vrf2rib_res->tun, &tun6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "macsec") == 0) {
        macsec_ntry.port = atoi(arg[2]);
        macsec_ntry.ethtyp = atoi(arg[3]);
        macsec_ntry.encrBlkLen = atoi(arg[4]);
        macsec_ntry.hashBlkLen = atoi(arg[5]);
        macsec_ntry.needMacs = atoi(arg[6]);
        macsec_ntry.encrAlg = getEncrAlg(arg[7]);
        if (macsec_ntry.encrAlg == NULL) return 0;
        macsec_ntry.hashAlg = getHashAlg(arg[8]);
        if (macsec_ntry.hashAlg == NULL) return 0;
        macsec_ntry.encrKeyLen = str2key(arg[9], macsec_ntry.encrKeyDat);
        macsec_ntry.hashKeyLen = str2key(arg[10], macsec_ntry.hashKeyDat);
        macsec_ntry.hashPkey = EVP_PKEY_new_mac_key(EVP_PKEY_HMAC, NULL, macsec_ntry.hashKeyDat, macsec_ntry.hashKeyLen);
        if (macsec_ntry.hashPkey == NULL) return 0;
        if (del == 0) table_del(&macsec_table, &macsec_ntry);
        else table_add(&macsec_table, &macsec_ntry);
        return 0;
    }
    if (strcmp(arg[0], "ipsec4") == 0) {
        neigh_ntry.id = atoi(arg[2]);
        tun4_ntry.aclport = neigh_ntry.aclport = atoi(arg[3]);
        neigh_ntry.port = atoi(arg[4]);
        inet_pton(AF_INET, arg[5], buf2);
        tun4_ntry.trgAddr = neigh_ntry.sip1 = get32msb(buf2, 0);
        inet_pton(AF_INET, arg[6], buf2);
        tun4_ntry.srcAddr = neigh_ntry.dip1 = get32msb(buf2, 0);
        vrf2rib_ntry.vrf = neigh_ntry.vrf = atoi(arg[8]);
        vrf2rib_res = vrf2rib_init4;
        str2mac(neigh_ntry.dmac, arg[7]);
        str2mac(neigh_ntry.smac, arg[9]);
        neigh_ntry.command = 9;
        tun4_ntry.encrBlkLen = neigh_ntry.encrBlkLen = atoi(arg[10]);
        tun4_ntry.hashBlkLen = neigh_ntry.hashBlkLen = atoi(arg[11]);
        tun4_ntry.encrAlg = neigh_ntry.encrAlg = getEncrAlg(arg[12]);
        if (neigh_ntry.encrAlg == NULL) return 0;
        tun4_ntry.hashAlg = neigh_ntry.hashAlg = getHashAlg(arg[13]);
        if (neigh_ntry.hashAlg == NULL) return 0;
        tun4_ntry.spi = atoi(arg[14]);
        tun4_ntry.encrKeyLen = str2key(arg[15], tun4_ntry.encrKeyDat);
        tun4_ntry.hashKeyLen = str2key(arg[16], tun4_ntry.hashKeyDat);
        tun4_ntry.hashPkey = EVP_PKEY_new_mac_key(EVP_PKEY_HMAC, NULL, tun4_ntry.hashKeyDat, tun4_ntry.hashKeyLen);
        if (tun4_ntry.hashPkey == NULL) return 0;
        neigh_ntry.spi = atoi(arg[17]);
        neigh_ntry.encrKeyLen = str2key(arg[18], neigh_ntry.encrKeyDat);
        neigh_ntry.hashKeyLen = str2key(arg[19], neigh_ntry.hashKeyDat);
        neigh_ntry.hashPkey = EVP_PKEY_new_mac_key(EVP_PKEY_HMAC, NULL, neigh_ntry.hashKeyDat, neigh_ntry.hashKeyLen);
        if (neigh_ntry.hashPkey == NULL) return 0;
        tun4_ntry.prot = 50;
        tun4_ntry.command = 6;
        if (del == 0) table_del(&neigh_table, &neigh_ntry);
        else table_add(&neigh_table, &neigh_ntry);
        if (del == 0) table_del(&vrf2rib_res->tun, &tun4_ntry);
        else table_add(&vrf2rib_res->tun, &tun4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "ipsec6") == 0) {
        neigh_ntry.id = atoi(arg[2]);
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
        str2mac(neigh_ntry.dmac, arg[7]);
        str2mac(neigh_ntry.smac, arg[9]);
        neigh_ntry.command = 10;
        tun6_ntry.encrBlkLen = neigh_ntry.encrBlkLen = atoi(arg[10]);
        tun6_ntry.hashBlkLen = neigh_ntry.hashBlkLen = atoi(arg[11]);
        tun6_ntry.encrAlg = neigh_ntry.encrAlg = getEncrAlg(arg[12]);
        if (neigh_ntry.encrAlg == NULL) return 0;
        tun6_ntry.hashAlg = neigh_ntry.hashAlg = getHashAlg(arg[13]);
        if (neigh_ntry.hashAlg == NULL) return 0;
        tun6_ntry.spi = atoi(arg[14]);
        tun6_ntry.encrKeyLen = str2key(arg[15], tun6_ntry.encrKeyDat);
        tun6_ntry.hashKeyLen = str2key(arg[16], tun6_ntry.hashKeyDat);
        tun6_ntry.hashPkey = EVP_PKEY_new_mac_key(EVP_PKEY_HMAC, NULL, tun6_ntry.hashKeyDat, tun6_ntry.hashKeyLen);
        if (tun6_ntry.hashPkey == NULL) return 0;
        neigh_ntry.spi = atoi(arg[17]);
        neigh_ntry.encrKeyLen = str2key(arg[18], neigh_ntry.encrKeyDat);
        neigh_ntry.hashKeyLen = str2key(arg[19], neigh_ntry.hashKeyDat);
        neigh_ntry.hashPkey = EVP_PKEY_new_mac_key(EVP_PKEY_HMAC, NULL, neigh_ntry.hashKeyDat, neigh_ntry.hashKeyLen);
        if (neigh_ntry.hashPkey == NULL) return 0;
        tun6_ntry.prot = 50;
        tun6_ntry.command = 6;
        if (del == 0) table_del(&neigh_table, &neigh_ntry);
        else table_add(&neigh_table, &neigh_ntry);
        if (del == 0) table_del(&vrf2rib_res->tun, &tun6_ntry);
        else table_add(&vrf2rib_res->tun, &tun6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "openvpn4") == 0) {
        neigh_ntry.id = atoi(arg[2]);
        tun4_ntry.aclport = neigh_ntry.aclport = atoi(arg[3]);
        neigh_ntry.port = atoi(arg[4]);
        inet_pton(AF_INET, arg[5], buf2);
        tun4_ntry.trgAddr = neigh_ntry.sip1 = get32msb(buf2, 0);
        inet_pton(AF_INET, arg[6], buf2);
        tun4_ntry.srcAddr = neigh_ntry.dip1 = get32msb(buf2, 0);
        vrf2rib_ntry.vrf = neigh_ntry.vrf = atoi(arg[8]);
        vrf2rib_res = vrf2rib_init4;
        str2mac(neigh_ntry.dmac, arg[7]);
        str2mac(neigh_ntry.smac, arg[9]);
        neigh_ntry.command = 11;
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
        tun4_ntry.hashPkey = EVP_PKEY_new_mac_key(EVP_PKEY_HMAC, NULL, tun4_ntry.hashKeyDat, tun4_ntry.hashKeyLen);
        if (tun4_ntry.hashPkey == NULL) return 0;
        neigh_ntry.hashPkey = EVP_PKEY_new_mac_key(EVP_PKEY_HMAC, NULL, neigh_ntry.hashKeyDat, neigh_ntry.hashKeyLen);
        if (neigh_ntry.hashPkey == NULL) return 0;
        tun4_ntry.prot = 17;
        tun4_ntry.command = 8;
        if (del == 0) table_del(&neigh_table, &neigh_ntry);
        else table_add(&neigh_table, &neigh_ntry);
        if (del == 0) table_del(&vrf2rib_res->tun, &tun4_ntry);
        else table_add(&vrf2rib_res->tun, &tun4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "openvpn6") == 0) {
        neigh_ntry.id = atoi(arg[2]);
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
        str2mac(neigh_ntry.dmac, arg[7]);
        str2mac(neigh_ntry.smac, arg[9]);
        neigh_ntry.command = 12;
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
        tun6_ntry.hashPkey = EVP_PKEY_new_mac_key(EVP_PKEY_HMAC, NULL, tun6_ntry.hashKeyDat, tun6_ntry.hashKeyLen);
        if (tun6_ntry.hashPkey == NULL) return 0;
        neigh_ntry.hashPkey = EVP_PKEY_new_mac_key(EVP_PKEY_HMAC, NULL, neigh_ntry.hashKeyDat, neigh_ntry.hashKeyLen);
        if (neigh_ntry.hashPkey == NULL) return 0;
        tun6_ntry.prot = 17;
        tun6_ntry.command = 8;
        if (del == 0) table_del(&neigh_table, &neigh_ntry);
        else table_add(&neigh_table, &neigh_ntry);
        if (del == 0) table_del(&vrf2rib_res->tun, &tun6_ntry);
        else table_add(&vrf2rib_res->tun, &tun6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "wireguard4") == 0) {
        neigh_ntry.id = atoi(arg[2]);
        tun4_ntry.aclport = neigh_ntry.aclport = atoi(arg[3]);
        neigh_ntry.port = atoi(arg[4]);
        inet_pton(AF_INET, arg[5], buf2);
        tun4_ntry.trgAddr = neigh_ntry.sip1 = get32msb(buf2, 0);
        inet_pton(AF_INET, arg[6], buf2);
        tun4_ntry.srcAddr = neigh_ntry.dip1 = get32msb(buf2, 0);
        vrf2rib_ntry.vrf = neigh_ntry.vrf = atoi(arg[8]);
        vrf2rib_res = vrf2rib_init4;
        str2mac(neigh_ntry.dmac, arg[7]);
        str2mac(neigh_ntry.smac, arg[9]);
        neigh_ntry.command = 13;
        tun4_ntry.trgPort = neigh_ntry.sprt = atoi(arg[10]);
        tun4_ntry.srcPort = neigh_ntry.dprt = atoi(arg[11]);
        neigh_ntry.tid = atoi(arg[12]);
        neigh_ntry.encrKeyLen = str2key(arg[13], neigh_ntry.encrKeyDat);
        tun4_ntry.encrKeyLen = str2key(arg[14], tun4_ntry.encrKeyDat);
        tun4_ntry.prot = 17;
        tun4_ntry.command = 9;
        if (del == 0) table_del(&neigh_table, &neigh_ntry);
        else table_add(&neigh_table, &neigh_ntry);
        if (del == 0) table_del(&vrf2rib_res->tun, &tun4_ntry);
        else table_add(&vrf2rib_res->tun, &tun4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "wireguard6") == 0) {
        neigh_ntry.id = atoi(arg[2]);
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
        str2mac(neigh_ntry.dmac, arg[7]);
        str2mac(neigh_ntry.smac, arg[9]);
        neigh_ntry.command = 14;
        tun6_ntry.trgPort = neigh_ntry.sprt = atoi(arg[10]);
        tun6_ntry.srcPort = neigh_ntry.dprt = atoi(arg[11]);
        neigh_ntry.tid = atoi(arg[12]);
        neigh_ntry.encrKeyLen = str2key(arg[13], neigh_ntry.encrKeyDat);
        tun6_ntry.encrKeyLen = str2key(arg[14], tun6_ntry.encrKeyDat);
        tun6_ntry.prot = 17;
        tun6_ntry.command = 9;
        if (del == 0) table_del(&neigh_table, &neigh_ntry);
        else table_add(&neigh_table, &neigh_ntry);
        if (del == 0) table_del(&vrf2rib_res->tun, &tun6_ntry);
        else table_add(&vrf2rib_res->tun, &tun6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "monitor") == 0) {
        monitor_ntry.port = atoi(arg[2]);
        monitor_ntry.target = atoi(arg[3]);
        monitor_ntry.sample = atoi(arg[5]);
        monitor_ntry.truncate = atoi(arg[6]);
        if (del == 0) table_del(&monitor_table, &monitor_ntry);
        else table_add(&monitor_table, &monitor_ntry);
        return 0;
    }
    if (strcmp(arg[0], "mlocal4") == 0) {
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init4;
        inet_pton(AF_INET, arg[4], buf2);
        mroute4_ntry.grp = get32msb(buf2, 0);
        inet_pton(AF_INET, arg[5], buf2);
        mroute4_ntry.src = get32msb(buf2, 0);
        mroute4_res = table_addinited(&vrf2rib_res->mcst, &mroute4_ntry, &mroute4_ntry.flood, sizeof(struct flood_entry), &flood_compare);
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
        mroute6_res = table_addinited(&vrf2rib_res->mcst, &mroute6_ntry, &mroute6_ntry.flood, sizeof(struct flood_entry), &flood_compare);
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
        mroute4_res = table_addinited(&vrf2rib_res->mcst, &mroute4_ntry, &mroute4_ntry.flood, sizeof(struct flood_entry), &flood_compare);
        mroute4_res->ingr = atoi(arg[6]);
        flood_ntry.trg = atoi(arg[8]);
        flood_ntry.command = 1;
        str2mac(flood_ntry.smac, arg[9]);
        str2mac(flood_ntry.dmac, arg[10]);
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
        mroute6_res = table_addinited(&vrf2rib_res->mcst, &mroute6_ntry, &mroute6_ntry.flood, sizeof(struct flood_entry), &flood_compare);
        mroute6_res->ingr = atoi(arg[6]);
        flood_ntry.trg = atoi(arg[8]);
        flood_ntry.command = 1;
        str2mac(flood_ntry.smac, arg[9]);
        str2mac(flood_ntry.dmac, arg[10]);
        if (del == 0) table_del(&mroute6_res->flood, &flood_ntry);
        else table_add(&mroute6_res->flood, &flood_ntry);
        return 0;
    }
    if (strcmp(arg[0], "duplabloc4") == 0) {
        mpls_ntry.vrf = atoi(arg[2]);
        mpls_ntry.label = atoi(arg[4]);
        mpls_ntry.ver = 4;
        mpls_ntry.command = 7;
        mpls_res = table_addinited(&mpls_table, &mpls_ntry, &mpls_ntry.flood, sizeof(struct flood_entry), &flood_compare);
        mpls_res->command = 7;
        mpls_res->swap = del;
        return 0;
    }
    if (strcmp(arg[0], "duplabloc6") == 0) {
        mpls_ntry.vrf = atoi(arg[2]);
        mpls_ntry.label = atoi(arg[4]);
        mpls_ntry.ver = 6;
        mpls_ntry.command = 7;
        mpls_res = table_addinited(&mpls_table, &mpls_ntry, &mpls_ntry.flood, sizeof(struct flood_entry), &flood_compare);
        mpls_res->command = 7;
        mpls_res->swap = del;
        return 0;
    }
    if (strcmp(arg[0], "duplabel4") == 0) {
        mpls_ntry.vrf = atoi(arg[2]);
        mpls_ntry.label = atoi(arg[4]);
        mpls_ntry.ver = 4;
        mpls_ntry.command = 7;
        mpls_res = table_addinited(&mpls_table, &mpls_ntry, &mpls_ntry.flood, sizeof(struct flood_entry), &flood_compare);
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
        mpls_res = table_addinited(&mpls_table, &mpls_ntry, &mpls_ntry.flood, sizeof(struct flood_entry), &flood_compare);
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
        mroute4_res = table_addinited(&vrf2rib_res->mcst, &mroute4_ntry, &mroute4_ntry.flood, sizeof(struct flood_entry), &flood_compare);
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
        mroute6_res = table_addinited(&vrf2rib_res->mcst, &mroute6_ntry, &mroute6_ntry.flood, sizeof(struct flood_entry), &flood_compare);
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
        mpls_res = table_addinited(&mpls_table, &mpls_ntry, &mpls_ntry.flood, sizeof(struct flood_entry), &flood_compare);
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
        mpls_res = table_addinited(&mpls_table, &mpls_ntry, &mpls_ntry.flood, sizeof(struct flood_entry), &flood_compare);
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
        mpls_res = table_addinited(&mpls_table, &mpls_ntry, &mpls_ntry.flood, sizeof(struct flood_entry), &flood_compare);
        mpls_res->command = 8;
        for (int i=0; i<8; i++) mpls_res->bier[i] = atoi(arg[5+i]);
        if (del == 0) table_del(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "bierlabloc6") == 0) {
        mpls_ntry.vrf = atoi(arg[2]);
        mpls_ntry.label = atoi(arg[4]);
        mpls_ntry.ver = 6;
        mpls_ntry.command = 8;
        mpls_res = table_addinited(&mpls_table, &mpls_ntry, &mpls_ntry.flood, sizeof(struct flood_entry), &flood_compare);
        mpls_res->command = 8;
        for (int i=0; i<8; i++) mpls_res->bier[i] = atoi(arg[5+i]);
        if (del == 0) table_del(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "mbierroute4") == 0) {
        vrf2rib_ntry.vrf = atoi(arg[2]);
        vrf2rib_res = vrf2rib_init4;
        inet_pton(AF_INET, arg[4], buf2);
        mroute4_ntry.grp = get32msb(buf2, 0);
        inet_pton(AF_INET, arg[5], buf2);
        mroute4_ntry.src = get32msb(buf2, 0);
        mroute4_res = table_addinited(&vrf2rib_res->mcst, &mroute4_ntry, &mroute4_ntry.flood, sizeof(struct flood_entry), &flood_compare);
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
        mroute6_res = table_addinited(&vrf2rib_res->mcst, &mroute6_ntry, &mroute6_ntry.flood, sizeof(struct flood_entry), &flood_compare);
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
#endif
    return 0;
}






void doStatRound_rou4(void* buffer, int fixed, void* param) {
    struct route4_entry *ntry = buffer;
    FILE *commands = param;
    unsigned char buf[128];
    unsigned char buf2[32];
    put32msb(buf2, 0, ntry->addr);
    inet_ntop(AF_INET, &buf2[0], (char*)&buf[0], sizeof(buf));
    fprintf(commands, "route4_cnt %i %s %i %li %li\r\n", fixed, (char*)&buf[0], ntry->mask, ntry->pack, ntry->byte);
}

void doStatRound_rou6(void* buffer, int fixed, void* param) {
    struct route6_entry *ntry = buffer;
    FILE *commands = param;
    unsigned char buf[128];
    unsigned char buf2[32];
    put32msb(buf2, 0, ntry->addr1);
    put32msb(buf2, 4, ntry->addr2);
    put32msb(buf2, 8, ntry->addr3);
    put32msb(buf2, 12, ntry->addr4);
    inet_ntop(AF_INET6, &buf2[0], (char*)&buf[0], sizeof(buf));
    fprintf(commands, "route6_cnt %i %s %i %li %li\r\n", fixed, (char*)&buf[0], ntry->mask, ntry->pack, ntry->byte);
}

void doStatRound_nat4(void* buffer, int fixed, void* param) {
    FILE *commands = param;
    struct table_head *nat_table = buffer;
    unsigned char buf[1024];
    unsigned char buf2[1024];
    unsigned char buf3[1024];
    for (int i=0; i<nat_table->size; i++) {
        struct nat4_entry *ntry = table_get(nat_table, i);
        put32msb(buf, 0, ntry->oSrcAddr);
        inet_ntop(AF_INET, &buf[0], (char*)&buf2[0], sizeof(buf2));
        put32msb(buf, 0, ntry->oTrgAddr);
        inet_ntop(AF_INET, &buf[0], (char*)&buf3[0], sizeof(buf3));
        fprintf(commands, "nattrns4_cnt %i %i %s %s %i %i %li %li\r\n", fixed, ntry->prot, (char*)&buf2[0], (char*)&buf3[0], ntry->oSrcPort, ntry->oTrgPort, ntry->pack, ntry->byte);
    }
}

void doStatRound_nat6(void* buffer, int fixed, void* param) {
    FILE *commands = param;
    struct table_head *nat_table = buffer;
    unsigned char buf[1024];
    unsigned char buf2[1024];
    unsigned char buf3[1024];
    for (int i=0; i<nat_table->size; i++) {
        struct nat6_entry *ntry = table_get(nat_table, i);
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
        fprintf(commands, "nattrns6_cnt %i %i %s %s %i %i %li %li\r\n", fixed, ntry->prot, (char*)&buf2[0], (char*)&buf3[0], ntry->oSrcPort, ntry->oTrgPort, ntry->pack, ntry->byte);
    }
}

void doStatRound_tun4(void* buffer, int fixed, void* param) {
    FILE *commands = param;
    struct table_head *tun_table = buffer;
    unsigned char buf[1024];
    unsigned char buf2[1024];
    unsigned char buf3[1024];
    for (int i=0; i<tun_table->size; i++) {
        struct tun4_entry *ntry = table_get(tun_table, i);
        fprintf(commands, "counter %i %li %li 0 0 0 0\r\n", ntry->aclport, ntry->pack, ntry->byte);
    }
    for (int i=0; i<tun_table->size; i++) {
        struct tun4_entry *ntry = table_get(tun_table, i);
        put32msb(buf, 0, ntry->srcAddr);
        inet_ntop(AF_INET, &buf[0], (char*)&buf2[0], sizeof(buf2));
        put32msb(buf, 0, ntry->trgAddr);
        inet_ntop(AF_INET, &buf[0], (char*)&buf3[0], sizeof(buf3));
        fprintf(commands, "tun4_cnt %i %i %s %s %i %i %li %li\r\n", fixed, ntry->prot, (char*)&buf2[0], (char*)&buf3[0], ntry->srcPort, ntry->trgPort, ntry->pack, ntry->byte);
    }
}

void doStatRound_tun6(void* buffer, int fixed, void* param) {
    FILE *commands = param;
    struct table_head *tun_table = buffer;
    unsigned char buf[1024];
    unsigned char buf2[1024];
    unsigned char buf3[1024];
    for (int i=0; i<tun_table->size; i++) {
        struct tun6_entry *ntry = table_get(tun_table, i);
        fprintf(commands, "counter %i %li %li 0 0 0 0\r\n", ntry->aclport, ntry->pack, ntry->byte);
    }
    for (int i=0; i<tun_table->size; i++) {
        struct tun6_entry *ntry = table_get(tun_table, i);
        put32msb(buf, 0, ntry->srcAddr1);
        put32msb(buf, 4, ntry->srcAddr2);
        put32msb(buf, 8, ntry->srcAddr3);
        put32msb(buf, 12, ntry->srcAddr4);
        inet_ntop(AF_INET6, &buf[0], (char*)&buf2[0], sizeof(buf2));
        put32msb(buf, 0, ntry->trgAddr1);
        put32msb(buf, 4, ntry->trgAddr2);
        put32msb(buf, 8, ntry->trgAddr3);
        put32msb(buf, 12, ntry->trgAddr4);
        inet_ntop(AF_INET6, &buf[0], (char*)&buf3[0], sizeof(buf3));
        fprintf(commands, "tun6_cnt %i %i %s %s %i %i %li %li\r\n", fixed, ntry->prot, (char*)&buf2[0], (char*)&buf3[0], ntry->srcPort, ntry->trgPort, ntry->pack, ntry->byte);
    }
}

void doStatRound_mcst4(void* buffer, int fixed, void* param) {
    FILE *commands = param;
    struct table_head *mroute_table = buffer;
    unsigned char buf[1024];
    unsigned char buf2[1024];
    unsigned char buf3[1024];
    for (int i=0; i<mroute_table->size; i++) {
        struct mroute4_entry *ntry = table_get(mroute_table, i);
        put32msb(buf, 0, ntry->src);
        inet_ntop(AF_INET, &buf[0], (char*)&buf2[0], sizeof(buf2));
        put32msb(buf, 0, ntry->grp);
        inet_ntop(AF_INET, &buf[0], (char*)&buf3[0], sizeof(buf3));
        fprintf(commands, "mroute4_cnt %i %s %s %li %li\r\n", fixed, (char*)&buf2[0], (char*)&buf3[0], ntry->pack, ntry->byte);
    }
}

void doStatRound_mcst6(void* buffer, int fixed, void* param) {
    FILE *commands = param;
    struct table_head *mroute_table = buffer;
    unsigned char buf[1024];
    unsigned char buf2[1024];
    unsigned char buf3[1024];
    for (int i=0; i<mroute_table->size; i++) {
        struct mroute6_entry *ntry = table_get(mroute_table, i);
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
        fprintf(commands, "mroute6_cnt %i %s %s %li %li\r\n", fixed, (char*)&buf2[0], (char*)&buf3[0], ntry->pack, ntry->byte);
    }
}

void doStatRount_ipvX(struct table_head *tab, void doer(void *, int, void *), void natter(void *, int, void *), void tunner(void *, int, void *), void mcaster(void *, int, void *), int ver, void*param) {
    FILE *commands = param;
    for (int i = 0; i < tab->size; i++) {
        struct vrf2rib_entry *res = table_get(tab, i);
        fprintf(commands, "vrf%i_cnt %i %li %li\r\n", ver, res->vrf, res->pack, res->byte);
        tree_walk(&res->rou, doer, res->vrf, param);
        natter(&res->nat, res->vrf, param);
        tunner(&res->tun, res->vrf, param);
        mcaster(&res->mcst, res->vrf, param);
    }
}


void doStatRount_acl(struct acls_entry *ntry1, int ver, FILE *commands) {
    unsigned char buf2[1024];
    switch (ntry1->dir) {
    case 1:
        snprintf((char*)&buf2[0], 128, "inacl%i_cnt %i", ver, ntry1->port);
        break;
    case 2:
        snprintf((char*)&buf2[0], 128, "outacl%i_cnt %i", ver, ntry1->port);
        break;
    case 3:
        snprintf((char*)&buf2[0], 128, "natacl%i_cnt %i", ver, ntry1->port);
        break;
    case 4:
        snprintf((char*)&buf2[0], 128, "coppacl%i_cnt", ver);
        break;
    case 5:
        snprintf((char*)&buf2[0], 128, "pbracl%i_cnt %i", ver, ntry1->port);
        break;
    case 6:
        snprintf((char*)&buf2[0], 128, "inqos%i_cnt %i", ver, ntry1->port);
        break;
    case 7:
        snprintf((char*)&buf2[0], 128, "outqos%i_cnt %i", ver, ntry1->port);
        break;
    case 8:
        snprintf((char*)&buf2[0], 128, "flowspec%i_cnt %i", ver, ntry1->port);
        break;
    default:
        return;
    }
    for (int i=0; i<ntry1->aces.size; i++) {
        struct aclH_entry *ntry2 = table_get(&ntry1->aces, i);
        fprintf(commands, "%s %i %li %li\r\n", (char*)&buf2[0], ntry2->pri, ntry2->pack, ntry2->byte);
    }
}

void doStatRound(FILE *commands, int round) {
    punts = 10;
    for (int i = 0; i < policer_table.size; i++) {
        struct policer_entry *ntry = table_get(&policer_table, i);
        ntry->avail = ntry->allow;
    }
    if ((round % 10) != 0) return;
    for (int i = 0; i < ports; i++) {
        fprintf(commands, "counter %i %li %li %li %li %li %li\r\n", i, packRx[i], byteRx[i], packTx[i], byteTx[i], packDr[i], byteDr[i]);
        int o = getState(i);
        fprintf(commands, "state %i %i\r\n", i, o);
    }
#ifndef basicLoop
    for (int i=0; i<bundle_table.size; i++) {
        struct bundle_entry *ntry = table_get(&bundle_table, i);
        fprintf(commands, "counter %i 0 0 %li %li 0 0\r\n", ntry->id, ntry->pack, ntry->byte);
    }
    for (int i=0; i<pppoe_table.size; i++) {
        struct pppoe_entry *ntry = table_get(&pppoe_table, i);
        fprintf(commands, "counter %i %li %li 0 0 0 0\r\n", ntry->aclport, ntry->pack, ntry->byte);
    }
    for (int i=0; i<vlanout_table.size; i++) {
        struct vlan_entry *ontry = table_get(&vlanout_table, i);
        int o = table_find(&vlanin_table, ontry);
        if (o < 0) continue;
        struct vlan_entry *intry = table_get(&vlanin_table, o);
        fprintf(commands, "counter %i %li %li %li %li 0 0\r\n", intry->id, intry->pack, intry->byte, ontry->pack, ontry->byte);
    }
    if ((round % 150) != 0) {
        fflush(commands);
        return;
    }
    unsigned char buf[1024];
    unsigned char buf2[1024];
    for (int i = 0; i < ports; i++) {
        fprintf(commands, "ethertype %i %i %li %li\r\n", i, ETHERTYPE_MPLS_UCAST, packMpls[i], byteMpls[i]);
        fprintf(commands, "ethertype %i %i %li %li\r\n", i, ETHERTYPE_VLAN, packVlan[i], byteVlan[i]);
        fprintf(commands, "ethertype %i %i %li %li\r\n", i, ETHERTYPE_IPV4, packIpv4[i], byteIpv4[i]);
        fprintf(commands, "ethertype %i %i %li %li\r\n", i, ETHERTYPE_IPV6, packIpv6[i], byteIpv6[i]);
        fprintf(commands, "ethertype %i %i %li %li\r\n", i, ETHERTYPE_PPPOE_DATA, packPppoe[i], bytePppoe[i]);
        fprintf(commands, "ethertype %i %i %li %li\r\n", i, ETHERTYPE_ROUTEDMAC, packBridge[i], byteBridge[i]);
        fprintf(commands, "ethertype %i %i %li %li\r\n", i, ETHERTYPE_POLKA, packPolka[i], bytePolka[i]);
        fprintf(commands, "ethertype %i %i %li %li\r\n", i, ETHERTYPE_NSH, packNsh[i], byteNsh[i]);
    }
    for (int i=0; i<polkaIdx_table.size; i++) {
        struct polkaIdx_entry *ntry = table_get(&polkaIdx_table, i);
        fprintf(commands, "polka_cnt %i %i %li %li\r\n", ntry->vrf, ntry->index, ntry->pack, ntry->byte);
    }
    for (int i=0; i<nsh_table.size; i++) {
        struct nsh_entry *ntry = table_get(&nsh_table, i);
        fprintf(commands, "nsh_cnt %i %i %li %li\r\n", ntry->sp, ntry->si, ntry->pack, ntry->byte);
    }
    for (int i=0; i<mpls_table.size; i++) {
        struct mpls_entry *ntry = table_get(&mpls_table, i);
        fprintf(commands, "mpls_cnt %i %li %li\r\n", ntry->label, ntry->pack, ntry->byte);
    }
    for (int i=0; i<neigh_table.size; i++) {
        struct neigh_entry *ntry = table_get(&neigh_table, i);
        fprintf(commands, "neigh_cnt %i %li %li\r\n", ntry->id, ntry->pack, ntry->byte);
    }
    for (int i=0; i<bridge_table.size; i++) {
        struct bridge_entry *ntry = table_get(&bridge_table, i);
        put16msb(buf2, 0, ntry->mac1);
        put32msb(buf2, 2, ntry->mac2);
        mac2str(buf2, buf);
        fprintf(commands, "bridge_cnt %i %s %li %li %li %li\r\n", ntry->id, (char*)&buf[0], ntry->packRx, ntry->byteRx, ntry->packTx, ntry->byteTx);
    }
    doStatRount_ipvX(&vrf2rib4_table, &doStatRound_rou4, &doStatRound_nat4, &doStatRound_tun4, &doStatRound_mcst4, 4, commands);
    doStatRount_ipvX(&vrf2rib6_table, &doStatRound_rou6, &doStatRound_nat6, &doStatRound_tun6, &doStatRound_mcst6, 6, commands);
    for (int i=0; i<macsec_table.size; i++) {
        struct macsec_entry *ntry = table_get(&macsec_table, i);
        fprintf(commands, "macsec_cnt %i %li %li %li %li\r\n", ntry->port, ntry->packRx, ntry->byteRx, ntry->packTx, ntry->byteTx);
    }
    for (int i=0; i<acls4_table.size; i++) {
        struct acls_entry *ntry1 = table_get(&acls4_table, i);
        doStatRount_acl(ntry1, 4, commands);
    }
    for (int i=0; i<acls6_table.size; i++) {
        struct acls_entry *ntry1 = table_get(&acls6_table, i);
        doStatRount_acl(ntry1, 6, commands);
    }
#endif
    fflush(commands);
}





void doConsoleCommand_ipv4(void* buffer, int fixed, void* param) {
    struct route4_entry *ntry = buffer;
    unsigned char buf[32];
    unsigned char buf2[128];
    put32msb(buf, 0, ntry->addr);
    inet_ntop(AF_INET, &buf[0], (char*)&buf2[0], sizeof(buf2));
    printf("%16s %3i %10i %3i %10i %10i %10i\n", (char*)&buf2[0], ntry->mask, fixed, ntry->command, ntry->nexthop, ntry->label1, ntry->label2);
}

void doConsoleCommand_ipv6(void* buffer, int fixed, void* param) {
    struct route6_entry *ntry = buffer;
    unsigned char buf[32];
    unsigned char buf2[128];
    put32msb(buf, 0, ntry->addr1);
    put32msb(buf, 4, ntry->addr2);
    put32msb(buf, 8, ntry->addr3);
    put32msb(buf, 12, ntry->addr4);
    inet_ntop(AF_INET6, &buf[0], (char*)&buf2[0], sizeof(buf2));
    printf("%40s %3i %10i %3i %10i %10i %10i\n", (char*)&buf2[0], ntry->mask, fixed, ntry->command, ntry->nexthop, ntry->label1, ntry->label2);
}

void doConsoleCommand_ipvX(struct table_head *tab, void doer(void *, int, void *)) {
    for (int i = 0; i < tab->size; i++) {
        struct vrf2rib_entry *res = table_get(tab, i);
        tree_walk(&res->rou, doer, res->vrf, NULL);
    }
}

int doConsoleCommand(unsigned char*buf) {
#ifndef basicLoop
    unsigned char buf2[1024];
    unsigned char buf3[1024];
#endif
    switch (buf[0]) {
    case 0:
        break;
    case 'H':
    case 'h':
    case '?':
        printf("commands:\n");
        printf("h - this help\n");
        printf("x - exit process\n");
        printf("i - interface counters\n");
#ifndef basicLoop
        printf("p - display portvrf table\n");
        printf("b - display bridge table\n");
        printf("m - display mpls table\n");
        printf("4 - display ipv4 table\n");
        printf("6 - display ipv6 table\n");
        printf("n - display nexthop table\n");
        printf("a - display acl table\n");
        printf("q - display qos table\n");
        printf("v - display vlan table\n");
#endif
        break;
    case 'x':
    case 'X':
        return 1;
        break;
    case 'i':
    case 'I':
        printf("                           iface         rx         tx       drop         rx         tx       drop\n");
        for (int i=0; i<ports; i++) {
            printf("%32s %10li %10li %10li %10li %10li %10li\n", ifaceName[i], packRx[i], packTx[i], packDr[i], byteRx[i], byteTx[i], byteDr[i]);
        }
        break;
#ifndef basicLoop
    case 'm':
    case 'M':
        printf("     label ip        vrf cmd       swap    nexthop\n");
        for (int i=0; i<mpls_table.size; i++) {
            struct mpls_entry *ntry = table_get(&mpls_table, i);
            printf("%10i %2i %10i %3i %10i %10i\n", ntry->label, ntry->ver, ntry->vrf, ntry->command, ntry->swap, ntry->nexthop);
        }
        break;
    case 'a':
    case 'A':
        printf("  vrf/port dir ver       aces\n");
        for (int i=0; i<acls4_table.size; i++) {
            struct acls_entry *ntry = table_get(&acls4_table, i);
            printf("%10i %3i 4   %10i\n", ntry->port, ntry->dir, ntry->aces.size);
        }
        for (int i=0; i<acls6_table.size; i++) {
            struct acls_entry *ntry = table_get(&acls6_table, i);
            printf("%10i %3i 6   %10i\n", ntry->port, ntry->dir, ntry->aces.size);
        }
        break;
    case 'p':
    case 'P':
        printf("      port cmd        vrf     bridge\n");
        for (int i=0; i<portvrf_table.size; i++) {
            struct portvrf_entry *ntry = table_get(&portvrf_table, i);
            printf("%10i %3i %10i %10i\n", ntry->port, ntry->command, ntry->vrf, ntry->bridge);
        }
        break;
    case 'n':
    case 'N':
        printf("        id        vrf       port              smac              dmac\n");
        for (int i=0; i<neigh_table.size; i++) {
            struct neigh_entry *ntry = table_get(&neigh_table, i);
            mac2str(ntry->smac, buf2);
            mac2str(ntry->dmac, buf3);
            printf("%10i %10i %10i %s %s\n", ntry->id, ntry->vrf, ntry->port, (char*)&buf2[0], (char*)&buf3[0]);
        }
        break;
    case 'b':
    case 'B':
        printf("    bridge               mac       port    nexthop\n");
        for (int i=0; i<bridge_table.size; i++) {
            struct bridge_entry *ntry = table_get(&bridge_table, i);
            put16msb(buf2, 0, ntry->mac1);
            put32msb(buf2, 2, ntry->mac2);
            mac2str(buf2, buf);
            printf("%10i %s %10i %10i\n", ntry->id, (char*)&buf[0], ntry->port, ntry->nexthop);
        }
        break;
    case 'q':
    case 'Q':
        printf("       vrf      meter dir       rate\n");
        for (int i=0; i<policer_table.size; i++) {
            struct policer_entry *ntry = table_get(&policer_table, i);
            printf("%10i %10i %3i %10li\n", ntry->vrf, ntry->meter, ntry->dir, ntry->allow);
        }
        break;
    case 'v':
    case 'V':
        printf("        id       vlan       port\n");
        for (int i=0; i<vlanin_table.size; i++) {
            struct vlan_entry *ntry = table_get(&vlanin_table, i);
            printf("%10i %10i %10i\n", ntry->id, ntry->vlan, ntry->port);
        }
        break;
    case '4':
        printf("            addr msk        vrf cmd    nexthop     label1     label2\n");
        doConsoleCommand_ipvX(&vrf2rib4_table, &doConsoleCommand_ipv4);
        break;
    case '6':
        printf("                                    addr msk        vrf cmd    nexthop     label1     label2\n");
        doConsoleCommand_ipvX(&vrf2rib6_table, &doConsoleCommand_ipv6);
        break;
#endif
    default:
        printf("unknown command '%s', try ?\n", buf);
        break;
    }
    return 0;
}
