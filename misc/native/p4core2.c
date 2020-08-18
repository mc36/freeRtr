#define accumulate_sum(sum, val, mul)                           \
    put32msb(buf2, 0, val);                                     \
    sum += mul*get16lsb(buf2, 0);                               \
    sum += mul*get16lsb(buf2, 2);


void str2mac(unsigned char *dst, unsigned char *src) {
    sscanf(src, "%hhx:%hhx:%hhx:%hhx:%hhx:%hhx", &dst[0], &dst[1], &dst[2], &dst[3], &dst[4], &dst[5]);
}

void mac2str(unsigned char *src, unsigned char *dst) {
    snprintf(dst, 128, "%02x:%02x:%02x:%02x:%02x:%02x", src[0], src[1], src[2], src[3], src[4], src[5]);
}

void readAcl4(struct acl4_entry *acl4_ntry, unsigned char**arg) {
    unsigned char buf2[1024];
    acl4_ntry->pri = atoi(arg[3]);
    acl4_ntry->act = strcmp(arg[4], "permit");
    acl4_ntry->protV = atoi(arg[5]);
    acl4_ntry->protM = atoi(arg[6]);
    inet_pton(AF_INET, arg[7], buf2);
    acl4_ntry->srcAddr = get32msb(buf2, 0);
    inet_pton(AF_INET, arg[8], buf2);
    acl4_ntry->srcMask = get32msb(buf2, 0);
    inet_pton(AF_INET, arg[9], buf2);
    acl4_ntry->trgAddr = get32msb(buf2, 0);
    inet_pton(AF_INET, arg[10], buf2);
    acl4_ntry->trgMask = get32msb(buf2, 0);
    acl4_ntry->srcPortV = atoi(arg[11]);
    acl4_ntry->srcPortM = atoi(arg[12]);
    acl4_ntry->trgPortV = atoi(arg[13]);
    acl4_ntry->trgPortM = atoi(arg[14]);
}

void readAcl6(struct acl6_entry *acl6_ntry, unsigned char**arg) {
    unsigned char buf2[1024];
    acl6_ntry->pri = atoi(arg[3]);
    acl6_ntry->act = strcmp(arg[4], "permit");
    acl6_ntry->protV = atoi(arg[5]);
    acl6_ntry->protM = atoi(arg[6]);
    inet_pton(AF_INET6, arg[7], buf2);
    acl6_ntry->srcAddr1 = get32msb(buf2, 0);
    acl6_ntry->srcAddr2 = get32msb(buf2, 4);
    acl6_ntry->srcAddr3 = get32msb(buf2, 8);
    acl6_ntry->srcAddr4 = get32msb(buf2, 12);
    inet_pton(AF_INET6, arg[8], buf2);
    acl6_ntry->srcMask1 = get32msb(buf2, 0);
    acl6_ntry->srcMask2 = get32msb(buf2, 4);
    acl6_ntry->srcMask3 = get32msb(buf2, 8);
    acl6_ntry->srcMask4 = get32msb(buf2, 12);
    inet_pton(AF_INET6, arg[9], buf2);
    acl6_ntry->trgAddr1 = get32msb(buf2, 0);
    acl6_ntry->trgAddr2 = get32msb(buf2, 4);
    acl6_ntry->trgAddr3 = get32msb(buf2, 8);
    acl6_ntry->trgAddr4 = get32msb(buf2, 12);
    inet_pton(AF_INET6, arg[10], buf2);
    acl6_ntry->trgMask1 = get32msb(buf2, 0);
    acl6_ntry->trgMask2 = get32msb(buf2, 4);
    acl6_ntry->trgMask3 = get32msb(buf2, 8);
    acl6_ntry->trgMask4 = get32msb(buf2, 12);
    acl6_ntry->srcPortV = atoi(arg[11]);
    acl6_ntry->srcPortM = atoi(arg[12]);
    acl6_ntry->trgPortV = atoi(arg[13]);
    acl6_ntry->trgPortM = atoi(arg[14]);
}

int doOneCommand(unsigned char* buf) {
    unsigned char buf2[1024];
    unsigned char* arg[128];
    int cnt;
    cnt = 0;
    arg[0] = &buf[0];
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
                arg[cnt] = &buf[i + 1];
                break;
        }
        if (o > 0) break;
        i++;
    }
    printf("rx: ");
    for (int i=0; i < cnt; i++) printf("'%s' ",arg[i]);
    printf("\n");
    int del = strcmp(arg[1], "del");
    struct mpls_entry mpls_ntry;
    memset(&mpls_ntry, 0, sizeof(mpls_ntry));
    struct portvrf_entry portvrf_ntry;
    memset(&portvrf_ntry, 0, sizeof(portvrf_ntry));
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
    int index = 0;
    if (strcmp(arg[0], "quit") == 0) {
        return 1;
    }
    if (strcmp(arg[0], "state") == 0) {
        i = atoi(arg[1]);
        o = atoi(arg[2]);
        if (i >= ports) return 0;
        setState(i, o);
        return 0;
    }
    if (strcmp(arg[0], "mylabel4") == 0) {
        mpls_ntry.label = atoi(arg[2]);
        mpls_ntry.vrf = atoi(arg[3]);
        mpls_ntry.ver = 4;
        mpls_ntry.command = 1;
        if (del == 0) table_del(&mpls_table, &mpls_ntry); else table_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "mylabel6") == 0) {
        mpls_ntry.label = atoi(arg[2]);
        mpls_ntry.vrf = atoi(arg[3]);
        mpls_ntry.ver = 6;
        mpls_ntry.command = 1;
        if (del == 0) table_del(&mpls_table, &mpls_ntry); else table_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "unlabel4") == 0) {
        mpls_ntry.label = atoi(arg[2]);
        mpls_ntry.nexthop = atoi(arg[3]);
        mpls_ntry.ver = 4;
        mpls_ntry.command = 2;
        if (del == 0) table_del(&mpls_table, &mpls_ntry); else table_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "unlabel6") == 0) {
        mpls_ntry.label = atoi(arg[2]);
        mpls_ntry.nexthop = atoi(arg[3]);
        mpls_ntry.ver = 6;
        mpls_ntry.command = 2;
        if (del == 0) table_del(&mpls_table, &mpls_ntry); else table_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "label4") == 0) {
        mpls_ntry.label = atoi(arg[2]);
        mpls_ntry.nexthop = atoi(arg[3]);
        mpls_ntry.swap = atoi(arg[5]);
        mpls_ntry.ver = 4;
        mpls_ntry.command = 3;
        if (del == 0) table_del(&mpls_table, &mpls_ntry); else table_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "label6") == 0) {
        mpls_ntry.label = atoi(arg[2]);
        mpls_ntry.nexthop = atoi(arg[3]);
        mpls_ntry.swap = atoi(arg[5]);
        mpls_ntry.ver = 6;
        mpls_ntry.command = 3;
        if (del == 0) table_del(&mpls_table, &mpls_ntry); else table_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "portvrf") == 0) {
        portvrf_ntry.command = 1;
        portvrf_ntry.port = atoi(arg[2]);
        portvrf_ntry.vrf = atoi(arg[3]);
        if (del == 0) table_del(&portvrf_table, &portvrf_ntry); else table_add(&portvrf_table, &portvrf_ntry);
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
        if (del == 0) table_del(&portvrf_table, &portvrf_ntry); else table_add(&portvrf_table, &portvrf_ntry);
        if (del == 0) table_del(&mpls_table, &mpls_ntry); else table_add(&mpls_table, &mpls_ntry);
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
        if (del == 0) table_del(&bridge_table, &bridge_ntry); else table_add(&bridge_table, &bridge_ntry);
        return 0;
    }
    if (strcmp(arg[0], "bridgelabel") == 0) {
        mpls_ntry.label = atoi(arg[3]);
        mpls_ntry.bridge = atoi(arg[2]);
        mpls_ntry.command = 5;
        if (del == 0) table_del(&mpls_table, &mpls_ntry); else table_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "portbridge") == 0) {
        portvrf_ntry.command = 2;
        portvrf_ntry.port = atoi(arg[2]);
        portvrf_ntry.bridge = atoi(arg[3]);
        if (del == 0) table_del(&portvrf_table, &portvrf_ntry); else table_add(&portvrf_table, &portvrf_ntry);
        return 0;
    }
    if (strcmp(arg[0], "bridgemac") == 0) {
        bridge_ntry.id = atoi(arg[2]);
        str2mac(buf2, arg[3]);
        bridge_ntry.mac1 = get16msb(buf2, 0);
        bridge_ntry.mac2 = get32msb(buf2, 2);
        bridge_ntry.port = atoi(arg[4]);
        bridge_ntry.command = 1;
        if (del == 0) table_del(&bridge_table, &bridge_ntry); else table_add(&bridge_table, &bridge_ntry);
        return 0;
    }
    if (strcmp(arg[0], "portvlan") == 0) {
        vlan_ntry.id = atoi(arg[2]);
        vlan_ntry.port = atoi(arg[3]);
        vlan_ntry.vlan = atoi(arg[4]);
        if (del == 0) table_del(&vlanin_table, &vlan_ntry); else table_add(&vlanin_table, &vlan_ntry);
        if (del == 0) table_del(&vlanout_table, &vlan_ntry); else table_add(&vlanout_table, &vlan_ntry);
        return 0;
    }
    if (strcmp(arg[0], "myaddr4") == 0) {
        inet_pton(AF_INET, arg[2], buf2);
        route4_ntry.addr = get32msb(buf2, 0);
        route4_ntry.mask = atoi(arg[3]);
        route4_ntry.vrf = atoi(arg[5]);
        route4_ntry.command = 2;
        if (del == 0) table_del(&route4_table, &route4_ntry); else table_add(&route4_table, &route4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "route4") == 0) {
        inet_pton(AF_INET, arg[2], buf2);
        route4_ntry.addr = get32msb(buf2, 0);
        route4_ntry.mask = atoi(arg[3]);
        route4_ntry.nexthop = atoi(arg[4]);
        route4_ntry.vrf = atoi(arg[6]);
        route4_ntry.command = 1;
        if (del == 0) table_del(&route4_table, &route4_ntry); else table_add(&route4_table, &route4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "labroute4") == 0) {
        inet_pton(AF_INET, arg[2], buf2);
        route4_ntry.addr = get32msb(buf2, 0);
        route4_ntry.mask = atoi(arg[3]);
        route4_ntry.nexthop = atoi(arg[4]);
        route4_ntry.vrf = atoi(arg[6]);
        route4_ntry.label1 = atoi(arg[7]);
        route4_ntry.command = 3;
        if (del == 0) table_del(&route4_table, &route4_ntry); else table_add(&route4_table, &route4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "vpnroute4") == 0) {
        inet_pton(AF_INET, arg[2], buf2);
        route4_ntry.addr = get32msb(buf2, 0);
        route4_ntry.mask = atoi(arg[3]);
        route4_ntry.nexthop = atoi(arg[4]);
        route4_ntry.vrf = atoi(arg[6]);
        route4_ntry.label1 = atoi(arg[7]);
        route4_ntry.label2 = atoi(arg[8]);
        route4_ntry.command = 4;
        if (del == 0) table_del(&route4_table, &route4_ntry); else table_add(&route4_table, &route4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "neigh4") == 0) {
        route4_ntry.nexthop = atoi(arg[2]);
        inet_pton(AF_INET, arg[3], buf2);
        route4_ntry.addr = get32msb(buf2, 0);
        route4_ntry.mask = 32;
        route4_ntry.vrf = atoi(arg[5]);
        route4_ntry.command = 1;
        neigh_ntry.id = route4_ntry.nexthop;
        neigh_ntry.vrf = route4_ntry.vrf;
        neigh_ntry.aclport = neigh_ntry.port = atoi(arg[7]);
        neigh_ntry.command = 1;
        str2mac(neigh_ntry.dmac, arg[4]);
        str2mac(neigh_ntry.smac, arg[6]);
        if (del == 0) table_del(&route4_table, &route4_ntry); else table_add(&route4_table, &route4_ntry);
        if (del == 0) table_del(&neigh_table, &neigh_ntry); else table_add(&neigh_table, &neigh_ntry);
        return 0;
    }
    if (strcmp(arg[0], "myaddr6") == 0) {
        inet_pton(AF_INET6, arg[2], buf);
        route6_ntry.addr1 = get32msb(buf, 0);
        route6_ntry.addr2 = get32msb(buf, 4);
        route6_ntry.addr3 = get32msb(buf, 8);
        route6_ntry.addr4 = get32msb(buf, 12);
        route6_ntry.mask = atoi(arg[3]);
        route6_ntry.vrf = atoi(arg[5]);
        route6_ntry.command = 2;
        if (del == 0) table_del(&route6_table, &route6_ntry); else table_add(&route6_table, &route6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "route6") == 0) {
        inet_pton(AF_INET6, arg[2], buf2);
        route6_ntry.addr1 = get32msb(buf2, 0);
        route6_ntry.addr2 = get32msb(buf2, 4);
        route6_ntry.addr3 = get32msb(buf2, 8);
        route6_ntry.addr4 = get32msb(buf2, 12);
        route6_ntry.mask = atoi(arg[3]);
        route6_ntry.nexthop = atoi(arg[4]);
        route6_ntry.vrf = atoi(arg[6]);
        route6_ntry.command = 1;
        if (del == 0) table_del(&route6_table, &route6_ntry); else table_add(&route6_table, &route6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "labroute6") == 0) {
        inet_pton(AF_INET6, arg[2], buf2);
        route6_ntry.addr1 = get32msb(buf2, 0);
        route6_ntry.addr2 = get32msb(buf2, 4);
        route6_ntry.addr3 = get32msb(buf2, 8);
        route6_ntry.addr4 = get32msb(buf2, 12);
        route6_ntry.mask = atoi(arg[3]);
        route6_ntry.nexthop = atoi(arg[4]);
        route6_ntry.vrf = atoi(arg[6]);
        route6_ntry.label1 = atoi(arg[7]);
        route6_ntry.command = 3;
        if (del == 0) table_del(&route6_table, &route6_ntry); else table_add(&route6_table, &route6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "vpnroute6") == 0) {
        inet_pton(AF_INET6, arg[2], buf2);
        route6_ntry.addr1 = get32msb(buf2, 0);
        route6_ntry.addr2 = get32msb(buf2, 4);
        route6_ntry.addr3 = get32msb(buf2, 8);
        route6_ntry.addr4 = get32msb(buf2, 12);
        route6_ntry.mask = atoi(arg[3]);
        route6_ntry.nexthop = atoi(arg[4]);
        route6_ntry.vrf = atoi(arg[6]);
        route6_ntry.label1 = atoi(arg[7]);
        route6_ntry.label2 = atoi(arg[8]);
        route6_ntry.command = 4;
        if (del == 0) table_del(&route6_table, &route6_ntry); else table_add(&route6_table, &route6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "neigh6") == 0) {
        route6_ntry.nexthop = atoi(arg[2]);
        inet_pton(AF_INET6, arg[3], buf2);
        route6_ntry.addr1 = get32msb(buf2, 0);
        route6_ntry.addr2 = get32msb(buf2, 4);
        route6_ntry.addr3 = get32msb(buf2, 8);
        route6_ntry.addr4 = get32msb(buf2, 12);
        route6_ntry.mask = 128;
        route6_ntry.vrf = atoi(arg[5]);
        route6_ntry.command = 1;
        neigh_ntry.id = route6_ntry.nexthop;
        neigh_ntry.vrf = route6_ntry.vrf;
        neigh_ntry.aclport = neigh_ntry.port = atoi(arg[7]);
        neigh_ntry.command = 1;
        str2mac(neigh_ntry.dmac, arg[4]);
        str2mac(neigh_ntry.smac, arg[6]);
        if (del == 0) table_del(&route6_table, &route6_ntry); else table_add(&route6_table, &route6_ntry);
        if (del == 0) table_del(&neigh_table, &neigh_ntry); else table_add(&neigh_table, &neigh_ntry);
        return 0;
    }
    if (strcmp(arg[0], "inacl4") == 0) {
        acls_ntry.dir = 1;
        acls_ntry.ver = 4;
        acls_ntry.port = atoi(arg[2]);
        index = table_find(&acls_table, &acls_ntry);
        if (index < 0) {
            table_init(&acls_ntry.aces, sizeof(struct acl4_entry), &acl4_compare);
            table_add(&acls_table, &acls_ntry);
            acls_res = table_get(&acls_table, table_find(&acls_table, &acls_ntry));
        } else {
            acls_res = table_get(&acls_table, index);
        }
        readAcl4(&acl4_ntry, &arg[0]);
        if (del == 0) table_del(&acls_res->aces, &acl4_ntry); else table_add(&acls_res->aces, &acl4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "outacl4") == 0) {
        acls_ntry.dir = 2;
        acls_ntry.ver = 4;
        acls_ntry.port = atoi(arg[2]);
        index = table_find(&acls_table, &acls_ntry);
        if (index < 0) {
            table_init(&acls_ntry.aces, sizeof(struct acl4_entry), &acl4_compare);
            table_add(&acls_table, &acls_ntry);
            acls_res = table_get(&acls_table, table_find(&acls_table, &acls_ntry));
        } else {
            acls_res = table_get(&acls_table, index);
        }
        readAcl4(&acl4_ntry, &arg[0]);
        if (del == 0) table_del(&acls_res->aces, &acl4_ntry); else table_add(&acls_res->aces, &acl4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "natcfg4") == 0) {
        acls_ntry.dir = 3;
        acls_ntry.ver = 4;
        acls_ntry.port = atoi(arg[2]);
        index = table_find(&acls_table, &acls_ntry);
        if (index < 0) {
            table_init(&acls_ntry.aces, sizeof(struct acl4_entry), &acl4_compare);
            table_add(&acls_table, &acls_ntry);
            acls_res = table_get(&acls_table, table_find(&acls_table, &acls_ntry));
        } else {
            acls_res = table_get(&acls_table, index);
        }
        readAcl4(&acl4_ntry, &arg[0]);
        if (del == 0) table_del(&acls_res->aces, &acl4_ntry); else table_add(&acls_res->aces, &acl4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "copp4") == 0) {
        acls_ntry.dir = 4;
        acls_ntry.ver = 4;
        acls_ntry.port = 0;
        index = table_find(&acls_table, &acls_ntry);
        if (index < 0) {
            table_init(&acls_ntry.aces, sizeof(struct acl4_entry), &acl4_compare);
            table_add(&acls_table, &acls_ntry);
            acls_res = table_get(&acls_table, table_find(&acls_table, &acls_ntry));
        } else {
            acls_res = table_get(&acls_table, index);
        }
        readAcl4(&acl4_ntry, &arg[-1]);
        if (del == 0) table_del(&acls_res->aces, &acl4_ntry); else table_add(&acls_res->aces, &acl4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "inacl6") == 0) {
        acls_ntry.dir = 1;
        acls_ntry.ver = 6;
        acls_ntry.port = atoi(arg[2]);
        index = table_find(&acls_table, &acls_ntry);
        if (index < 0) {
            table_init(&acls_ntry.aces, sizeof(struct acl6_entry), &acl6_compare);
            table_add(&acls_table, &acls_ntry);
            acls_res = table_get(&acls_table, table_find(&acls_table, &acls_ntry));
        } else {
            acls_res = table_get(&acls_table, index);
        }
        readAcl6(&acl6_ntry, &arg[0]);
        if (del == 0) table_del(&acls_res->aces, &acl6_ntry); else table_add(&acls_res->aces, &acl6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "outacl6") == 0) {
        acls_ntry.dir = 2;
        acls_ntry.ver = 6;
        acls_ntry.port = atoi(arg[2]);
        index = table_find(&acls_table, &acls_ntry);
        if (index < 0) {
            table_init(&acls_ntry.aces, sizeof(struct acl6_entry), &acl6_compare);
            table_add(&acls_table, &acls_ntry);
            acls_res = table_get(&acls_table, table_find(&acls_table, &acls_ntry));
        } else {
            acls_res = table_get(&acls_table, index);
        }
        readAcl6(&acl6_ntry, &arg[0]);
        if (del == 0) table_del(&acls_res->aces, &acl6_ntry); else table_add(&acls_res->aces, &acl6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "natcfg6") == 0) {
        acls_ntry.dir = 3;
        acls_ntry.ver = 6;
        acls_ntry.port = atoi(arg[2]);
        index = table_find(&acls_table, &acls_ntry);
        if (index < 0) {
            table_init(&acls_ntry.aces, sizeof(struct acl6_entry), &acl6_compare);
            table_add(&acls_table, &acls_ntry);
            acls_res = table_get(&acls_table, table_find(&acls_table, &acls_ntry));
        } else {
            acls_res = table_get(&acls_table, index);
        }
        readAcl6(&acl6_ntry, &arg[0]);
        if (del == 0) table_del(&acls_res->aces, &acl6_ntry); else table_add(&acls_res->aces, &acl6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "copp6") == 0) {
        acls_ntry.dir = 4;
        acls_ntry.ver = 6;
        acls_ntry.port = 0;
        index = table_find(&acls_table, &acls_ntry);
        if (index < 0) {
            table_init(&acls_ntry.aces, sizeof(struct acl6_entry), &acl6_compare);
            table_add(&acls_table, &acls_ntry);
            acls_res = table_get(&acls_table, table_find(&acls_table, &acls_ntry));
        } else {
            acls_res = table_get(&acls_table, index);
        }
        readAcl6(&acl6_ntry, &arg[-1]);
        if (del == 0) table_del(&acls_res->aces, &acl6_ntry); else table_add(&acls_res->aces, &acl6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "nattrns4") == 0) {
        nat4_ntry.vrf = atoi(arg[2]);
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
        if (del == 0) table_del(&nat4_table, &nat4_ntry); else table_add(&nat4_table, &nat4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "nattrns6") == 0) {
        nat6_ntry.vrf = atoi(arg[2]);
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
        if (del == 0) table_del(&nat6_table, &nat6_ntry); else table_add(&nat6_table, &nat6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "hairpin") == 0) {
        bundle_ntry.id = atoi(arg[2]);
        bundle_ntry.command = 2;
        o = atoi(arg[3]);
        for (int i = 0; i < 16; i++) bundle_ntry.out[i] = o;
        if (del == 0) table_del(&bundle_table, &bundle_ntry); else table_add(&bundle_table, &bundle_ntry);
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
        if (del == 0) table_del(&vlanin_table, &vlan_ntry); else table_add(&vlanin_table, &vlan_ntry);
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
        if (del == 0) table_del(&pppoe_table, &pppoe_ntry); else table_add(&pppoe_table, &pppoe_ntry);
        if (del == 0) table_del(&neigh_table, &neigh_ntry); else table_add(&neigh_table, &neigh_ntry);
        return 0;
    }
    return 0;
}






void doReportRount(FILE *commands) {
    unsigned char buf[1024];
    unsigned char buf2[1024];
    unsigned char buf3[1024];
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
        fprintf(commands, "bridge_cnt %i %s %li %li\r\n", ntry->id, &buf, ntry->pack, ntry->byte);
    }
    for (int i=0; i<route4_table.size; i++) {
        struct route4_entry *ntry = table_get(&route4_table, i);
        put32msb(buf2, 0, ntry->addr);
        inet_ntop(AF_INET, &buf2[0], &buf[0], sizeof(buf));
        fprintf(commands, "route4_cnt %i %s %i %li %li\r\n", ntry->vrf, &buf, ntry->mask, ntry->pack, ntry->byte);
    }
    for (int i=0; i<route6_table.size; i++) {
        struct route6_entry *ntry = table_get(&route6_table, i);
        put32msb(buf2, 0, ntry->addr1);
        put32msb(buf2, 4, ntry->addr2);
        put32msb(buf2, 8, ntry->addr3);
        put32msb(buf2, 12, ntry->addr4);
        inet_ntop(AF_INET6, &buf2[0], &buf[0], sizeof(buf));
        fprintf(commands, "route6_cnt %i %s %i %li %li\r\n", ntry->vrf, &buf, ntry->mask, ntry->pack, ntry->byte);
    }
    for (int i=0; i<nat4_table.size; i++) {
        struct nat4_entry *ntry = table_get(&nat4_table, i);
        put32msb(buf, 0, ntry->oSrcAddr);
        inet_ntop(AF_INET, &buf[0], &buf2[0], sizeof(buf2));
        put32msb(buf, 0, ntry->oTrgAddr);
        inet_ntop(AF_INET, &buf[0], &buf3[0], sizeof(buf3));
        fprintf(commands, "nattrns4_cnt %i %i %s %s %i %i %li %li\r\n", ntry->vrf, ntry->prot, &buf2, &buf3, ntry->oSrcPort, ntry->oTrgPort, ntry->pack, ntry->byte);
    }
    for (int i=0; i<nat6_table.size; i++) {
        struct nat6_entry *ntry = table_get(&nat6_table, i);
        put32msb(buf, 0, ntry->oSrcAddr1);
        put32msb(buf, 4, ntry->oSrcAddr2);
        put32msb(buf, 8, ntry->oSrcAddr3);
        put32msb(buf, 12, ntry->oSrcAddr4);
        inet_ntop(AF_INET6, &buf[0], &buf2[0], sizeof(buf2));
        put32msb(buf, 0, ntry->oTrgAddr1);
        put32msb(buf, 4, ntry->oTrgAddr2);
        put32msb(buf, 8, ntry->oTrgAddr3);
        put32msb(buf, 12, ntry->oTrgAddr4);
        inet_ntop(AF_INET6, &buf[0], &buf3[0], sizeof(buf3));
        fprintf(commands, "nattrns6_cnt %i %i %s %s %i %i %li %li\r\n", ntry->vrf, ntry->prot, &buf2, &buf3, ntry->oSrcPort, ntry->oTrgPort, ntry->pack, ntry->byte);
    }
    fflush(commands);
}



void doStatRount(FILE *commands) {
    punts = 10;
    for (int i = 0; i < ports; i++) {
        fprintf(commands, "counter %i %li %li %li %li %li %li\r\n", i, packRx[i], byteRx[i], packTx[i], byteTx[i], packDr[i], byteDr[i]);
        int o = getState(i);
        fprintf(commands, "state %i %i\r\n", i, o);
    }
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
    fflush(commands);
}





int doConsoleCommand(unsigned char*buf) {
    unsigned char buf2[1024];
    switch (buf[0]) {
        case 0:
            break;
        case 'H':
        case 'h':
        case '?':
            printf("commands:\n");
            printf("h - this help\n");
            printf("q - exit process\n");
            printf("i - interface counters\n");
            printf("p - display portvrf table\n");
            printf("b - display bridge table\n");
            printf("m - display mpls table\n");
            printf("4 - display ipv4 table\n");
            printf("6 - display ipv6 table\n");
            printf("n - display nexthop table\n");
            printf("a - display acl table\n");
            printf("v - display vlan table\n");
            break;
        case 'Q':
        case 'q':
            return 1;
            break;
        case 'i':
        case 'I':
            printf("                           iface         rx         tx       drop         rx         tx       drop\n");
            for (int i=0; i<ports; i++) {
                printf("%32s %10li %10li %10li %10li %10li %10li\n", ifaceName[i], packRx[i], packTx[i], packDr[i], byteRx[i], byteTx[i], byteDr[i]);
            }
            break;
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
            for (int i=0; i<acls_table.size; i++) {
                struct acls_entry *ntry = table_get(&acls_table, i);
                printf("%10i %3i %3i %10i\n", ntry->port, ntry->dir, ntry->ver, ntry->aces.size);
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
                mac2str(ntry->smac, buf);
                mac2str(ntry->dmac, buf2);
                printf("%10i %10i %10i %s %s\n", ntry->id, ntry->vrf, ntry->port, &buf, &buf2);
            }
            break;
        case 'b':
        case 'B':
            printf("    bridge               mac       port\n");
            for (int i=0; i<bridge_table.size; i++) {
                struct bridge_entry *ntry = table_get(&bridge_table, i);
                put16msb(buf2, 0, ntry->mac1);
                put32msb(buf2, 2, ntry->mac2);
                mac2str(buf2, buf);
                printf("%10i %s %10i\n", ntry->id, buf, ntry->port);
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
            for (int i=0; i<route4_table.size; i++) {
                struct route4_entry *ntry = table_get(&route4_table, i);
                put32msb(buf2, 0, ntry->addr);
                inet_ntop(AF_INET, &buf2[0], &buf[0], sizeof(buf));
                printf("%16s %3i %10i %3i %10i %10i %10i\n", &buf, ntry->mask, ntry->vrf, ntry->command, ntry->nexthop, ntry->label1, ntry->label2);
            }
            break;
        case '6':
            printf("                                    addr msk        vrf cmd    nexthop     label1     label2\n");
            for (int i=0; i<route6_table.size; i++) {
                struct route6_entry *ntry = table_get(&route6_table, i);
                put32msb(buf2, 0, ntry->addr1);
                put32msb(buf2, 4, ntry->addr2);
                put32msb(buf2, 8, ntry->addr3);
                put32msb(buf2, 12, ntry->addr4);
                inet_ntop(AF_INET6, &buf2[0], &buf[0], sizeof(buf));
                printf("%40s %3i %10i %3i %10i %10i %10i\n", &buf, ntry->mask, ntry->vrf, ntry->command, ntry->nexthop, ntry->label1, ntry->label2);
            }
            break;
        default:
            printf("unknown command '%s', try ?\n", buf);
            break;
    }
    return 0;
}
