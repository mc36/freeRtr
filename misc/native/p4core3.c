void send2port(unsigned char *bufD, int bufS, int port) {
    sendpack(bufD, bufS, port);
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
    if (prt >= ports) return;
    send2port(&bufD[preBuff + 2], bufS - 2, prt);
    if (get16msb(bufD, preBuff + 14) != 0x8100) return;
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


void processDataPacket(unsigned char *bufD, int bufS, int port) {
    struct vlan_entry vlan_ntry;
    struct vlan_entry *vlan_res;
    int index;
    packRx[port]++;
    byteRx[port] += bufS;
    send2cpu(bufD, bufS, port);
    if (get16msb(bufD, preBuff + 12) != 0x8100) return;
    vlan_ntry.port = port;
    vlan_ntry.vlan = get16msb(bufD, preBuff + 14) & 0xfff;
    index = table_find(&vlanin_table, &vlan_ntry);
    if (index < 0) return;
    vlan_res = table_get(&vlanin_table, index);
    vlan_res->pack++;
    vlan_res->byte += bufS;
}



#else



#define extract_layer4(ntry)                                    \
    switch (ntry.protV) {                                       \
        case 6:                                                 \
            ntry.srcPortV = get16msb(bufD, bufT + 0);           \
            ntry.trgPortV = get16msb(bufD, bufT + 2);           \
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


#define update_chksum(ofs, val)                                 \
    sum = get16lsb(bufD, ofs);                                  \
    sum -= val;                                                 \
    sum = (sum & 0xffff) + (sum >> 16);                         \
    put16lsb(bufD, ofs, sum);



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


#define reduce_hash                                             \
    hash = ((hash >> 16) ^ hash) & 0xffff;                      \
    hash = ((hash >> 8) ^ hash) & 0xff;                         \
    hash = ((hash >> 4) ^ hash) & 0xf;


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


int calcIPsum(unsigned char *buf, int pos, int len) {
    int sum = 0;
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



void processDataPacket(unsigned char *bufD, int bufS, int port) {
    packRx[port]++;
    byteRx[port] += bufS;
    unsigned char buf2[preBuff];
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
    struct bundle_entry bundle_ntry;
    struct pppoe_entry pppoe_ntry;
    struct tun4_entry tun4_ntry;
    struct tun6_entry tun6_ntry;
    struct mpls_entry *mpls_res;
    struct portvrf_entry *portvrf_res;
    struct route4_entry *route4_res;
    struct route6_entry *route6_res;
    struct neigh_entry *neigh_res;
    struct vlan_entry *vlan_res;
    struct bridge_entry *bridge_res;
    struct acls_entry *acls_res;
    struct nat4_entry *nat4_res;
    struct nat6_entry *nat6_res;
    struct bundle_entry *bundle_res;
    struct pppoe_entry *pppoe_res;
    struct tun4_entry *tun4_res;
    struct tun6_entry *tun6_res;
    int index;
    int label;
    int sum;
    int ttl;
    int hash = 0;
    int bufP;
    int bufT;
    int ethtyp;
    int prt = port;
    int prt2 = port;
ether_rx:
    bufP = preBuff;
    bufP += 6 * 2; // dmac, smac
ethtyp_rx:
    ethtyp = get16msb(bufD, bufP);
    bufP += 2;
    switch (ethtyp) {
    case 0x8847:
mpls_rx:
        label = get32msb(bufD, bufP);
        ttl = (label & 0xff) - 1;
        if (ttl <= 1) goto punt;
        bufP += 4;
        mpls_ntry.label = (label >> 12) & 0xfffff;
        hash ^= mpls_ntry.label;
        index = table_find(&mpls_table, &mpls_ntry);
        if (index < 0) {
            packDr[port]++;
            byteDr[port] += bufS;
            return;
        }
        mpls_res = table_get(&mpls_table, index);
        mpls_res->pack++;
        mpls_res->byte += bufS;
        switch (mpls_res->command) {
        case 1: // route
            route4_ntry.vrf = mpls_res->vrf;
            route6_ntry.vrf = mpls_res->vrf;
            if ((label & 0x100) == 0) goto mpls_rx;
            switch (mpls_res->ver) {
            case 4:
                ethtyp = 0x800;
                goto ipv4_rx;
            case 6:
                ethtyp = 0x86dd;
                goto ipv6_rx;
            default:
                ethtyp = 0;
                break;
            }
            packDr[port]++;
            byteDr[port] += bufS;
            return;
        case 2: // pop
            neigh_ntry.id = mpls_res->nexthop;
            if ((label & 0x100) == 0) goto ethtyp_tx;
            switch (mpls_res->ver) {
            case 4:
                ethtyp = 0x800;
                break;
            case 6:
                ethtyp = 0x86dd;
                break;
            default:
                ethtyp = 0;
                break;
            }
            goto ethtyp_tx;
            return;
        case 3: // swap
            bufP -= 4;
            label = (label & 0xf00) | ttl | (mpls_res->swap << 12);
            put32msb(bufD, bufP, label);
            neigh_ntry.id = mpls_res->nexthop;
ethtyp_tx:
            bufP -= 2;
            put16msb(bufD, bufP, ethtyp);
            index = table_find(&neigh_table, &neigh_ntry);
            if (index < 0) {
                packDr[port]++;
                byteDr[port] += bufS;
                return;
            }
            neigh_res = table_get(&neigh_table, index);
neigh_tx:
            neigh_res->pack++;
            neigh_res->byte += bufS;
            prt = neigh_res->port;
            switch (neigh_res->command) {
            case 1: // raw ip
                break;
            case 2: // pppoe
                switch (ethtyp) {
                case 0x800:
                    index = 0x0021;
                    break;
                case 0x86dd:
                    index = 0x0057;
                    break;
                case 0x8847:
                    index = 0x0281;
                    break;
                default:
                    packDr[port]++;
                    byteDr[port] += bufS;
                    return;
                }
                put16msb(bufD, bufP, index);
                index = bufS - bufP + preBuff;
                bufP -= 6;
                put16msb(bufD, bufP + 0, 0x1100);
                put16msb(bufD, bufP + 2, neigh_res->session);
                put16msb(bufD, bufP + 4, index);
                ethtyp = 0x8864;
                bufP -= 2;
                put16msb(bufD, bufP, ethtyp);
                break;
            case 3: // gre4
                bufP -= 2;
                put16msb(bufD, bufP, 0x0000); // gre header
                bufP -= 20;
                put16msb(bufD, bufP + 0, 0x4500); // ip ver, hdrlen, tos
                put16msb(bufD, bufP + 2, bufS - bufP + preBuff); // total length
                ipids++;
                put16msb(bufD, bufP + 4, ipids); // identify
                put16msb(bufD, bufP + 6, 0); // fragment
                put16msb(bufD, bufP + 8, 0xff2f); // ttl, protocol
                put16msb(bufD, bufP + 10, 0); // checksum
                put32msb(bufD, bufP + 12, neigh_res->sip1); // source
                put32msb(bufD, bufP + 16, neigh_res->dip1); // target
                put16lsb(bufD, bufP + 10, 0xffff - calcIPsum(bufD, bufP, 20));
                ethtyp = 0x800;
                bufP -= 2;
                put16msb(bufD, bufP, ethtyp);
                break;
            case 4: // gre6
                bufP -= 2;
                put16msb(bufD, bufP, 0x0000); // gre header
                bufP -= 40;
                put16msb(bufD, bufP + 0, 0x6000); // ip ver, tos
                put16msb(bufD, bufP + 2, 0); // flow label
                put16msb(bufD, bufP + 4, bufS - bufP + preBuff - 40); // payload length
                put16msb(bufD, bufP + 6, 0x2fff); // protocol, ttl
                put32msb(bufD, bufP + 8, neigh_res->sip1); // source
                put32msb(bufD, bufP + 12, neigh_res->sip2); // source
                put32msb(bufD, bufP + 16, neigh_res->sip3); // source
                put32msb(bufD, bufP + 20, neigh_res->sip4); // source
                put32msb(bufD, bufP + 24, neigh_res->dip1); // target
                put32msb(bufD, bufP + 28, neigh_res->dip2); // target
                put32msb(bufD, bufP + 32, neigh_res->dip3); // target
                put32msb(bufD, bufP + 36, neigh_res->dip4); // target
                ethtyp = 0x86dd;
                bufP -= 2;
                put16msb(bufD, bufP, ethtyp);
                break;
            default:
                packDr[port]++;
                byteDr[port] += bufS;
                return;
            }
            vlan_ntry.id = prt;
            index = table_find(&vlanout_table, &vlan_ntry);
            if (index >= 0) {
                vlan_res = table_get(&vlanout_table, index);
                bufP -= 2;
                put16msb(bufD, bufP, vlan_res->vlan);
                bufP -= 2;
                put16msb(bufD, bufP, 0x8100);
                prt = vlan_res->port;
                vlan_res->pack++;
                vlan_res->byte += bufS;
            }
            bufP -= 6;
            memmove(&bufD[bufP], &neigh_res->smac, 6);
            bufP -= 6;
            memmove(&bufD[bufP], &neigh_res->dmac, 6);
            bundle_ntry.id = prt;
            index = table_find(&bundle_table, &bundle_ntry);
            if (index >= 0) {
                bundle_res = table_get(&bundle_table, index);
                reduce_hash;
                prt = bundle_res->out[hash];
                bundle_res->pack++;
                bundle_res->byte += bufS;
                if (bundle_res->command == 2) {
                    bufS = bufS - bufP + preBuff;
                    memmove(&bufD[preBuff], &bufD[bufP], bufS);
                    prt2 = prt;
                    goto ether_rx;
                }
            }
            if (prt >= ports) {
                packDr[port]++;
                byteDr[port] += bufS;
                return;
            }
            send2port(&bufD[bufP], bufS - bufP + preBuff, prt);
            return;
        case 4: // xconn
            memmove(&buf2[0], &bufD[bufP], 12);
            bufP += 12;
            prt = mpls_res->port;
            goto layer2_tx;
        case 5: // vpls
            memmove(&buf2[0], &bufD[bufP], 12);
            bufP += 12;
            bufP += 2;
            goto bridgevpls_rx;
        case 6: // punt
            goto cpu;
        default:
            return;
        }
        return;
    case 0x8100:
        vlan_ntry.port = prt;
        vlan_ntry.vlan = get16msb(bufD, bufP) & 0xfff;
        bufP += 2;
        index = table_find(&vlanin_table, &vlan_ntry);
        if (index < 0) {
            packDr[port]++;
            byteDr[port] += bufS;
            return;
        }
        vlan_res = table_get(&vlanin_table, index);
        prt = vlan_res->id;
        vlan_res->pack++;
        vlan_res->byte += bufS;
        goto ethtyp_rx;
        return;
    case 0x800:
        portvrf_ntry.port = prt;
        index = table_find(&portvrf_table, &portvrf_ntry);
        if (index < 0) {
            packDr[port]++;
            byteDr[port] += bufS;
            return;
        }
        portvrf_res = table_get(&portvrf_table, index);
        switch (portvrf_res->command) {
        case 1:
            break;
        case 2:
            goto bridge_rx;
        case 3:
            goto xconn_rx;
        default:
            return;
        }
        route4_ntry.vrf = portvrf_res->vrf;
ipv4_rx:
        acl4_ntry.protV = bufD[bufP + 9];
        acl4_ntry.srcAddr = get32msb(bufD, bufP + 12);
        acl4_ntry.trgAddr = route4_ntry.addr = get32msb(bufD, bufP + 16);
        hash ^= acl4_ntry.srcAddr ^ acl4_ntry.trgAddr;
        ttl = bufD[bufP + 8] - 1;
        if (ttl <= 1) goto punt;
        bufD[bufP + 8] = ttl;
        update_chksum(bufP + 10, -1);
        bufT = bufP + 20;
        extract_layer4(acl4_ntry);
        acls_ntry.ver = 4;
        acls_ntry.dir = 1;
        acls_ntry.port = prt;
        index = table_find(&acls_table, &acls_ntry);
        if (index >= 0) {
            acls_res = table_get(&acls_table, index);
            if (apply_acl(&acls_res->aces, &acl4_ntry, &acl4_matcher) != 0) goto punt;
        }
        acls_ntry.dir = 3;
        acls_ntry.port = route4_ntry.vrf;
        index = table_find(&acls_table, &acls_ntry);
        if (index >= 0) {
            acls_res = table_get(&acls_table, index);
            nat4_ntry.vrf = route4_ntry.vrf;
            nat4_ntry.prot = acl4_ntry.protV;
            nat4_ntry.oSrcAddr = acl4_ntry.srcAddr;
            nat4_ntry.oTrgAddr = acl4_ntry.trgAddr;
            nat4_ntry.oSrcPort = acl4_ntry.srcPortV;
            nat4_ntry.oTrgPort = acl4_ntry.trgPortV;
            index = table_find(&nat4_table, &nat4_ntry);
            if (index < 0) {
                if (apply_acl(&acls_res->aces, &acl4_ntry, &acl4_matcher) == 0) goto cpu;
                goto ipv4_rou;
            }
            nat4_res = table_get(&nat4_table, index);
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
        }
ipv4_rou:
        for (int i = 32; i >= 0; i--) {
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
                bufP -= 2;
                put16msb(bufD, bufP, ethtyp);
                index = table_find(&neigh_table, &neigh_ntry);
                if (index < 0) {
                    packDr[port]++;
                    byteDr[port] += bufS;
                    return;
                }
                neigh_res = table_get(&neigh_table, index);
                acls_ntry.dir = 2;
                acls_ntry.port = neigh_res->aclport;
                index = table_find(&acls_table, &acls_ntry);
                if (index >= 0) {
                    acls_res = table_get(&acls_table, index);
                    if (apply_acl(&acls_res->aces, &acl4_ntry, &acl4_matcher) != 0) goto punt;
                }
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
                    tun4_res = table_get(&tun4_table, index);
                    switch (tun4_res->command) {
                    case 1: //gre
                        bufP = bufT + 2; // gre header
                        break;
                    default:
                        packDr[port]++;
                        byteDr[port] += bufS;
                        return;
                    }
                    bufP -= 12;
                    memset(&bufD[bufP], 0, 12);
                    tun4_res->pack++;
                    tun4_res->byte += bufS;
                    bufS = bufS - bufP + preBuff;
                    memmove(&bufD[preBuff], &bufD[bufP], bufS);
                    prt2 = prt = tun4_res->aclport;
                    goto ether_rx;
                }
                acls_ntry.dir = 4;
                acls_ntry.port = 0;
                index = table_find(&acls_table, &acls_ntry);
                if (index >= 0) {
                    acls_res = table_get(&acls_table, index);
                    if (apply_acl(&acls_res->aces, &acl4_ntry, &acl4_matcher) != 0) {
                        packDr[port]++;
                        byteDr[port] += bufS;
                        return;
                    }
                }
                goto cpu;
            case 3: // mpls1
                ethtyp = 0x8847;
                bufP -= 4;
                label = 0x100 | ttl | (route4_res->label1 << 12);
                put32msb(bufD, bufP, label);
                neigh_ntry.id = route4_res->nexthop;
                goto ethtyp_tx;
            case 4: // mpls2
                ethtyp = 0x8847;
                bufP -= 4;
                label = 0x100 | ttl | (route4_res->label2 << 12);
                put32msb(bufD, bufP, label);
                bufP -= 4;
                label = ttl | (route4_res->label1 << 12);
                put32msb(bufD, bufP, label);
                neigh_ntry.id = route4_res->nexthop;
                goto ethtyp_tx;
            }
        }
        goto punt;
    case 0x86dd:
        portvrf_ntry.port = prt;
        index = table_find(&portvrf_table, &portvrf_ntry);
        if (index < 0) {
            packDr[port]++;
            byteDr[port] += bufS;
            return;
        }
        portvrf_res = table_get(&portvrf_table, index);
        switch (portvrf_res->command) {
        case 1:
            break;
        case 2:
            goto bridge_rx;
        case 3:
            goto xconn_rx;
        default:
            return;
        }
        route6_ntry.vrf = portvrf_res->vrf;
ipv6_rx:
        acl6_ntry.protV = bufD[bufP + 6];
        acl6_ntry.srcAddr1 = get32msb(bufD, bufP + 8);
        acl6_ntry.srcAddr2 = get32msb(bufD, bufP + 12);
        acl6_ntry.srcAddr3 = get32msb(bufD, bufP + 16);
        acl6_ntry.srcAddr4 = get32msb(bufD, bufP + 20);
        acl6_ntry.trgAddr1 = route6_ntry.addr1 = get32msb(bufD, bufP + 24);
        acl6_ntry.trgAddr2 = route6_ntry.addr2 = get32msb(bufD, bufP + 28);
        acl6_ntry.trgAddr3 = route6_ntry.addr3 = get32msb(bufD, bufP + 32);
        acl6_ntry.trgAddr4 = route6_ntry.addr4 = get32msb(bufD, bufP + 36);
        hash ^= acl6_ntry.srcAddr4 ^ acl6_ntry.trgAddr4;
        ttl = bufD[bufP + 7] - 1;
        if (ttl <= 1) goto punt;
        bufD[bufP + 7] = ttl;
        bufT = bufP + 40;
        extract_layer4(acl6_ntry);
        acls_ntry.ver = 6;
        acls_ntry.dir = 1;
        acls_ntry.port = prt;
        index = table_find(&acls_table, &acls_ntry);
        if (index >= 0) {
            acls_res = table_get(&acls_table, index);
            if (apply_acl(&acls_res->aces, &acl6_ntry, &acl6_matcher) != 0) goto punt;
        }
        acls_ntry.dir = 3;
        acls_ntry.port = route6_ntry.vrf;
        index = table_find(&acls_table, &acls_ntry);
        if (index >= 0) {
            acls_res = table_get(&acls_table, index);
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
            if (index < 0) {
                if (apply_acl(&acls_res->aces, &acl6_ntry, &acl6_matcher) == 0) goto cpu;
                goto ipv6_rou;
            }
            nat6_res = table_get(&nat6_table, index);
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
        }
ipv6_rou:
        for (int i = 32; i >= 0; i--) {
            route6_ntry.mask = 96 + i;
            route6_ntry.addr4 &= masks[i];
            index = table_find(&route6_table, &route6_ntry);
            if (index < 0) continue;
            goto ipv6_hit;
        }
        for (int i = 32; i >= 0; i--) {
            route6_ntry.mask = 64 + i;
            route6_ntry.addr3 &= masks[i];
            index = table_find(&route6_table, &route6_ntry);
            if (index < 0) continue;
            goto ipv6_hit;
        }
        for (int i = 32; i >= 0; i--) {
            route6_ntry.mask = 32 + i;
            route6_ntry.addr2 &= masks[i];
            index = table_find(&route6_table, &route6_ntry);
            if (index < 0) continue;
            goto ipv6_hit;
        }
        for (int i = 32; i >= 0; i--) {
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
                bufP -= 2;
                put16msb(bufD, bufP, ethtyp);
                index = table_find(&neigh_table, &neigh_ntry);
                if (index < 0) {
                    packDr[port]++;
                    byteDr[port] += bufS;
                    return;
                }
                neigh_res = table_get(&neigh_table, index);
                acls_ntry.dir = 2;
                acls_ntry.port = neigh_res->aclport;
                index = table_find(&acls_table, &acls_ntry);
                if (index >= 0) {
                    acls_res = table_get(&acls_table, index);
                    if (apply_acl(&acls_res->aces, &acl6_ntry, &acl6_matcher) != 0) goto punt;
                }
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
                    tun6_res = table_get(&tun6_table, index);
                    switch (tun6_res->command) {
                    case 1: //gre
                        bufP = bufT + 2; // gre header
                        break;
                    default:
                        packDr[port]++;
                        byteDr[port] += bufS;
                        return;
                    }
                    bufP -= 12;
                    memset(&bufD[bufP], 0, 12);
                    tun6_res->pack++;
                    tun6_res->byte += bufS;
                    bufS = bufS - bufP + preBuff;
                    memmove(&bufD[preBuff], &bufD[bufP], bufS);
                    prt2 = prt = tun6_res->aclport;
                    goto ether_rx;
                }
                acls_ntry.dir = 4;
                acls_ntry.port = 0;
                index = table_find(&acls_table, &acls_ntry);
                if (index >= 0) {
                    acls_res = table_get(&acls_table, index);
                    if (apply_acl(&acls_res->aces, &acl6_ntry, &acl6_matcher) != 0) {
                        packDr[port]++;
                        byteDr[port] += bufS;
                        return;
                    }
                }
                goto cpu;
            case 3: // mpls1
                ethtyp = 0x8847;
                bufP -= 4;
                label = 0x100 | ttl | (route6_res->label1 << 12);
                put32msb(bufD, bufP, label);
                neigh_ntry.id = route6_res->nexthop;
                goto ethtyp_tx;
            case 4: // mpls2
                ethtyp = 0x8847;
                bufP -= 4;
                label = 0x100 | ttl | (route6_res->label2 << 12);
                put32msb(bufD, bufP, label);
                bufP -= 4;
                label = ttl | (route6_res->label1 << 12);
                put32msb(bufD, bufP, label);
                neigh_ntry.id = route6_res->nexthop;
                goto ethtyp_tx;
            }
        }
        goto punt;
    case 0x8864:
        pppoe_ntry.port = prt;
        pppoe_ntry.session = get16msb(bufD, bufP + 2);
        index = table_find(&pppoe_table, &pppoe_ntry);
        if (index < 0) {
            packDr[port]++;
            byteDr[port] += bufS;
            return;
        }
        pppoe_res = table_get(&pppoe_table, index);
        pppoe_res->pack++;
        pppoe_res->byte += bufS;
        prt = pppoe_res->aclport;
        bufP += 6;
        ethtyp = get16msb(bufD, bufP);
        bufP += 2;
        if ((ethtyp & 0x8000) != 0) goto cpu;
        portvrf_ntry.port = prt;
        index = table_find(&portvrf_table, &portvrf_ntry);
        if (index < 0) {
            packDr[port]++;
            byteDr[port] += bufS;
            return;
        }
        portvrf_res = table_get(&portvrf_table, index);
        if (portvrf_res->command != 1) {
            packDr[port]++;
            byteDr[port] += bufS;
            return;
        }
        route4_ntry.vrf = portvrf_res->vrf;
        route6_ntry.vrf = portvrf_res->vrf;
        switch (ethtyp) {
        case 0x0021:
            ethtyp = 0x800;
            goto ipv4_rx;
        case 0x0057:
            ethtyp = 0x86dd;
            goto ipv6_rx;
        case 0x0281:
            ethtyp = 0x8847;
            goto mpls_rx;
        }
        packDr[port]++;
        byteDr[port] += bufS;
        return;
    case 0x65580001:
xconn_rx:
        memmove(&buf2[0], &bufD[preBuff], 12);
        bufP -= 2;
        bufP -= 12;
        memmove(&bufD[bufP], &buf2[0], 12);
        ethtyp = 0x8847;
        bufP -= 4;
        label = 0x1ff | (portvrf_res->label2 << 12);
        put32msb(bufD, bufP, label);
        bufP -= 4;
        label = 0xff | (portvrf_res->label1 << 12);
        put32msb(bufD, bufP, label);
        neigh_ntry.id = portvrf_res->nexthop;
        goto ethtyp_tx;
    case 0x65580000:
bridge_rx:
        bridge_ntry.id = portvrf_res->bridge;
        memmove(&buf2[0], &bufD[preBuff], 12);
bridgevpls_rx:
        bridge_ntry.mac1 = get16msb(buf2, 6);
        bridge_ntry.mac2 = get32msb(buf2, 8);
        hash ^= bridge_ntry.mac2;
        index = table_find(&bridge_table, &bridge_ntry);
        if (index < 0) goto punt;
        bridge_ntry.mac1 = get16msb(buf2, 0);
        bridge_ntry.mac2 = get32msb(buf2, 2);
        hash ^= bridge_ntry.mac2;
        index = table_find(&bridge_table, &bridge_ntry);
        if (index < 0) goto punt;
        bridge_res = table_get(&bridge_table, index);
        bridge_res->pack++;
        bridge_res->byte += bufS;
        bufP -= 2;
        switch (bridge_res->command) {
        case 1:
            break;
        case 2:
            bufP -= 12;
            memmove(&bufD[bufP], &buf2[0], 12);
            ethtyp = 0x8847;
            bufP -= 4;
            label = 0x1ff | (bridge_res->label2 << 12);
            put32msb(bufD, bufP, label);
            bufP -= 4;
            label = 0xff | (bridge_res->label1 << 12);
            put32msb(bufD, bufP, label);
            neigh_ntry.id = bridge_res->nexthop;
            goto ethtyp_tx;
        default:
            return;
        }
        prt = bridge_res->port;
layer2_tx:
        vlan_ntry.id = prt;
        index = table_find(&vlanout_table, &vlan_ntry);
        if (index >= 0) {
            vlan_res = table_get(&vlanout_table, index);
            bufP -= 2;
            put16msb(bufD, bufP, vlan_res->vlan);
            bufP -= 2;
            put16msb(bufD, bufP, 0x8100);
            prt = vlan_res->port;
            vlan_res->pack++;
            vlan_res->byte += bufS;
        }
        bufP -= 12;
        memmove(&bufD[bufP], &buf2[0], 12);
        bundle_ntry.id = prt;
        index = table_find(&bundle_table, &bundle_ntry);
        if (index >= 0) {
            bundle_res = table_get(&bundle_table, index);
            reduce_hash;
            prt = bundle_res->out[hash];
            bundle_res->pack++;
            bundle_res->byte += bufS;
            if (bundle_res->command == 2) {
                bufS = bufS - bufP + preBuff;
                memmove(&bufD[preBuff], &bufD[bufP], bufS);
                prt2 = prt;
                goto ether_rx;
            }
        }
        if (prt >= ports) {
            packDr[port]++;
            byteDr[port] += bufS;
            return;
        }
        send2port(&bufD[bufP], bufS - bufP + preBuff, prt);
        return;
    case 0x806:
        goto cpu;
    case 0x8863:
        goto cpu;
    default:
punt:
        if (punts < 0) {
            packDr[port]++;
            byteDr[port] += bufS;
            return;
        }
        punts--;
cpu:
        send2cpu(bufD, bufS, prt2);
        return;
    }
}


#endif
