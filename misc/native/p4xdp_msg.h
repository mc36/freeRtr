void str2mac(unsigned char *dst, char *src) {
    sscanf(src, "%hhx:%hhx:%hhx:%hhx:%hhx:%hhx", &dst[0], &dst[1], &dst[2], &dst[3], &dst[4], &dst[5]);
}


void doStatRound(FILE *commands, int round) {
    int i = 1;
    int o = 10;
    if (bpf_map_update_elem(cpu_port_fd, &i, &o, BPF_ANY) != 0) err("error setting cpuport");
}


int doOneCommand(unsigned char* buf) {
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
    struct vrfp_res vrfp;
    memset(&vrfp, 0, sizeof(vrfp));
    struct neigh_res neir;
    memset(&neir, 0, sizeof(neir));
    struct route4_key rou4;
    memset(&rou4, 0, sizeof(rou4));
    struct route6_key rou6;
    memset(&rou6, 0, sizeof(rou6));
    struct routes_res rour;
    memset(&rour, 0, sizeof(rour));
    struct label_res labr;
    memset(&labr, 0, sizeof(labr));
    if (strcmp(arg[0], "portvrf") == 0) {
        i = atoi(arg[2]);
        vrfp.cmd = 1;
        vrfp.vrf = atoi(arg[3]);
        if (del == 0) {
            if (bpf_map_delete_elem(vrf_port_fd, &i) != 0) warn("error removing entry");
        } else {
            if (bpf_map_update_elem(vrf_port_fd, &i, &vrfp, BPF_ANY) != 0) warn("error setting entry");
        }
        return 0;
    }
    if (strcmp(arg[0], "xconnect") == 0) {
        vrfp.cmd = 3;
        i = atoi(arg[2]);
        vrfp.hop = atoi(arg[4]);
        vrfp.label1 = atoi(arg[5]);
        vrfp.label2 = atoi(arg[7]);
        o = atoi(arg[6]);
        labr.port = i;
        labr.cmd= 4;
        if (del == 0) {
            if (bpf_map_delete_elem(vrf_port_fd, &i) != 0) warn("error removing entry");
            if (bpf_map_delete_elem(labels_fd, &o) != 0) warn("error removing entry");
        } else {
            if (bpf_map_update_elem(vrf_port_fd, &i, &vrfp, BPF_ANY) != 0) warn("error setting entry");
            if (bpf_map_update_elem(labels_fd, &o, &labr, BPF_ANY) != 0) warn("error setting entry");
        }
        return 0;
    }
    if (strcmp(arg[0], "neigh4") == 0) {
        inet_pton(AF_INET, arg[3], buf2);
        rou4.vrf = atoi(arg[5]);
        memcpy(rou4.addr, buf2, sizeof(rou4.addr));
        rou4.bits = routes_bits + (sizeof(rou4.addr) * 8);
        rour.cmd = 1;
        i = rour.hop = atoi(arg[2]);
        str2mac(neir.dmac, arg[4]);
        str2mac(neir.smac, arg[6]);
        neir.port = atoi(arg[7]);
        neir.cmd = 1;
        if (del == 0) {
            if (bpf_map_delete_elem(route4_fd, &rou4) != 0) warn("error removing entry");
            if (bpf_map_delete_elem(neighs_fd, &i) != 0) warn("error removing entry");
        } else {
            if (bpf_map_update_elem(route4_fd, &rou4, &rour, BPF_ANY) != 0) warn("error setting entry");
            if (bpf_map_update_elem(neighs_fd, &i, &neir, BPF_ANY) != 0) warn("error setting entry");
        }
        return 0;
    }
    if (strcmp(arg[0], "neigh6") == 0) {
        inet_pton(AF_INET6, arg[3], buf2);
        rou6.vrf = atoi(arg[5]);
        memcpy(rou6.addr, buf2, sizeof(rou6.addr));
        rou6.bits = routes_bits + (sizeof(rou6.addr) * 8);
        rour.cmd = 1;
        i = rour.hop = atoi(arg[2]);
        str2mac(neir.dmac, arg[4]);
        str2mac(neir.smac, arg[6]);
        neir.port = atoi(arg[7]);
        neir.cmd = 1;
        if (del == 0) {
            if (bpf_map_delete_elem(route6_fd, &rou6) != 0) warn("error removing entry");
            if (bpf_map_delete_elem(neighs_fd, &i) != 0) warn("error removing entry");
        } else {
            if (bpf_map_update_elem(route6_fd, &rou6, &rour, BPF_ANY) != 0) warn("error setting entry");
            if (bpf_map_update_elem(neighs_fd, &i, &neir, BPF_ANY) != 0) warn("error setting entry");
        }
        return 0;
    }
    if (strcmp(arg[0], "myaddr4") == 0) {
        inet_pton(AF_INET, arg[2], buf2);
        rou4.vrf = atoi(arg[5]);
        memcpy(rou4.addr, buf2, sizeof(rou4.addr));
        rou4.bits = routes_bits + atoi(arg[3]);
        rour.cmd = 2;
        if (del == 0) {
            if (bpf_map_delete_elem(route4_fd, &rou4) != 0) warn("error removing entry");
        } else {
            if (bpf_map_update_elem(route4_fd, &rou4, &rour, BPF_ANY) != 0) warn("error setting entry");
        }
        return 0;
    }
    if (strcmp(arg[0], "myaddr6") == 0) {
        inet_pton(AF_INET6, arg[2], buf2);
        rou6.vrf = atoi(arg[5]);
        memcpy(rou6.addr, buf2, sizeof(rou6.addr));
        rou6.bits = routes_bits + atoi(arg[3]);
        rour.cmd = 2;
        if (del == 0) {
            if (bpf_map_delete_elem(route6_fd, &rou6) != 0) warn("error removing entry");
        } else {
            if (bpf_map_update_elem(route6_fd, &rou6, &rour, BPF_ANY) != 0) warn("error setting entry");
        }
        return 0;
    }
    if (strcmp(arg[0], "route4") == 0) {
        inet_pton(AF_INET, arg[2], buf2);
        rou4.vrf = atoi(arg[6]);
        memcpy(rou4.addr, buf2, sizeof(rou4.addr));
        rou4.bits = routes_bits + atoi(arg[3]);
        rour.cmd = 1;
        rour.hop = atoi(arg[4]);
        if (del == 0) {
            if (bpf_map_delete_elem(route4_fd, &rou4) != 0) warn("error removing entry");
        } else {
            if (bpf_map_update_elem(route4_fd, &rou4, &rour, BPF_ANY) != 0) warn("error setting entry");
        }
        return 0;
    }
    if (strcmp(arg[0], "route6") == 0) {
        inet_pton(AF_INET6, arg[2], buf2);
        rou6.vrf = atoi(arg[6]);
        memcpy(rou6.addr, buf2, sizeof(rou6.addr));
        rou6.bits = routes_bits + atoi(arg[3]);
        rour.cmd = 1;
        rour.hop = atoi(arg[4]);
        if (del == 0) {
            if (bpf_map_delete_elem(route6_fd, &rou6) != 0) warn("error removing entry");
        } else {
            if (bpf_map_update_elem(route6_fd, &rou6, &rour, BPF_ANY) != 0) warn("error setting entry");
        }
        return 0;
    }
    if (strcmp(arg[0], "labroute4") == 0) {
        inet_pton(AF_INET, arg[2], buf2);
        rou4.vrf = atoi(arg[6]);
        memcpy(rou4.addr, buf2, sizeof(rou4.addr));
        rou4.bits = routes_bits + atoi(arg[3]);
        rour.cmd = 3;
        rour.hop = atoi(arg[4]);
        rour.label1 = atoi(arg[7]);
        if (del == 0) {
            if (bpf_map_delete_elem(route4_fd, &rou4) != 0) warn("error removing entry");
        } else {
            if (bpf_map_update_elem(route4_fd, &rou4, &rour, BPF_ANY) != 0) warn("error setting entry");
        }
        return 0;
    }
    if (strcmp(arg[0], "labroute6") == 0) {
        inet_pton(AF_INET6, arg[2], buf2);
        rou6.vrf = atoi(arg[6]);
        memcpy(rou6.addr, buf2, sizeof(rou6.addr));
        rou6.bits = routes_bits + atoi(arg[3]);
        rour.cmd = 3;
        rour.hop = atoi(arg[4]);
        rour.label1 = atoi(arg[7]);
        if (del == 0) {
            if (bpf_map_delete_elem(route6_fd, &rou6) != 0) warn("error removing entry");
        } else {
            if (bpf_map_update_elem(route6_fd, &rou6, &rour, BPF_ANY) != 0) warn("error setting entry");
        }
        return 0;
    }
    if (strcmp(arg[0], "vpnroute4") == 0) {
        inet_pton(AF_INET, arg[2], buf2);
        rou4.vrf = atoi(arg[6]);
        memcpy(rou4.addr, buf2, sizeof(rou4.addr));
        rou4.bits = routes_bits + atoi(arg[3]);
        rour.cmd = 4;
        rour.hop = atoi(arg[4]);
        rour.label1 = atoi(arg[7]);
        rour.label2 = atoi(arg[8]);
        if (del == 0) {
            if (bpf_map_delete_elem(route4_fd, &rou4) != 0) warn("error removing entry");
        } else {
            if (bpf_map_update_elem(route4_fd, &rou4, &rour, BPF_ANY) != 0) warn("error setting entry");
        }
        return 0;
    }
    if (strcmp(arg[0], "vpnroute6") == 0) {
        inet_pton(AF_INET6, arg[2], buf2);
        rou6.vrf = atoi(arg[6]);
        memcpy(rou6.addr, buf2, sizeof(rou6.addr));
        rou6.bits = routes_bits + atoi(arg[3]);
        rour.cmd = 4;
        rour.hop = atoi(arg[4]);
        rour.label1 = atoi(arg[7]);
        rour.label2 = atoi(arg[8]);
        if (del == 0) {
            if (bpf_map_delete_elem(route6_fd, &rou6) != 0) warn("error removing entry");
        } else {
            if (bpf_map_update_elem(route6_fd, &rou6, &rour, BPF_ANY) != 0) warn("error setting entry");
        }
        return 0;
    }
    if (strcmp(arg[0], "mylabel4") == 0) {
        i = atoi(arg[2]);
        labr.vrf = atoi(arg[3]);
        labr.ver = 4;
        labr.cmd = 1;
        if (del == 0) {
            if (bpf_map_delete_elem(labels_fd, &i) != 0) warn("error removing entry");
        } else {
            if (bpf_map_update_elem(labels_fd, &i, &labr, BPF_ANY) != 0) warn("error setting entry");
        }
        return 0;
    }
    if (strcmp(arg[0], "mylabel6") == 0) {
        i = atoi(arg[2]);
        labr.vrf = atoi(arg[3]);
        labr.ver = 6;
        labr.cmd = 1;
        if (del == 0) {
            if (bpf_map_delete_elem(labels_fd, &i) != 0) warn("error removing entry");
        } else {
            if (bpf_map_update_elem(labels_fd, &i, &labr, BPF_ANY) != 0) warn("error setting entry");
        }
        return 0;
    }
    if (strcmp(arg[0], "unlabel4") == 0) {
        i = atoi(arg[2]);
        labr.hop = atoi(arg[3]);
        labr.ver = 4;
        labr.cmd = 2;
        if (del == 0) {
            if (bpf_map_delete_elem(labels_fd, &i) != 0) warn("error removing entry");
        } else {
            if (bpf_map_update_elem(labels_fd, &i, &labr, BPF_ANY) != 0) warn("error setting entry");
        }
        return 0;
    }
    if (strcmp(arg[0], "unlabel6") == 0) {
        i = atoi(arg[2]);
        labr.hop = atoi(arg[3]);
        labr.ver = 6;
        labr.cmd = 2;
        if (del == 0) {
            if (bpf_map_delete_elem(labels_fd, &i) != 0) warn("error removing entry");
        } else {
            if (bpf_map_update_elem(labels_fd, &i, &labr, BPF_ANY) != 0) warn("error setting entry");
        }
        return 0;
    }
    if (strcmp(arg[0], "label4") == 0) {
        i = atoi(arg[2]);
        labr.hop = atoi(arg[3]);
        labr.swap = atoi(arg[5]);
        labr.ver = 4;
        labr.cmd = 3;
        if (del == 0) {
            if (bpf_map_delete_elem(labels_fd, &i) != 0) warn("error removing entry");
        } else {
            if (bpf_map_update_elem(labels_fd, &i, &labr, BPF_ANY) != 0) warn("error setting entry");
        }
        return 0;
    }
    if (strcmp(arg[0], "label6") == 0) {
        i = atoi(arg[2]);
        labr.hop = atoi(arg[3]);
        labr.swap = atoi(arg[5]);
        labr.ver = 6;
        labr.cmd = 3;
        if (del == 0) {
            if (bpf_map_delete_elem(labels_fd, &i) != 0) warn("error removing entry");
        } else {
            if (bpf_map_update_elem(labels_fd, &i, &labr, BPF_ANY) != 0) warn("error setting entry");
        }
        return 0;
    }
    if (strcmp(arg[0], "portbundle") == 0) {
        int p = atoi(arg[2]);
        if (del == 0) {
            if (bpf_map_delete_elem(bundles_fd, &p) != 0) warn("error removing entry");
            return 0;
        }
        struct bundle_res bunn;
        memset(&bunn, 0, sizeof(bunn));
        struct bundle_res* bunr = &bunn;
        bpf_map_update_elem(bundles_fd, &p, &bunn, BPF_NOEXIST);
        if (bpf_map_lookup_elem(bundles_fd, &p, bunr) != 0) err("error getting entry");
        i = atoi(arg[3]);
        bunr->out[i] = atoi(arg[4]);
        if (bpf_map_update_elem(bundles_fd, &p, bunr, BPF_ANY) != 0) warn("error setting entry");
        return 0;
    }
    if (strcmp(arg[0], "portvlan") == 0) {
        struct vlan_key vlnk;
        memset(&vlnk, 0, sizeof(vlnk));
        struct vlan_res vlnr;
        memset(&vlnr, 0, sizeof(vlnr));
        i = atoi(arg[2]);
        o = atoi(arg[3]);
        vlnk.vlan = vlnr.vlan = atoi(arg[4]);
        vlnr.port = o;
        vlnk.port = o;
        if (del == 0) {
            if (bpf_map_delete_elem(vlan_in_fd, &vlnk) != 0) warn("error removing entry");
            if (bpf_map_delete_elem(vlan_out_fd, &i) != 0) warn("error removing entry");
        } else {
            if (bpf_map_update_elem(vlan_in_fd, &vlnk, &i, BPF_ANY) != 0) warn("error setting entry");
            if (bpf_map_update_elem(vlan_out_fd, &i, &vlnr, BPF_ANY) != 0) warn("error setting entry");
        }
        return 0;
    }
    if (strcmp(arg[0], "bundlevlan") == 0) {
        struct vlan_key vlnk;
        memset(&vlnk, 0, sizeof(vlnk));
        i = atoi(arg[4]);
        o = atoi(arg[2]);
        vlnk.vlan = atoi(arg[3]);
        vlnk.port = o;
        if (del == 0) {
            if (bpf_map_delete_elem(vlan_in_fd, &vlnk) != 0) warn("error removing entry");
        } else {
            if (bpf_map_update_elem(vlan_in_fd, &vlnk, &i, BPF_ANY) != 0) warn("error setting entry");
        }
        return 0;
    }
    if (strcmp(arg[0], "pppoe") == 0) {
        struct pppoe_key pppoe;
        memset(&pppoe, 0, sizeof(pppoe));
        i = atoi(arg[2]);
        pppoe.port = atoi(arg[3]);
        pppoe.sess = atoi(arg[6]);
        o = atoi(arg[4]);
        neir.port = pppoe.port;
        neir.sess = pppoe.sess;
        neir.cmd = 2;
        str2mac(neir.dmac, arg[7]);
        str2mac(neir.smac, arg[8]);
        if (del == 0) {
            if (bpf_map_delete_elem(pppoes_fd, &pppoe) != 0) warn("error removing entry");
            if (bpf_map_delete_elem(neighs_fd, &o) != 0) warn("error removing entry");
        } else {
            if (bpf_map_update_elem(pppoes_fd, &pppoe, &i, BPF_ANY) != 0) warn("error setting entry");
            if (bpf_map_update_elem(neighs_fd, &o, &neir, BPF_ANY) != 0) warn("error setting entry");
        }
        return 0;
    }
    return 0;
}
