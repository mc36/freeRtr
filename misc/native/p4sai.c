#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <sai.h>

void err(char*buf) {
    printf("%s\n", buf);
    _exit(1);
}

#include "table.h"


void str2mac(unsigned char *dst, char *src) {
    sscanf(src, "%hhx:%hhx:%hhx:%hhx:%hhx:%hhx", &dst[0], &dst[1], &dst[2], &dst[3], &dst[4], &dst[5]);
}

void mask2byte(unsigned char *dst, int bits, int max) {
    unsigned char bitVals[] = { 0x80, 0x40, 0x20, 0x10, 0x8, 0x4, 0x2, 0x1 };
    memset(&dst[0], 0, max);
    for (int i=0; i < bits; i++) {
        dst[i/8] |= bitVals[i%8];
    }
}


sai_object_id_t switchId;
sai_object_id_t routerId;
sai_switch_api_t* sai_switch_api;
sai_port_api_t* sai_port_api;
sai_vlan_api_t* sai_vlan_api;
sai_my_mac_api_t* sai_mymac_api;
sai_virtual_router_api_t* sai_vr_api;
sai_router_interface_api_t* sai_rif_api;
sai_neighbor_api_t* sai_neighbor_api;
sai_route_api_t* sai_route_api;
sai_next_hop_api_t* sai_next_hop_api;
sai_fdb_api_t* sai_fdb_api;


struct port2sai_entry {
    int port;
    sai_object_id_t sid;
    sai_object_id_t rid;
};

struct neigh4sai_entry {
    int id;
    int port;
    sai_object_id_t sai;
    unsigned char mac[6];
    unsigned char ip[4];
};

struct neigh6sai_entry {
    int id;
    int port;
    sai_object_id_t sai;
    unsigned char mac[6];
    unsigned char ip[16];
};


struct table_head port2sai_table;
struct table_head neigh4sai_table;
struct table_head neigh6sai_table;




const char* dummy_profile_get_value(_In_ sai_switch_profile_id_t profile_id, _In_ const char* variable) {
    printf("get_value: %s\n", variable);
    return NULL;
}

int dummy_profile_get_next_value(_In_ sai_switch_profile_id_t profile_id, _Out_ const char** variable, _Out_ const char** value) {
    printf("get_next: %s\n", *variable);
    return -1;
}

const sai_service_method_table_t dummy_services = { dummy_profile_get_value, dummy_profile_get_next_value };

void dummy_port_state_change(_In_ unsigned int count, _In_ sai_port_oper_status_notification_t *data) {
    printf("port state change\n");
}

void dummy_packet_event(_In_ sai_object_id_t switch_id, _In_ const void *buffer, _In_ sai_size_t buffer_size, _In_ unsigned int attr_count, _In_ const sai_attribute_t *attr_list) {
    printf("packet in\n");
}




int main(int argc, char **argv) {

    if (argc < 2) err("using: dp <addr> <port>");

    table_init(&port2sai_table, sizeof(struct port2sai_entry), 1);
    table_init(&neigh4sai_table, sizeof(struct neigh4sai_entry), 1);
    table_init(&neigh6sai_table, sizeof(struct neigh6sai_entry), 1);

    int port = atoi(argv[2]);
    struct sockaddr_in addr;
    memset(&addr, 0, sizeof (addr));
    if (inet_aton(argv[1], &addr.sin_addr) == 0) err("bad addr address");
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    printf("connecting %s %i.\n", inet_ntoa(addr.sin_addr), port);
    int commandSock = socket(AF_INET, SOCK_STREAM, 0);
    if (commandSock < 0) err("unable to open socket");
    if (connect(commandSock, (struct sockaddr*)&addr, sizeof(addr)) < 0) err("failed to connect socket");
    FILE *cmdTx = fdopen(commandSock, "w");
    FILE *cmdRx = fdopen(commandSock, "r");
    if ((cmdTx == NULL) || (cmdRx == NULL)) err("failed to open file");
    fprintf(cmdTx, "platform p4sai\r\n");
    fprintf(cmdTx, "capabilities punting route\r\n");
    fprintf(cmdTx, "dynrange 32768 65535\r\n");
    fprintf(cmdTx, "vrfrange 1 65535\r\n");
    fprintf(cmdTx, "neirange 4096 65535\r\n");
    fprintf(cmdTx, "nomore\r\n");
    fflush(cmdTx);

    printf("opening api...\n");
    sai_api_initialize(0, &dummy_services);
    sai_api_query(SAI_API_SWITCH, (void**)&sai_switch_api);
    sai_api_query(SAI_API_PORT, (void**)&sai_port_api);
    sai_api_query(SAI_API_VLAN, (void**)&sai_vlan_api);
    sai_api_query(SAI_API_MY_MAC, (void**)&sai_mymac_api);
    sai_api_query(SAI_API_VIRTUAL_ROUTER, (void**)&sai_vr_api);
    sai_api_query(SAI_API_ROUTER_INTERFACE, (void**)&sai_rif_api);
    sai_api_query(SAI_API_NEIGHBOR, (void**)&sai_neighbor_api);
    sai_api_query(SAI_API_ROUTE, (void**)&sai_route_api);
    sai_api_query(SAI_API_NEXT_HOP, (void**)&sai_next_hop_api);
    sai_api_query(SAI_API_FDB, (void**)&sai_fdb_api);

    printf("opening asic...\n");
    sai_attribute_t attr[8];
    memset(&attr, 0, sizeof(attr));
    attr[0].id = SAI_SWITCH_ATTR_INIT_SWITCH;
    attr[0].value.booldata = true;
    attr[1].id = SAI_SWITCH_ATTR_PORT_STATE_CHANGE_NOTIFY;
    attr[1].value.ptr = &dummy_port_state_change;
    attr[2].id = SAI_SWITCH_ATTR_PACKET_EVENT_NOTIFY;
    attr[2].value.ptr = &dummy_packet_event;
    if (sai_switch_api->create_switch(&switchId, 3, attr) != SAI_STATUS_SUCCESS) err("unable to create switch");
    printf("switch id=%lx\n", switchId);
    memset(&attr, 0, sizeof(attr));
    attr[0].id = SAI_SWITCH_ATTR_DEFAULT_VIRTUAL_ROUTER_ID;
    if (sai_switch_api->get_switch_attribute(switchId, 1, attr) != SAI_STATUS_SUCCESS) err("unable to query router");
    routerId = attr[0].value.u64;
    printf("router id=%lx\n", routerId);

    int printCmds = getenv("p4emuNOCMDS") == NULL;
    struct port2sai_entry port2sai_ntry;
    struct port2sai_entry *port2sai_res;
    struct neigh4sai_entry neigh4sai_ntry;
    struct neigh4sai_entry *neigh4sai_res;
    struct neigh6sai_entry neigh6sai_ntry;
    struct neigh6sai_entry *neigh6sai_res;
    unsigned char buf[1024];
    char* arg[128];
    for (;;) {
        memset(&buf, 0, sizeof(buf));
        if (fgets((char*)&buf[0], sizeof(buf), cmdRx) == NULL) break;
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
        if (strcmp(arg[0], "ports") == 0) {
            memset(&port2sai_ntry, 0, sizeof(port2sai_ntry));
            port2sai_ntry.port = atoi(arg[2]);
            sai_object_id_t p = 0;
            unsigned int lanes[8];
            lanes[0] = port2sai_ntry.port;
            memset(&attr, 0, sizeof(attr));
            attr[0].id = SAI_PORT_ATTR_ADMIN_STATE;
            attr[0].value.booldata = true;
            attr[1].id = SAI_PORT_ATTR_SPEED;
            attr[1].value.u32 = 1000 * atoi(arg[3]);
            attr[2].id = SAI_PORT_ATTR_FEC_MODE;
            attr[2].value.u32 = SAI_PORT_FEC_MODE_FC;
            attr[3].id = SAI_PORT_ATTR_AUTO_NEG_MODE;
            attr[3].value.booldata = false;
            attr[4].id = SAI_PORT_ATTR_HW_LANE_LIST;
            attr[4].value.u32list.list = lanes;
            attr[4].value.u32list.count = 1;
            if (sai_port_api->create_port(&p, switchId, 5, attr) != SAI_STATUS_SUCCESS) err("unable to create port");
            port2sai_ntry.sid = p;
            memset(&attr, 0, sizeof(attr));
            attr[0].id = SAI_MY_MAC_ATTR_PRIORITY;
            attr[0].value.u32 = 0;
            attr[1].id = SAI_MY_MAC_ATTR_PORT_ID;
            attr[1].value.oid = p;
            attr[2].id = SAI_MY_MAC_ATTR_VLAN_ID;
            attr[2].value.u16 = 0;
            attr[3].id = SAI_MY_MAC_ATTR_MAC_ADDRESS;
            attr[4].id = SAI_MY_MAC_ATTR_MAC_ADDRESS_MASK;
            if (sai_mymac_api->create_my_mac(&p, switchId, 5, attr) != SAI_STATUS_SUCCESS) err("unable to create mymac");
            table_add(&port2sai_table, &port2sai_ntry);
            continue;
        }
        if (strcmp(arg[0], "portvrf") == 0) {
            port2sai_ntry.port = atoi(arg[2]);
            i = table_find(&port2sai_table, &port2sai_ntry);
            if (i < 0) continue;
            port2sai_res = table_get(&port2sai_table, i);
            sai_object_id_t p = 0;
            sai_attribute_t attr[4];
            memset(&attr, 0, sizeof(attr));
            attr[0].id = SAI_ROUTER_INTERFACE_ATTR_VIRTUAL_ROUTER_ID;
            attr[0].value.oid = routerId;
            attr[1].id = SAI_ROUTER_INTERFACE_ATTR_TYPE;
            attr[1].value.s32 = SAI_ROUTER_INTERFACE_TYPE_PORT;
            attr[2].id = SAI_ROUTER_INTERFACE_ATTR_PORT_ID;
            attr[2].value.oid = port2sai_res->sid;
            attr[3].id = SAI_ROUTER_INTERFACE_ATTR_SRC_MAC_ADDRESS;
            attr[3].value.mac[5] = 0x55;
            if (sai_rif_api->create_router_interface(&p, switchId, 4, attr) != SAI_STATUS_SUCCESS) err("unable to create port");
            port2sai_res->rid = p;
            continue;
        }
        if (strcmp(arg[0], "neigh4") == 0) {
            memset(&neigh4sai_ntry, 0, sizeof(neigh4sai_ntry));
            neigh4sai_ntry.id = atoi(arg[2]);
            inet_pton(AF_INET, arg[3], neigh4sai_ntry.ip);
            str2mac(neigh4sai_ntry.mac, arg[4]);
            neigh4sai_ntry.port = atoi(arg[7]);
            port2sai_ntry.port = neigh4sai_ntry.port;
            i = table_find(&port2sai_table, &port2sai_ntry);
            if (i < 0) continue;
            port2sai_res = table_get(&port2sai_table, i);
            sai_neighbor_entry_t nei;
            memset(&nei, 0, sizeof(nei));
            nei.switch_id = switchId;
            nei.rif_id = port2sai_res->rid;
            nei.ip_address.addr_family = SAI_IP_ADDR_FAMILY_IPV4;
            memcpy(&nei.ip_address.addr.ip4, neigh4sai_ntry.ip, 4);
            memset(&attr, 0, sizeof(attr));
            attr[0].id = SAI_NEIGHBOR_ENTRY_ATTR_DST_MAC_ADDRESS;
            memcpy(attr[0].value.mac, neigh4sai_ntry.mac, 6);
            if (sai_neighbor_api->create_neighbor_entry(&nei, 1, attr) != SAI_STATUS_SUCCESS) err("unable to create neighbor");
            memset(&attr, 0, sizeof(attr));
            attr[0].id = SAI_NEXT_HOP_ATTR_TYPE;
            attr[0].value.s32 = SAI_NEXT_HOP_TYPE_IP;
            attr[1].id = SAI_NEXT_HOP_ATTR_IP;
            attr[1].value.ipaddr.addr_family = SAI_IP_ADDR_FAMILY_IPV4;
            memcpy(&attr[1].value.ipaddr.addr.ip4, neigh4sai_ntry.ip, 4);
            attr[2].id = SAI_NEXT_HOP_ATTR_ROUTER_INTERFACE_ID;
            attr[2].value.oid = port2sai_res->rid;
            sai_object_id_t p = 0;
            if (sai_next_hop_api->create_next_hop(&p, switchId, 3, attr) != SAI_STATUS_SUCCESS) err("unable to create neighbor");
            neigh4sai_ntry.sai = p;
            table_add(&neigh4sai_table, &neigh4sai_ntry);
            continue;
        }
        if (strcmp(arg[0], "neigh6") == 0) {
            memset(&neigh6sai_ntry, 0, sizeof(neigh6sai_ntry));
            neigh6sai_ntry.id = atoi(arg[2]);
            inet_pton(AF_INET6, arg[3], neigh6sai_ntry.ip);
            str2mac(neigh6sai_ntry.mac, arg[4]);
            neigh6sai_ntry.port = atoi(arg[7]);
            port2sai_ntry.port = neigh6sai_ntry.port;
            i = table_find(&port2sai_table, &port2sai_ntry);
            if (i < 0) continue;
            port2sai_res = table_get(&port2sai_table, i);
            sai_neighbor_entry_t nei;
            memset(&nei, 0, sizeof(nei));
            nei.switch_id = switchId;
            nei.rif_id = port2sai_res->rid;
            nei.ip_address.addr_family = SAI_IP_ADDR_FAMILY_IPV6;
            memcpy(&nei.ip_address.addr.ip6, neigh6sai_ntry.ip, 16);
            memset(&attr, 0, sizeof(attr));
            attr[0].id = SAI_NEIGHBOR_ENTRY_ATTR_DST_MAC_ADDRESS;
            memcpy(attr[0].value.mac, neigh6sai_ntry.mac, 6);
            if (sai_neighbor_api->create_neighbor_entry(&nei, 1, attr) != SAI_STATUS_SUCCESS) err("unable to create neighbor");
            memset(&attr, 0, sizeof(attr));
            attr[0].id = SAI_NEXT_HOP_ATTR_TYPE;
            attr[0].value.s32 = SAI_NEXT_HOP_TYPE_IP;
            attr[1].id = SAI_NEXT_HOP_ATTR_IP;
            attr[1].value.ipaddr.addr_family = SAI_IP_ADDR_FAMILY_IPV6;
            memcpy(&attr[1].value.ipaddr.addr.ip6, neigh6sai_ntry.ip, 16);
            attr[2].id = SAI_NEXT_HOP_ATTR_ROUTER_INTERFACE_ID;
            attr[2].value.oid = port2sai_res->rid;
            sai_object_id_t p = 0;
            if (sai_next_hop_api->create_next_hop(&p, switchId, 3, attr) != SAI_STATUS_SUCCESS) err("unable to create neighbor");
            neigh6sai_ntry.sai = p;
            table_add(&neigh6sai_table, &neigh6sai_ntry);
            continue;
        }
        if (strcmp(arg[0], "route4") == 0) {
            neigh4sai_ntry.id = atoi(arg[4]);
            i = table_find(&neigh4sai_table, &neigh4sai_ntry);
            if (i < 0) continue;
            neigh4sai_res = table_get(&neigh4sai_table, i);
            sai_route_entry_t route_entry;
            route_entry.switch_id = switchId;
            route_entry.vr_id = routerId;
            route_entry.destination.addr_family = SAI_IP_ADDR_FAMILY_IPV4;
            inet_pton(AF_INET, arg[2], &route_entry.destination.addr.ip4);
            mask2byte((void*)&route_entry.destination.mask.ip4, atoi(arg[3]), 4);
            memset(&attr, 0, sizeof(attr));
            attr[0].id = SAI_ROUTE_ENTRY_ATTR_PACKET_ACTION;
            attr[0].value.s32 = SAI_PACKET_ACTION_FORWARD;
            attr[1].id = SAI_ROUTE_ENTRY_ATTR_NEXT_HOP_ID;
            attr[1].value.oid = neigh4sai_res->sai;
            if (sai_route_api->create_route_entry(&route_entry, 2, attr) != SAI_STATUS_SUCCESS) err("unable to create route");
            continue;
        }
        if (strcmp(arg[0], "route6") == 0) {
            neigh6sai_ntry.id = atoi(arg[4]);
            i = table_find(&neigh6sai_table, &neigh6sai_ntry);
            if (i < 0) continue;
            neigh6sai_res = table_get(&neigh6sai_table, i);
            sai_route_entry_t route_entry;
            route_entry.switch_id = switchId;
            route_entry.vr_id = routerId;
            route_entry.destination.addr_family = SAI_IP_ADDR_FAMILY_IPV6;
            inet_pton(AF_INET6, arg[2], &route_entry.destination.addr.ip6);
            mask2byte((void*)&route_entry.destination.mask.ip6, atoi(arg[3]), 16);
            memset(&attr, 0, sizeof(attr));
            attr[0].id = SAI_ROUTE_ENTRY_ATTR_PACKET_ACTION;
            attr[0].value.s32 = SAI_PACKET_ACTION_FORWARD;
            attr[1].id = SAI_ROUTE_ENTRY_ATTR_NEXT_HOP_ID;
            attr[1].value.oid = neigh6sai_res->sai;
            if (sai_route_api->create_route_entry(&route_entry, 2, attr) != SAI_STATUS_SUCCESS) err("unable to create route");
            continue;
        }
    }

    err("command thread exited");
}
