#ifndef _UDP_P4_
#define _UDP_P4_

header udp_t {
    layer4_port_t src_port;
    layer4_port_t dst_port;
    bit<16> length;
    bit<16> checksum;
}

#endif // _UDP_P4_
