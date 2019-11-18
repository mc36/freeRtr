#ifndef _UDP_P4_
#define _UDP_P4_

header udp_t {
    bit<16> src_port;
    bit<16> dst_port;
    bit<16> length_;
    bit<16> checksum; 
}

#endif // _UDP_P4_
