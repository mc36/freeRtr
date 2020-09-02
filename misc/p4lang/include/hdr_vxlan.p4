#ifndef _VXLAN_P4_
#define _VXLAN_P4_

header vxlan_t {
    bit<16> flags;
    bit<16> policy;
    bit<24> instance;
    bit<8> reserved;
}

#endif // _VXLAN_P4_
