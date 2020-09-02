#ifndef _PPPOE_P4_
#define _PPPOE_P4_

header pppoe_t {
    bit<4>  ver;
    bit<4>  type;
    bit<8>  code;
    bit<16> session;
    bit<16> length;
    bit<16> ppptyp;
}

#endif // _PPPOE_P4_
