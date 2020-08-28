#ifndef _L2TP_P4_
#define _L2TP_P4_

header l2tp_t {
    bit<16> flags;
    bit<32> tidsid;
    bit<16> offset;
    bit<16> pppflags;
    bit<16> ppptyp;
}

#endif // _L2TP_P4_
