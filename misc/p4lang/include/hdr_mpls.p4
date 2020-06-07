#ifndef _MPLS_P4_
#define _MPLS_P4_

header mpls_t {
   label_t label;
   bit<3>  exp;       
   bit<1>  bos;
   bit<8>  ttl;       
}

#endif // _MPLS_P4_
