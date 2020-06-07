#ifndef _IG_CTL_MPLS2_P4_  
#define _IG_CTL_MPLS2_P4_  
   
control IngressControlMPLS2(inout headers hdr,
                           inout ingress_metadata_t ig_md,
                           inout standard_metadata_t ig_intr_md) {


   apply {
           if (ig_md.mpls0_remove == 1) {
              hdr.mpls[0].setInvalid();
              if (ig_md.ipv4_valid == 1) {
                ig_md.ethertype = ETHERTYPE_IPV4;
              } else {
                ig_md.ethertype = ETHERTYPE_IPV6;
              }
           }

           if (ig_md.mpls1_remove == 1) {
              hdr.mpls[1].setInvalid();
              if (ig_md.ipv4_valid == 1) {
                ig_md.ethertype = ETHERTYPE_IPV4;
              } else {
                ig_md.ethertype = ETHERTYPE_IPV6;
              }
           }

   }                               
}   

#endif // _IG_CTL_MPLS2_P4_
   
