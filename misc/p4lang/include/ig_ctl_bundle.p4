#ifndef _IG_CTL_BUNDLE_P4_  
#define _IG_CTL_BUNDLE_P4_  
   
control IngressControlBundle(inout headers hdr,
                             inout ingress_metadata_t ig_md, 
                             inout standard_metadata_t ig_intr_md) { 

   action act_set_hash(PortId_t port) {
      ig_intr_md.egress_spec = port;
   }

   action act_set_port() {
      ig_intr_md.egress_spec = (PortId_t)ig_md.outport_id;
   }

   table tbl_bundle {
      key = {
         ig_md.outport_id: exact;  
         ig_md.hash_id: exact;  
      }
      actions = {
         act_set_port;
         act_set_hash;
      }
      size = 1024;
      default_action = act_set_port();
   }

   apply {
        bit<16> tmp = ig_md.layer4_srcprt ^ ig_md.layer4_dstprt;
        tmp = (tmp >> 8) ^ (tmp & 0xff);
        tmp = (tmp >> 4) ^ (tmp & 0xf);
        ig_md.hash_id = (bit<4>)tmp;
        tbl_bundle.apply();
   }                               
}   

#endif // _IG_CTL_BUNDLE_P4_
   
