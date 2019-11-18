#ifndef _IG_CTL_CoPP_P4_  
#define _IG_CTL_CoPP_P4_  
   
control IngressControlCoPP(inout headers hdr,
                           inout ingress_metadata_t ig_md,
                           inout standard_metadata_t ig_intr_md) {

   action act_deny() {
      mark_to_drop(ig_intr_md); 
   }

   action act_permit() {
   }
   
   
   table tbl_ipv4_copp {
      key = {
         hdr.ipv4.protocol: ternary;
         hdr.ipv4.src_ipv4_addr: ternary;
         hdr.ipv4.dst_ipv4_addr: ternary;
         ig_md.layer4_srcprt: ternary;
         ig_md.layer4_dstprt: ternary;
      }
      actions = {
         act_permit;
         act_deny;
         @defaultonly NoAction;
      }
      size = IPV4_HOST_TABLE_SIZE;
      const default_action = NoAction();
   }

   table tbl_ipv6_copp {
      key = {
         hdr.ipv6.next_hdr: ternary;
         hdr.ipv6.src_addr: ternary;
         hdr.ipv6.dst_addr: ternary;
         ig_md.layer4_srcprt: ternary;
         ig_md.layer4_dstprt: ternary;
      }
      actions = {
         act_permit;
         act_deny;
         @defaultonly NoAction;
      }
      size = IPV4_HOST_TABLE_SIZE;
      const default_action = NoAction();
   }

   apply {
        if (hdr.tcp.isValid()) {
          ig_md.layer4_srcprt = hdr.tcp.src_port;
          ig_md.layer4_dstprt = hdr.tcp.dst_port;
        }
        if (hdr.udp.isValid()) {
          ig_md.layer4_srcprt = hdr.udp.src_port;
          ig_md.layer4_dstprt = hdr.udp.dst_port;
        }
        if (ig_md.ipv4_valid==1)  {  
          tbl_ipv4_copp.apply();                    
        }               
        if (ig_md.ipv6_valid==1)  {  
          tbl_ipv6_copp.apply();                    
        }               
   }                               
}   

#endif // _IG_CTL_CoPP_P4_
   
