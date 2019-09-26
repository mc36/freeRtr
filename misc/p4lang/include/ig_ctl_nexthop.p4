#ifndef _NEXTHOP_P4_
#define _NEXTHOP_P4_

control IngressControlNexthop(inout headers hdr,                                      
                   inout ingress_metadata_t ig_md,                  
                   inout standard_metadata_t ig_intr_md) {    

   action act_ipv4_fib_hit(mac_addr_t dst_mac_addr, PortId_t egress_port) {
      /*
       * the packet header src_mac is now set to the previous header dst_mac
       */
      hdr.ethernet.src_mac_addr = hdr.ethernet.dst_mac_addr;

      /*
       * the new packet header dst_mac is now the dst_mac 
       * set by the control plane entry
       */
      hdr.ethernet.dst_mac_addr = dst_mac_addr;

      /*
       * the egress_spec port is set now the egress_port 
       * set by the control plane entry
       */
      ig_md.target_id = egress_port;

      /*
       * We decrement the TTL
       */
      hdr.ipv4.ttl = hdr.ipv4.ttl -1;
   }   

   action act_ipv4_fib_discard() {
      mark_to_drop(ig_intr_md);
   }

   table tbl_nexthop {
      /*
       * custom metadat is used for the lookup key
       */
      key = {
         ig_md.nexthop_id: exact;
      }
      actions = {
         act_ipv4_fib_hit;
         act_ipv4_fib_discard;
      }
      size = NEXTHOP_TABLE_SIZE;
      default_action = act_ipv4_fib_discard();
   }

   apply {
      tbl_nexthop.apply();
   }

}

#endif // _NEXTHOP_P4
