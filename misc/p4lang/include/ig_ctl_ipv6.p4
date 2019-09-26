#ifndef _IG_CTL_IPv6_P4_  
#define _IG_CTL_IPv6_P4_  
   
control IngressControlIPv6(inout headers hdr,
                           inout ingress_metadata_t ig_md,
                           inout standard_metadata_t ig_intr_md) {

   action send_to_cpu() {                                                
      /*                                                                 
       * Prepend pkt_in header to pkt sent to controller                 
       * by calling setValid() so it is tekne into account by deparser   
       */                                                                
        ig_md.nexthop_id = CPU_PORT;                                                                   
   }                                                                     

   action act_ipv6_cpl_set_nexthop() {
      /*
       * Send to CPU 
       * CPU => 64 
       */
      send_to_cpu();
   } 

   action act_ipv6_fib_discard() {
      mark_to_drop(ig_intr_md); 
   }
   
   /*
    * Perform L3 forwarding
    */
   action act_ipv6_set_nexthop(PortId_t nexthop_id) {
       /*
       * ig_md.nexthop_id is set now to the egress_port 
       * set by the control plane entry 
       * for further processing by ig_tm_md.ucast_egress_port
       */
      ig_md.nexthop_id = nexthop_id;
   }


   action act_ipv6_mpls_encap_set_nexthop(label_t vpn_label, label_t egress_label, PortId_t nexthop_id) {
      /*
       * Egress packet is now a MPLS packet
       * (LABEL imposition)
       */
      ig_md.ethertype = ETHERTYPE_MPLS_UCAST;
      /*
       * Encapsulate MPLS header
       * And set egress label
       */
      hdr.mpls.push_front(2);
      hdr.mpls[0].setValid();
      hdr.mpls[0].label = egress_label;
      hdr.mpls[0].ttl = hdr.ipv6.hop_limit;
      /*
       * MPLS VPN
       */

      hdr.mpls[1].setValid();
      hdr.mpls[1].label = vpn_label;
      hdr.mpls[1].ttl = hdr.ipv6.hop_limit;
      hdr.mpls[1].bos = 1;
      /*
       * Set nexthop_id for further forwarding process
       */
      ig_md.nexthop_id = nexthop_id;
   }




   
   table tbl_ipv6_fib_host {
      key = {
         /*
          * we match /32 host route
          */
         hdr.ipv6.dst_addr: exact;
         ig_md.vrf: exact;
      }
      actions = {
         act_ipv6_cpl_set_nexthop;
         act_ipv6_set_nexthop;
         act_ipv6_mpls_encap_set_nexthop;
         @defaultonly NoAction;
      }
      size = IPV6_HOST_TABLE_SIZE;
      const default_action = NoAction();
   }
   
   table tbl_ipv6_fib_lpm {
      key = {
         /*
          * we match network route via Long Prefix Match kind operation
          */
         hdr.ipv6.dst_addr: lpm;
         ig_md.vrf: exact;
      }
      actions = {
         act_ipv6_cpl_set_nexthop;
         act_ipv6_set_nexthop;
         act_ipv6_mpls_encap_set_nexthop;
         act_ipv6_fib_discard;
      }
      size = IPV6_LPM_TABLE_SIZE;
      default_action = act_ipv6_fib_discard();
   }

   apply {
        /*                                             
         * It is a dataplane packet                 
         */                                            
        //if (hdr.ipv6.isValid() && hdr.ipv6.ttl > 1) {  
        if (ig_md.ipv6_valid==1)  {  
           if (!tbl_ipv6_fib_host.apply().hit) {           
              tbl_ipv6_fib_lpm.apply();                    
           }                                           
        }               
   }                               
}   

#endif // _IG_CTL_IPv6_P4_
   
