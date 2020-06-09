#ifndef _IG_CTL_IPv4b_P4_  
#define _IG_CTL_IPv4b_P4_  
   
control IngressControlIPv4b(inout headers hdr,
                           inout ingress_metadata_t ig_md,
                           inout standard_metadata_t ig_intr_md) {

   action send_to_cpu() {                                                
      /*                                                                 
       * Prepend pkt_in header to pkt sent to controller                 
       * by calling setValid() so it is tekne into account by deparser   
       */                                                                
        ig_md.nexthop_id = CPU_PORT;                                                                   
   }                                                                     

   action act_ipv4_cpl_set_nexthop() {
      /*
       * Send to CPU 
       * CPU => 64 
       */
      send_to_cpu();
   } 

   action act_ipv4_fib_discard() {
      mark_to_drop(ig_intr_md); 
   }
   
   /*
    * Perform L3 forwarding
    */
   action act_ipv4_set_nexthop(NextHopId_t nexthop_id) {
       /*
       * ig_md.nexthop_id is set now to the egress_port 
       * set by the control plane entry 
       * for further processing by ig_tm_md.ucast_egress_port
       */
      ig_md.nexthop_id = nexthop_id;
   }


   action act_ipv4_mpls_encap_set_nexthop(label_t vpn_label, label_t egress_label, NextHopId_t nexthop_id) {
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
      hdr.mpls[0].ttl = hdr.ipv4.ttl;
      /*
       * MPLS VPN     
       */

      hdr.mpls[1].setValid();
      hdr.mpls[1].label = vpn_label;
      hdr.mpls[1].ttl = hdr.ipv4.ttl;
      hdr.mpls[1].bos = 1;
      /*
       * Set nexthop_id for further forwarding process
       */
      ig_md.nexthop_id = nexthop_id;
   }



   action act_ipv4_srv_encap_set_nexthop(ipv6_addr_t target, NextHopId_t nexthop_id) {
      ig_md.ethertype = ETHERTYPE_IPV6;
      hdr.ipv4b.setValid();
      hdr.ipv4b = hdr.ipv4;
      hdr.ipv4.setInvalid();
      hdr.ipv6.setValid();
      hdr.ipv6.version = 6;
      hdr.ipv6.payload_len = hdr.ipv4b.total_len;
      hdr.ipv6.next_hdr = IP_PROTOCOL_IPV4;
      hdr.ipv6.hop_limit = 255;
      hdr.ipv6.src_addr = target;
      hdr.ipv6.dst_addr = target;
      ig_md.nexthop_id = nexthop_id;
   }



   
   table tbl_ipv4_fib_host {
      key = {
         /*
          * we match /32 host route
          */
         hdr.ipv4b.dst_addr: exact;
         ig_md.vrf: exact;
      }
      actions = {
         act_ipv4_cpl_set_nexthop;
         act_ipv4_set_nexthop;
         act_ipv4_mpls_encap_set_nexthop;
         act_ipv4_srv_encap_set_nexthop;
         @defaultonly NoAction;
      }
      size = IPV4_HOST_TABLE_SIZE;
      const default_action = NoAction();
   }
   
   table tbl_ipv4_fib_lpm {
      key = {
         /*
          * we match network route via Long Prefix Match kind operation
          */
         hdr.ipv4b.dst_addr: lpm;
         ig_md.vrf: exact;
      }
      actions = {
         act_ipv4_cpl_set_nexthop;
         act_ipv4_set_nexthop;
         act_ipv4_mpls_encap_set_nexthop;
         act_ipv4_srv_encap_set_nexthop;
         act_ipv4_fib_discard;
         @defaultonly NoAction;
      }
      size = IPV4_LPM_TABLE_SIZE;
      default_action = NoAction();
   }

   apply {
        /*                                             
         * It is a dataplane packet                 
         */                                            
        //if (hdr.ipv4.isValid() && hdr.ipv4.ttl > 1) {  
        if (ig_md.srv_op_type==4)  {  
            ig_md.ethertype = ETHERTYPE_IPV4;
           if (!tbl_ipv4_fib_host.apply().hit) {           
              tbl_ipv4_fib_lpm.apply();                    
           }                                           
        }               
   }                               
}   

#endif // _IG_CTL_IPv4b_P4_
   
