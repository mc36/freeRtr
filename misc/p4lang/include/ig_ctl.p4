#ifndef _INGRESS_CONTROL_P4_ 
#define _INGRESS_CONTROL_P4_ 


/*------------------ I N G R E S S - M A T C H - A C T I O N ---------------- */   
                                                                                   
control ig_ctl(inout headers hdr,                                                  
               inout ingress_metadata_t ig_md,                                                
               inout standard_metadata_t ig_intr_md) {        
                                                                                   
   IngressControlARP() ig_ctl_arp; 
   IngressControlMPLS() ig_ctl_mpls;
   IngressControlMPLS2() ig_ctl_mpls2;
   IngressControlBridge() ig_ctl_bridge;
   IngressControlIPv4() ig_ctl_ipv4;
   IngressControlIPv6() ig_ctl_ipv6;
   IngressControlIPv4b() ig_ctl_ipv4b;
   IngressControlIPv6b() ig_ctl_ipv6b;
   IngressControlNexthop() ig_ctl_nexthop;
   IngressControlVlanIn() ig_ctl_vlan_in;  
   IngressControlVlanOut() ig_ctl_vlan_out;
   IngressControlVRF() ig_ctl_vrf; 
   IngressControlLLC() ig_ctl_llc;
   
                                                                                   
   apply {                                                                         
      if (ig_intr_md.ingress_port == CPU_PORT) {                                   
         /*                                                                        
          * pkt received from the controlled has a pkt_out header                  
          * that containes egress port id. Once retrieve                           
          * we remove the pkt_out header (setInvalid)                              
          * So it will not be taken into accoiunt by deparser                      
          */                                                                       
         ig_intr_md.egress_spec = hdr.pkt_out.egress_port;                    
         hdr.pkt_out.setInvalid();                                                
      } else {                                                                     
         /*                                                                        
          * So it is a dataplane packet                                                     
          */                                                                       
           if (hdr.mpls[0].isValid()) {
             ig_md.mpls0_valid = 1;
           }
           if (hdr.mpls[1].isValid()) {
             ig_md.mpls1_valid = 1;
           }
           if (ig_md.bridge_id != 0) {
             ig_md.ipv4_valid=0;
             ig_md.ipv6_valid=0;
           }
         ig_ctl_vlan_in.apply(hdr,ig_md,ig_intr_md);
         ig_ctl_vrf.apply(hdr,ig_md);
         ig_ctl_arp.apply(hdr,ig_md,ig_intr_md);
         ig_ctl_llc.apply(hdr,ig_md,ig_intr_md); 
         ig_ctl_mpls.apply(hdr,ig_md,ig_intr_md); 
         ig_ctl_ipv4.apply(hdr,ig_md,ig_intr_md); 
         ig_ctl_ipv6.apply(hdr,ig_md,ig_intr_md); 
         ig_ctl_bridge.apply(hdr,ig_md,ig_intr_md); 
         ig_ctl_ipv4b.apply(hdr,ig_md,ig_intr_md); 
         ig_ctl_ipv6b.apply(hdr,ig_md,ig_intr_md); 
         if ( ig_md.nexthop_id == CPU_PORT) {
           hdr.pkt_in.setValid();
           hdr.pkt_in.ingress_port = ig_intr_md.ingress_port;
           ig_intr_md.egress_spec = ig_md.nexthop_id;
           return;
         }

           if (hdr.vlan.isValid()) {
              hdr.vlan.setInvalid();
           }

           if (ig_md.srv_op_type != 0) {
              hdr.ipv6.setInvalid();
           }
           if (ig_md.srv_op_type == 2) {
              hdr.eth3.setInvalid();
           }

         ig_ctl_mpls2.apply(hdr,ig_md,ig_intr_md); 
         ig_ctl_nexthop.apply(hdr,ig_md,ig_intr_md); 
         ig_ctl_vlan_out.apply(hdr,ig_md,ig_intr_md);
      }                                                                               
   }
}                                                                                  
                                                                                   
#endif // _INGRESS_CONTROL_P4_
