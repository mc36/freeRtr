#ifndef _INGRESS_PARSER_P4_ 
#define _INGRESS_PARSER_P4_ 

/*------------------ I N G R E S S   P A R S E R -----------------------------*/ 
                                                                                 
parser ig_prs_main(packet_in pkt,                                                
   /* User */                                                                    
   out headers hdr,                                                              
   inout ingress_metadata_t ig_md,                                                            
   /* Intrinsic */                                                               
   inout standard_metadata_t ig_intr_md) {                                
                                                                                 
   state start {                                                             
      transition select(ig_intr_md.ingress_port) {                               
          CPU_PORT: prs_pkt_out;                                                 
          default: prs_ethernet;                                                       
      }                                                                          
   }                                                                             
                                                                                 
   state prs_pkt_out {                                                           
      pkt.extract(hdr.pkt_out);                                                  
      transition accept;                                                         
   }                                                                             

   state prs_ethernet {                              
      pkt.extract(hdr.ethernet);                  
      transition select(hdr.ethernet.ethertype) {    
         0 &&& 0xfe00: prs_llc; /* LLC SAP frame */ 
         0 &&& 0xfa00: prs_llc; /* LLC SAP frame */ 
         ETHERTYPE_VLAN : prs_vlan;
         ETHERTYPE_MPLS_UCAST : prs_mpls0;
         ETHERTYPE_IPV4: prs_ipv4;                   
         ETHERTYPE_IPV6: prs_ipv6;                   
         ETHERTYPE_ARP: prs_arp;
         ETHERTYPE_LACP:prs_control;
         ETHERTYPE_LLDP:prs_control;
         default: accept;                            
      }                                              
   }                                                 

   state prs_vlan {
      pkt.extract(hdr.vlan);
      transition select(hdr.vlan.ethertype) {
         0 &&& 0xfe00: prs_llc; /* LLC SAP frame */ 
         0 &&& 0xfa00: prs_llc; /* LLC SAP frame */ 
         ETHERTYPE_MPLS_UCAST : prs_mpls0;
         ETHERTYPE_IPV4: prs_ipv4;
         ETHERTYPE_IPV6: prs_ipv6;                   
         ETHERTYPE_ARP: prs_arp;
         ETHERTYPE_LACP:prs_control;
         ETHERTYPE_LLDP:prs_control;
         default: accept;
      }
   }

   state prs_mpls0 {
      pkt.extract(hdr.mpls0);
      ig_md.mpls0_valid = 1;
      transition select(hdr.mpls0.bos) {
          1w0: prs_mpls1;
          1w1: prs_mpls_bos;
          default: accept;
      }
   }

   state prs_mpls1 {
      pkt.extract(hdr.mpls1);
      ig_md.mpls1_valid = 1;
      transition select(hdr.mpls1.bos) {
          1w0: accept;
          1w1: prs_mpls_bos;
          default: accept;
      }
   }

   state prs_mpls_bos {
      transition select((pkt.lookahead<bit<4>>())[3:0]) {
         4w0x4: prs_ipv4; /* IPv4 only for now */
         4w0x6: prs_ipv6; /* IPv6 is in next lab */
         default: prs_eth2; /* EoMPLS is pausing problem if we don't resubmit() */
      }
   }

   state prs_eth2 {
      pkt.extract(hdr.eth2);
      transition select(hdr.eth2.ethertype) {
         ETHERTYPE_IPV4: prs_ipv4;
         ETHERTYPE_IPV6: prs_ipv6;
         default: accept;
      }
   }



   state prs_ipv4 {                                                                 
      pkt.extract(hdr.ipv4);                                                     
      ig_md.ipv4_valid = 1;
      transition select(hdr.ipv4.protocol) {
         IP_PROTOCOL_UDP: prs_udp;
         IP_PROTOCOL_TCP: prs_tcp;
         IP_PROTOCOL_IPV4: prs_ipv4b;
         IP_PROTOCOL_IPV6: prs_ipv6b;
         IP_PROTOCOL_SRL2: prs_eth3;
         default: accept;
      }
   }                                                                                

   state prs_ipv6 {                                                                 
      pkt.extract(hdr.ipv6);                                                     
      ig_md.ipv6_valid = 1;
      transition select(hdr.ipv6.next_hdr) {
         IP_PROTOCOL_UDP: prs_udp;
         IP_PROTOCOL_TCP: prs_tcp;
         IP_PROTOCOL_IPV4: prs_ipv4b;
         IP_PROTOCOL_IPV6: prs_ipv6b;
         IP_PROTOCOL_SRL2: prs_eth3;
         default: accept;
      }
   }                                                                                


   state prs_udp {
      pkt.extract(hdr.udp);
      ig_md.layer4_srcprt = hdr.udp.src_port;
      ig_md.layer4_dstprt = hdr.udp.dst_port;
      transition accept;
   }

   state prs_tcp {
      pkt.extract(hdr.tcp);
      ig_md.layer4_srcprt = hdr.tcp.src_port;
      ig_md.layer4_dstprt = hdr.tcp.dst_port;
      transition accept;
   }

   state prs_eth3 {
      pkt.extract(hdr.eth3);
      transition accept;
   }

   state prs_ipv4b {                                                                 
      pkt.extract(hdr.ipv4b);                                                     
      transition accept;
   }                                                                                

   state prs_ipv6b {                                                                 
      pkt.extract(hdr.ipv6b);                                                     
      transition accept;
   }                                                                                

    state prs_arp {          
       pkt.extract(hdr.arp); 
       ig_md.arp_valid = 1;
       transition accept;    
    }                        

  state prs_control {
      ig_md.llc_valid = 1;
    transition accept;
  }                   

   state prs_llc {                                       
      pkt.extract(hdr.llc);                           
      ig_md.llc_valid = 1;
      transition select(hdr.llc.dsap, hdr.llc.ssap) {    
         /*                                              
          * (0xaa, 0xaa): prs_snap_header;               
          * From switch.p4 this case should be processed.
          * We are not there yet :-)                     
          */                                             
         (0xfe, 0xfe): accept;                           
         default: accept;                                
      }                                                  
   }                                                     

}                                                                                

#endif // _INGRESS_PARSER_P4_
