#ifndef _IG_CTL_BRIDGE_P4_  
#define _IG_CTL_BRIDGE_P4_  
   
control IngressControlBridge(inout headers hdr,
                           inout ingress_metadata_t ig_md,
                           inout standard_metadata_t ig_intr_md) {


   action send_to_cpu() {
        ig_md.nexthop_id = CPU_PORT;
        // Packets sent to the controller needs to be prepended with the
        // packet-in header. By setting it valid we make sure it will be
        // deparsed on the wire (see c_deparser).
   }


   action act_set_bridge_port(PortId_t port) {
      ig_md.bridge_src = port;
   }

   action act_bridge_miss() {
      ig_md.bridge_src = 0;
   }
   
   table tbl_bridge_learn {
      key = {
         ig_md.bridge_id: exact;
         hdr.ethernet.src_mac_addr: exact;
      }
      actions = {
         act_set_bridge_port;
         act_bridge_miss;
      }
      size = VRF_TABLE_SIZE;                                                                           
      default_action = act_bridge_miss();
   }


   action act_set_bridge_out(PortId_t port) {
      ig_md.bridge_trg = port;
   }  
   
   action act_set_bridge_vpls(PortId_t port, label_t lab_tun, label_t lab_svc) {
      ig_md.bridge_trg = port;
      ig_md.vrf = 0;
      ig_md.mpls0_valid = 0;
      ig_md.mpls1_valid = 0;
      ig_md.arp_valid = 0;
      ig_md.llc_valid = 0;
      ig_md.ipv4_valid = 0;
      ig_md.ipv6_valid = 0;
      hdr.eth2.setValid();
      hdr.eth2.dst_mac_addr = hdr.ethernet.dst_mac_addr;
      hdr.eth2.src_mac_addr = hdr.ethernet.src_mac_addr;
      hdr.eth2.ethertype = ig_md.ethertype;
      ig_md.target_id = port;
      ig_md.ethertype = ETHERTYPE_MPLS_UCAST;
      hdr.mpls.push_front(2);
      hdr.mpls[0].setValid();
      hdr.mpls[0].label = lab_tun;
      hdr.mpls[0].ttl = 255;
      hdr.mpls[1].setValid();
      hdr.mpls[1].label = lab_svc;
      hdr.mpls[1].ttl = 255;
      hdr.mpls[1].bos = 1;
      ig_md.mpls_op_type = 3;
   }

   action act_set_bridge_srv(PortId_t port, ipv6_addr_t target) {
      ig_md.bridge_trg = port;
      ig_md.vrf = 0;
      ig_md.mpls0_valid = 0;
      ig_md.mpls1_valid = 0;
      ig_md.arp_valid = 0;
      ig_md.llc_valid = 0;
      ig_md.ipv4_valid = 0;
      ig_md.ipv6_valid = 0;
      hdr.vlan.setInvalid();
      hdr.eth2.setValid();
      hdr.eth2 = hdr.ethernet;
      hdr.eth2.ethertype = ig_md.ethertype;
      ig_md.ethertype = ETHERTYPE_IPV6;
      hdr.ipv6c.setValid();
      hdr.ipv6c.version = 6;
      hdr.ipv6c.payload_len = (bit<16>)ig_intr_md.packet_length - (bit<16>)ig_md.vlan_size;
      hdr.ipv6c.next_hdr = IP_PROTOCOL_SRL2;
      hdr.ipv6c.hop_limit = 255;
      hdr.ipv6c.src_addr = target;
      hdr.ipv6c.dst_addr = target;
   }

   action act_bridge_punt() {
      ig_md.bridge_trg = 0;
   }

   table tbl_bridge_target {
      key = {
         ig_md.bridge_id: exact;
         hdr.ethernet.dst_mac_addr: exact;
      }
      actions = {
         act_set_bridge_out;
         act_set_bridge_vpls;
         act_set_bridge_srv;
         act_bridge_punt;
      }
      size = VRF_TABLE_SIZE;                                                                           
      default_action = act_bridge_punt();
   }


   apply {
         if (ig_md.bridge_id == 0) {
            return;
         }
        ig_md.vrf = 0;
        ig_md.mpls0_valid = 0;
        ig_md.mpls1_valid = 0;
        ig_md.arp_valid = 0;
        ig_md.llc_valid = 0;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
        tbl_bridge_learn.apply();
        tbl_bridge_target.apply();
        ig_md.target_id = ig_md.bridge_trg;
        if ((ig_md.bridge_src == 0) || (ig_md.bridge_trg == 0)) {
           send_to_cpu();
           return;
        }
        if (hdr.mpls[1].isValid() && (ig_md.mpls_op_type != 3)) {
          hdr.eth2.setInvalid();
          hdr.mpls[1].setInvalid();
          hdr.mpls[0].setInvalid();
          hdr.vlan.setInvalid();
          ig_md.mpls0_remove = 0;
          ig_md.mpls1_remove = 0;
        }
   }                               
}   

#endif // _IG_CTL_BRIDGE_P4_
   
