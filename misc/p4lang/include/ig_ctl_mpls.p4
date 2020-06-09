#ifndef _IG_CTL_MPLS_P4_  
#define _IG_CTL_MPLS_P4_  
   
control IngressControlMPLS(inout headers hdr,
                           inout ingress_metadata_t ig_md,
                           inout standard_metadata_t ig_intr_md) {

   action act_mpls_swap0_set_nexthop(label_t egress_label, NextHopId_t nexthop_id) {
      /*              
       * Encapsulate MPLS header
       */             
      hdr.mpls[0].label = egress_label;
      /*              
       * Indicate nexthop_id
       */
      ig_md.nexthop_id = nexthop_id;
      ig_md.mpls_op_type = 0;
      ig_md.ipv4_valid = 0;
      ig_md.ipv6_valid = 0;
   }

   action act_mpls_swap1_set_nexthop(label_t egress_label, NextHopId_t nexthop_id) {
      /*              
       * Encapsulate MPLS header
       */             
      hdr.mpls[1].label = egress_label;
      /*              
       * Indicate nexthop_id
       */
      ig_md.nexthop_id = nexthop_id;
      ig_md.mpls_op_type = 0;
      ig_md.ipv4_valid = 0;
      ig_md.ipv6_valid = 0;
   }

   action act_mpls_decap_ipv4(switch_vrf_t vrf) {
      /*
       * Egress packet is back now an IPv4 packet
       * (LABEL PHP )
       */
      /*
       * Decapsulate MPLS header
       */
//      hdr.mpls[0].setInvalid();
//      hdr.ipv4.setValid();
      /*
       * Indicate effective VRF during
       * MPLS tunnel decap
       */
      ig_md.vrf = vrf;
      ig_md.mpls_op_type = 1;
      ig_md.mpls0_remove = 1;

   }

   action act_mpls_decap_l3vpn(switch_vrf_t vrf) {
      /*
       * Egress packet is back now an IPv4 packet
       * (LABEL PHP )
       */
      /*
       * Decapsulate MPLS header
       */
//      hdr.mpls[1].setInvalid();
//      hdr.ipv4.setValid();
      /*
       * Indicate effective VRF during
       * MPLS tunnel decap
       */
      ig_md.vrf = vrf;
      ig_md.mpls_op_type = 1;
      ig_md.mpls1_remove = 1;

   }

   action act_mpls_decap_l2vpn(SubIntId_t port) {
      /*
       * Egress packet is back now an IPv4 packet
       * (LABEL PHP )
       */
      /*
       * Decapsulate MPLS header
       */
      ig_md.ethertype = hdr.eth2.ethertype;
      hdr.ethernet.dst_mac_addr = hdr.eth2.dst_mac_addr;
      hdr.ethernet.src_mac_addr = hdr.eth2.src_mac_addr;
      hdr.eth2.setInvalid();
      hdr.mpls[1].setInvalid();
      hdr.mpls[0].setInvalid();
      hdr.vlan.setInvalid();
      ig_md.target_id = port;
      /*
       * Indicate effective VRF during
       * MPLS tunnel decap
       */
      ig_md.mpls0_remove = 0;
      ig_md.mpls1_remove = 0;
      ig_md.mpls_op_type = 2;
      ig_md.ipv4_valid = 0;
      ig_md.ipv6_valid = 0;
   }

   action act_mpls_decap_vpls(SubIntId_t bridge) {
      /*
       * Egress packet is back now an IPv4 packet
       * (LABEL PHP )
       */
      ig_md.bridge_id = bridge;
      ig_md.bridge_src = bridge;
      ig_md.ethertype = hdr.eth2.ethertype;
      hdr.ethernet.dst_mac_addr = hdr.eth2.dst_mac_addr;
      hdr.ethernet.src_mac_addr = hdr.eth2.src_mac_addr;
      /*
       * Indicate effective VRF during
       * MPLS tunnel decap
       */
      ig_md.mpls_op_type = 2;
      ig_md.ipv4_valid = 0;
      ig_md.ipv6_valid = 0;
   }




   table tbl_mpls_fib {
      key = {
         ig_md.mpls_label: exact;
      }
      actions = {
         /*
          * mpls core swap
          */
         act_mpls_swap0_set_nexthop;

         /*
          * mpls decapsulation if PHP
          */
         act_mpls_decap_ipv4;

         /*
          * mpls decapsulation if PHP
          */
         act_mpls_decap_l2vpn;

          
         /*
          * mpls decapsulation if PHP
          */
         act_mpls_decap_vpls;
          
         /*
          * Default action;
          */
         NoAction;
      }
        size = MPLS_TABLE_SIZE;
        default_action = NoAction();
    }


   table tbl_mpls_fib_decap {
      key = {
         ig_md.mpls_label: exact;
      }
      actions = {
         /*
          * mpls core swap
          */
         act_mpls_swap1_set_nexthop;
          
         /*
          * mpls decapsulation if PHP
          */
         act_mpls_decap_l3vpn;

         /*
          * mpls decapsulation if PHP
          */
         act_mpls_decap_l2vpn;
          
         /*
          * mpls decapsulation if PHP
          */
         act_mpls_decap_vpls;
          
         /*
          * Default action;
          */
         NoAction;
      }
        size = MPLS_TABLE_SIZE;
        default_action = NoAction();
    }



   apply {
         if (ig_md.mpls0_valid == 1) {
            ig_md.mpls_label = hdr.mpls[0].label;
            tbl_mpls_fib.apply();
            if ((ig_md.mpls_op_type == 1) && (ig_md.mpls1_valid == 1)) {
               ig_md.mpls_label = hdr.mpls[1].label;
               tbl_mpls_fib_decap.apply();
            }
         }
   }                               
}   

#endif // _IG_CTL_MPLS_P4_
   
