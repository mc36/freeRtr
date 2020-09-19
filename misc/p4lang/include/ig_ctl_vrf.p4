#ifndef _IG_CTL_VRF_P4_
#define _IG_CTL_VRF_P4_

control IngressControlVRF(inout headers hdr,
                          inout ingress_metadata_t ig_md,
                          inout standard_metadata_t ig_intr_md) {

    action act_set_vrf (switch_vrf_t vrf) {
        ig_md.vrf = vrf;
    }

    action act_set_default_vrf () {
        ig_md.vrf = 0;
    }

    action act_set_mpls_xconn_encap(NextHopId_t target, label_t tunlab, label_t svclab) {
        ig_md.vrf = 0;
        ig_md.mpls0_valid = 0;
        ig_md.mpls1_valid = 0;
        ig_md.mpls0_remove = 0;
        ig_md.mpls1_remove = 0;
        ig_md.arp_valid = 0;
        ig_md.llc_valid = 0;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
        hdr.eth2.setValid();
        hdr.eth2.dst_mac_addr = hdr.ethernet.dst_mac_addr;
        hdr.eth2.src_mac_addr = hdr.ethernet.src_mac_addr;
        hdr.eth2.ethertype = ig_md.ethertype;
        ig_md.nexthop_id = target;
        ig_md.ethertype = ETHERTYPE_MPLS_UCAST;
        hdr.mpls0.setValid();
        hdr.mpls0.label = tunlab;
        hdr.mpls0.ttl = 255;
        hdr.mpls1.setValid();
        hdr.mpls1.label = svclab;
        hdr.mpls1.ttl = 255;
        hdr.mpls1.bos = 1;
    }

    action act_set_bridge (SubIntId_t bridge) {
        ig_md.vrf = 0;
        ig_md.mpls0_valid = 0;
        ig_md.mpls1_valid = 0;
        ig_md.arp_valid = 0;
        ig_md.llc_valid = 0;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
        ig_md.bridge_id = bridge;
    }

    table tbl_vrf {
        key = {
ig_md.source_id:
            exact;
        }
        actions = {
            act_set_vrf;
            act_set_mpls_xconn_encap;
            act_set_bridge;
            act_set_default_vrf;
        }
        size = VRF_TABLE_SIZE;
        default_action = act_set_default_vrf();
    }

    apply {
        tbl_vrf.apply();
    }

}
#endif // _IG_CTL_VRF_P4_                                                         
