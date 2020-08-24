#ifndef _IG_CTL_VLAN_OUT_P4_
#define _IG_CTL_VLAN_OUT_P4_

control IngressControlVlanOut(inout headers hdr,
                              inout ingress_metadata_t ig_md,
                              inout standard_metadata_t ig_intr_md) {

    action act_set_vlan_port(SubIntId_t port, vlan_id_t vlan) {
        ig_md.outport_id = port;
        hdr.vlan.setValid();
        hdr.vlan.ethertype = ig_md.ethertype;
        hdr.ethernet.ethertype = ETHERTYPE_VLAN;
        hdr.vlan.vid = vlan;
    }

    action act_set_port() {
        ig_md.outport_id = ig_md.target_id;
        hdr.ethernet.ethertype = ig_md.ethertype;
    }

    table tbl_vlan_out {
        key = {
ig_md.target_id:
            exact;
        }
        actions = {
            act_set_port;
            act_set_vlan_port;
        }
        size = 1024;
        default_action = act_set_port();
    }

    apply {
        tbl_vlan_out.apply();
    }
}

#endif // _IG_CTL_VLAN_OUT_P4_

