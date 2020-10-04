#ifndef _IG_CTL_CoPP_P4_
#define _IG_CTL_CoPP_P4_

control IngressControlCoPP(inout headers hdr,
                           inout ingress_metadata_t ig_md,
                           inout standard_metadata_t ig_intr_md) {

    action act_deny() {
        mark_to_drop(ig_intr_md);
        ig_md.dropping = 1;
    }

    action act_permit() {
        ig_md.dropping = 0;
    }


    table tbl_ipv4_copp {
        key = {
hdr.ipv4.protocol:
            ternary;
hdr.ipv4.src_addr:
            ternary;
hdr.ipv4.dst_addr:
            ternary;
ig_md.layer4_srcprt:
            ternary;
ig_md.layer4_dstprt:
            ternary;
        }
        actions = {
            act_permit;
            act_deny;
            @defaultonly NoAction;
        }
        size = IPV4_COPP_TABLE_SIZE;
        const default_action = NoAction();
    }

    table tbl_ipv6_copp {
        key = {
hdr.ipv6.next_hdr:
            ternary;
hdr.ipv6.src_addr:
            ternary;
hdr.ipv6.dst_addr:
            ternary;
ig_md.layer4_srcprt:
            ternary;
ig_md.layer4_dstprt:
            ternary;
        }
        actions = {
            act_permit;
            act_deny;
            @defaultonly NoAction;
        }
        size = IPV6_COPP_TABLE_SIZE;
        const default_action = NoAction();
    }

    apply {
        if (ig_md.ipv4_valid==1)  {
            tbl_ipv4_copp.apply();
        }
        if (ig_md.ipv6_valid==1)  {
            tbl_ipv6_copp.apply();
        }
    }
}

#endif // _IG_CTL_CoPP_P4_

