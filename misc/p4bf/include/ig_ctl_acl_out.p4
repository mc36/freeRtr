#ifndef _IG_CTL_Acl_out_P4_
#define _IG_CTL_Acl_out_P4_

control IngressControlAclOut(inout headers hdr, inout ingress_metadata_t ig_md,
                             in ingress_intrinsic_metadata_t ig_intr_md,
                             inout ingress_intrinsic_metadata_for_deparser_t ig_dprsr_md,
                             inout ingress_intrinsic_metadata_for_tm_t ig_tm_md)
{

    action act_deny() {
        ig_dprsr_md.drop_ctl = 1;
    }

    action act_permit() {
    }


    table tbl_ipv4_acl {
        key = {
ig_md.aclport_id:
            exact;
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
        size = IPV4_HOST_TABLE_SIZE;
        const default_action = NoAction();
    }

    table tbl_ipv6_acl {
        key = {
ig_md.aclport_id:
            exact;
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
        size = IPV4_HOST_TABLE_SIZE;
        const default_action = NoAction();
    }

    apply {
        if (ig_md.ipv4_valid==1)  {
            tbl_ipv4_acl.apply();
        }
        if (ig_md.ipv6_valid==1)  {
            tbl_ipv6_acl.apply();
        }
    }
}

#endif // _IG_CTL_Acl_out_P4_
