#ifndef _EGRESS_CONTROL_P4_
#define _EGRESS_CONTROL_P4_

/*------------------ E G R E S S  M A T C H - A C T I O N ------------------- */

control eg_ctl(
    /* User */
    inout headers ig_hdr,
    inout ingress_metadata_t ig_md,
    /* Intrinsic */
    inout standard_metadata_t ig_intr_md)
{
    apply {
        egress_headers_t eg_hdr;
        if (ig_md.need_recir == 1) recirculate<egress_headers_t>(eg_hdr);
    }
}

#endif // _EGRESS_CONTROL_P4_                                                    
