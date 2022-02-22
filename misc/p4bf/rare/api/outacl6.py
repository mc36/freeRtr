from ..bf_gbl_env.var_env import *


def writeOutAcl6Rules(
    self, op_type, port, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm, gr, grm
):
    tbl_global_path = "ig_ctl.ig_ctl_acl_out"
    tbl_name = "%s.tbl_ipv6_acl" % (tbl_global_path)
    tbl_action_name = "%s.act_%s6" % (tbl_global_path, act)
    key_field_list = [
        gc.KeyTuple("ig_md.aclport_id", port),
        gc.KeyTuple("$MATCH_PRIORITY", pri),
        gc.KeyTuple("hdr.ipv6.next_hdr", pr, prm),
        gc.KeyTuple("hdr.ipv6.src_addr", sa, sam),
        gc.KeyTuple("hdr.ipv6.dst_addr", da, dam),
        gc.KeyTuple("ig_md.layer4_srcprt", sp, spm),
        gc.KeyTuple("ig_md.layer4_dstprt", dp, dpm),
        gc.KeyTuple("hdr.ipv6.traffic_class", ts, tsm),
        gc.KeyTuple("hdr.ipv6.flow_label", fl, flm),
    ]
    if self.sgt == True:
        key_field_list.append(gc.KeyTuple("ig_md.sec_grp_id", gr, grm))
    data_field_list = []
    key_annotation_fields = {
        "hdr.ipv6.src_addr": "ipv6",
        "hdr.ipv6.dst_addr": "ipv6",
    }
    data_annotation_fields = {}
    self._processEntryFromControlPlane(
        op_type,
        tbl_name,
        key_field_list,
        data_field_list,
        tbl_action_name,
        key_annotation_fields,
        data_annotation_fields,
    )
