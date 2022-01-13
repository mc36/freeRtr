from ..bf_gbl_env.var_env import *


def writePbrVrfRules6(
    self, op_type, vrf, tvrf, thop, pri, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm
):
    if self.pbr == False:
        return
    tbl_global_path = "ig_ctl.ig_ctl_pbr"
    tbl_name = "%s.tbl_ipv6_pbr" % (tbl_global_path)
    tbl_action_name = "%s.act_setvrf" % (tbl_global_path)
    key_field_list = [
        gc.KeyTuple("ig_md.vrf", vrf),
        gc.KeyTuple("$MATCH_PRIORITY", pri),
        gc.KeyTuple("hdr.ipv6.next_hdr", pr, prm),
        gc.KeyTuple("hdr.ipv6.src_addr", sa, sam),
        gc.KeyTuple("hdr.ipv6.dst_addr", da, dam),
        gc.KeyTuple("ig_md.layer4_srcprt", sp, spm),
        gc.KeyTuple("ig_md.layer4_dstprt", dp, dpm),
        gc.KeyTuple("hdr.ipv6.traffic_class", ts, tsm),
        gc.KeyTuple("hdr.ipv6.flow_label", fl, flm),
    ]
    data_field_list = [
        gc.DataTuple("vrf_id", tvrf),
    ]
    key_annotation_fields = {
        "hdr.ipv6.src_addr": "ipv6",
        "hdr.ipv6.dst_addr": "ipv6",
    }
    data_annotation_fields = {
    }
    self._processEntryFromControlPlane(
        op_type,
        tbl_name,
        key_field_list,
        data_field_list,
        tbl_action_name,
        key_annotation_fields,
        data_annotation_fields,
    )
