from ..bf_gbl_env.var_env import *


def writeFlowspec6Rules(
    self, op_type, vrf, meter, bytes, interval, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm
):
    tbl_global_path = "ig_ctl.ig_ctl_flowspec"
    tbl_name = "%s.tbl_ipv6_flowspec" % (tbl_global_path)
    tbl_action_name = "%s.act6_%s" % (tbl_global_path, act)
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
         gc.DataTuple("metid", (meter+1)),
    ]
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
    tbl_global_path = "ig_ctl.ig_ctl_flowspec"
    tbl_name = "%s.policer6" % (tbl_global_path)
    self._processMeterFromControlPlane(
        op_type,
        tbl_name,
        (meter+1),
        bytes,
        interval
    )
