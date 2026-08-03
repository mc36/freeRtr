from ..bf_gbl_env.var_env import *


def writeInQos6Rules(
    self, op_type, port, meter, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm, gr, grm
):
    tbl_global_path = "ig_ctl.ig_ctl_qos_in"
    tbl_name = "%s.tbl_ipv6_qos" % (tbl_global_path)
    tbl_action_name = "%s.act_%s" % (tbl_global_path, act)
    key_field_list = [
        gc.KeyTuple("ig_md.source_id", port),
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
    if act == "permit":
        data_field_list = [  gc.DataTuple("metid", meter),  ]
    else:
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
