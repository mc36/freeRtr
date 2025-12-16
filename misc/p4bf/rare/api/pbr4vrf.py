from ..bf_gbl_env.var_env import *


def writePbrVrfRules4(
    self, op_type, vrf, tvrf, thop, pri, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm, gr, grm
):
    if self.pbr == False:
        return
    tbl_global_path = "ig_ctl.ig_ctl_pbr"
    tbl_name = "%s.tbl_ipv4_pbr" % (tbl_global_path)
    tbl_action_name = "%s.act_setvrf" % (tbl_global_path)
    key_field_list = [
        gc.KeyTuple("ig_md.vrf", vrf),
        gc.KeyTuple("$MATCH_PRIORITY", pri),
        gc.KeyTuple("hdr.ipv4.protocol", pr, prm),
        gc.KeyTuple("hdr.ipv4.src_addr", sa, sam),
        gc.KeyTuple("hdr.ipv4.dst_addr", da, dam),
        gc.KeyTuple("ig_md.layer4_srcprt", sp, spm),
        gc.KeyTuple("ig_md.layer4_dstprt", dp, dpm),
        gc.KeyTuple("hdr.ipv4.diffserv", ts, tsm),
        gc.KeyTuple("hdr.ipv4.identification", fl, flm),
    ]
    if self.sgt == True:
        key_field_list.append(gc.KeyTuple("ig_md.sec_grp_id", gr, grm))
    data_field_list = [
        gc.DataTuple("vrf_id", tvrf),
    ]
    key_annotation_fields = {
        "hdr.ipv4.src_addr": "ipv4",
        "hdr.ipv4.dst_addr": "ipv4",
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
