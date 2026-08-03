from ..bf_gbl_env.var_env import *


def writeInspectRules6(
    self, op_type, port, proto, sa, sp, ta, tp
):
    tbl_global_path1 = "ig_ctl.ig_ctl_acl_in"
    tbl_name1 = "%s.tbl_ipv6_insp" % (tbl_global_path1)
    tbl_action_name1 = "%s.act_insp6" % (tbl_global_path1)
    key_field_list1 = [
        gc.KeyTuple("ig_md.source_id", port),
        gc.KeyTuple("hdr.ipv6.next_hdr", proto),
        gc.KeyTuple("hdr.ipv6.src_addr", sa),
        gc.KeyTuple("hdr.ipv6.dst_addr", ta),
        gc.KeyTuple("ig_md.layer4_srcprt", sp),
        gc.KeyTuple("ig_md.layer4_dstprt", tp),
    ]
    data_field_list1 = []
    key_annotation_fields1 = {
        "hdr.ipv6.src_addr": "ipv6",
        "hdr.ipv6.dst_addr": "ipv6",
    }
    data_annotation_fields1 = {}
    self._processEntryFromControlPlane(
        op_type,
        tbl_name1,
        key_field_list1,
        data_field_list1,
        tbl_action_name1,
        key_annotation_fields1,
        data_annotation_fields1,
    )

    tbl_global_path2 = "ig_ctl.ig_ctl_acl_out"
    tbl_name2 = "%s.tbl_ipv6_insp" % (tbl_global_path2)
    tbl_action_name2 = "%s.act_insp6" % (tbl_global_path2)
    key_field_list2 = [
        gc.KeyTuple("ig_md.aclport_id", port),
        gc.KeyTuple("hdr.ipv6.next_hdr", proto),
        gc.KeyTuple("hdr.ipv6.src_addr", ta),
        gc.KeyTuple("hdr.ipv6.dst_addr", sa),
        gc.KeyTuple("ig_md.layer4_srcprt", tp),
        gc.KeyTuple("ig_md.layer4_dstprt", sp),
    ]
    data_field_list2 = []
    key_annotation_fields2 = {
        "hdr.ipv6.src_addr": "ipv6",
        "hdr.ipv6.dst_addr": "ipv6",
    }
    data_annotation_fields2 = {}
    self._processEntryFromControlPlane(
        op_type,
        tbl_name2,
        key_field_list2,
        data_field_list2,
        tbl_action_name2,
        key_annotation_fields2,
        data_annotation_fields2,
    )
