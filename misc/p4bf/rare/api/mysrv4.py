from ..bf_gbl_env.var_env import *


def writeMySrv4rules(self, op_type, glob, dst_addr, vrf):
    if self.srv6 == False:
        return
    tbl_global_path = "ig_ctl.ig_ctl_ipv6"
    tbl_name = "%s.tbl_ipv6_fib_host" % (tbl_global_path)
    tbl_action_name = "%s.act_srv_decap_ipv4" % (tbl_global_path)
    key_field_list = [
        gc.KeyTuple("ig_md.vrf", (glob)),
        gc.KeyTuple("hdr.ipv6.dst_addr", (dst_addr)),
    ]
    data_field_list = [gc.DataTuple("vrf", vrf)]
    key_annotation_fields = {"hdr.ipv6.dst_addr": "ipv6"}
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
