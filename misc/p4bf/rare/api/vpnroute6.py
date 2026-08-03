from ..bf_gbl_env.var_env import *


def writeVpnRules6(
    self, op_type, dst_ip_addr, dst_net_mask, port, vrf, egress_label, vpn_label
):
    if self.mpls == False:
        return
    tbl_global_path = "ig_ctl.ig_ctl_ipv6"
    tbl_name = "%s.tbl_ipv6_fib_lpm" % (tbl_global_path)
    tbl_action_name = "%s.act_ipv6_mpls2_encap_set_nexthop" % (tbl_global_path)

    key_fields = [
        gc.KeyTuple("hdr.ipv6.dst_addr", dst_ip_addr, prefix_len=dst_net_mask),
        gc.KeyTuple("ig_md.vrf", vrf),
    ]
    data_fields = [
        gc.DataTuple("vpn_label", vpn_label),
        gc.DataTuple("egress_label", egress_label),
        gc.DataTuple("nexthop_id", port),
    ]
    key_annotation_fields = {"hdr.ipv6.dst_addr": "ipv6"}
    data_annotation_fields = {}
    self._processEntryFromControlPlane(
        op_type,
        tbl_name,
        key_fields,
        data_fields,
        tbl_action_name,
        key_annotation_fields,
        data_annotation_fields,
    )
