from ..bf_gbl_env.var_env import *


def writePwheNhRules(self, op_type, nexthop, dst_mac_addr, src_mac_addr, port, aclport, core_dst_mac, core_src_mac, label, vpnlab):
    # for any reason, control plane is sending a msg
    # with port=-1
    if port < 0:
        return

    tbl_global_path = "eg_ctl.eg_ctl_nexthop"
    tbl_name = "%s.tbl_nexthop" % (tbl_global_path)
    tbl_action_name = "%s.act_ipv4_pwhe" % (tbl_global_path)

    key_fields = [gc.KeyTuple("eg_md.nexthop_id", nexthop)]
    data_fields = [
        gc.DataTuple("dst_mac_addr", dst_mac_addr),
        gc.DataTuple("src_mac_addr", src_mac_addr),
        gc.DataTuple("egress_port", port),
        gc.DataTuple("acl_port", aclport),
        gc.DataTuple("core_dst_mac", core_dst_mac),
        gc.DataTuple("core_src_mac", core_src_mac),
        gc.DataTuple("egress_label", label),
        gc.DataTuple("vpn_label", vpnlab),
    ]
    key_annotation_fields = {}
    data_annotation_fields = {"dst_mac_addr": "mac", "src_mac_addr": "mac", "core_dst_mac": "mac", "core_src_mac": "mac"}
    self._processEntryFromControlPlane(
        op_type,
        tbl_name,
        key_fields,
        data_fields,
        tbl_action_name,
        key_annotation_fields,
        data_annotation_fields,
    )


def writeNeighborRules6(self, op_type, dst_ip_addr, port, vrf):
    # for any reason, control plane is sending a msg
    # with port=-1
    if port < 0:
        return

    tbl_global_path = "ig_ctl.ig_ctl_ipv6"
    tbl_name = "%s.tbl_ipv6_fib_host" % (tbl_global_path)
    tbl_action_name = "%s.act_ipv6_set_nexthop" % (tbl_global_path)

    key_fields = [
        gc.KeyTuple("hdr.ipv6.dst_addr", dst_ip_addr),
        gc.KeyTuple("ig_md.vrf", vrf),
    ]
    data_fields = [gc.DataTuple("nexthop_id", port)]
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
    if self.srv6 == False:
        return
    tbl_global_path = "ig_ctl.ig_ctl_ipv6b"
    tbl_name = "%s.tbl_ipv6_fib_host" % (tbl_global_path)
    tbl_action_name = "%s.act_ipv6_set_nexthop" % (tbl_global_path)

    key_fields = [
        gc.KeyTuple("hdr.ipv6b.dst_addr", dst_ip_addr),
        gc.KeyTuple("ig_md.vrf", vrf),
    ]
    data_fields = [gc.DataTuple("nexthop_id", port)]
    key_annotation_fields = {"hdr.ipv6b.dst_addr": "ipv6"}
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
