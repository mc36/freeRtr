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


def writeLabsNhRules(self, op_type, nexthop, dst_mac_addr, src_mac_addr, port, aclport, labs, lab0, lab1, lab2, lab3, lab4, lab5, lab6, lab7, lab8, lab9):
    # for any reason, control plane is sending a msg
    # with port=-1
    if port < 0:
        return

    tbl_global_path = "eg_ctl.eg_ctl_nexthop"
    tbl_name = "%s.tbl_nexthop" % (tbl_global_path)
    tbl_action_name = "%s.act_ipv4_labs%d" % (tbl_global_path, labs-1)

    key_fields = [gc.KeyTuple("eg_md.nexthop_id", nexthop)]
    data_fields = [
        gc.DataTuple("dst_mac_addr", dst_mac_addr),
        gc.DataTuple("src_mac_addr", src_mac_addr),
        gc.DataTuple("egress_port", port),
        gc.DataTuple("acl_port", aclport),
    ]
    if labs >= 1:
        data_fields.append(gc.DataTuple("lab0", lab0))
    if labs >= 2:
        data_fields.append(gc.DataTuple("lab1", lab1))
    if labs >= 3:
        data_fields.append(gc.DataTuple("lab2", lab2))
    if labs >= 4:
        data_fields.append(gc.DataTuple("lab3", lab3))
    if labs >= 5:
        data_fields.append(gc.DataTuple("lab4", lab4))
    if labs >= 6:
        data_fields.append(gc.DataTuple("lab5", lab5))
    if labs >= 7:
        data_fields.append(gc.DataTuple("lab6", lab6))
    if labs >= 8:
        data_fields.append(gc.DataTuple("lab7", lab7))
    if labs >= 9:
        data_fields.append(gc.DataTuple("lab8", lab8))
    if labs >= 10:
        data_fields.append(gc.DataTuple("lab9", lab9))
    key_annotation_fields = {}
    data_annotation_fields = {"dst_mac_addr": "mac", "src_mac_addr": "mac"}
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
