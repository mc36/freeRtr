from ..bf_gbl_env.var_env import *



def writeNexthopRules(self, op_type, nexthop, dst_mac_addr, src_mac_addr, port):
    # for any reason, control plane is sending a msg
    # with port=-1
    if port < 0:
        return

    tbl_global_path = "eg_ctl.eg_ctl_nexthop"
    tbl_name = "%s.tbl_nexthop" % (tbl_global_path)
    tbl_action_name = "%s.act_ipv4_fib_hit" % (tbl_global_path)

    key_fields = [gc.KeyTuple("eg_md.nexthop_id", nexthop)]
    data_fields = [
        gc.DataTuple("dst_mac_addr", dst_mac_addr),
        gc.DataTuple("src_mac_addr", src_mac_addr),
        gc.DataTuple("egress_port", port),
    ]
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


def writeNeighborRules4(self, op_type, dst_ip_addr, port, vrf):
    # for any reason, control plane is sending a msg
    # with port=-1
    if port < 0:
        return

    tbl_global_path = "ig_ctl.ig_ctl_ipv4"
    tbl_name = "%s.tbl_ipv4_fib_host" % (tbl_global_path)
    tbl_action_name = "%s.act_ipv4_set_nexthop" % (tbl_global_path)

    key_fields = [
        gc.KeyTuple("hdr.ipv4.dst_addr", dst_ip_addr),
        gc.KeyTuple("ig_md.vrf", vrf),
    ]
    data_fields = [gc.DataTuple("nexthop_id", port)]
    key_annotation_fields = {"hdr.ipv4.dst_addr": "ipv4"}
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
    tbl_global_path = "ig_ctl.ig_ctl_ipv4b"
    tbl_name = "%s.tbl_ipv4_fib_host" % (tbl_global_path)
    tbl_action_name = "%s.act_ipv4_set_nexthop" % (tbl_global_path)

    key_fields = [
        gc.KeyTuple("hdr.ipv4b.dst_addr", dst_ip_addr),
        gc.KeyTuple("ig_md.vrf", vrf),
    ]
    data_fields = [gc.DataTuple("nexthop_id", port)]
    key_annotation_fields = {"hdr.ipv4b.dst_addr": "ipv4"}
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
