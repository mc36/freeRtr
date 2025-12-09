from ..bf_gbl_env.var_env import *


def writeGre4rules(
    self, op_type, nexthop, port, phport, sip, dip, dmac, vrf, smac
):
    if self.tun == False:
        return
    tbl_global_path_1 = "ig_ctl.ig_ctl_tunnel"
    tbl_name_1 = "%s.tbl_tunnel4" % (tbl_global_path_1)
    tbl_action_name_1 = "%s.act_tunnel_gre" % (tbl_global_path_1)
    key_field_list_1 = [
        gc.KeyTuple("ig_md.layer4_srcprt", 0),
        gc.KeyTuple("ig_md.layer4_dstprt", 0),
        gc.KeyTuple("hdr.ipv4.src_addr", dip),
        gc.KeyTuple("hdr.ipv4.dst_addr", sip),
        gc.KeyTuple("ig_md.vrf", vrf),
        gc.KeyTuple("hdr.ipv4.protocol", 47),
    ]
    data_field_list_1 = [
        gc.DataTuple("port", port),
    ]
    key_annotation_fields_1 = {
        "hdr.ipv4.src_addr": "ipv4",
        "hdr.ipv4.dst_addr": "ipv4",
    }
    data_annotation_fields_1 = {
    }
    self._processEntryFromControlPlane(
        op_type,
        tbl_name_1,
        key_field_list_1,
        data_field_list_1,
        tbl_action_name_1,
        key_annotation_fields_1,
        data_annotation_fields_1,
    )
    tbl_global_path_2 = "eg_ctl.eg_ctl_nexthop"
    tbl_name_2 = "%s.tbl_nexthop" % (tbl_global_path_2)
    tbl_action_name_2 = "%s.act_ipv4_gre4" % (tbl_global_path_2)
    key_field_list_2 = [
        gc.KeyTuple("eg_md.nexthop_id", nexthop),
    ]
    data_field_list_2 = [
        gc.DataTuple("dst_mac_addr", dmac),
        gc.DataTuple("src_mac_addr", smac),
        gc.DataTuple("egress_port", phport),
        gc.DataTuple("acl_port", port),
        gc.DataTuple("src_ip_addr", sip),
        gc.DataTuple("dst_ip_addr", dip),
    ]
    key_annotation_fields_2 = {
    }
    data_annotation_fields_2 = {
        "src_mac_addr": "mac",
        "dst_mac_addr": "mac",
        "src_ip_addr": "ipv4",
        "dst_ip_addr": "ipv4",
    }
    self._processEntryFromControlPlane(
        op_type,
        tbl_name_2,
        key_field_list_2,
        data_field_list_2,
        tbl_action_name_2,
        key_annotation_fields_2,
        data_annotation_fields_2,
    )
