from ..bf_gbl_env.var_env import *


def writeIpip4rules(
    self, op_type, nexthop, port, phport, sip, dip, dmac, vrf, smac
):
    if self.tun == False:
        return
    tbl_global_path_1 = "ig_ctl.ig_ctl_tunnel"
    tbl_name_1 = "%s.tbl_tunnel4" % (tbl_global_path_1)
    tbl_action_name_1 = "%s.act_tunnel_ipip" % (tbl_global_path_1)
    key_field_list_1 = [
        gc.KeyTuple("ig_md.layer4_srcprt", 0),
        gc.KeyTuple("ig_md.layer4_dstprt", 0),
        gc.KeyTuple("hdr.ipv4.src_addr", dip),
        gc.KeyTuple("hdr.ipv4.dst_addr", sip),
        gc.KeyTuple("ig_md.vrf", vrf),
        gc.KeyTuple("hdr.ipv4.protocol", 4),
    ]
    data_field_list_1 = [
        gc.DataTuple("port", port),
        gc.DataTuple("ethtyp", 0x0800),
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
    tbl_global_path_3 = "ig_ctl.ig_ctl_tunnel"
    tbl_name_3 = "%s.tbl_tunnel4" % (tbl_global_path_3)
    tbl_action_name_3 = "%s.act_tunnel_ipip" % (tbl_global_path_3)
    key_field_list_3 = [
        gc.KeyTuple("ig_md.vrf", vrf),
        gc.KeyTuple("hdr.ipv4.protocol", 41),
        gc.KeyTuple("hdr.ipv4.src_addr", dip),
        gc.KeyTuple("hdr.ipv4.dst_addr", sip),
        gc.KeyTuple("ig_md.layer4_srcprt", 0),
        gc.KeyTuple("ig_md.layer4_dstprt", 0),
    ]
    data_field_list_3 = [
        gc.DataTuple("port", port),
        gc.DataTuple("ethtyp", 0x86dd),
    ]
    key_annotation_fields_3 = {
        "hdr.ipv4.src_addr": "ipv4",
        "hdr.ipv4.dst_addr": "ipv4",
    }
    data_annotation_fields_3 = {
    }
    self._processEntryFromControlPlane(
        op_type,
        tbl_name_3,
        key_field_list_3,
        data_field_list_3,
        tbl_action_name_3,
        key_annotation_fields_3,
        data_annotation_fields_3,
    )
    tbl_global_path_4 = "ig_ctl.ig_ctl_tunnel"
    tbl_name_4 = "%s.tbl_tunnel4" % (tbl_global_path_4)
    tbl_action_name_4 = "%s.act_tunnel_ipip" % (tbl_global_path_4)
    key_field_list_4 = [
        gc.KeyTuple("ig_md.vrf", vrf),
        gc.KeyTuple("hdr.ipv4.protocol", 145),
        gc.KeyTuple("hdr.ipv4.src_addr", dip),
        gc.KeyTuple("hdr.ipv4.dst_addr", sip),
        gc.KeyTuple("ig_md.layer4_srcprt", 0),
        gc.KeyTuple("ig_md.layer4_dstprt", 0),
    ]
    data_field_list_4 = [
        gc.DataTuple("port", port),
        gc.DataTuple("ethtyp", 0x894f),
    ]
    key_annotation_fields_4 = {
        "hdr.ipv4.src_addr": "ipv4",
        "hdr.ipv4.dst_addr": "ipv4",
    }
    data_annotation_fields_4 = {
    }
    self._processEntryFromControlPlane(
        op_type,
        tbl_name_4,
        key_field_list_4,
        data_field_list_4,
        tbl_action_name_4,
        key_annotation_fields_4,
        data_annotation_fields_4,
    )
    tbl_global_path_5 = "ig_ctl.ig_ctl_tunnel"
    tbl_name_5 = "%s.tbl_tunnel4" % (tbl_global_path_5)
    tbl_action_name_5 = "%s.act_tunnel_ipip" % (tbl_global_path_5)
    key_field_list_5 = [
        gc.KeyTuple("ig_md.vrf", vrf),
        gc.KeyTuple("hdr.ipv4.protocol", 57),
        gc.KeyTuple("hdr.ipv4.src_addr", dip),
        gc.KeyTuple("hdr.ipv4.dst_addr", sip),
        gc.KeyTuple("ig_md.layer4_srcprt", 0),
        gc.KeyTuple("ig_md.layer4_dstprt", 0),
    ]
    data_field_list_5 = [
        gc.DataTuple("port", port),
        gc.DataTuple("ethtyp", 0x8909),
    ]
    key_annotation_fields_5 = {
        "hdr.ipv4.src_addr": "ipv4",
        "hdr.ipv4.dst_addr": "ipv4",
    }
    data_annotation_fields_5 = {
    }
    self._processEntryFromControlPlane(
        op_type,
        tbl_name_5,
        key_field_list_5,
        data_field_list_5,
        tbl_action_name_5,
        key_annotation_fields_5,
        data_annotation_fields_5,
    )
    tbl_global_path_6 = "ig_ctl.ig_ctl_tunnel"
    tbl_name_6 = "%s.tbl_tunnel4" % (tbl_global_path_6)
    tbl_action_name_6 = "%s.act_tunnel_ipip" % (tbl_global_path_6)
    key_field_list_6 = [
        gc.KeyTuple("ig_md.vrf", vrf),
        gc.KeyTuple("hdr.ipv4.protocol", 137),
        gc.KeyTuple("hdr.ipv4.src_addr", dip),
        gc.KeyTuple("hdr.ipv4.dst_addr", sip),
        gc.KeyTuple("ig_md.layer4_srcprt", 0),
        gc.KeyTuple("ig_md.layer4_dstprt", 0),
    ]
    data_field_list_6 = [
        gc.DataTuple("port", port),
        gc.DataTuple("ethtyp", 0x8847),
    ]
    key_annotation_fields_6 = {
        "hdr.ipv4.src_addr": "ipv4",
        "hdr.ipv4.dst_addr": "ipv4",
    }
    data_annotation_fields_6 = {
    }
    self._processEntryFromControlPlane(
        op_type,
        tbl_name_6,
        key_field_list_6,
        data_field_list_6,
        tbl_action_name_6,
        key_annotation_fields_6,
        data_annotation_fields_6,
    )
    tbl_global_path_7 = "ig_ctl.ig_ctl_tunnel"
    tbl_name_7 = "%s.tbl_tunnel4" % (tbl_global_path_7)
    tbl_action_name_7 = "%s.act_tunnel_ipip" % (tbl_global_path_7)
    key_field_list_7 = [
        gc.KeyTuple("ig_md.vrf", vrf),
        gc.KeyTuple("hdr.ipv4.protocol", 143),
        gc.KeyTuple("hdr.ipv4.src_addr", dip),
        gc.KeyTuple("hdr.ipv4.dst_addr", sip),
        gc.KeyTuple("ig_md.layer4_srcprt", 0),
        gc.KeyTuple("ig_md.layer4_dstprt", 0),
    ]
    data_field_list_7 = [
        gc.DataTuple("port", port),
        gc.DataTuple("ethtyp", 0x6558),
    ]
    key_annotation_fields_7 = {
        "hdr.ipv4.src_addr": "ipv4",
        "hdr.ipv4.dst_addr": "ipv4",
    }
    data_annotation_fields_7 = {
    }
    self._processEntryFromControlPlane(
        op_type,
        tbl_name_7,
        key_field_list_7,
        data_field_list_7,
        tbl_action_name_7,
        key_annotation_fields_7,
        data_annotation_fields_7,
    )
    tbl_global_path_2 = "eg_ctl.eg_ctl_nexthop"
    tbl_name_2 = "%s.tbl_nexthop" % (tbl_global_path_2)
    tbl_action_name_2 = "%s.act_ipv4_ipip4" % (tbl_global_path_2)
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

