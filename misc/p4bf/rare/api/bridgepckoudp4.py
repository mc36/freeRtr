from ..bf_gbl_env.var_env import *


def writePckoudp4rules(
    self, op_type, bridge, addr, sip, dip, sprt, dprt, nexthop, vrf, port
):
    if self.tun == False:
        return
    if self.bridge == False:
        return

    tbl_global_path_1 = "ig_ctl.ig_ctl_bridge"
    tbl_name_1 = "%s.tbl_bridge_learn" % (tbl_global_path_1)
    tbl_action_name_1 = "%s.act_set_bridge_port" % (tbl_global_path_1)
    key_field_list_1 = [
        gc.KeyTuple("hdr.ethernet.src_mac_addr", addr),
        gc.KeyTuple("ig_md.bridge_id", bridge),
    ]
    data_field_list_1 = []
    key_annotation_fields_1 = {"hdr.ethernet.src_mac_addr": "mac"}
    data_annotation_fields_1 = {}
    self._processEntryFromControlPlane(
        op_type,
        tbl_name_1,
        key_field_list_1,
        data_field_list_1,
        tbl_action_name_1,
        key_annotation_fields_1,
        data_annotation_fields_1,
    )

    tbl_global_path_2 = "ig_ctl.ig_ctl_bridge"
    tbl_name_2 = "%s.tbl_bridge_target" % (tbl_global_path_2)
    sap_type = 4
    tbl_action_name_2 = "%s.act_set_bridge_pckoudp4" % (tbl_global_path_2)
    key_field_list_2 = [
        # gc.KeyTuple('ig_md.sap_type', sap_type),
        gc.KeyTuple("hdr.ethernet.dst_mac_addr", addr),
        gc.KeyTuple("ig_md.bridge_id", bridge),
    ]
    data_field_list_2 = [
        gc.DataTuple("nexthop", nexthop),
        gc.DataTuple("dst_ip_addr", dip),
        gc.DataTuple("src_ip_addr", sip),
        gc.DataTuple("dst_port", dprt),
        gc.DataTuple("src_port", sprt),
    ]
    key_annotation_fields_2 = {"hdr.ethernet.dst_mac_addr": "mac"}
    data_annotation_fields_2 = {
        "dst_ip_addr": "ipv4",
        "src_ip_addr": "ipv4",
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

    tbl_global_path_3 = "ig_ctl.ig_ctl_tunnel"
    tbl_name_3 = "%s.tbl_tunnel4" % (tbl_global_path_3)
    tbl_action_name_3 = "%s.act_tunnel_pckoudp" % (tbl_global_path_3)
    key_field_list_3 = [
        gc.KeyTuple("ig_md.layer4_srcprt", dprt),
        gc.KeyTuple("ig_md.layer4_dstprt", sprt),
        gc.KeyTuple("hdr.ipv4.src_addr", dip),
        gc.KeyTuple("hdr.ipv4.dst_addr", sip),
        gc.KeyTuple("ig_md.vrf", vrf),
        gc.KeyTuple("hdr.ipv4.protocol", 17),
    ]
    data_field_list_3 = [
        gc.DataTuple("port", port),
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
