from ..bf_gbl_env.var_env import *


def writePppoeRules(
    self, op_type, port, phport, nexthop, vrf, ses, dmac, smac
):
    if self.poe == False:
        return
    tbl_global_path_1 = "ig_ctl.ig_ctl_pppoe"
    tbl_name_1 = "%s.tbl_pppoe" % (tbl_global_path_1)
    tbl_action_name_1 = "%s.act_pppoe_data" % (tbl_global_path_1)
    key_field_list_1 = [
        gc.KeyTuple("ig_md.source_id", phport),
        gc.KeyTuple("hdr.pppoeD.session", ses),
    ]
    data_field_list_1 = [
        gc.DataTuple("port", port),
    ]
    key_annotation_fields_1 = {
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
    tbl_action_name_2 = "%s.act_ipv4_pppoe" % (tbl_global_path_2)
    key_field_list_2 = [
        gc.KeyTuple("eg_md.nexthop_id", nexthop),
    ]
    data_field_list_2 = [
        gc.DataTuple("dst_mac_addr", dmac),
        gc.DataTuple("src_mac_addr", smac),
        gc.DataTuple("egress_port", phport),
        gc.DataTuple("acl_port", port),
        gc.DataTuple("session", ses),
    ]
    key_annotation_fields_2 = {
    }
    data_annotation_fields_2 = {
        "src_mac_addr": "mac",
        "dst_mac_addr": "mac",
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
