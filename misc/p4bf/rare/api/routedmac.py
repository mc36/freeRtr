from ..bf_gbl_env.var_env import *


def writeRoumacRules(self, op_type, bridge, addr, nexthop, ppp):
    # for any reason, control plane is sending a msg
    # with port=-1

    if self.bridge == False:
        return
    if ppp != 0:
        return
    tbl_global_path_1 = "ig_ctl.ig_ctl_bridge"
    tbl_name_1 = "%s.tbl_bridge_learn" % (tbl_global_path_1)
    tbl_action_name_1 = "%s.act_set_bridge_port" % (tbl_global_path_1)
    key_field_list_1 = [
        gc.KeyTuple("ig_md.bridge_id", bridge),
        gc.KeyTuple("hdr.ethernet.src_mac_addr", addr),
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
    tbl_action_name_2 = "%s.act_set_bridge_routed" % (tbl_global_path_2)
    sap_type = 4
    key_field_list_2 = [
        gc.KeyTuple("ig_md.bridge_id", bridge),
        # gc.KeyTuple('ig_md.sap_type', sap_type),
        gc.KeyTuple("hdr.ethernet.dst_mac_addr", addr),
    ]
    data_field_list_2 = [gc.DataTuple("nexthop", nexthop)]
    key_annotation_fields_2 = {"hdr.ethernet.dst_mac_addr": "mac"}
    data_annotation_fields_2 = {}
    self._processEntryFromControlPlane(
        op_type,
        tbl_name_2,
        key_field_list_2,
        data_field_list_2,
        tbl_action_name_2,
        key_annotation_fields_2,
        data_annotation_fields_2,
    )
