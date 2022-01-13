from ..bf_gbl_env.var_env import *


def writeBunVlanRules(self, op_type, main, vlan, port):
    # for any reason, control plane is sending a msg
    # with port=-1
    if port < 0:
        return

    tbl_global_path_1 = "ig_ctl.ig_ctl_vlan_in"
    tbl_name_1 = "%s.tbl_vlan_in" % (tbl_global_path_1)
    tbl_action_name_1 = "%s.act_set_iface" % (tbl_global_path_1)
    key_field_list_1 = [
        gc.KeyTuple("ig_md.ingress_id", main),
        gc.KeyTuple("hdr.vlan.vid", vlan),
    ]
    data_field_list_1 = [gc.DataTuple("src", port)]

    key_annotation_fields_1 = {}
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
