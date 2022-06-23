from ..bf_gbl_env.var_env import *


def writeLoconnRules(self, op_type, port, target):
    # for any reason, control plane is sending a msg
    # with port=-1
    if port < 0:
        return

    tbl_global_path_1 = "ig_ctl.ig_ctl_vrf"
    tbl_name_1 = "%s.tbl_vrf" % (tbl_global_path_1)
    tbl_action_name_1 = "%s.act_set_loconn" % (tbl_global_path_1)
    key_field_list_1 = [gc.KeyTuple("ig_md.source_id", port)]
    data_field_list_1 = [
        gc.DataTuple("port", target),
    ]
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

    tbl_global_path_2 = "ig_ctl.ig_ctl_vrf"
    tbl_name_2 = "%s.tbl_vrf" % (tbl_global_path_2)
    tbl_action_name_2 = "%s.act_set_loconn" % (tbl_global_path_2)
    key_field_list_2 = [gc.KeyTuple("ig_md.source_id", target)]
    data_field_list_2 = [
        gc.DataTuple("port", port),
    ]
    key_annotation_fields_2 = {}
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
