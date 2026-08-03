from ..bf_gbl_env.var_env import *


def writeNshconnRules(self, op_type, port, sp, si):
    # for any reason, control plane is sending a msg
    # with port=-1
    if port < 0:
        return

    tbl_global_path_1 = "ig_ctl.ig_ctl_vrf"
    tbl_name_1 = "%s.tbl_vrf" % (tbl_global_path_1)
    tbl_action_name_1 = "%s.act_set_nshconn" % (tbl_global_path_1)
    key_field_list_1 = [gc.KeyTuple("ig_md.source_id", port)]
    data_field_list_1 = [
        gc.DataTuple("sp", sp),
        gc.DataTuple("si", si),
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
