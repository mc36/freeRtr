from ..bf_gbl_env.var_env import *


def writeSgtTagRules(self, op_type, port):
    if self.sgt == False:
        return
    tbl_global_path_1 = "ig_ctl.ig_ctl_sgt"
    tbl_name_1 = "%s.tbl_sgt_in" % (tbl_global_path_1)
    tbl_action_name_1 = "%s.act_need_tag" % (tbl_global_path_1)
    key_field_list_1 = [
        gc.KeyTuple("ig_md.source_id", (port)),
    ]
    data_field_list_1 = [
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
    tbl_global_path_2 = "eg_ctl.eg_ctl_sgt"
    tbl_name_2 = "%s.tbl_sgt_out" % (tbl_global_path_2)
    tbl_action_name_2 = "%s.act_need_tag" % (tbl_global_path_2)
    key_field_list_2 = [
        gc.KeyTuple("eg_md.aclport_id", (port)),
    ]
    data_field_list_2 = [
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
