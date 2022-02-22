from ..bf_gbl_env.var_env import *


def writeSgtSetRules(self, op_type, port, group):
    if self.sgt == False:
        return
    tbl_global_path = "ig_ctl.ig_ctl_sgt"
    tbl_name = "%s.tbl_sgt_set" % (tbl_global_path)
    tbl_action_name = "%s.act_set_tag" % (tbl_global_path)
    key_field_list = [
        gc.KeyTuple("ig_md.source_id", (port)),
    ]
    data_field_list = [
        gc.DataTuple("group", group),
    ]
    key_annotation_fields = {}
    data_annotation_fields = {}
    self._processEntryFromControlPlane(
        op_type,
        tbl_name,
        key_field_list,
        data_field_list,
        tbl_action_name,
        key_annotation_fields,
        data_annotation_fields,
    )
