from ..bf_gbl_env.var_env import *


def writeNshNeiRules(self, op_type, sp, si, nei, tsp, tsi):
    if self.nsh == False:
        return
    tbl_global_path = "ig_ctl.ig_ctl_nsh"
    tbl_name = "%s.tbl_nsh" % (tbl_global_path)
    tbl_action_name = "%s.act_fwd_nei" % (tbl_global_path)
    key_field_list = [
        gc.KeyTuple("hdr.nsh.sp", (sp)),
        gc.KeyTuple("hdr.nsh.si", (si)),
    ]
    data_field_list = [
        gc.DataTuple("nei", nei),
        gc.DataTuple("sp", tsp),
        gc.DataTuple("si", tsi),
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
