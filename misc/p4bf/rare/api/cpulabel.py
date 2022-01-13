from ..bf_gbl_env.var_env import *


def writeCpuMplsRules(self, op_type, dst_label):
    if self.mpls == False:
        return
    tbl_global_path_1 = "ig_ctl.ig_ctl_mpls"
    tbl_name_1 = "%s.tbl_mpls_fib" % (tbl_global_path_1)
    tbl_action_name_1 = "%s.act_mpls_cpulabel" % (tbl_global_path_1)
    key_field_list_1 = [gc.KeyTuple("hdr.mpls0.label", (dst_label))]
    data_field_list_1 = []
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

    tbl_global_path_2 = "ig_ctl.ig_ctl_mpls"
    tbl_name_2 = "%s.tbl_mpls_fib_decap" % (tbl_global_path_2)
    tbl_action_name_2 = "%s.act_mpls_cpulabel" % (tbl_global_path_2)
    key_field_list_2 = [gc.KeyTuple("hdr.mpls1.label", (dst_label))]
    data_field_list_2 = []
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
