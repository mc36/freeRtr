from ..bf_gbl_env.var_env import *


def writeMplsRules(self, op_type, dst_label, new_label, port):
    # for any reason, control plane is sending a msg
    # with port=-1
    if port < 0:
        return

    if self.mpls == False:
        return
    tbl_global_path_1 = "ig_ctl.ig_ctl_mpls"
    tbl_name_1 = "%s.tbl_mpls_fib" % (tbl_global_path_1)
    tbl_action_name_1 = "%s.act_mpls_swap0_set_nexthop" % (tbl_global_path_1)
    key_field_list_1 = [gc.KeyTuple("hdr.mpls0.label", (dst_label))]
    data_field_list_1 = [
        gc.DataTuple("egress_label", new_label),
        gc.DataTuple("nexthop_id", port),
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

    tbl_global_path_2 = "ig_ctl.ig_ctl_mpls"
    tbl_name_2 = "%s.tbl_mpls_fib_decap" % (tbl_global_path_2)
    tbl_action_name_2 = "%s.act_mpls_swap1_set_nexthop" % (tbl_global_path_2)
    key_field_list_2 = [gc.KeyTuple("hdr.mpls1.label", (dst_label))]
    data_field_list_2 = [
        gc.DataTuple("egress_label", new_label),
        gc.DataTuple("nexthop_id", port),
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
