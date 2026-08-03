from ..bf_gbl_env.var_env import *


def writeXconnRules(self, op_type, port, target, lab_tun, lab_loc, lab_rem):
    # for any reason, control plane is sending a msg
    # with port=-1
    if port < 0:
        return

    if self.mpls == False:
        return
    tbl_global_path_1 = "ig_ctl.ig_ctl_mpls"
    tbl_name_1 = "%s.tbl_mpls_fib" % (tbl_global_path_1)
    tbl_action_name_1 = "%s.act_mpls_decap_l2vpn" % (tbl_global_path_1)
    key_field_list_1 = [gc.KeyTuple("hdr.mpls0.label", lab_loc)]
    data_field_list_1 = [gc.DataTuple("port", port)]
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
    tbl_action_name_2 = "%s.act_mpls_decap_l2vpn" % (tbl_global_path_2)
    key_field_list_2 = [gc.KeyTuple("hdr.mpls1.label", lab_loc)]
    data_field_list_2 = [gc.DataTuple("port", port)]
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

    tbl_global_path_3 = "ig_ctl.ig_ctl_vrf"
    tbl_name_3 = "%s.tbl_vrf" % (tbl_global_path_3)
    tbl_action_name_3 = "%s.act_set_mpls_xconn_encap" % (tbl_global_path_3)
    key_field_list_3 = [gc.KeyTuple("ig_md.source_id", port)]
    data_field_list_3 = [
        gc.DataTuple("target", target),
        gc.DataTuple("tunlab", lab_tun),
        gc.DataTuple("svclab", lab_rem),
    ]
    key_annotation_fields_3 = {}
    data_annotation_fields_3 = {}
    self._processEntryFromControlPlane(
        op_type,
        tbl_name_3,
        key_field_list_3,
        data_field_list_3,
        tbl_action_name_3,
        key_annotation_fields_3,
        data_annotation_fields_3,
    )
