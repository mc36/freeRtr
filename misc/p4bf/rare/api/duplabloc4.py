from ..bf_gbl_env.var_env import *


def writeDupLabLocRules(
    self, op_type, ipver, vrf, sess, inlab, delete2, common
):
    if self.mcast == False:
        return
    if op_type == 1:
        act = "act_decap_mpls_ipv"+ipver
        par = [gc.DataTuple("label", common)]
    else:
        act = "act_drop"
        par = []
    if op_type != 3:
        self.mcast_nid.append(sess << 16)
        self.mcast_xid.append(0)
    tbl_global_path1 = "ig_ctl.ig_ctl_mpls"
    tbl_name1 = "%s.tbl_mpls_fib" % (tbl_global_path1)
    tbl_action_name1 = "%s.act_mpls_bcast_label" % (tbl_global_path1)
    key_field_list1 = [
        gc.KeyTuple("hdr.mpls0.label", inlab),
    ]
    data_field_list1 = [
         gc.DataTuple("sess", sess),
    ]
    key_annotation_fields1 = {}
    data_annotation_fields1 = {}
    if delete2 == "add":
        op_type2 = 1
    elif delete2 == "mod":
        op_type2 = 2
    else:
        op_type2 = 3
    self._processEntryFromControlPlane(
        op_type2,
        tbl_name1,
        key_field_list1,
        data_field_list1,
        tbl_action_name1,
        key_annotation_fields1,
        data_annotation_fields1,
    )

    tbl_global_path2 = "eg_ctl.eg_ctl_mcast"
    tbl_name2 = "%s.tbl_mcast" % (tbl_global_path2)
    tbl_action_name2 = "%s.%s" % (tbl_global_path2, act)
    key_field_list2 = [
        gc.KeyTuple("hdr.internal.clone_session", sess),
        gc.KeyTuple("eg_intr_md.egress_rid", 0),
    ]
    data_field_list2 = par
    key_annotation_fields2 = {}
    data_annotation_fields2 = {}
    self._processEntryFromControlPlane(
        op_type2,
        tbl_name2,
        key_field_list2,
        data_field_list2,
        tbl_action_name2,
        key_annotation_fields2,
        data_annotation_fields2,
    )

    self._processMcastNodeFromControlPlane(
        op_type,
        sess << 16,
        0,
        self.recirc_port,
    )

    self._processMcastMgidFromControlPlane(
        op_type2,
        sess,
        self.mcast_nid,
        self.mcast_xid,
    )
    self.mcast_nid = []
    self.mcast_xid = []

