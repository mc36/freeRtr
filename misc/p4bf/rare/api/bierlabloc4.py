from ..bf_gbl_env.var_env import *

def writeBierLabLocRules(
    self, op_type, ipver, vrf, sess, inlab, bs0, bs1, bs2, bs3, bs4, bs5, bs6, bs7
):
    if self.mcast == False:
        return
    if op_type != 3:
        self.mcast_nid.append(sess << 16)
        self.mcast_xid.append(0)
    tbl_global_path1 = "ig_ctl.ig_ctl_mpls"
    tbl_name1 = "%s.tbl_mpls_fib" % (tbl_global_path1)
    tbl_action_name1 = "%s.act_mpls_bier_label" % (tbl_global_path1)
    key_field_list1 = [
        gc.KeyTuple("hdr.mpls0.label", inlab),
    ]
    data_field_list1 = [
         gc.DataTuple("sess", sess),
    ]
    key_annotation_fields1 = {}
    data_annotation_fields1 = {}
    self._processEntryFromControlPlane(
        op_type,
        tbl_name1,
        key_field_list1,
        data_field_list1,
        tbl_action_name1,
        key_annotation_fields1,
        data_annotation_fields1,
    )

    tbl_global_path2 = "eg_ctl.eg_ctl_mcast"
    tbl_name2 = "%s.tbl_mcast" % (tbl_global_path2)
    tbl_action_name2 = "%s.act_decap_bier_ipv%s" % (tbl_global_path2, ipver)
    key_field_list2 = [
        gc.KeyTuple("hdr.internal.clone_session", sess),
        gc.KeyTuple("eg_intr_md.egress_rid", 0),
    ]
    data_field_list2 = [
         gc.DataTuple("bs0", bs0 & 0xffffffff),
         gc.DataTuple("bs1", bs1 & 0xffffffff),
         gc.DataTuple("bs2", bs2 & 0xffffffff),
         gc.DataTuple("bs3", bs3 & 0xffffffff),
         gc.DataTuple("bs4", bs4 & 0xffffffff),
         gc.DataTuple("bs5", bs5 & 0xffffffff),
         gc.DataTuple("bs6", bs6 & 0xffffffff),
         gc.DataTuple("bs7", bs7 & 0xffffffff),
    ]
    key_annotation_fields2 = {}
    data_annotation_fields2 = {}
    self._processEntryFromControlPlane(
        op_type,
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
        op_type,
        sess,
        self.mcast_nid,
        self.mcast_xid,
    )
    self.mcast_nid = []
    self.mcast_xid = []
