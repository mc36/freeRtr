from ..bf_gbl_env.var_env import *

def writeBierLabelRules(
    self, op_type, vrf, sess, inlab, port, subif, hopid, outlab, bs0, bs1, bs2, bs3, bs4, bs5, bs6, bs7
):
    if self.mcast == False:
        return
    nodid = (sess << 16 ) | hopid;
    if op_type != 3:
        self.mcast_nid.append(nodid)
        self.mcast_xid.append(0)
    tbl_global_path = "eg_ctl.eg_ctl_mcast"
    tbl_name = "%s.tbl_mcast" % (tbl_global_path)
    tbl_action_name = "%s.act_bier" % (tbl_global_path)
    key_field_list = [
        gc.KeyTuple("hdr.internal.clone_session", sess),
        gc.KeyTuple("eg_intr_md.egress_rid", hopid),
    ]
    data_field_list = [
         gc.DataTuple("hop", hopid),
         gc.DataTuple("label", outlab),
         gc.DataTuple("bs0", bs0 & 0xffffffff),
         gc.DataTuple("bs1", bs1 & 0xffffffff),
         gc.DataTuple("bs2", bs2 & 0xffffffff),
         gc.DataTuple("bs3", bs3 & 0xffffffff),
         gc.DataTuple("bs4", bs4 & 0xffffffff),
         gc.DataTuple("bs5", bs5 & 0xffffffff),
         gc.DataTuple("bs6", bs6 & 0xffffffff),
         gc.DataTuple("bs7", bs7 & 0xffffffff),
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
    self._processMcastNodeFromControlPlane(
        op_type,
        nodid,
        hopid,
        self.hairpin2recirc(port),
    )
