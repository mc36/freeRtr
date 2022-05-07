from ..bf_gbl_env.var_env import *


def writeMlocal6rules(
    self, op_type, vrf, sess, dip, sip, ingr, delete2
):
    if self.mcast == False:
        return
    if op_type == 1:
        act = "act_local"
    else:
        act = "act_flood"
    tbl_global_path = "ig_ctl.ig_ctl_mcast"
    tbl_name = "%s.tbl_mcast6" % (tbl_global_path)
    tbl_action_name = "%s.%s" % (tbl_global_path, act)
    key_field_list = [
        gc.KeyTuple("hdr.ipv6.src_addr", sip),
        gc.KeyTuple("hdr.ipv6.dst_addr", dip),
        gc.KeyTuple("ig_md.vrf", vrf),
    ]
    data_field_list = [
         gc.DataTuple("ingr", ingr),
         gc.DataTuple("sess", sess),
    ]
    key_annotation_fields = {
        "hdr.ipv6.src_addr": "ipv6",
        "hdr.ipv6.dst_addr": "ipv6",
    }
    data_annotation_fields = {}
    if delete2 == "add":
        op_type2 = 1
    elif delete2 == "mod":
        op_type2 = 2
    else:
        op_type2 = 3
    self._processEntryFromControlPlane(
        op_type2,
        tbl_name,
        key_field_list,
        data_field_list,
        tbl_action_name,
        key_annotation_fields,
        data_annotation_fields,
    )
    self._processMcastMgidFromControlPlane(
        op_type2,
        sess,
        self.mcast_nid,
        self.mcast_xid,
    )
    self.mcast_nid = []
    self.mcast_xid = []
