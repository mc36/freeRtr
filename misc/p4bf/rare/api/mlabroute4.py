from ..bf_gbl_env.var_env import *


def writeMlabRouteRules(
    self, op_type, ipver, vrf, sess, dip, sip, ingr, port, hopid, outlab, subif
):
    if self.mcast == False:
        return
    nodid = (sess << 16 ) | hopid;
    if op_type != 3:
        self.mcast_nid.append(nodid)
        self.mcast_xid.append(0)

    tbl_global_path = "eg_ctl.eg_ctl_mcast"
    tbl_name = "%s.tbl_mcast" % (tbl_global_path)
    tbl_action_name = "%s.act_encap_ipv%s_mpls" % (tbl_global_path, ipver)
    key_field_list = [
        gc.KeyTuple("hdr.internal.clone_session", sess),
        gc.KeyTuple("eg_intr_md.egress_rid", hopid),
    ]
    data_field_list = [
         gc.DataTuple("hop", hopid),
         gc.DataTuple("label", outlab),
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

