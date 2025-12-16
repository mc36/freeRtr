from ..bf_gbl_env.var_env import *

def writeVrfRules(self, op_type, port, vrf):
    # for any reason, control plane is sending a msg
    # with port=-1
    if port < 0:
        return

    tbl_global_path = "ig_ctl.ig_ctl_vrf"
    tbl_name = "%s.tbl_vrf" % (tbl_global_path)
    tbl_action_name = "%s.act_set_vrf" % (tbl_global_path)

    key_fields = [gc.KeyTuple("ig_md.source_id", port)]
    data_fields = [gc.DataTuple("vrf", vrf)]
    key_annotation_fields = {}
    data_annotation_fields = {}

    self._processEntryFromControlPlane(
        op_type,
        tbl_name,
        key_fields,
        data_fields,
        tbl_action_name,
        key_annotation_fields,
        data_annotation_fields,
    )
