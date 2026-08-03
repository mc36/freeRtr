from ..bf_gbl_env.var_env import *


def writePolkaOwnRules(self, op_type, idx, vrf):
    if self.polka == False:
        return
    tbl_global_path = "ig_ctl.ig_ctl_polka"
    tbl_name = "%s.tbl_polka" % (tbl_global_path)
    tbl_action_name = "%s.act_route" % (tbl_global_path)
    key_fields = [
        gc.KeyTuple("ig_md.vrf", vrf),
        gc.KeyTuple("ig_md.polka_next", idx),
    ]
    data_fields = [
    ]
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
