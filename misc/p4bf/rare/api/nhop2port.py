from ..bf_gbl_env.var_env import *


def writeNhop2portRules(self, op_type, nhop, subif, port):
    tbl_global_path = "ig_ctl.ig_ctl_outport"
    tbl_name = "%s.tbl_nexthop" % (tbl_global_path)
    tbl_action_name = "%s.act_set_port_nexthop" % (tbl_global_path)

    key_fields = [gc.KeyTuple("ig_md.nexthop_id", nhop)]
    data_fields = [
        gc.DataTuple("port", port),
        gc.DataTuple("subif", subif),
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
