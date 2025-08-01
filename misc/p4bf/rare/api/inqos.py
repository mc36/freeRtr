from ..bf_gbl_env.var_env import *


def writeInQosRules(
    self, op_type, meter, bytes, interval
):
    tbl_global_path = "ig_ctl.ig_ctl_qos_in"
    tbl_name = "%s.policer" % (tbl_global_path)
    self._processMeterFromControlPlane(
        op_type,
        tbl_name,
        meter,
        bytes,
        interval
    )

def writeInRateRules(
    self, op_type, subif, bytes, interval
):
    tbl_global_path = "ig_ctl.ig_ctl_rate_in"
    tbl_name = "%s.rater" % (tbl_global_path)
    self._processMeterFromControlPlane(
        op_type,
        tbl_name,
        subif,
        bytes,
        interval
    )

    tbl_name = "%s.tbl_rate" % (tbl_global_path)
    tbl_action_name = "%s.act_rate" % (tbl_global_path)
    key_fields = [gc.KeyTuple("ig_md.source_id", subif)]
    data_fields = [
        gc.DataTuple("metid", subif),
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
