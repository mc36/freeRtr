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
