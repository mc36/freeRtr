from ..bf_gbl_env.cst_env import *

def _getStrAN(self,autoneg):
    if autoneg == 0:
        return "PM_AN_DEFAULT"
    elif autoneg == AN_OFF:
        return "PM_AN_FORCE_DISABLE"
    elif autoneg == AN_ON:
        return "PM_AN_FORCE_ENABLE"
    else:
        return "PM_AN_DEFAULT"
