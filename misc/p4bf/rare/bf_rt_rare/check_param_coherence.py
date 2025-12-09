from ..bf_gbl_env.cst_env import *

def _checkParamCoherence(self,port_speed,fec,autoneg,flowctrl):
    if (port_speed == 1) or (port_speed == 10):
        if (fec == FEC_RS):
           return False
    elif (port_speed == 100 or port_speed == 200 or port_speed == 400):
        if (fec == FEC_FC):
           return False
        if (port_speed == 200 or port_speed == 400) and fec != FEC_RS:
            return False
    else:
        return True
