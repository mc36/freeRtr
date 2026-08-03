from ..bf_gbl_env.cst_env import *

def _getStrFEC(self,fec):
    if fec == FEC_NONE:
        return "BF_FEC_TYP_NONE"
    elif fec == FEC_FC:
        return "BF_FEC_TYP_FC"
    elif fec == FEC_RS:
        return "BF_FEC_TYP_RS"
    else:
        return "BF_FEC_TYP_NONE"
