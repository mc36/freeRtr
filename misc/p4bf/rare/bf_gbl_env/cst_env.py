from .var_env import *

try:
    from sal_services_pb2 import (
        UNDEFINED_SPEED,
        SPEED_NONE,
        SPEED_1G,
        SPEED_2P5G,
        SPEED_5G,
        SPEED_10G,
        SPEED_25G,
        SPEED_40G,
        SPEED_50G,
        SPEED_100G,
    )
    from sal_services_pb2 import UNDEFINED_FEC, FEC_NONE, FEC_FC, FEC_RS
    from sal_services_pb2 import UNDEFINED_FC, FC_NONE, FC_PAUSE, FC_PFC
    from sal_services_pb2 import UNDEFINED_AN, AN_OFF, AN_ON
    from salgrpcclient import SalGrpcClient, SAL_PORT_ID
except ImportError:
    logger.warning("sal import failed")

PORT_SPEED = [1,10,25,40, 50,100]
UNDEFINED_FEC = 0 ;
FEC_NONE = 1;
FEC_FC = 2;
FEC_RS = 3;

UNDEFINED_AN = 0
AN_OFF = 1
AN_ON = 2


MODEL = 0
WEDGEBF10032X = 1
SESSION_TYPE = ["MODEL", "WEDGEBF10032X"]

# which session
if os.path.exists(BSP_FILE_PATH):
    SESSION_ID = WEDGEBF10032X
    CPU_PORT = 192
else:
    SESSION_ID = MODEL
    CPU_PORT = 64
op_type = None

WRITE = 1
UPDATE = 2
DELETE = 3
