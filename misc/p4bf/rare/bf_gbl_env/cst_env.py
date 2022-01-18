from .var_env import *

SAL_PORT_ID = {
                0:1, 1:2, 2:3, 3:4, 4:5, 5:6, 6:7, 7:8,
                12:9, 13:10, 14:11, 15:12, 
                28:13, 29:14, 30:15, 31:16
                }

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
