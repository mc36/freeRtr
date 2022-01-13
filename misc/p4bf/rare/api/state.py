from ..bf_gbl_env.var_env import *

import inspect
import logging, linecache

PROGRAM_NAME = os.path.basename(sys.argv[0])
log_level = logging.WARNING
logger = logging.getLogger(PROGRAM_NAME)

PORT_SPEED = [1,10,25,40, 50,100]

def _Exception():
    exc_type, exc_obj, tb = sys.exc_info()
    f = tb.tb_frame
    lineno = tb.tb_lineno
    filename = f.f_code.co_filename
    linecache.checkcache(filename)
    line = linecache.getline(filename, lineno, f.f_globals)
    return 'EXCEPTION IN ({}, LINE {} "{}"): {}'.format(filename, lineno, line.strip(), exc_obj)

def _setPortAdmStatusBF2556X1T(self, port_id, adm_status,
                               port_speed,fec=0,autoneg=1,flowctrl=0):
    method_name = inspect.currentframe().f_code.co_name
    new_speed = self.salgc.speedTnaToSal(port_speed)
    sal_port = SAL_PORT_ID[port_id]
    if adm_status == 1:
        if self._checkParamCoherence(port_speed,fec,autoneg,flowctrl) == False:
            logger.warning("%s - Error in enabling port [%s] with parameters:[%s,%s,%s,%s]"
                             % (self.class_name,port_id,port_speed,fec,autoneg,flowctrl ))
            return None
        try:
            self.salgc.AddPort(sal_port,lane=0,
                               speed=new_speed,fec=fec,an=autoneg,fc=flowctrl,
                               enable=adm_status,up=adm_status)

            # FL ACTIVE_PORTS[port_id]=port_speed
        except:
            logger.warning("%s:%s - Error in enabling port [%s] with parameters:[%s,%s,%s,%s]"
                         % (self.class_name,method_name,port_id,port_speed,fec,autoneg,flowctrl ))
            logger.warning("%s:%s - with code [%s]" % (self.class_name,method_name,_Exception()))
    else:
        try:
            self.salgc.DelPort(port_num=sal_port, lane=0)
            # FL ACTIVE_PORTS.pop(port_id)
        except:
            logger.warning("%s:%s - Error in deleting port [%s]" % (self.class_name,method_name,port_id))


def _setPortAdmStatus(self, port_id, adm_status=0,
                      port_speed=10,fec=0,autoneg=1,flowctrl=0):
    # set port_id to adm_status
    method_name = inspect.currentframe().f_code.co_name

    str_port_speed = ""
    str_fec = ""
    str_an = ""

    if adm_status == 1:
        if port_speed in PORT_SPEED:
            str_port_speed = "BF_SPEED_%sG" % (port_speed)
        else:
            logger.error("Unknown port speed: %s", port_speed)
            return

        if self._checkParamCoherence(port_speed,fec,autoneg,flowctrl) == False:
            logger.warning("%s - Error in enabling port [%s] with parameters:[%s,%s,%s,%s]"
                             % (self.class_name,port_id,port_speed,fec,autoneg,flowctrl ))
            return None

        str_fec = self._getStrFEC(fec)
        str_an = self._getStrAN(autoneg)

        try:
            self.bfgc.port_table.entry_add(
                self.bfgc.target,
                [self.bfgc.port_table.make_key([gc.KeyTuple("$DEV_PORT", port_id)])],
                [
                    self.bfgc.port_table.make_data(
                        [
                            gc.DataTuple("$SPEED", str_val=str_port_speed),
                            gc.DataTuple("$FEC", str_val=str_fec),
                            gc.DataTuple("$AUTO_NEGOTIATION", str_val=str_an),
                            gc.DataTuple("$PORT_ENABLE", bool_val=True),
                        ]
                    )
                ],
            )
            # FL ACTIVE_PORTS[port_id]=port_speed
            logger.debug("Port[%s] administratively enabled", port_id)
        except:
            logger.warning("%s:%s - Error in enabling port [%s] with parameters:[%s,%s,%s,%s]"
                        % (self.class_name,method_name,port_id,port_speed,fec,autoneg,flowctrl ))
            logger.warning("%s:%s - with code [%s]" % (self.class_name,method_name,_Exception()))
    else:
        try:
            self.bfgc.port_table.entry_del(
                self.bfgc.target,
                [self.bfgc.port_table.make_key([gc.KeyTuple("$DEV_PORT", port_id)])]
            )

            # FL ACTIVE_PORTS.pop(port_id)

            logger.debug("Port[%s] administratively disabled", port_id)
        except:
            logger.warning("%s:%s - Error in deleting port [%s]" % (self.class_name,method_name,port_id))

