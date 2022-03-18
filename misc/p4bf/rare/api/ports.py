from ..bf_gbl_env.cst_env import *

PORT_SPEED = [1, 10, 25, 40, 50, 100]

def managePort(self, platform, mode, port_id, port_speed=10,
               fec=0, autoneg=1, flowctrl=0):
    method_name = inspect.currentframe().f_code.co_name
    
    if mode == 1:
        if self._checkParamCoherence(port_speed, fec, autoneg, flowctrl) == False:
            self.controlPlaneMsg(
                "%s - Error in adding port [%s], inconsistent parameters:[%s,%s,%s,%s]"
                % (self.class_name, port_id, port_speed, fec, autoneg, flowctrl)
            )
            return

        try:
            if platform == "stordis_bf2556x_1t" and port_id in SAL_PORT_ID:
                new_speed = self.salgc.speedTnaToSal(port_speed)
                sal_port = SAL_PORT_ID[port_id]
                self.salgc.AddPort(
                    sal_port,
                    lane=0,
                    speed=new_speed,
                    fec=fec,
                    an=autoneg,
                    fc=flowctrl,
                    enable=False,
                    up=False,
                )
            else:
                if port_speed in PORT_SPEED:
                    str_port_speed = "BF_SPEED_%sG" % (port_speed)
                else:
                    self.controlPlaneMsg("Unsupported port speed: %s" % port_speed)
                    return
                str_fec = self._getStrFEC(fec)
                str_an = self._getStrAN(autoneg)
                ## Accept anything > 0 to enable FC
                fc_enable = False if flowctrl == 0 else True
                self.bfgc.port_table.entry_add(
                    self.bfgc.target,
                    [self.bfgc.port_table.make_key([gc.KeyTuple("$DEV_PORT", port_id)])],
                    [
                        self.bfgc.port_table.make_data(
                            [
                                gc.DataTuple("$SPEED", str_val=str_port_speed),
                                gc.DataTuple("$FEC", str_val=str_fec),
                                gc.DataTuple("$AUTO_NEGOTIATION", str_val=str_an),
                                gc.DataTuple("$PORT_ENABLE", bool_val=False),
                                gc.DataTuple("$TX_PAUSE_FRAME_EN", bool_val=fc_enable),
                                gc.DataTuple("$RX_PAUSE_FRAME_EN", bool_val=fc_enable),
                            ]
                        )
                    ],
                )
        except Exception as e:
            self.controlPlaneMsg(
                "%s:%s - Error in adding port [%s] with parameters:[%s,%s,%s,%s]: %s"
                % (
                    self.class_name,
                    method_name,
                    port_id,
                    port_speed,
                    fec,
                    autoneg,
                    flowctrl,
                    e
                )
            )
        else:
            logger.warning("Port[%s] added", port_id)
            self.active_ports[port_id] = True
            
    elif mode == 3:
        if port_id not in self.active_ports:
            self.controlPlaneMsg("Attempting to delete inactive port [%s]" % port_id)
            return
            
        try:
            if platform == "stordis_bf2556x_1t" and port_id in SAL_PORT_ID:
                sal_port = SAL_PORT_ID[port_id]
                self.salgc.DelPort(port_num=sal_port, lane=0)
            else:
                self.bfgc.port_table.entry_del(
                    self.bfgc.target,
                    [self.bfgc.port_table.make_key([gc.KeyTuple("$DEV_PORT", port_id)])],
                )
        except Exception as e:
            self.controlPlaneMsg(
                "%s:%s - Error in deleting port [%s]: %s"
                % (self.class_name, method_name, port_id, e)
            )
        else:
            logger.warning("Port[%s] deleted", port_id)
            del self.active_ports[port_id]
    else:
        logger.warning("ports: mod operation not supported")
