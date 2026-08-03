from ..bf_gbl_env.cst_env import *
import re

## Supported port speeds by Tofino family.  Some speeds support more
## than one set of serdes lanes.  The "port_speed" parameter passed to
## us from freertr is a string of the form "<speed>[:<lanes>]", where
## <speed> and <lanes> must be integers, and lanes must be 1, 2, 4 or
## 8.  If the lanes parameter is omitted, the following defaults
## apply: 1:1, 10:1, 25:1, 40:4, 50:2, 100:4, 200:4, 400:8. The
## defaults are applied implicitly by bf_switchd when the $N_LANES
## entry in the $PORTS table is omitted.
PORT_SPEEDS = {
    "tofino": {
        "1": [1],
        "10": [1],
        "25": [1],
        "40": [4],
        "50": [2],
        "100": [4]
    },
    "tofino2": {
        "1": [1],
        "10": [1],
        "25": [1],
        ## Note: Tofio2 also supports the non-standard 40G config as
        ## 2x20, but it appears that it is not configurable via BFRT.
        "40": [4],
        "50": [1, 2],
        "100": [2, 4],
        "200": [4, 8],
        "400": [8]
    },
}

def managePort(self, platform, mode, port_id, port_speed=10,
               fec=0, autoneg=1, flowctrl=0):
    method_name = inspect.currentframe().f_code.co_name

    if mode == WRITE:
        ## speed is a string of the form "<speed>[:<lanes>]". <speed>
        ## and <lanes> must be integers, lanes must be 1, 2, 4 or 8.
        result = re.fullmatch(r'^([0-9]+)(?:|:([1,2,4,8]))', port_speed)
        if result is None:
            self.controlPlaneMsg(
                "%s - Error in adding port [%s], invalid speed: %s, expected <speed>[:<lanes>]"
                % (self.class_name, port_id, port_speed)
            )
            return
        (speed, lanes) = result.groups()
        port_speeds = PORT_SPEEDS[self.bfgc.arch]
        if self._checkParamCoherence(int(speed), fec, autoneg, flowctrl) == False:
            self.controlPlaneMsg(
                "%s - Error in adding port [%s], inconsistent parameters:[%s,%s,%s,%s]"
                % (self.class_name, port_id, speed, fec, autoneg, flowctrl)
            )
            return

        try:
            if self.haveSAL() and port_id in SAL_PORT_ID:
                new_speed = self.salgc.speedTnaToSal(int(speed))
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
                if speed in port_speeds:
                    str_port_speed = "BF_SPEED_%sG" % (speed)
                    if lanes is not None and int(lanes) not in port_speeds[speed]:
                        self.controlPlaneMsg(
                            "%s - Error in adding port [%s], invalid lanes for speed %s: %s, expected %s"
                            % (self.class_name, port_id, speed, lanes, port_speeds[speed])
                        )
                        return
                else:
                    self.controlPlaneMsg("Unsupported port speed: %s, expected %s" %
                                         (speed, list(port_speeds.keys())))
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
                            ] + ([
                                gc.DataTuple("$N_LANES", val=int(lanes))
                            ] if lanes is not None else [])
                        )
                    ],
                )
            self.bfgc.metadata_table.entry_add(
                self.bfgc.target,
                [self.bfgc.metadata_table.make_key([gc.KeyTuple("ig_intr_md.ingress_port", port_id)])],
                [
                    self.bfgc.metadata_table.make_data(
                        [
                            gc.DataTuple("portid", port_id),
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
                    speed,
                    fec,
                    autoneg,
                    flowctrl,
                    e
                )
            )
        else:
            logger.warning("Port[%s] added", port_id)
            self.active_ports[port_id] = True

    elif mode == DELETE:
        if port_id not in self.active_ports:
            self.controlPlaneMsg("Attempting to delete inactive port [%s]" % port_id)
            return

        try:
            if self.haveSAL() and port_id in SAL_PORT_ID:
                sal_port = SAL_PORT_ID[port_id]
                self.salgc.DelPort(port_num=sal_port, lane=0)
            else:
                self.bfgc.port_table.entry_del(
                    self.bfgc.target,
                    [self.bfgc.port_table.make_key([gc.KeyTuple("$DEV_PORT", port_id)])],
                )
            self.bfgc.metadata_table.entry_del(
                self.bfgc.target,
                [self.bfgc.metadata_table.make_key([gc.KeyTuple("ig_intr_md.ingress_port", port_id)])],
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
