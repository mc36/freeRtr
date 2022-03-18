from ..bf_gbl_env.cst_env import *

def _Exception():
    exc_type, exc_obj, tb = sys.exc_info()
    f = tb.tb_frame
    lineno = tb.tb_lineno
    filename = f.f_code.co_filename
    linecache.checkcache(filename)
    line = linecache.getline(filename, lineno, f.f_globals)
    return 'EXCEPTION IN ({}, LINE {} "{}"): {}'.format(
        filename, lineno, line.strip(), exc_obj
    )

def setPortAdmStatus(self, platform, port_id, adm_status=0):
    method_name = inspect.currentframe().f_code.co_name

    if port_id not in self.active_ports:
        self.controlPlaneMsg("Attempting to set administrative state for inactive port [%s]" % port_id)
        return

    port_enable = True if adm_status == 1 else False
    port_enable_str = "up" if port_enable else "down"
    try:
        if platform == "stordis_bf2556x_1t" and port_id in SAL_PORT_ID:
            sal_port = SAL_PORT_ID[port_id]
            self.salgc.EnablePort(
                sal_port,
                lane=0,
                new_enb=port_enable
            )
        else:
            self.bfgc.port_table.entry_mod(
                self.bfgc.target,
                [self.bfgc.port_table.make_key([gc.KeyTuple("$DEV_PORT", port_id)])],
                [
                    self.bfgc.port_table.make_data(
                        [
                            gc.DataTuple("$PORT_ENABLE", bool_val=port_enable),
                        ]
                    )
                ],
            )
    except Exception as e:
        self.controlPlaneMsg(
            "%s:%s - Error setting admin state of port [%s] to %s: %s"
            % (
                self.class_name,
                method_name,
                port_id,
                port_enable_str,
                e
            )
        )
        self.controlPlaneMsg(
            "%s:%s - with code [%s]" % (self.class_name, method_name, _Exception())
        )
    else:
        logger.warning("Port[%s] administrative status set to %s" %
                     (port_id, port_enable_str))
